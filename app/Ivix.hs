module Ivix where

import Data.Time (Day, parseTimeM, defaultTimeLocale, diffDays, formatTime)
import Data.Text (Text, unpack, pack)
import qualified Data.Vector.Strict as V
import qualified RiskQuantLib.Operation.Vector as OPV

import Spline (cubicSpline)

dayCounter :: Double
dayCounter = 365

shiborLen :: Int
shiborLen = 8

shiborT :: V.Vector Double
shiborT = V.map (/360.0) $ V.fromList [1.0, 7.0, 14.0, 30.0, 90.0, 180.0, 270.0, 360.0]

parseDate :: String -> Text -> Maybe Day
parseDate fmt txt = parseTimeM True defaultTimeLocale fmt (unpack txt)

formatDay :: String -> Day -> Text
formatDay fmt day = pack $ formatTime defaultTimeLocale fmt day

calculateT :: Day -> Day -> Double
calculateT start end = (fromIntegral $ diffDays end start) / dayCounter

calculateRf :: V.Vector Day -> Day -> V.Vector Double -> V.Vector (Maybe Double)
calculateRf optionsExpDates vixDate shiborRate
  | V.length shiborRate /= shiborLen = V.replicate shiborLen Nothing
  | otherwise = V.map (\i -> (/100) <$> cubicSpline shiborT shiborRate i) optionsExpTAdj
    where
      optionsExpT = V.map (calculateT vixDate) optionsExpDates
      (minT, maxT) = (V.head shiborT, V.last shiborT)
      optionsExpTAdj = V.map (\i -> if i > maxT then maxT else if i < minT then minT else i) optionsExpT

getNearNextOptExpDate :: V.Vector Day -> Day -> Maybe (Day, Day)
getNearNextOptExpDate optionsExpDates vixDate = do
  let optionsExpDatesUS = V.uniq . OPV.sortAsc . V.filter (\i -> diffDays i vixDate >=1) $ optionsExpDates
  let optionsExpDatesLen = V.length optionsExpDatesUS
  case optionsExpDatesLen of
    0 -> Nothing
    1 -> Nothing
    _ -> Just (optionsExpDatesUS V.! 0, optionsExpDatesUS V.! 1)

getStrikeATM :: V.Vector Double -> V.Vector Double -> Maybe Double
getStrikeATM strike price
  | strikeLen == 0 || priceLen == 0 || strikeLen /= priceLen = Nothing
  | otherwise = Just strikeMin
  where
    strikeLen = V.length strike
    priceLen = V.length price
    priceGroup = V.map (\g -> (fst (V.head g), abs $ snd (V.head g) - snd (V.last g))) . V.filter (\i -> V.length i == 2) . OPV.groupBy fst $ V.zip strike price
    priceDiffMin = V.minimum $ V.map snd priceGroup
    strikeMin = fst . V.head $ V.filter (\i -> snd i == priceDiffMin) priceGroup

calculateDeltaDiff :: Int -> V.Vector Double -> Int -> Double
calculateDeltaDiff len strike i
  | i == 0 = strike V.! 1 - strike V.! 0
  | i == len - 1 = strike V.! (len-1) - strike V.! (len-2)
  | otherwise = (strike V.! (i+1) - strike V.! (i-1)) / 2.0

calculateDeltaK :: Int -> V.Vector Double -> V.Vector Double
calculateDeltaK len strike
  | len < 2 = V.empty
  | len == 2 = V.replicate 2 $ strike V.! 1 - strike V.! 0
  | otherwise = V.generate len $ calculateDeltaDiff len strike

calculateComponent :: (Double, Double, Double) -> Double
calculateComponent (strike, price, deltaK) = price * deltaK / (strike * strike) 

calculateComponentGeneral :: Double -> Double -> Double -> (Double, Double, Double) -> Double
calculateComponentGeneral forwardIndexPrice rf t (strike, price, deltaK) = price * deltaK * (1 + log forwardIndexPrice - rf * t - log strike) / (strike * strike) 

calculateSigma :: V.Vector Double -> Double -> Double -> Double -> Double -> Maybe Double
calculateSigma componentVec k0 forwardIndexPrice rf t
  | V.null componentVec = Nothing
  | otherwise = Just $ V.sum componentVec * exp (t * rf) * 2 / t - (forwardIndexPrice / k0 - 1) ** 2 / t

calculateSigmaGeneral :: V.Vector Double -> Double -> Double -> Double -> Double -> Double -> Maybe Double
calculateSigmaGeneral componentVec k0 forwardIndexPrice rf t muT
  | V.null componentVec = Nothing
  | otherwise = Just sigmaGeneral
  where
    sigmaRaw = V.sum componentVec * exp (t * rf) * 2
    sigma = sigmaRaw / t - (forwardIndexPrice / k0 - 1) ** 2 / t
    lnK0S0 = rf * t - log (forwardIndexPrice / k0)
    vegaT = lnK0S0 ** 2 + 2 * lnK0S0 * (forwardIndexPrice / k0 - 1) + sigmaRaw
    sigmaGeneral = (vegaT - muT ** 2) / t

calculateSigmaSquare :: V.Vector Double -> V.Vector Double -> V.Vector Double -> V.Vector Double -> Double -> Double -> Double -> Maybe Double
calculateSigmaSquare strikeCall priceCall strikePut pricePut forwardIndexPrice rf t
  | strikeCallLen == 0 || priceCallLen == 0 || strikePutLen == 0 || pricePutLen == 0 || strikeCallLen /= priceCallLen || strikePutLen /= pricePutLen = Nothing
  | otherwise = sigma
  where
    (strikeCallLen, priceCallLen, strikePutLen, pricePutLen) = (V.length strikeCall, V.length priceCall, V.length strikePut, V.length pricePut)
    (deltaKCall, deltaKPut) = (calculateDeltaK strikeCallLen strikeCall, calculateDeltaK strikePutLen strikePut)
    (callPack, putPack) = (V.zip3 strikeCall priceCall deltaKCall, V.zip3 strikePut pricePut deltaKPut)
    k0Half = V.filter (<forwardIndexPrice) . OPV.sortAsc $ strikePut V.++ strikeCall
    k0 = if V.null k0Half then forwardIndexPrice else V.last k0Half
    (callOTM, putOTM) = (V.filter (\(k, _, _) -> k > k0) callPack, V.filter (\(k, _, _) -> k < k0) putPack)
    (callATM, putATM) = (V.filter (\(k, _, _) -> k == k0) callPack, V.filter (\(k, _, _) -> k == k0) putPack)
    (callComponent, putComponent) = (V.map calculateComponent callOTM, V.map calculateComponent putOTM)
    middleComponent = V.singleton $ case (V.null callATM, V.null putATM) of
      (False, False) -> (V.sum (V.map calculateComponent callATM) + V.sum (V.map calculateComponent putATM)) / 2.0
      (False, True) -> V.sum (V.map calculateComponent callATM)
      (True, False) -> V.sum (V.map calculateComponent putATM)
      (True, True) -> 0
    component = V.filter (/=0) $ putComponent V.++ middleComponent V.++ callComponent
    sigma = if V.null component then Nothing else calculateSigma component k0 forwardIndexPrice rf t

calculateSigmaSquareGeneral :: V.Vector Double -> V.Vector Double -> V.Vector Double -> V.Vector Double -> Double -> Double -> Double -> (Maybe Double, Maybe Double)
calculateSigmaSquareGeneral strikeCall priceCall strikePut pricePut forwardIndexPrice rf t
  | strikeCallLen == 0 || priceCallLen == 0 || strikePutLen == 0 || pricePutLen == 0 || strikeCallLen /= priceCallLen || strikePutLen /= pricePutLen = (Nothing, Nothing)
  | otherwise = (sigma, sigmaGeneral)
  where
    (strikeCallLen, priceCallLen, strikePutLen, pricePutLen) = (V.length strikeCall, V.length priceCall, V.length strikePut, V.length pricePut)
    (deltaKCall, deltaKPut) = (calculateDeltaK strikeCallLen strikeCall, calculateDeltaK strikePutLen strikePut)
    (callPack, putPack) = (V.zip3 strikeCall priceCall deltaKCall, V.zip3 strikePut pricePut deltaKPut)
    k0Half = V.filter (<forwardIndexPrice) . OPV.sortAsc $ strikePut V.++ strikeCall
    k0 = if V.null k0Half then forwardIndexPrice else V.last k0Half
    (callOTM, putOTM) = (V.filter (\(k, _, _) -> k > k0) callPack, V.filter (\(k, _, _) -> k < k0) putPack)
    (callATM, putATM) = (V.filter (\(k, _, _) -> k == k0) callPack, V.filter (\(k, _, _) -> k == k0) putPack)
    (callComponent, putComponent) = (V.map calculateComponent callOTM, V.map calculateComponent putOTM)
    (callComponentGeneral, putComponentGeneral) = (V.map (calculateComponentGeneral forwardIndexPrice rf t) callOTM, V.map (calculateComponentGeneral forwardIndexPrice rf t) putOTM)
    middleComponent = V.singleton $ case (V.null callATM, V.null putATM) of
      (False, False) -> (V.sum (V.map calculateComponent callATM) + V.sum (V.map calculateComponent putATM)) / 2.0
      (False, True) -> V.sum (V.map calculateComponent callATM)
      (True, False) -> V.sum (V.map calculateComponent putATM)
      (True, True) -> 0
    middleComponentGeneral = V.singleton $ case (V.null callATM, V.null putATM) of
      (False, False) -> (V.sum (V.map (calculateComponentGeneral forwardIndexPrice rf t) callATM) + V.sum (V.map (calculateComponentGeneral forwardIndexPrice rf t) putATM)) / 2.0
      (False, True) -> V.sum (V.map (calculateComponentGeneral forwardIndexPrice rf t) callATM)
      (True, False) -> V.sum (V.map (calculateComponentGeneral forwardIndexPrice rf t) putATM)
      (True, True) -> 0
    component = V.filter (/=0) $ putComponent V.++ middleComponent V.++ callComponent
    componentGeneral = V.filter (/=0) $ putComponentGeneral V.++ middleComponentGeneral V.++ callComponentGeneral
    sigma = if V.null component then Nothing else calculateSigma component k0 forwardIndexPrice rf t
    muT = (\s -> (-1) * (s * t / 2 - rf * t)) <$> sigma
    sigmaGeneral = if V.null componentGeneral then Nothing else muT >>= calculateSigmaGeneral componentGeneral k0 forwardIndexPrice rf t

calculateForwardIndexPrice :: Double -> Double -> Double -> Double -> Double -> Double
calculateForwardIndexPrice strike priceCall pricePut rf t = strike + exp (t * rf) * (priceCall - pricePut)

calculateVix :: Double -> Double -> Double -> Double -> Double
calculateVix tNear tNext sigmaNear sigmaNext = 100 * sqrt ( abs vix30 * dayCounter / 30.0)
  where
    w = (tNext - 30.0 / dayCounter) / (tNext - tNear)
    vix30 = tNear * w * sigmaNear + tNext * (1 - w) * sigmaNext
    
