
import RiskQuantLib.Tool.CSV as CSV
import RiskQuantLib.Operation.Graph as G
import RiskQuantLib.Operation.NodeList as NL
import RiskQuantLib.Operation.Mix as MI
import RiskQuantLib.Operation.Series as SR
import RiskQuantLib.Operation.Vector as OPV
import RiskQuantLib.Attribute.Value as AV

import Control.Monad (guard)
import Data.Vector.Strict as V
import Control.Concurrent.Async (concurrently, concurrently_)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (fromMaybe)

import Ivix as IV

cores :: Int
cores = 16

shiborCol :: [String]
shiborCol = ["1D", "1W", "2W", "1M", "3M", "6M", "9M", "1Y"]

calSigmaSquare :: Graph -> Double -> Double -> IO (Graph, Graph)
calSigmaSquare opt rf t = do
  res <- runMaybeT $ do
    lift $ mapAtS ["EXE_PRICE", "CLOSE"] asDouble opt
    optAdj <- lift $ MI.filter opt $ \i -> (\k p -> AV.notNan k && AV.notNan p) <$> i ..> "EXE_PRICE" <*> i ..> "CLOSE"
    optSort <- lift $ NL.sortS optAdj ["EXE_PRICE", "EXE_MODE"] True
    let action4 = MI.filter optSort $ \i -> (== fromString "认购") <$> i ..> "EXE_MODE"
    let action5 = MI.filter optSort $ \i -> (== fromString "认沽") <$> i ..> "EXE_MODE"
    (optCall, optPut) <- lift $ action4 `concurrently` action5
    (strikeCallG, priceCallG) <- lift $ (optCall `get` "EXE_PRICE") `concurrently` (optCall `get` "CLOSE")
    (strikePutG, pricePutG) <- lift $ (optPut `get` "EXE_PRICE") `concurrently` (optPut `get` "CLOSE")
    let (strikeCall, priceCall, strikePut, pricePut) = (MI.map toDouble strikeCallG, MI.map toDouble priceCallG, MI.map toDouble strikePutG, MI.map toDouble pricePutG)
    strikeATM <- MaybeT . return $ getStrikeATM (strikePut V.++ strikeCall) (pricePut V.++ priceCall)
    let priceCallATM = snd . V.head . V.filter (\(k, _) -> if k <= strikeATM + 1e-5 && k >= strikeATM - 1e-5 then True else False) $ V.zip strikeCall priceCall
    let pricePutATM = snd . V.head . V.filter (\(k, _) -> if k <= strikeATM + 1e-5 && k >= strikeATM - 1e-5 then True else False) $ V.zip strikePut pricePut
    let forwardIndexPrice = calculateForwardIndexPrice strikeATM priceCallATM pricePutATM rf t
    return $ calculateSigmaSquareGeneral strikeCall priceCall strikePut pricePut forwardIndexPrice rf t
  let (ss, ssg) = fromMaybe (Nothing, Nothing) res
  return $ (fromMaybe nan $ fromDouble <$> ss, fromMaybe nan $ fromDouble <$> ssg)

main :: IO ()
main = do
  shibor <- readCSV "data/shibor.csv"
  option <- readCSV "data/options.csv" >>= \df -> dropDuplicateAtS df ["DATE", "EXE_ENDDATE", "EXE_MODE", "EXE_PRICE", "CLOSE"]
  tradeDay <- readCSV "data/tradeday.csv" >>= \i -> sortPN cores i "DateTime" True
  let action0 = joinPN cores tradeDay option "option" $ \t o -> (==) <$> t ..> "DateTime" <*> o ..> "DATE"
  let action1 = joinPN cores tradeDay shibor "shibor" $ \t s -> (\t1 t2 -> parseDate "%Y/%m/%d" (toText t1) == parseDate "%Y-%m-%d" (toText t2)) <$> t ..> "DateTime" <*> s ..> "col-0"
  action0 `concurrently_` action1
  vixZip <- for tradeDay $ \d -> do
    fmap (fromMaybe (nan, nan)) . runMaybeT $ do
      opt <- lift $ d ..> "option"
      sbr <- lift $ d ..> "shibor"
      guard $ not (MI.null opt) && not (MI.null sbr)
      vixDay <- MaybeT $ parseDate "%Y/%m/%d" . toText <$> d ..> "DateTime"
      optionsExpDates <- lift $ opt `get` "EXE_ENDDATE" >>= return . sortAsc . V.mapMaybe id . MI.map (parseDate "%Y/%m/%d %H:%M" . toText) . unique 
      (dNear, dNext) <- MaybeT . return $ getNearNextOptExpDate optionsExpDates vixDay
      let action2 = MI.filter opt $ \i -> (\j -> parseDate "%Y/%m/%d %H:%M" (toText j) == Just dNear) <$> i ..> "EXE_ENDDATE"
      let action3 = MI.filter opt $ \i -> (\j -> parseDate "%Y/%m/%d %H:%M" (toText j) == Just dNext) <$> i ..> "EXE_ENDDATE"
      (optNear, optNext) <- lift $ action2 `concurrently` action3
      guard $ not (MI.null optNear) && not (MI.null optNext)
      let (tNear, tNext) = (calculateT vixDay dNear, calculateT vixDay dNext)
      sbrv <- lift $ sbr `getS` shiborCol >>= return . MI.map toDouble . SR.dropNan . MI.apply asDouble . MI.head . CSV.transpose
      let rfVec = calculateRf (V.fromList [dNear, dNext]) vixDay sbrv
      rfNear <- MaybeT . return $ V.head rfVec
      rfNext <- MaybeT . return $ V.last rfVec
      ((sigmaNear, sigmaNearGeneral), (sigmaNext, sigmaNextGeneral)) <- lift $ calSigmaSquare optNear rfNear tNear `concurrently` calSigmaSquare optNext rfNext tNext
      let vixValidated = AV.notNan sigmaNear && AV.notNan sigmaNext
      let vixGeneralValidated = AV.notNan sigmaNearGeneral && AV.notNan sigmaNextGeneral
      let vixValue = if vixValidated then Just (calculateVix tNear tNext (toDouble sigmaNear) (toDouble sigmaNext)) else Nothing
      let vixGeneralValue = if vixGeneralValidated then Just (calculateVix tNear tNext (toDouble sigmaNearGeneral) (toDouble sigmaNextGeneral)) else Nothing
      return $ (fromMaybe nan $ fromDouble <$> vixValue, fromMaybe nan $ fromDouble <$> vixGeneralValue)
  let (vix, gvix) = V.unzip vixZip
  let (vixVec, gvixVec) = (fromVector vix, fromVector gvix)
  let gvSpread = gvixVec - vixVec
  tradeDay `get` "DateTime" >>= \idx -> toCSV "result/ivix.csv" ["DATE", "IVIX", "GVIX", "GV-Spread"] $ AV.fromList [idx, vixVec, gvixVec, gvSpread]
  putStrLn "This is main."

