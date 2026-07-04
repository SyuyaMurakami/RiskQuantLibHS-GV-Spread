module Spline (
    cubicSpline
) where

import qualified Data.Vector.Strict as V
import qualified Data.Vector.Generic.Mutable as VM 

cubicSpline :: V.Vector Double -> V.Vector Double -> Double -> Maybe Double
cubicSpline xs ys xNew
  | V.length xs < 3 = Nothing
  | V.length xs /= V.length ys = Nothing
  | xNew < V.head xs || xNew > V.last xs = Nothing
  | otherwise = Just (term1 + term2 + term3 + term4)
    where
      n = V.length xs - 1
      h = V.zipWith (-) (V.tail xs) xs
      a = V.generate (n-1) (\i -> h V.! i)
      b = V.generate (n-1) (\i -> 2 * (h V.! i + h V.! (i+1)))
      c = V.generate (n-1) (\i -> h V.! (i+1))
      d = V.generate (n-1) (\i ->
            6 * ((ys V.! (i+2) - ys V.! (i+1)) / (h V.! (i+1)) - 
                 (ys V.! (i+1) - ys V.! i) / (h V.! i)))
      mInternal = thomasAlgorithm a b c d
      m = V.concat [V.singleton 0.0, mInternal, V.singleton 0.0]
      i = findInterval xs xNew
      xi   = xs V.! i
      xip1 = xs V.! (i+1)
      yi   = ys V.! i
      yip1 = ys V.! (i+1)
      mi   = m V.! i
      mip1 = m V.! (i+1)
      hi   = h V.! i
      term1 = mip1 * (xNew - xi)**3 / (6 * hi)
      term2 = mi * (xip1 - xNew)**3 / (6 * hi)
      term3 = (yip1 / hi - hi * mip1 / 6) * (xNew - xi)
      term4 = (yi / hi - hi * mi / 6) * (xip1 - xNew)

thomasAlgorithm :: V.Vector Double -> V.Vector Double -> V.Vector Double -> V.Vector Double -> V.Vector Double
thomasAlgorithm a b c d = V.create $ do
  let n = V.length b
  c' <- V.thaw c
  d' <- V.thaw d
  VM.write c' 0 ( (c V.! 0) / (b V.! 0) )
  VM.write d' 0 ( (d V.! 0) / (b V.! 0) )
  
  let loop1 i
        | i == n = return ()
        | otherwise = do
            c'prev <- VM.read c' (i-1) 
            d'prev <- VM.read d' (i-1)
            let ai = a V.! (i-1)
                bi = b V.! i
                di = d V.! i
                denom = bi - ai * c'prev
            VM.write c' i ((if i < n-1 then c V.! i else 0) / denom)
            VM.write d' i ((di - ai * d'prev) / denom)
            loop1 (i+1)
  loop1 1

  res <- VM.new n
  d'last <- VM.read d' (n-1)
  VM.write res (n-1) d'last
  
  let loop2 i
        | i < 0 = return ()
        | otherwise = do
            c'i <- VM.read c' i
            d'i <- VM.read d' i
            resNext <- VM.read res (i+1)
            VM.write res i (d'i - c'i * resNext)
            loop2 (i-1)
  loop2 (n-2)
  
  return res

findInterval :: V.Vector Double -> Double -> Int
findInterval xs xNew = go 0
  where
    go i
      | i >= V.length xs - 2 = i
      | xNew <= xs V.! (i+1) = i
      | otherwise            = go (i+1)
