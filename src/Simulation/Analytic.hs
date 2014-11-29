{-# LANGUAGE RecordWildCards #-}
module Simulation.Analytic(
    Input(..)
  , Output(..)
  , simulate
  ) where
  
import Simulation.Types

simulate :: Input -> Output
simulate Input{..} = let finalLamda = until stopCond nextLamda startLamda
  in Output {
    averageResponse = responseTime finalLamda
  , loadWorkstation = (afterQueryTime + formQueryTime) / responseTime finalLamda
  , loadUser = formQueryTime / responseTime finalLamda
  , loadCable = 2 * finalLamda * sendingTime
  , loadServer = beta * finalLamda * diskP * (processoringTime + diskingTime)
  , loadDisk = beta * finalLamda * diskP * diskingTime
  , loadCP = beta * finalLamda * processoringTime / fprocCount / fservCount
  }
  where
  startLamda = k1 * minimum [lsending, lprocess, ldisking] * kd
  k1 = 0.9995
  beta = 1 / (1 - requeryChance)
  diskP = 1 / fromIntegral disksCount / fservCount
  lsending = 1/(2*sendingTime)
  lprocess = fromIntegral serversCount * fromIntegral processorsCount/(beta*processoringTime)
  ldisking = 1/(beta*diskP*diskingTime)
  kd = (fworkCount - 1) / fworkCount
  fworkCount = fromIntegral worksationsCount
  fprocCount = fromIntegral processorsCount 
  fservCount = fromIntegral serversCount
  
  calcTk lamda = 2 * sendingTime / (1 - 2 * lamda * sendingTime)
  calcTcp lamda = beta * processoringTime / (1 - (beta * lamda * processoringTime / (fservCount*fprocCount)) ** (fservCount*fprocCount))
  calcTd lamda = beta * diskingTime / (1 - beta * diskP * lamda * diskingTime)
  
  responseTime lamda = afterQueryTime + formQueryTime + calcTk lamda + calcTcp lamda + calcTd lamda
  newLamda lamda = (fworkCount + 1) / responseTime lamda
  nextLamda lamda = lamda - (lamda - newLamda lamda) / fromIntegral k2
  k2 = 10 :: Int
  delta = 0.00001
  stopCond lamda = abs (lamda - newLamda lamda) / lamda < delta  