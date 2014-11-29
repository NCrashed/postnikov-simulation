module Simulation.Utils where

import Simulation.Aivika
import Simulation.Aivika.Queue

type Buffer a = FCFSQueue a 
type BufferedProcess a = (Buffer a, Process ())

buffersCapacity :: Int
buffersCapacity = 100

newBuffer :: Event (Buffer a)
newBuffer = newFCFSQueue buffersCapacity

-- | Хелпер, который задерживает процесс по экспонециальному закону и возвращает время
holdExp :: Float -> Process Double
holdExp t = do
  htime <- liftParameter (randomExponential $ fromRational $ toRational t)
  holdProcess htime
  return htime

-- | Хелпер, который создает входной буффер процесса и пихает в его обработчик
processWithBuffer :: (Buffer a -> Process ()) -> Event (BufferedProcess a)
processWithBuffer action = do
   enterBuf <- newBuffer 
   return (enterBuf, action enterBuf)

-- | Хелпер, который выбирает с указанной вероятностью ветку then
randomChoice :: Float -> Process a -> Process a -> Process a
randomChoice succChance th els = do
  res <- liftParameter $ randomTrue (fromRational . toRational $ succChance)
  if res then th else els

-- | Тоже самое, что replicateM, но с индеском
replicateWithIndexM :: Monad m => Int -> (Int -> m a) -> m [a] 
replicateWithIndexM n f 
  | n <= 0 = return []
  | otherwise = sequence $ go 0 
  where go i 
          | i >= n = []
          | otherwise = f i : go (i+1)