module Simulation.Utils where

import Data.Functor
import Simulation.Aivika
import Simulation.Aivika.Queue
import Control.Monad

type Buffer a = FCFSQueue a 
type BufferedProcess a = (Buffer a, Process (), Ref Double)

buffersCapacity :: Int
buffersCapacity = 10000

newBuffer :: Event (Buffer a)
newBuffer = newFCFSQueue buffersCapacity

-- | Хелпер, который задерживает процесс по экспонециальному закону и возвращает время
holdExp :: Float -> Process Double
holdExp t = do
  htime <- liftParameter (randomExponential $ fromRational $ toRational t)
  holdProcess htime
  return htime

-- | Тоже самое, что и busyRef, но добавляет значение времени в переменную
holdExpStat :: Float -> Ref Double -> Process ()
holdExpStat t ref = do
  v <- holdExp t
  addToRef ref v
  
-- | Хелпер, который создает входной буффер процесса и пихает в его обработчик
processWithBuffer :: (Buffer a -> Ref Double -> Process ()) -> Event (BufferedProcess a)
processWithBuffer action = do
   enterBuf <- newBuffer 
   ref <- liftSimulation $ newRef 0
   return (enterBuf, forever $ action enterBuf ref, ref)

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
          
-- | Добавляет к переменной значение 
addToRef :: Num a => Ref a -> a -> Process ()
addToRef ref t = liftEvent $ modifyRef ref (+t)

-- | Вычисление среднего из списка переменных
averageRef :: Fractional a => [Ref a] -> Event a
averageRef refs = (/ fromIntegral (length refs)) <$> foldM (\acc r -> (acc +) <$> readRef r) 0 refs

average :: Fractional a => [a] -> a
average vals = sum vals / fromIntegral (length vals)

-- | Аналог zip для переменных
zipRefs :: Fractional a => (a -> b -> c) -> [Ref a] -> [Ref b] -> Event [c]
zipRefs f refsa refsb = forM (zip refsa refsb) $ \(ra,rb) -> do
  a <- readRef ra
  b <- readRef rb
  return $ f a b  