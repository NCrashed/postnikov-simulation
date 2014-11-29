module Simulation.Types(
  Input(..)
, Output(..)
  ) where
  
data Input = Input {
    afterQueryTime :: Float -- ^ среднее время дообработки на рабочей станции
  , formQueryTime :: Float -- ^ среднее время формирования запроса от рабочей станции
  , sendingTime :: Float -- ^ среднее время передачи запроса по каналу
  , serversCount :: Int -- ^ Число серверов
  , workstaionsCount :: Int -- ^ Число рабочих станций
  , processorsCount :: Int -- ^ Число процессоров
  , processoringTime :: Float -- ^ среднее время обработки в ЦП сервера
  , diskingTime :: Float -- ^ среднее время дообработки в диске сервера
  , disksCount :: Int -- ^ Количество процессоров
  , requeryChance :: Float -- ^ вероятность обращения запроса 
  } deriving Read

data Output = Output {
    averageResponse :: Float -- ^ среднее время реакции системы
  , loadWorkstation :: Float -- ^ коэффициент загрузки рабочей станции
  , loadUser :: Float -- ^ коэффициент загрузки пользователя
  , loadCable :: Float -- ^ коэффициент загрузки канала передачи данных
  , loadServer :: Float -- ^ коэффициент загрузки сервера
  , loadDisk :: Float -- ^ коэффициент загрузки диска сервера
  , loadCP :: Float -- ^ коэффициент загрузки процессора сервера
  } deriving Show