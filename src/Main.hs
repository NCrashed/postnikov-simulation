{-# LANGUAGE RecursiveDo, RecordWildCards #-}
module Main where

import Simulation.Types
import Simulation.Analytic
import Simulation.Process
import Graphics.Vty.Widgets.All
import qualified Data.Text as T
import Control.Monad
import System.Exit (exitSuccess)
import Data.IORef

main :: IO ()
main = gui

editingScreen :: Collection -> (Input -> IO ()) -> IO () -> IO (IO ())
editingScreen collection okAction cancelAction = do
  fg <- newFocusGroup
  tbl <- newTable [column ColAuto, column ColAuto] BorderFull
  afterQueryTimeEdit <- labeledText fg tbl "Время дообработки запроса" "80"
  formQueryTimeEdit <- labeledText fg tbl "Время формирования запроса" "80"
  sendingTimeEdit <- labeledText fg tbl "Время отправки по каналу" "10"
  serversCountEdit <- labeledText fg tbl "Кол-во серверов" "2"
  workstationsCountEdit <- labeledText fg tbl "Кол-во станций" "8"
  processorsCountEdit <- labeledText fg tbl "Кол-во процессоров"  "1"
  processoringTimeEdit <- labeledText fg tbl "Время обработки ЦПУ" "10"
  diskingTimeEdit <- labeledText fg tbl "Время обработки диском" "1"
  disksCountEdit <- labeledText fg tbl "Кол-во дисков" "1"
  requeryChanceEdit <- labeledText fg tbl "Вероятность повтора" "0"
  
  title <- hCentered =<< plainText "Enter model parameters"
  
  bOk <- newButton "OK"
  void $ addToFocusGroup fg (buttonWidget bOk)
  bOk `onButtonPressed` \_ -> do
    afterQueryTime' <- getEditText afterQueryTimeEdit
    formQueryTime' <- getEditText formQueryTimeEdit
    sendingTime' <- getEditText sendingTimeEdit 
    serversCount' <- getEditText serversCountEdit
    workstationsCount' <- getEditText workstationsCountEdit
    processorsCount' <- getEditText processorsCountEdit
    processoringTime' <- getEditText processoringTimeEdit
    diskingTime' <- getEditText diskingTimeEdit
    disksCount' <- getEditText disksCountEdit
    requeryChance' <- getEditText requeryChanceEdit
    
    okAction Input {
      workstationsCount = read $ T.unpack workstationsCount'
    , afterQueryTime = read $ T.unpack afterQueryTime'
    , formQueryTime = read $ T.unpack formQueryTime'
    , serversCount = read $ T.unpack serversCount'
    , processorsCount = read $ T.unpack processorsCount'
    , disksCount = read $ T.unpack disksCount'
    , sendingTime = read $ T.unpack sendingTime'
    , processoringTime = read $ T.unpack processoringTime'
    , diskingTime = read $ T.unpack diskingTime'
    , requeryChance = read $ T.unpack requeryChance'
    }
    
  bCancel <- newButton "Exit"
  void $ addToFocusGroup fg (buttonWidget bCancel)
  bCancel `onButtonPressed` const cancelAction
  
  screen <- return title 
    <--> return tbl 
    <--> (hCentered =<< (return (buttonWidget bOk) <++> return (buttonWidget bCancel)))
    
  addToCollection collection screen fg
  where
    labeledText fg tbl t1 t2 = do
      e <- editWidget
      setEditText e t2
      void $ addToFocusGroup fg e
      wt <- plainText t1
      addRow tbl $ wt .|.  e
      return e

resultsScreen :: Collection -> IO () -> IO () -> IO (IO (), Input -> IO ())
resultsScreen collection backAction exitAction = do
  fg <- newFocusGroup
  tbl <- newTable [column ColAuto, column ColAuto, column ColAuto, column ColAuto] BorderFull
  title0 <- plainText "Параметр модели"
  title1 <- plainText "Аналитическая модель"
  title2 <- plainText "Имитационная модель"
  title3 <- plainText "Разница моделей"
  addRow tbl $ title0 .|. title1 .|. title2 .|. title3
  (al1, sl1, dl1) <- row tbl "Время отклика системы"
  (al8, sl8, dl8) <- row tbl "Время цикла системы"
  (al2, sl2, dl2) <- row tbl "Загрузка рабочей станции"
  (al3, sl3, dl3) <- row tbl "Загрузка пользователя"
  (al4, sl4, dl4) <- row tbl "Загрузка канала"
  --(al5, sl5, dl5) <- row tbl "Загрузка сервера"
  (al6, sl6, dl6) <- row tbl "Загрузка дисков"
  (al7, sl7, dl7) <- row tbl "Загрузка процессора"
  (al9, sl9, dl9) <- row tbl "Кол-во работающих станций"
  (al10, sl10, dl10) <- row tbl "Кол-во станций форм. запрос"
  
  bBack <- newButton "Back"
  void $ addToFocusGroup fg (buttonWidget bBack)
  bBack `onButtonPressed` const backAction
  
  bRepeat <- newButton "Repeat"
  void $ addToFocusGroup fg (buttonWidget bRepeat)
  
  bExit <- newButton "Exit"
  void $ addToFocusGroup fg (buttonWidget bExit)
  bExit `onButtonPressed` const exitAction
  
  
  lastInput <- newIORef defaultInput
  let repeatHandler = \input -> do
      writeIORef lastInput input
      let analRes = simulateAnalytic input
      fillColumn input analRes al1 al2 al3 al4 al6 al7 al8 al9 al10
      simRes <- runSimulateProcess input
      fillColumn input simRes sl1 sl2 sl3 sl4 sl6 sl7 sl8 sl9 sl10
      return () 
      fillDifference input analRes simRes dl1 dl2 dl3 dl4 dl6 dl7 dl8 dl9 dl10
  bRepeat `onButtonPressed` (const $ repeatHandler =<< readIORef lastInput)
  
  screen <- return tbl <--> 
    (hCentered =<< (return (buttonWidget bBack) 
               <++> return (buttonWidget bRepeat) 
               <++> return (buttonWidget bExit)))
               
  switch <- addToCollection collection screen fg
  return (switch, repeatHandler)
  where
    row tbl tlabel = do
      cell1 <- plainText tlabel
      cell2 <- plainText ""
      cell3 <- plainText ""
      cell4 <- plainText ""
      addRow tbl $ cell1 .|. cell2 .|. cell3 .|. cell4
      return (cell2, cell3, cell4)
    
    fillColumn input Output{..} l1 l2 l3 l4 l6 l7 l8 l9 l10 = do
      setText l1 $ T.pack $ show $ averageResponse
      setText l2 $ T.pack $ show $ loadWorkstation
      setText l3 $ T.pack $ show $ loadUser
      setText l4 $ T.pack $ show $ loadCable
      --setText l5 $ T.pack $ show $ loadServer
      setText l6 $ T.pack $ show $ loadDisk
      setText l7 $ T.pack $ show $ loadCP
      setText l8 $ T.pack $ show $ averageResponse * 2
      setText l9 $ T.pack $ show $ loadWorkstation * fromIntegral (workstationsCount input)
      setText l10 $ T.pack $ show $ loadUser * fromIntegral (workstationsCount input)
      
    fillDifference input o1 o2 l1 l2 l3 l4 l6 l7 l8 l9 l10 = do
      let percent v1 v2 = T.pack $ show (100 * abs (v1 - v2) / v1) ++ "%"
      setText l1 $ percent (averageResponse o1) (averageResponse o2)
      setText l2 $ percent (loadWorkstation o1) (loadWorkstation o2)
      setText l3 $ percent (loadUser o1) (loadUser o2)
      setText l4 $ percent (loadCable o1) (loadCable o2)
      --setText l5 $ percent (loadServer o1) (loadServer o2)
      setText l6 $ percent (loadDisk o1) (loadDisk o2)
      setText l7 $ percent (loadCP o1) (loadCP o2)
      setText l8 $ percent (2*averageResponse o1) (2*averageResponse o2)
      setText l9 $ percent (loadWorkstation o1 * fromIntegral (workstationsCount input)) 
                           (loadWorkstation o2 * fromIntegral (workstationsCount input))
      setText l10 $ percent (loadUser o1 * fromIntegral (workstationsCount input)) 
                            (loadUser o2 * fromIntegral (workstationsCount input))
gui :: IO()
gui =  do

  c <- newCollection
  rec (goResults, runSim) <- resultsScreen c goEditing (void exitSuccess)
      goEditing <- editingScreen c (\input -> goResults >> runSim input) (void exitSuccess)
  
  schedule goEditing  
  runUi c defaultContext
  
defaultInput :: Input
defaultInput = Input {
  workstationsCount = 8
, afterQueryTime = 80
, formQueryTime = 80
, serversCount = 100
, processorsCount = 100
, disksCount = 100
, sendingTime = 10
, processoringTime = 10
, diskingTime = 10
, requeryChance = 0.1
}