module Main where

import Simulation.Types
import Simulation.Analytic
import Simulation.Process
import Graphics.Vty.Widgets.All
import Graphics.Vty.Input.Events
import qualified Data.Text as T
import Control.Monad
import System.Exit (exitSuccess)

main :: IO ()
main = gui
--  let outputAnal = simulateAnalytic defaultInput
--  outputSim <- runSimulateProcess defaultInput
--  print outputAnal
--  print outputSim

editingScreen :: Collection -> (Input -> IO ()) -> IO () -> IO (IO ())
editingScreen collection okAction cancelAction = do
  fg <- newFocusGroup
  tbl <- newTable [column (ColFixed 16), column ColAuto] BorderFull
  afterQueryTimeEdit <- labeledText fg tbl "afterQueryTime" "80"
  formQueryTimeEdit <- labeledText fg tbl "formQueryTime" "80"
  sendingTimeEdit <- labeledText fg tbl "sendingTime" "10"
  serversCountEdit <- labeledText fg tbl "serversCount" "100"
  workstationsCountEdit <- labeledText fg tbl "workstationsCount" "100"
  processorsCountEdit <- labeledText fg tbl "processorsCount"  "100"
  processoringTimeEdit <- labeledText fg tbl "processoringTime" "10"
  diskingTimeEdit <- labeledText fg tbl "diskingTime" "10"
  disksCountEdit <- labeledText fg tbl "disksCount" "100"
  requeryChanceEdit <- labeledText fg tbl "requeryChance" "0.1"
  
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
    
  bCancel <- newButton "Cancel"
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
      
gui :: IO()
gui =  do

  c <- newCollection
  _ <- editingScreen c (const $ return ()) (void exitSuccess)
    
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