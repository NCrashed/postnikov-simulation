module Main where

import Simulation.Types
import Simulation.Analytic
import Simulation.Process
import Graphics.Vty.Widgets.All
import qualified Data.Text as T
import Control.Monad

main :: IO()
main =  do
  e <- editWidget
  ui <- centered e
  
  fg <- newFocusGroup
  void $ addToFocusGroup fg e
  
  c <- newCollection
  void $ addToCollection c ui fg
  
  e `onActivate` (getEditText >=>
   (error . ("You entered: " ++) . T.unpack))
    
  runUi c defaultContext

inputs :: [Input]
inputs = [input1, input2, input3, input4, input5]

defaultInput :: Input
defaultInput = Input {
  worksationsCount = 8
, afterQueryTime = 80
, formQueryTime = 80
, serversCount = 5
, processorsCount = 1
, disksCount = 1
, sendingTime = 10
, processoringTime = 10
, diskingTime = 10
, requeryChance = 0.1
}

input1 :: Input
input1 = defaultInput {
  afterQueryTime = 80
, formQueryTime = 80
, sendingTime = 10
, processoringTime = 10 
}

input2 :: Input
input2 = defaultInput {
  afterQueryTime = 160
, formQueryTime = 160
, sendingTime = 20
, processoringTime = 20 
}

input3 :: Input
input3 = defaultInput {
  afterQueryTime = 240
, formQueryTime = 240
, sendingTime = 20
, processoringTime = 20 
}

input4 :: Input
input4 = defaultInput {
  afterQueryTime = 160
, formQueryTime = 160
, sendingTime = 10
, processoringTime = 10 
}

input5 :: Input
input5 = defaultInput {
  afterQueryTime = 80
, formQueryTime = 80
, sendingTime = 20
, processoringTime = 20 
}