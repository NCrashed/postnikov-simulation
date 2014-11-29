{-# LANGUAGE RecordWildCards, TupleSections, RecursiveDo #-}
module Simulation.Process(
  simulateProcess
  ) where
  
import Data.Functor
import Simulation.Types
import Simulation.Aivika
import Simulation.Aivika.Queue
import Simulation.Utils
import Control.Monad

data Message = Query Double Int Int | Response Double Int
type MsgBuffer = Buffer Message
type MsgBuffered = BufferedProcess Message

simulateProcess :: Input -> Event ()
simulateProcess Input{..} = do
  rec clients <- constructClients cable
      servers <- constructServers cable
      cable <- constructCable clients servers
  liftSimulation $ runEventInStopTime $ return ()
  where
  
  constructClients :: MsgBuffer -> Event [MsgBuffer]
  constructClients cable = do
    replicateWithIndexM worksationsCount $ \i -> do
      (b1, p1) <- formQueryProcess i cable
      (b2, p2) <- afterQueryProcess b1
      liftSimulation $ do 
        runProcessInStartTime p1
        runProcessInStartTime p2
      return b2
  
  constructServers :: MsgBuffer -> Event [MsgBuffer]
  constructServers cable = do
    replicateM serversCount $ do
        rec (diskBufs, diskPs) <- unzip <$> replicateM disksCount (diskProcess serverBuff)
            (cpuBufs, cpuPs) <- unzip <$> replicateM processorsCount (cpuProcess diskBufs)
            (serverBuff, serverPs) <- serverProcess cable cpuBufs
        liftSimulation $ do
          mapM_ runProcessInStartTime diskPs
          mapM_ runProcessInStartTime cpuPs
          runProcessInStartTime serverPs 
        return serverBuff
  
  constructCable :: [MsgBuffer] -> [MsgBuffer] -> Event MsgBuffer
  constructCable clients servers = do
    (b, p) <- cableProcess clients servers
    liftSimulation $ runProcessInStartTime p
    return b
          
  -- | Процесс дообработки ответа
  afterQueryProcess :: MsgBuffer -> Event MsgBuffered
  afterQueryProcess clientBuf = processWithBuffer $ \enterBuf -> do
      msg  <- dequeue enterBuf
      _ <- holdExp afterQueryTime
      enqueue clientBuf msg
      
  -- | Процесс генерации запроса (пользовательский)
  formQueryProcess :: Int -> MsgBuffer -> Event MsgBuffered
  formQueryProcess clientId cableBuf = processWithBuffer $ \enterBuf -> do
    _ <- holdExp formQueryTime
    newMsg <- formNewQuery
    enqueue cableBuf newMsg
    void $ dequeue enterBuf
    where formNewQuery :: Process Message
          formNewQuery = do
            birthTime <- liftDynamics time
            distServer <- liftParameter $ randomUniformInt 0 (serversCount-1)
            return $ Query birthTime clientId distServer
  
  -- | Процесс, отвечающий за канал передачи данных
  cableProcess :: [MsgBuffer] -> [MsgBuffer] -> Event MsgBuffered
  cableProcess clients servers = processWithBuffer $ \enterBuf -> do
    msg <- dequeue enterBuf
    _ <- holdExp sendingTime
    case msg of
      Query _ _ server -> enqueue (servers !! server) msg
      Response _ client -> enqueue (clients !! client) msg
  
  -- | Процесс, отвечающий за процессор сервера
  cpuProcess :: [MsgBuffer] -> Event MsgBuffered
  cpuProcess disks = processWithBuffer $ \enterBuf -> do
    msg <- dequeue enterBuf
    _ <- holdExp processoringTime
    disk <- liftParameter $ randomUniformInt 0 (disksCount-1) 
    enqueue (disks !! disk) msg
  
  -- | Процесс, отвечающий за диск сервера
  diskProcess :: MsgBuffer -> Event MsgBuffered
  diskProcess server = processWithBuffer $ \enterBuf -> do
    msg <- dequeue enterBuf
    _ <- holdExp diskingTime
    randomChoice requeryChance (enqueue server msg) $ do
      let Query birthTime client _ = msg
      let response = Response birthTime client
      enqueue server response
      
  -- | Процесс, отвечающий за сервер    
  serverProcess :: MsgBuffer -> [MsgBuffer] -> Event MsgBuffered
  serverProcess cable cpus = processWithBuffer $ \enterBuf -> do
    msg <- dequeue enterBuf
    case msg of
      Query{} -> do
        cpui <- liftParameter $ randomUniformInt 0 (processorsCount-1)
        enqueue (cpus !! cpui) msg
      Response{} -> enqueue cable msg