{-# LANGUAGE RecordWildCards, TupleSections, RecursiveDo, DoAndIfThenElse #-}
module Simulation.Process(
  runSimulateProcess
  ) where
  
import Data.Functor
import Data.List
import Data.Array
import Simulation.Types
import Simulation.Aivika
import Simulation.Aivika.Queue
import Simulation.Utils
import Control.Monad

data Message = Query Double Int Int | Response Double Int | Requery Message
  deriving (Show, Eq, Ord)
  
type MsgBuffer = Buffer Message
type MsgBuffered = BufferedProcess Message

runSimulateProcess :: Input -> IO Output
runSimulateProcess = flip runSimulation specs . runEventInStartTime . simulateProcess
  where specs = Specs {
    spcStartTime = 0.0,
    spcStopTime = 100000.0,
    spcDT = 1.0,
    spcMethod = RungeKutta4,
    spcGeneratorType = SimpleGenerator 
  }
  
simulateProcess :: Input -> Event Output
simulateProcess Input{..} = do
  rec (clients, afterTimes, userTimes, respTimes) <- unzip4 <$> constructClients cable
      (servers, serverTimes, cpuTimes, diskTimes) <- unzip4 <$> constructServers cable
      (cable, cableTime) <- constructCable clients servers
  liftSimulation $ runEventInStopTime $ do
    t <- liftDynamics time 
    (_, _, respVals) <- unzip3 <$> (mapM freezeVar respTimes)
    
    loadUser <- (/t) <$> averageRef userTimes
    loadWorkstation <- (/t) <$> average <$> (zipRefs (+) userTimes afterTimes)
    loadCable <- (/t) <$> readRef cableTime    
    let averageResponse = average $ average.elems <$> respVals
    loadDisk <- (/t) <$> average <$> (mapM averageRef diskTimes)
    loadCP <- (/t) <$> average <$> (mapM averageRef cpuTimes)
    loadServer <- (/t) <$> averageRef serverTimes
    
    return $ Output {
      averageResponse = toFloat averageResponse
    , loadWorkstation = toFloat loadWorkstation
    , loadUser = toFloat loadUser
    , loadCable = toFloat loadCable
    , loadServer = toFloat loadServer
    , loadDisk = toFloat loadDisk
    , loadCP = toFloat loadCP
    }
  where
  toFloat = fromRational . toRational
  
  constructClients :: MsgBuffer -> Event [(MsgBuffer, Ref Double, Ref Double, Var Double)]
  constructClients cable = do
    replicateWithIndexM workstationsCount $ \i -> do
      responseVar <- liftSimulation $ newVar 0 
      (b1, p1, ref1) <- formQueryProcess i cable responseVar
      (b2, p2, ref2) <- afterQueryProcess b1
      liftSimulation $ do 
        runProcessInStartTime p1
        runProcessInStartTime p2
      return (b2, ref2, ref1, responseVar)
  
  constructServers :: MsgBuffer -> Event [(MsgBuffer, Ref Double, [Ref Double], [Ref Double])]
  constructServers cable = do
    replicateM serversCount $ do
        rec (diskBufs, diskPs, diskRefs) <- unzip3 <$> replicateM disksCount (diskProcess serverBuff)
            (cpuBufs, cpuPs, cpuRefs) <- unzip3 <$> replicateM processorsCount (cpuProcess diskBufs)
            (serverBuff, serverPs, serverRef) <- serverProcess cable cpuBufs
        liftSimulation $ do
          mapM_ runProcessInStartTime diskPs
          mapM_ runProcessInStartTime cpuPs
          runProcessInStartTime serverPs 
        return (serverBuff, serverRef, cpuRefs, diskRefs)
  
  constructCable :: [MsgBuffer] -> [MsgBuffer] -> Event (MsgBuffer, Ref Double)
  constructCable clients servers = do
    (b, p, r) <- cableProcess clients servers
    liftSimulation $ runProcessInStartTime p
    return (b, r)
          
  -- | Процесс дообработки ответа
  afterQueryProcess :: MsgBuffer -> Event MsgBuffered
  afterQueryProcess clientBuf = processWithBuffer $ \enterBuf busyRef -> do
      msg  <- dequeue enterBuf
      holdExpStat afterQueryTime busyRef
      enqueue clientBuf msg
      
  -- | Процесс генерации запроса (пользовательский)
  formQueryProcess :: Int -> MsgBuffer -> Var Double -> Event MsgBuffered
  formQueryProcess clientId cableBuf respVar = processWithBuffer $ \enterBuf busyRef-> do
    holdExpStat formQueryTime busyRef
    newMsg <- formNewQuery
    enqueue cableBuf newMsg
    Response birthTime _ <- dequeue enterBuf
    t <- liftDynamics time
    liftEvent $ writeVar respVar (t - birthTime)
    where formNewQuery = do
            birthTime <- liftDynamics time
            distServer <- liftParameter $ randomUniformInt 0 (serversCount-1)
            return $ Query birthTime clientId distServer
  
  -- | Процесс, отвечающий за канал передачи данных
  cableProcess :: [MsgBuffer] -> [MsgBuffer] -> Event MsgBuffered
  cableProcess clients servers = processWithBuffer $ \enterBuf busyRef -> do
    msg <- dequeue enterBuf
    holdExpStat sendingTime busyRef
    case msg of
      Query _ _ server -> enqueue (servers !! server) msg
      Response _ client -> enqueue (clients !! client) msg
      Requery _ -> error "unexpected state"
       
  -- | Процесс, отвечающий за процессор сервера
  cpuProcess :: [MsgBuffer] -> Event MsgBuffered
  cpuProcess disks = processWithBuffer $ \enterBuf busyRef -> do
    msg <- dequeue enterBuf
    holdExpStat processoringTime busyRef
    disk <- liftParameter $ randomUniformInt 0 (disksCount-1) 
    enqueue (disks !! disk) msg
  
  -- | Процесс, отвечающий за диск сервера
  diskProcess :: MsgBuffer -> Event MsgBuffered
  diskProcess server = processWithBuffer $ \enterBuf busyRef -> do
    msg <- dequeue enterBuf
    holdExpStat diskingTime busyRef
    randomChoice requeryChance (enqueue server msg) $ do
      let Query birthTime client _ = msg
      enqueue server $ Response birthTime client
      
  -- | Процесс, отвечающий за сервер    
  serverProcess :: MsgBuffer -> [MsgBuffer] -> Event MsgBuffered
  serverProcess cable cpus = do
    counter <- liftSimulation $ newRef (0 :: Int)
    startRef <- liftSimulation $ newRef (0.0 :: Double)
    processWithBuffer $ \enterBuf busyRef -> do
      msg <- dequeue enterBuf
      case msg of
        Query{} -> do
          curCount <- getCount counter
          when (curCount <= 0) $ saveTime startRef
          when (curCount > 0) $ rebaseTime startRef busyRef
          addToRef counter 1
          cpui <- liftParameter $ randomUniformInt 0 (processorsCount-1)
          enqueue (cpus !! cpui) msg
        Requery q -> do
          rebaseTime startRef busyRef
          cpui <- liftParameter $ randomUniformInt 0 (processorsCount-1)
          enqueue (cpus !! cpui) q
        Response{} -> do
          addToRef counter (-1)
          rebaseTime startRef busyRef
          enqueue cable msg
    where
      getCount counter = liftEvent $ readRef counter
      saveTime ref = liftEvent (writeRef ref =<< liftDynamics time)
      rebaseTime ref loadRef= do 
        ot <- liftEvent $ readRef ref
        nt <- liftDynamics time
        addToRef loadRef (nt - ot)
        liftEvent $ writeRef ref nt