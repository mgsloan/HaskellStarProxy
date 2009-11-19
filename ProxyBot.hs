import Network
import qualified IO as IO
import System
import Control.Exception
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Concurrent (forkIO)
import Control.Monad (forever)

data Player = Player {  playerMinerals, playerGas, supplyUsed, supplyTotal :: Int,
                        unitProduction, techProduction, upgradeProduction :: [Bool] }

instance Read Player where
    readsPrec _ xs = [(Player (read a) (read b) (read c) (read d) (map fromOneZero e) (map fromOneZero f) (map fromOneZero g), "")]
        where (a:b:c:d:e:f:g:_) = splitOn (==';') xs :: [String]

data WorldMap = WorldMap {  mapName :: String,
                            mapWidth, mapHeight :: Int,
                            mapData :: ([Int], Bool, Bool) }
                            
data GameInfo = GameInfo {  gameMap         :: WorldMap,
                            gameUnitTypes   :: [UnitType],
                            gameTech        :: [TechType],
                            gameUpgrade     :: [UpgradeType] }

data GameState = GameState {gamePlayer  :: Player,
                            gameUnits   :: [Unit] }

instance Read GameState where
    readsPrec _ xs = [(GameState (read a) (map read bs), "")]
        where (a:bs) = splitOn (==':') xs

data Unit = Unit {  unitID, unitPlayerID, unitType :: Int,
                    unitLocation :: Location,
                    unitHitPoints, unitShields, unitEnergy, unitOrderTimer, unitOrder, unitResources :: Int,
                    unitIsCompleted, unitIsConstructing, unitIsTraining :: Bool }

instance Read Unit where
    readsPrec _ xs = [(Unit (read a) (read b) (read c) (Location (read d) (read e)) (read f) (read g) (read h) (read i) (read j) (read k) (read l) (read m) (read n), "")]
        where (a:b:c:d:e:f:g:h:i:j:k:l:m:n:_) = splitOn (==';') xs

data Location = Location { locationX, locationY :: Int }

instance Read Location where
    readsPrec _ xs = [(Location (read a) (read b), "")]
        where (a:b:_) = splitOn (==';') xs

instance Show Location where
    show (Location a b) = (show a) ++ ";" ++ (show b)
    
scOffset (Location x y) = Location (x+2) (y+1)

data UnitType = UnitType {  unitTypeID :: Int,
                            unitTypeRace, unitTypeName :: String,
                            unitTypeMineralsCost, unitTypeGasCost, unitTypeMaxHitPoints, unitTypeMaxShields, unitTypeMaxEnergy,
                            unitTypeBuildTime, unitTypeCanAttack, unitTypeCanMove, unitTypeTileWidth, unitTypeTileHeight,
                            unitTypeSupplyRequired, unitTypeSupplyProvided, unitTypeSightRange, unitTypeGroundMaxRange,
                            unitTypeGroundMinRange, unitTypeGroundDamage, unitTypeAirRange, unitTypeAirDamage :: Int,
                            unitTypeBuilding, unitTypeFlyer, unitTypeSpellCaster, unitTypeWorker :: Bool,
                            unitTypeWhatBuilds :: Int }

instance Read UnitType where
    readsPrec _ xs = [(UnitType (read a) (read b) (read c) (read d) (read e) (read f) (read g) (read h) (read i) (read j)
                                (read k) (read l) (read m) (read n) (read o) (read p) (read q) (read r) (read s) (read t)
                                (read u) (read v) (read w) (read x) (read y) (read z), "")]
        where (a:b:c:d:e:f:g:h:i:j:k:l:m:n:o:p:q:r:s:t:u:v:w:x:y:z:_) = splitOn (==';') xs

instance Show UnitType where
    show (UnitType a b c d e f g h i j k l m n o p q r s t u v w x y z) =
        (show a) ++ ";" ++ (show b) ++ ";" ++ (show c) ++ ";" ++ (show d) ++ ";" ++ (show e) ++ ";" ++ (show f) ++ ";" ++
        (show g) ++ ";" ++ (show h) ++ ";" ++ (show i) ++ ";" ++ (show j) ++ ";" ++ (show k) ++ ";" ++ (show l) ++ ";" ++
        (show m) ++ ";" ++ (show n) ++ ";" ++ (show o) ++ ";" ++ (show p) ++ ";" ++ (show q) ++ ";" ++ (show r) ++ ";" ++
        (show s) ++ ";" ++ (show t) ++ ";" ++ (show u) ++ ";" ++ (show v) ++ ";" ++ (show w) ++ ";" ++ (show x) ++ ";" ++
        (show y) ++ ";" ++ (show z)

data TechType = TechType {  techID :: Int,
                            techName :: String,
                            techResearcherID, techMineralsCost, techGasCost :: Int }

instance Read TechType where
    readsPrec _ xs = [(TechType (read a) (read b) (read c) (read d) (read e), "")]
        where (a:b:c:d:e:_) = splitOn (==';') xs

instance Show TechType where
    show (TechType a b c d e) = (show a) ++ ";" ++ (show b) ++ ";" ++ (show c) ++ ";" ++ (show d) ++ ";" ++ (show e)

data UpgradeType = UpgradeType {   upgradeID :: Int,
                                   upgradeName :: String,
                                   upgradeResearcherID, upgradeRepeats, upgradeMineralsBase, upgradeMineralsFactor,
                                   upgradeGasBase, upgradeGasFactor :: Int }

instance Read UpgradeType where
    readsPrec _ xs = [(UpgradeType (read a) (read b) (read c) (read d) (read e) (read f) (read g) (read h), "")]
        where (a:b:c:d:e:f:g:h:_) = splitOn (==';') xs

instance Show UpgradeType where
    show (UpgradeType a b c d e f g h) =
        (show a) ++ ";" ++ (show b) ++ ";" ++ (show c) ++ ";" ++ (show d) ++ ";" ++ (show e) ++ ";" ++ (show f) ++ ";" ++
        (show g) ++ ";" ++ (show h)

data CommandType = NoneCommand |   AttackMoveCommand | AttackUnitCommand | RightClickCommand |   RightClickUnitCommand 
                 |TrainCommand |        BuildCommand | BuildAddonCommand |   ResearchCommand |          UpgradeCommand
                 | StopCommand | HoldPositionCommand |     PatrolCommand |     FollowCommand | SetRallyPositionCommand 
                 | SetRallyUnitCommand | RepairCommand | MorphCommand | BurrowCommand | UnburrowCommand | SiegeCommand
                 | UnsiegeCommand | CloakCommand | DecloakCommand | LiftCommand | LandCommand | LoadCommand | UnloadCommand 
                 | UnloadAllCommand | UnloadAllPositionCommand | CancelConstructionCommand | HaltConstructionCommand
                 | CancelMorphCommand | CancelTrainCommand | CancelTrainSlotCommand | CancelAddonCommand | CancelResearchCommand 
                 | CancelUpgradeCommand | UseTechCommand | UseTechPositionCommand | UseTechTargetCommand
    deriving (Enum)

data Command = Command {commandType :: CommandType,
                        commandUnitID, commandArg0, commandArg1, commandArg2 :: Int }

instance Read Command where
    readsPrec _ xs = [(Command (toEnum $ read a) (read b) (read c) (read d) (read e), "")]
        where (a:b:c:d:e:_) = splitOn (==';') xs

                 
splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn _ [] = []
splitOn f l@(x:xs)
  | f x = splitOn f xs
  | otherwise = let (h,t) = break f l in h:(splitOn f t)

readFields :: (Read a) => String -> [a]
readFields = map read . tail . splitOn (==':')
readFieldsLine :: (Read a) => IO.Handle -> IO [a]
readFieldsLine = fmap readFields . IO.hGetLine

port = PortNumber 13337
allowUserControl = False
completeInformation = False
oneZero True = "1"
oneZero False = "0"
fromOneZero '1' = True
fromOneZero _ = False

run_ai ai = withSocketsDo . bracket (listenOn port) sClose $ \sock ->
 do putStrLn "Waiting for client" 
    (h, _, _) <- accept sock
    putStrLn "Client connceted"
    bracket (return h) IO.hClose (\handle ->
     do putStrLn "Waiting for client ACK message"
        [playerID,playerRace,enemyID,enemyRace:_] <- readFieldsLine handle
        IO.hPutStrLn handle $ (oneZero allowUserControl) ++ (oneZero completeInformation)
        
        startingLocations <- fmap (map scOffset) $ (readFieldsLine handle :: IO [Location])
        unitTypes <- readFieldsLine handle
        worldMap <- fmap read $ IO.hGetLine handle
        techTypes <- readFieldsLine handle
        upgradeTypes <- readFieldsLine handle
        
        commandChannel <- newChan :: IO(Chan Command)
        state <- newEmptyMVar
        forkIO $ ai (GameInfo worldMap unitTypes techTypes upgradeTypes) state commandChannel
        forkIO $ forever ((fmap read $ IO.hGetLine handle) >>= putMVar state)
        getChanContents commandChannel >>= mapM_ (IO.hPutStrLn handle . show)
     )