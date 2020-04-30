module Main where

import           ConstantPool         (Constant (..), getConstant,
                                       getConstantClass, poolValue)
import           Control.Monad        (guard, replicateM)
import           Data.Binary
import           Data.Binary.Get
import           Data.Bits
import qualified Data.ByteString.Lazy as BL
import           Text.Printf

data Class = Class { versionMajor     :: Int
                   , versionMinor     :: Int
                   , constantCount    :: Int
                   , constantPool     :: [Constant]
                   , classAccessFlags :: [ClassAccessFlag]
                   , thisClass        :: Constant
                   , superClass       :: Maybe Constant
                   , interfaceCount   :: Int
                   , interfaces       :: [Constant]
                   , fieldCount       :: Int
                   } deriving (Eq)

data FieldAccessFlag = FPublic
                     | FPrivate
                     | FProtected
                     | FStatic
                     | FFinal
                     | FVolatile
                     | FTransient
                     | FSynthetic
                     | FEnum
                     deriving (Show, Eq)

data Attribute = AttConst Word16 Word32 Word16


getAttributeConstant = do
  nameIndex  <- getWord16be
  attrSize   <- getWord32be
  constIndex <- getWord16be
  return $ AttConst nameIndex attrSize constIndex
  

data Field = Field { fieldAccessFlags :: FieldAccessFlag
                   , fieldNameIndex   :: Int
                   , descriptorIndex  :: Int
                   , attributesCount  :: Int
                   , attributes       :: [Attribute]
                   }

data ClassAccessFlag = CPublic
                     | CFinal
                     | CSuper
                     | CInterface
                     | CAbstract
                     | CSynthetic
                     | CAnnotation
                     | CEnum
                     | CModule
                     deriving (Show, Eq)

showLines :: Show a => [a] -> String
showLines xs = concatMap (\(y, x) -> "\t" ++ printf fmt y ++ " -> " ++ show x ++ "\n") pairs
  where
    fmt = "%0" ++ (show . length . show . length $ xs) ++ "d"
    pairs = zip ([1..] :: [Int]) xs

instance Show Class where
  show (Class vmaj vmin cc cp flags this super ic ifaces fc) = versionString ++
                                                               constantCountString ++
                                                               constantPoolString ++
                                                               flagString ++
                                                               thisString ++
                                                               superString ++
                                                               interfaceCountString ++
                                                               interfacesString ++
                                                               fieldCountString
    where
      versionString        = "Version: " ++ show vmaj ++ "." ++ show vmin ++ "\n"
      constantCountString  = "Constant count: " ++ show cc ++ "\n"
      constantPoolString   = "Constants: \n" ++ showLines cp
      flagString           = "Class access flags: " ++ show flags ++ "\n"
      thisString           = "This class: " ++ nameFromConstantClass cp this ++ "\n"
      superString          = "Superclass: " ++ show (fmap (nameFromConstantClass cp) super) ++ "\n"
      interfaceCountString = "Interface count: " ++ show ic ++ "\n"
      interfacesString     = "Interfaces: " ++ show ifaces ++ "\n"
      fieldCountString     = "Field count: " ++ show fc ++ "\n"

printClass :: IO ()
printClass = result >>= print
  where
    classData = BL.readFile "Main.class"
    result    = runGet tryGetClass <$> classData

getVersion :: Get (Int, Int)
getVersion = do
  minor <- fromIntegral <$> getWord16be
  major <- fromIntegral <$> getWord16be
  return (major, minor)

extractFlags :: Word16 -> [ClassAccessFlag]
extractFlags word = snd <$> filter (\(mask, _) -> (word .&. mask) == mask) flagMatchers
  where
    flagMatchers = [(0x0001, CPublic),
                    (0x0010, CFinal),
                    (0x0020, CSuper),
                    (0x0200, CInterface),
                    (0x0400, CAbstract),
                    (0x1000, CSynthetic),
                    (0x2000, CEnum),
                    (0x8000, CModule)]

getAccessFlags :: Get [ClassAccessFlag]
getAccessFlags = extractFlags <$> getWord16be

getSuperClass :: Get (Maybe Constant)
getSuperClass = do
  super <- getWord16be
  guard (super /= 0)
  return . Just . ConstantClass $ super

nameFromConstantClass :: [Constant] -> Constant -> String
nameFromConstantClass pool (ConstantClass x) = nameFromConstantClass pool (pool !! (fromIntegral x - 1))
nameFromConstantClass _    (ConstantUtf8 s)  = s
nameFromConstantClass _    _                 = error "provided Constant that is not a ConstantClass"

-- getField :: Get Field
-- getField = do
--   flags     <- getWord16be
--   nameIndex <- getWord16be

getClass :: Get Class
getClass = do
  (major, minor) <- getVersion
  numConstants   <- subtract 1 . fromIntegral <$> getWord16be
  constants      <- replicateM numConstants getConstant
  flags          <- getAccessFlags
  this           <- getConstantClass
  super          <- getSuperClass
  ifaceCount     <- fromIntegral <$> getWord16be
  ifaces         <- replicateM ifaceCount getConstant
  numFields      <- fromIntegral <$> getWord16be
  return (Class major minor numConstants constants flags this super ifaceCount ifaces numFields)

tryGetClass :: Get Class
tryGetClass = do
  magic <- getWord32be
  if magic /= 0xCAFEBABE
    then fail "File missing magic number 0xCAFEBABE"
    else getClass

getConstants :: IO [Constant]
getConstants = fmap constantPool result
  where
    classData = BL.readFile "Main.class"
    result    = runGet tryGetClass <$> classData

inspectIndex :: Int -> IO ()
inspectIndex i = do
  constants <- getConstants
  let value = poolValue i constants
  print value

main :: IO ()
main = putStrLn "Hello world"


add :: Int -> Int -> Either Int String
add x y = if x + y >= 10
             then Right "greater"
             else Left $ x + y
