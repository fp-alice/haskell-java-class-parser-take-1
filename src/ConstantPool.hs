module ConstantPool(Constant(..), getConstant, poolValue, getConstantClass) where

import           Control.Monad        (guard)
import           Data.Binary          (Word16, Word8, get)
import           Data.Binary.Get
import           Data.ByteString.UTF8 (toString)
import           Data.Int             (Int32, Int64)

data Constant = ConstantUtf8 String
              | ConstantInteger Int32
              | ConstantFloat Float
              | ConstantLong Int64
              | ConstantDouble Double
              | ConstantClass Word16
              | ConstantString Word16
              | ConstantFieldRef Word16 Word16
              | ConstantMethodRef Word16 Word16
              | ConstantInterfaceMethodRef Word16 Word16
              | ConstantNameAndType Word16 Word16
              | ConstantMethodHandle Word8 Word16
              | ConstantMethodType Word16
              | ConstantInvokeDynamic Word16 Word16
              deriving (Show, Eq)

getConstantMethodRef :: Get Constant
getConstantMethodRef = ConstantMethodRef <$> getWord16be <*> getWord16be

getConstantFieldRef :: Get Constant
getConstantFieldRef = ConstantFieldRef <$> getWord16be <*> getWord16be

getConstantUtf8 :: Get Constant
getConstantUtf8 = ConstantUtf8 . toString <$> (getByteString . fromIntegral =<< getWord16be)

getConstantInteger :: Get Constant
getConstantInteger = ConstantInteger <$> get

getConstantFloat :: Get Constant
getConstantFloat = ConstantFloat <$> get

getConstantString :: Get Constant
getConstantString = ConstantString <$> getWord16be

getConstantClass :: Get Constant
getConstantClass = ConstantClass <$> getWord16be

getConstantInterfaceMethodRef :: Get Constant
getConstantInterfaceMethodRef = ConstantInterfaceMethodRef <$> getWord16be <*> getWord16be

getConstantLong :: Get Constant
getConstantLong = ConstantLong <$> get

getConstantDouble :: Get Constant
getConstantDouble = ConstantDouble <$> get

getConstantNameAndType :: Get Constant
getConstantNameAndType = ConstantNameAndType <$> getWord16be <*> getWord16be

getConstantMethodHandle :: Get Constant
getConstantMethodHandle = ConstantMethodHandle <$> getWord8 <*> getWord16be

getConstantMethodType :: Get Constant
getConstantMethodType = ConstantMethodType <$> getWord16be

getConstantInvokeDynamic :: Get Constant
getConstantInvokeDynamic = ConstantInvokeDynamic <$> getWord16be <*> getWord16be

failParsingConstant :: Word8 -> Get Constant
failParsingConstant tag = do
  pos <- bytesRead
  fail ("Encountered invalid type tag \'" ++ show tag ++ "\' at position " ++ show pos)

getConstant :: Get Constant
getConstant = do
  tag <- getWord8
  case tag of
    1  -> getConstantUtf8
    3  -> getConstantInteger
    4  -> getConstantFloat
    5  -> getConstantLong
    6  -> getConstantDouble
    7  -> getConstantClass
    8  -> getConstantString
    9  -> getConstantFieldRef
    10 -> getConstantMethodRef
    11 -> getConstantInterfaceMethodRef
    12 -> getConstantNameAndType
    15 -> getConstantMethodHandle
    16 -> getConstantMethodType
    18 -> getConstantInvokeDynamic
    _  -> failParsingConstant tag

-- Constant pool is 1-indexed! Important!
poolValue :: Int -> [Constant] -> Maybe Constant
poolValue i cp = do
  guard canAccess
  return $ cp !! (i - 1)
  where
    len       = length cp
    canAccess = len >= i && i > 0
