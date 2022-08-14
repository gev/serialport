{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE LambdaCase #-}

module System.Hardware.Serialport.Types where

import           Data.Word
import           System.Posix.Terminal (BaudRate (..))

-- | Supported baudrates
data CommSpeed
  = CS50
  | CS75
  | CS110
  | CS134
  | CS150
  | CS200
  | CS300
  | CS600
  | CS1200
  | CS1800
  | CS2400
  | CS4800
  | CS9600
  | CS19200
  | CS38400
  | CS57600
  | CS115200
  | CS230400
  | CS460800
  | CS500000
  | CS576000
  | CS921600
  | CS1000000
  | CS1152000
  | CS1500000
  | CS2000000
  | CS2500000
  | CS3000000
  | CS3500000
  | CS4000000
  deriving (Show, Read, Eq, Bounded)

commSpeedToBaudRate :: CommSpeed -> BaudRate
commSpeedToBaudRate = \case
  CS50      -> B50
  CS75      -> B75
  CS110     -> B110
  CS134     -> B134
  CS150     -> B150
  CS200     -> B200
  CS300     -> B300
  CS600     -> B600
  CS1200    -> B1200
  CS1800    -> B1800
  CS2400    -> B2400
  CS4800    -> B4800
  CS9600    -> B9600
  CS19200   -> B19200
  CS38400   -> B38400
  CS57600   -> B57600
  CS115200  -> B115200
  CS230400  -> B230400
  CS460800  -> B460800
  CS500000  -> B500000
  CS576000  -> B576000
  CS921600  -> B921600
  CS1000000 -> B1000000
  CS1152000 -> B1152000
  CS1500000 -> B1500000
  CS2000000 -> B2000000
  CS2500000 -> B2500000
  CS3000000 -> B3000000
  CS3500000 -> B3500000
  CS4000000 -> B4000000


data StopBits = One | Two
  deriving (Show, Read, Eq, Bounded)

data Parity = Even | Odd | NoParity
  deriving (Show, Read, Eq)

data FlowControl = Software | NoFlowControl
  deriving (Show, Read, Eq)

data SerialPortSettings = SerialPortSettings
  { commSpeed   :: CommSpeed,   -- ^ baudrate
    bitsPerWord :: Word8,       -- ^ Number of bits in a word
    stopb       :: StopBits,    -- ^ Number of stop bits
    parity      :: Parity,      -- ^ Type of parity
    flowControl :: FlowControl, -- ^ Type of flowcontrol
    timeout     :: Int          -- ^ Timeout when receiving a char in tenth of seconds
  } deriving (Show, Read, Eq)


-- | Most commonly used configuration
--
--  - 9600 baud
--
--  - 8 data bits
--
--  - 1 stop bit
--
--  - no parity
--
--  - no flow control
--
--  - 0.1 second receive timeout
--
defaultSerialSettings :: SerialPortSettings
defaultSerialSettings =
  SerialPortSettings CS9600 8 One NoParity NoFlowControl 1

