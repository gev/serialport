{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE LambdaCase #-}

module System.Hardware.Serialport.Types where

import           Data.Word

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

