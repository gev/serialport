{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveDataTypeable       #-}
{-# LANGUAGE LambdaCase               #-}
{-# OPTIONS_HADDOCK hide #-}
module System.Hardware.Serialport.Posix where

import GHC.IO.Handle
import GHC.IO.Encoding

import System.Posix.IO
import System.Posix.Types (Fd, ByteCount)
import System.Posix.Terminal

import Foreign (Ptr, nullPtr, castPtr, alloca, peek, with)
import Foreign.C

import Data.Typeable
import Data.Bits

import qualified Data.ByteString.Char8 as B
import qualified Control.Exception     as Ex

import System.Hardware.Serialport.Types


data SerialPort = SerialPort
  { fd           :: Fd
  , portSettings :: SerialPortSettings
  } deriving (Show, Typeable)

-- |Open and configure a serial port returning a standard Handle
hOpenSerial :: FilePath
           -> SerialPortSettings
           -> IO Handle
hOpenSerial dev settings = do
  h <- fdToHandle . fd =<< openSerial dev settings
  hSetBuffering h NoBuffering
  return h


-- |Open and configure a serial port
openSerial :: FilePath            -- ^ Serial port, such as @\/dev\/ttyS0@ or @\/dev\/ttyUSB0@
           -> SerialPortSettings
           -> IO SerialPort
openSerial dev settings = do
  fd' <- openFd dev ReadWrite defaultFileFlags { noctty = True, nonBlock = True }
  setTIOCEXCL fd'
  setFdOption fd' NonBlockingRead False
  let serial_port = SerialPort fd' defaultSerialSettings
  setSerialSettings serial_port settings


-- |Use specific encoding for an action and restore old encoding afterwards
withEncoding :: TextEncoding -> IO a -> IO a
#if MIN_VERSION_base(4,5,0)
withEncoding encoding fun = do
  cur_enc <- getForeignEncoding
  setForeignEncoding encoding
  result <- fun
  setForeignEncoding cur_enc
  return result
#else
withEncoding _ fun = fun
#endif


-- |Receive bytes, given the maximum number
recv :: SerialPort -> Int -> IO B.ByteString
recv port n = do
  result <- withEncoding char8 $ Ex.try $ fdRead (fd port) count :: IO (Either IOError (String, ByteCount))
  return $ case result of
     Right (str, _) -> B.pack str
     Left _         -> B.empty
  where
    count = fromIntegral n


-- |Send bytes
send
  :: SerialPort
  -> B.ByteString
  -> IO Int          -- ^ Number of bytes actually sent
send port msg =
  fromIntegral <$> withEncoding char8 (fdWrite (fd port) (B.unpack msg))


-- |Flush buffers
flush :: SerialPort -> IO ()
flush port = discardData (fd port) BothQueues


-- |Close the serial port
closeSerial :: SerialPort -> IO ()
closeSerial = closeFd . fd


#include <sys/ioctl.h>

foreign import ccall "ioctl" c_ioctl :: CInt -> CInt -> Ptr () -> IO CInt

cIoctl' :: Fd -> Int -> Ptr d -> IO ()
cIoctl' f req =
  throwErrnoIfMinus1_ "ioctl" .
     c_ioctl (fromIntegral f) (fromIntegral req) . castPtr


getTIOCM :: Fd -> IO Int
getTIOCM fd' =
  alloca $ \p -> cIoctl' fd' #{const TIOCMGET} p >> peek p


setTIOCM :: Fd -> Int -> IO ()
setTIOCM fd' val =
  with val $ cIoctl' fd' #{const TIOCMSET}


setTIOCEXCL :: Fd -> IO ()
setTIOCEXCL fd' = cIoctl' fd' #{const TIOCEXCL} nullPtr


-- |Set the Data Terminal Ready level
setDTR :: SerialPort -> Bool -> IO ()
setDTR (SerialPort fd' _) set = do
  current <- getTIOCM fd'
  setTIOCM fd' $ if set
                   then current .|. #{const TIOCM_DTR}
                   else current .&. complement #{const TIOCM_DTR}


-- |Set the Ready to send level
setRTS :: SerialPort -> Bool -> IO ()
setRTS (SerialPort fd' _) set = do
  current <- getTIOCM fd'
  setTIOCM fd' $ if set
                   then current .|. #{const TIOCM_RTS}
                   else current .&. complement #{const TIOCM_RTS}


-- |Configure the serial port
setSerialSettings :: SerialPort           -- ^ The currently opened serial port
                  -> SerialPortSettings   -- ^ The new settings
                  -> IO SerialPort        -- ^ New serial port
setSerialSettings port new_settings = do
  termOpts <- getTerminalAttributes $ fd port
  let termOpts' = configureSettings termOpts new_settings
  setTerminalAttributes (fd port) termOpts' Immediately
  return $ SerialPort (fd port) new_settings


-- |Get configuration from serial port
getSerialSettings :: SerialPort -> SerialPortSettings
getSerialSettings = portSettings


withParity :: TerminalAttributes -> Parity -> TerminalAttributes
withParity termOpts Even =
    termOpts `withMode` EnableParity
             `withoutMode` OddParity
withParity termOpts Odd =
    termOpts `withMode` EnableParity
             `withMode` OddParity
withParity termOpts NoParity =
    termOpts `withoutMode` EnableParity


withFlowControl :: TerminalAttributes -> FlowControl -> TerminalAttributes
withFlowControl termOpts NoFlowControl =
    termOpts `withoutMode` StartStopInput
             `withoutMode` StartStopOutput
withFlowControl termOpts Software =
    termOpts `withMode` StartStopInput
             `withMode` StartStopOutput


withStopBits :: TerminalAttributes -> StopBits -> TerminalAttributes
withStopBits termOpts One =
    termOpts `withoutMode` TwoStopBits
withStopBits termOpts Two =
    termOpts `withMode` TwoStopBits


configureSettings :: TerminalAttributes -> SerialPortSettings -> TerminalAttributes
configureSettings termOpts settings =
    termOpts `withInputSpeed` commSpeedToBaudRate (commSpeed settings)
             `withOutputSpeed` commSpeedToBaudRate (commSpeed settings)
             `withBits` fromIntegral (bitsPerWord settings)
             `withStopBits` stopb settings
             `withParity` parity settings
             `withFlowControl` flowControl settings
             `withoutMode` EnableEcho
             `withoutMode` EchoErase
             `withoutMode` EchoKill
             `withoutMode` ProcessInput
             `withoutMode` ProcessOutput
             `withoutMode` MapCRtoLF
             `withoutMode` EchoLF
             `withoutMode` HangupOnClose
             `withoutMode` KeyboardInterrupts
             `withoutMode` ExtendedFunctions
             `withMode` LocalMode
             `withMode` ReadEnable
             `withTime` timeout settings
             `withMinInput` 0


commSpeedToBaudRate :: CommSpeed -> BaudRate
commSpeedToBaudRate = \case
  CS50 -> B50
  CS75 -> B75
  CS110 -> B110
  CS134 -> B134
  CS150 -> B150
  CS200 -> B200
  CS300 -> B300
  CS600 -> B600
  CS1200 -> B1200
  CS1800 -> B1800
  CS2400 -> B2400
  CS4800 -> B4800
  CS9600 -> B9600
  CS19200 -> B19200
  CS38400 -> B38400
  CS57600 -> B57600
  CS115200 -> B115200
  CS230400 -> B230400
  CS460800 -> B460800
  CS500000 -> B500000
  CS576000 -> B576000
  CS921600 -> B921600
  CS1000000 -> B1000000
  CS1152000 -> B1152000
  CS1500000 -> B1500000
  CS2000000 -> B2000000
  CS2500000 -> B2500000
  CS3000000 -> B3000000
  CS3500000 -> B3500000
  CS4000000 -> B4000000
