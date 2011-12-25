{- | Dump binary file to hex array -}
module Main (main) where
import Control.Monad (liftM2)
import Data.ByteString.Lazy (unpack, hGetNonBlocking)
import System.IO
import System.Environment
import Text.Printf (printf)

{- | dumphex [binary file] [output file] -}
main :: IO ()
main = do
  (f:t:_) <- getArgs
  liftM2 (,) (openFile f ReadMode) (openFile t WriteMode) >>= dumpOut_ >>= \(f,t) -> hClose f >> hClose t
    
dumpOut_ :: (Handle, Handle) -> IO (Handle, Handle)
dumpOut_ (f, t) = hPutStrLn t "const data [] = {" >> dumpOut f t >> hPutStrLn t "};" >> return (f, t)
  
{- | dump to output file, input->output->IO () -}
dumpOut :: Handle -> Handle -> IO ()
dumpOut from to = do
  chunk <- hGetNonBlocking from 16
  let s = "\t" ++ (concatMap (printf "0x%02x," . fromEnum) $ unpack chunk)
  hIsEOF from >>= \eof -> case eof of
    True -> hPutStrLn to $ init s
    _    -> hPutStrLn to s >> dumpOut from to