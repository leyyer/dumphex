{- | Dump binary file to hex array -}
module Main (main) where
import Control.Monad (liftM2)
import Data.ByteString.Lazy as B hiding (init, concatMap)
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
  
{- | dump to output file -}
dumpOut :: Handle -- ^ Input file handle
        -> Handle -- ^ Output file handle
        -> IO ()
dumpOut from to = do
  chunk <- B.hGetNonBlocking from 16
  eof <- hIsEOF from
  let s = "\t" ++ toString (B.unpack chunk)
  if eof then
    hPutStrLn to $ init s
    else hPutStrLn to s >> dumpOut from to
  where toString s = concatMap (printf "0x%02x," . fromEnum ) s
