{- | Dump binary file to hex array -}
module Main where
import Control.Exception (bracket)
import Data.ByteString.Lazy as B hiding (init, concatMap)
import System.IO
import System.Environment
import Text.Printf (printf)

main :: IO ()
main = do
  (f:t:_) <- getArgs
  bracket (openFile f ReadMode)
    (hClose)
    (\from -> do
        bracket (openFile t WriteMode)
          (hClose)
          (\to -> hPutStrLn to "const data[] = {" >> dumpOut from to >> hPutStrLn to "};"))
  
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
