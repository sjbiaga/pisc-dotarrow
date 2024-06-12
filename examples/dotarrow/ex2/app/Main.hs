module Main where

import System.Exit (exitFailure)     --  dotarrow
import System.IO (hPutStrLn, stderr) --  dotarrow
import Inp_gUgVwYdD8r                --  dotarrow
import Out_gUgVwYdD8r                --  dotarrow

data C = C { m :: Int, n :: Int }

main :: IO ()
main = do
    x :: Int <- pure ( 2 + 5 )
    y :: Int <- pure ( x * 4 )
    z <- pure $ C (x * y) (1 * 1)
    let C a b = z
    _u :: () <- print (a * b)
    return ()
