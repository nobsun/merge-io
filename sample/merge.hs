module Main where

import Control.Concurrent hiding (mergeIO,nmergeIO)
import Control.Concurrent.Merge
import System.IO.Unsafe

main :: IO ()
main = print =<< do { as <- aa
                    ; bs <- bb
                    ; cs <- cc
                    ; nmergeIO [as,bs,cs]
                    }

delayedList :: Int -> [a] -> IO [a]
delayedList _ [] = unsafeInterleaveIO $ return []
delayedList n (x:xs)
  = threadDelay (n*10^5) >> unsafeInterleaveIO (return . (x:) =<< delayedList n xs)

aa, bb, cc :: IO String
aa = delayedList 2 (repeat 'a')
bb = delayedList 3 (repeat 'b')
cc = delayedList 5 (repeat 'c')