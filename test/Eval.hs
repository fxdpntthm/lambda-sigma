module Main where

import Language

main = do putStrLn "Testing: "
          putStrLn $ (show exp3) ++  " ==> " ++ (show $ eval exp3)
          putStrLn $ (show expId) ++ " ==> " ++ (show $ eval expId) 
          return ()