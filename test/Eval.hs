module Main where


import Common
import UTLang
import STLang
import Examples

main = do putStrLn "Testing: "
          putStrLn $ (show exp3) ++  " ==> " ++ (show $ eval exp3)
          putStrLn $ (show expId) ++ " ==> " ++ (show $ eval expId) 
          return ()
