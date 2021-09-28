module Utils.Logger
  ( stdLog
  ) where

stdLog :: String -> String -> IO ()
stdLog scope msg = putStrLn $ "[" ++ scope ++ "] " ++ msg
