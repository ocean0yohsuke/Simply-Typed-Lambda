module Config where

data Config = Config {
          startDir :: String
        , isLazyMode :: Bool
        } 
    deriving (Show, Eq)

initConfig = Config {
      isLazyMode = False
    --  isLazyMode = True
    , startDir = "Start"
    }

