module Config where

data Config = Config {
          isLazyMode :: Bool
        , startDir :: String
        } 
    deriving (Show, Eq)

initConfig = Config {
    --  isLazyMode = False
      isLazyMode = True
    , startDir = "Start"
    }

