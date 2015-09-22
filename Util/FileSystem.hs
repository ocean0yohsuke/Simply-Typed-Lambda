-- TODO: エラー処理
-- | This is the filesystem module.
module Util.FileSystem (
    -- * Data and Type
    FDInfo(..),
    Order, Predicate, 
    -- * Functions
    toFDInfo,
    lookover,
    lookoverP,
    traverse,
    traverseP,
    isDirectory,
    ) where 

import Control.Monad (forM)
import Control.Applicative ((<$>),(<*>))
import Control.Exception (SomeException, handle)
import System.Directory (searchable, getDirectoryContents, Permissions, getPermissions, getModificationTime)
import System.FilePath ((</>))
import System.IO (withFile, IOMode(..), hFileSize)
import Data.Time.Clock (UTCTime)

-- | Information of file or directory.
data FDInfo = FDInfo {
      fdPath :: FilePath              -- Path of directory or file.
    , fdPerms :: Maybe Permissions    -- Nothing if the file not existed.
    , fdSize :: Maybe Integer         -- Nothing if the file not existed.
    , fdModTime :: Maybe UTCTime      -- Nothing if the file not existed.
    } deriving (Eq, Show)

type Order = [FDInfo] -> [FDInfo]
type Predicate = FDInfo -> Bool

instance Ord FDInfo where
  compare x y 
    | isDirectory x && isDirectory y       = compare (fdPath x) (fdPath y)
    | not (isDirectory x) && isDirectory y = GT
    | isDirectory x && not (isDirectory y) = LT
    | otherwise                            = compare (fdPath x) (fdPath y)

-----------------------------------------------------------------------------------------------
-- Traversal functions
-----------------------------------------------------------------------------------------------

lookover :: Order -> FilePath -> IO [FDInfo]
lookover order path = do
    xs <- items path
    let paths = (path </>) <$> xs
    fdInfos <- mapM toFDInfo paths
    return $ order fdInfos
    
lookoverP :: Predicate -> Order -> FilePath -> IO [FDInfo]
lookoverP pred order fp = filter pred <$> lookover order fp 

traverse :: Order -> FilePath -> IO [FDInfo]
traverse order path = do
    xs <- items path
    let paths = path : ((path </>) <$> xs)
    fdInfos <- mapM toFDInfo paths
    concat <$> forM (order fdInfos) (\info -> do
        if isDirectory info && fdPath info /= path
        then traverse order (fdPath info)
        else return [info])

traverseP :: Predicate -> Order -> FilePath -> IO [FDInfo]
traverseP pred order fp = filter pred <$> traverse order fp 

-----------------------------------------------------------------------------------------------
-- Utility
-----------------------------------------------------------------------------------------------

-- | returns all subitems of a directory except self (.) and parent (..)
items :: FilePath -> IO [String]
items path = filter (`notElem` [".", ".."]) <$> getDirectoryContents path

isDirectory :: Predicate
isDirectory = (maybe False searchable) . fdPerms

toFDInfo :: FilePath -> IO FDInfo
toFDInfo path = FDInfo path <$> maybeIO perms <*> maybeIO size <*> maybeIO modified
  where
    perms    = getPermissions path
    size     = withFile path ReadMode hFileSize
    modified = getModificationTime path
    maybeIO :: IO a -> IO (Maybe a)
    maybeIO act = handle handler (Just <$> act)
      where 
        handler :: SomeException -> IO (Maybe a)
        handler _ = return Nothing

