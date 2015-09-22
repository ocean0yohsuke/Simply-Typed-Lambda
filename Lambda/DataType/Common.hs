{-# LANGUAGE MultiParamTypeClasses #-}
module Lambda.DataType.Common where

import MonadX.Applicative
import MonadX.Monad

import Text.Parsec (SourcePos)
import Text.Parsec.Pos (sourceLine, sourceColumn, sourceName)

-------------------------------------------
-- 
-------------------------------------------

type Index = Int
type Field = String
type Name = String

isSymbolic :: String -> Bool
isSymbolic xs = and $ xs <$| (`elem` "!#$&|:<=>?@_~%*+-/^") 

-- TODO:
--showc :: (Convert a b, Show b) => a -> String
--showc = show . convert

-------------------------------------------
-- SourcePos
-------------------------------------------

showSourcePos :: SourcePos -> String
showSourcePos sp = 
    let (line, column, sourcename) = (sourceLine sp, sourceColumn sp, sourceName sp) 
    in  sourcename ++ " (line " ++ show line ++ ", column " ++ show column ++ ")"

type MSP = Maybe SourcePos
showMSP :: MSP -> String
showMSP Nothing   = ""
showMSP (Just sp) = showSourcePos sp

