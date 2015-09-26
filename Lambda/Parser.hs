module Lambda.Parser (
    ParseError,
    readSLexeme, readSExpr, readSCode, readSFile
    ) where

import MonadX.Applicative hiding ((<|>), many, optional)
import MonadX.Monad

import qualified Lambda.DataType.PatternMatch as PM
import qualified Lambda.DataType.Type as Ty
import Lambda.DataType.SExpr (SExpr(..))
import qualified Lambda.DataType.SExpr as SE
import Lambda.DataType (Type((:->)), PM, SFile, SCode(..), Name, Filename, convert)

import Text.Parsec hiding (spaces, lower) -- lower が '\955' とマッチしてしまう為。 Text.Parsec　のバグか？
import Text.Parsec.String
import Data.Char
import Data.List (nub)

-- for debug
import Debug.Trace 

reservedwords :: [String]
reservedwords = [ "if", "then", "else", "let", "letrec", "in", "case", "of"
                , "data"
                , "True", "False"
                , "fix"
                , "λ", "\\", "#"
                , "=", "->", ",", ".", "::"
                , "--", "{-", "-}"
                ]

----------------------------------------------------------------------------------------------------------------
-- read Sugars
----------------------------------------------------------------------------------------------------------------

hookParserState :: Parser a -> Parser (a, String)
hookParserState p = do
    expr <- p 
    rest <- stateInput |$> getParserState
    (*:) (expr, rest)

readSLexeme :: String -> Either ParseError (SExpr, String)
readSLexeme = parse (hookParserState $ pLexeme) "lambda lexeme" 

readSExpr :: String -> Either ParseError (SExpr, String)
readSExpr = parse (hookParserState $ pExpr) "lambda expression" 
 
readSCode :: String -> Either ParseError (SCode, String)
readSCode = parse (hookParserState pCode) "lambda code" 

readSFile :: Filename -> String -> Either ParseError (SFile, String)
readSFile filename = parse (hookParserState pFile) filename

----------------------------------------------------------------------------------------------------------------
-- Preliminary 
----------------------------------------------------------------------------------------------------------------

getSourcePos :: Parser SourcePos
getSourcePos = statePos |$> getParserState

lower :: Parser Char
lower = oneOf ['a'..'z']

idsymbol :: Parser Char
idsymbol = oneOf "'?_"

oprchars :: String
oprchars = "!#$&|:<=>?@_~%*+-/^"

oprsymbol :: Parser Char
oprsymbol = oneOf oprchars

delimiter :: Parser Char
delimiter = oneOf " \t\n"

spaces :: Parser ()
spaces = skipMany $ oneOf " \t"
spaces1 :: Parser ()
spaces1 = skipMany1 $ oneOf " \t"

newlines :: Parser ()
newlines = skipMany newline
newlines1 :: Parser ()
newlines1 = skipMany1 newline

spacelines :: Parser ()
spacelines = skipMany delimiter
spacelines1 :: Parser ()
spacelines1 = skipMany1 delimiter

commentline :: Parser String
commentline = do
    string "--" 
    manyTill anyChar (try newline)
commentblock :: Parser String
commentblock = block1 <|> block2 <?> "commentblock"
  where
    block1 = do
        c <- commentline
        cs <- many $ commentline
        (*:) (c ++ concat cs)
    block2 = do
        string "{-" 
        manyTill anyChar (try (string "-}"))
comspaceline :: Parser ()
comspaceline = spaces *> ((newline >> (*:) ()) <|> (commentblock >> (*:) ()))

-- comment-space or newindentline
csnl :: Parser ()
csnl = try (skipMany1 (spacelines *> commentblock <* snl0)) <|> snl1 <?> ""
  where
    -- space or newindentline
    snl0 :: Parser ()
    snl0 = skipMany $ try spaces1 <|> newindentline
    snl1 :: Parser ()
    snl1 = skipMany1 $ try spaces1 <|> newindentline
    newindentline :: Parser ()
    newindentline = skipMany1 (char '\n') *> spaces1

trycsnl :: Parser ()
trycsnl = try csnl <|> spaces

parens :: Parser a -> Parser a
parens p = between (char '(' <* spacelines) (spacelines *> char ')') p
tryparens = try . parens 
braket :: Parser a -> Parser a
braket p = between (char '[' <* spacelines) (spacelines *> char ']') p
trybraket = try . braket 

notFollowedByReservedwords :: Parser ()
notFollowedByReservedwords = notFollowedBy $ spaces *> (choice $ string |$> reservedwords)

notFollowedByAlphaNumSym :: Parser ()
notFollowedByAlphaNumSym = notFollowedBy (alphaNum <|> idsymbol <|> oprsymbol)

never :: String -> Parser a
never str = string "%never%" *> unexpected str

manytry = many . try
many1try = many1 . try

----------------------------------------------------------------------------------------------------------------
-- pCode, pFile
----------------------------------------------------------------------------------------------------------------

pEXPR :: Parser SCode
pEXPR = do
    expr <- pExpr
    (*:) $ EXPR expr
pCOMMENT :: Parser SCode
pCOMMENT = do
    cb <- spaces *> commentblock <?> "comment"
    (*:) $ COMMENT cb
pLINEBREAK :: Parser SCode
pLINEBREAK = do
    spaces *> newline
    (*:) LINEBREAK

pMargin :: Parser SCode
pMargin = try pCOMMENT <|> pLINEBREAK

pCode :: Parser SCode
pCode = pMargin <|> pEXPR

pFile :: Parser SFile
pFile = do
    c <- pCode
    cs <- many pCode
    eof
    (*:) (c:cs)

----------------------------------------------------------------------------------------------------------------
-- pSingle
----------------------------------------------------------------------------------------------------------------

pDataStructure :: Parser SExpr
pDataStructure = pList <|> pTuple
pQuotekind :: Parser SExpr
pQuotekind = pQut <|> pQQut <|> pUnQut
pAtom :: Parser SExpr
pAtom = pBool <|> pInt <|> pChar <|> pStr <|> pUnit

pTerm :: Parser SExpr
pTerm = tryparens pOprVar
    <|> pDataStructure
    <|> pQuotekind
    <|> pAtom
    <|> pVar

pBundle :: Parser SExpr
pBundle = tryparens pInfAppSeq <|> tryparens pFix <|> tryparens pApp
      <|> tryparens pSyntax
      <|> tryparens pTerm 

pSingle :: Parser SExpr
pSingle = pBundle <|> pTerm

----------------------------------------------------------------------------------------------------------------
-- pLexeme
----------------------------------------------------------------------------------------------------------------

pLetkind :: Parser SExpr
pLetkind = try pLet <|> pLetrec
pLamkind :: Parser SExpr
pLamkind = try pLam <|> pLamMcr

pSyntax :: Parser SExpr
pSyntax = pIf 
      <|> pCase 
      <|> pLamkind
      <|> pLetkind 

pAppkind :: Parser SExpr
pAppkind = try pInfAppSeq <|> try pFix <|> pApp

pLexeme :: Parser SExpr
pLexeme = try pAppkind
      <|> try pSyntax 
      <|> pSingle 

----------------------------------------------------------------------------------------------------------------
-- pExpr
----------------------------------------------------------------------------------------------------------------

pSentence :: Parser SExpr
pSentence = try pDef <|> try pTypeSig <|> pBNF

-- sugared expression
pExpr :: Parser SExpr
pExpr = try (pSentence <* comspaceline)
        <|> (pLexeme <* comspaceline)
pExpr_ :: Parser SExpr
pExpr_ = try pSentence
         <|> pLexeme

------------------------------------
-- sentence
------------------------------------

pDef :: Parser SExpr
pDef = do
    sp <- getSourcePos
    id <- pLowerId <|> parens pSymbolId 
    pms <- manytry $ spaces1 *> (tryparens pPM <|> pPMSingle)
    spaces1 *> string "=" <* csnl
    t <- pLexeme <* spaces
    let d = (pms, t, Just sp)
    ds <- manytry $ newlines1 *> pDefline id -- TODO: comment
    (*:) $ DEF id (d:ds) 
  <?> "definition"
  where
    pDefline id = do
        sp <- getSourcePos
        if and $ id <$| (`elem` oprchars) 
          then string ("("++ id ++")") 
          else string id 
        pms <- manytry $ spaces1 *> (tryparens pPM <|> pPMSingle)  
        spaces1 *> string "=" <* csnl
        t <- pLexeme <* spaces
        (*:) (pms, t, Just sp)

-- type signature
pTypeSig :: Parser SExpr
pTypeSig = do
    sp <- getSourcePos
    id <- pLowerId <|> parens pSymbolId
    spaces1 *> string "::" <* csnl
    ty <- pType
    (*:) $ TYPESig (id, ty) (Just sp)
  <?> "type-signature"

------------------------------------
-- mixed parsers
------------------------------------

pFix :: Parser SExpr
pFix = do
    sp <- getSourcePos
    string "fix" <* csnl
    lambda <- tryparens pLam <|> pLam
    (*:) $ FIX lambda (Just sp)
  <?> "fix"

pApp :: Parser SExpr
pApp = do
    sp <- getSourcePos
    e <- pOpr
    args <- many1try $ spaces1 *> pSingle -- TODO: 改行
    (*:) $ APP e args (Just sp)
  <?> "application"
  where
    pOpr = tryparens pAppkind 
       <|> tryparens pLamkind
       <|> tryparens pOprVar
       <|> tryparens pVar <|> pVar
       <|> tryparens pQuotekind <|> pQuotekind

pInfAppSeq :: Parser SExpr
pInfAppSeq = do
    sp <- getSourcePos
    t <- (try pFix <|> try pApp <|> pSingle) <|> pInfOpr
    seq <- case t of
        OPR sym msp -> spaces *> pSeqTerm
        _           -> spaces *> pSeqOpr
    (*:) $ APPSeq (t : seq) (Just sp)
  <?> "infix application sequence"
  where
    pSeqTerm :: Parser [SExpr]
    pSeqTerm = do
        t <- spaces *> (try pFix <|> try pApp <|> pSingle)
        mseq <- optionMaybe . try $ spaces *> pSeqOpr
        case mseq of
          Nothing  -> (*:) $ t : []
          Just seq -> (*:) $ t : seq
    pSeqOpr :: Parser [SExpr]
    pSeqOpr = do
        opr <- spaces *> pInfOpr
        mseq <- optionMaybe . try $ spaces *> pSeqTerm
        case mseq of
          Nothing  -> (*:) $ opr : []
          Just seq -> (*:) $ opr : seq


-- tuple 
pTuple :: Parser SExpr
pTuple = parens (do
    sp <- getSourcePos
    t <- pLexeme
    ts <- many1 $ spacelines *> char ',' *> spacelines *> pLexeme
    (*:) $ TUPLE (t:ts) (Just sp)
  ) <?> "tuple"

-- lambda
pLam :: Parser SExpr
pLam = do
    sp <- getSourcePos
    oneOf ['\\','\955']
    params <- manytry $ spaces *> (tryparens pParam <|> pParam <?> "lambda-parameter")
    spaces *> choice [string "->", string "."] <* trycsnl
    t <- pLexeme
    (*:) $ LAM params t (Just sp)
  <?> "lambda"

-- lambda-macro
pLamMcr :: Parser SExpr
pLamMcr = do
    sp <- getSourcePos
    oneOf ['#']
    params <- manytry $ spaces *> (tryparens pParam <|> pParam <?> "lambda-parameter")
    spaces *> choice [string "->", string "."] <* trycsnl
    t <- pLexeme
    (*:) $ LAMM params t (Just sp)
  <?> "lambda-macro"

-- let 
pLet :: Parser SExpr
pLet = do 
    sp <- getSourcePos
    string "let" <* notFollowedByAlphaNumSym <* csnl
    ((pm,ty), t1) <- pPair <* csnl
    string "in" <* notFollowedByAlphaNumSym <* csnl
    t2 <- pLexeme
    (*:) $ LET (pm,ty) t1 t2 (Just sp)
  <?> "let"
  where
    pPair = do
        (pm,ty) <- pParam <* (spaces1 *> string "=" <* csnl)
        t <- pLexeme
        (*:) ((pm,ty), t)

-- let 
pLetrec :: Parser SExpr
pLetrec = do 
    sp <- getSourcePos
    string "letrec" <* notFollowedByAlphaNumSym <* csnl
    (AS (VAR var _,ty) _, t1) <- pPairExpr <* csnl
    string "in" <* notFollowedByAlphaNumSym <* csnl
    t2 <- pLexeme
    (*:) $ LETREC (var,ty) t1 t2 (Just sp)
  <?> "letrec"
  where
    pPairExpr = do
        as <- pAs <* (spaces1 *> string "=" <* csnl)
        t <- pLexeme
        (*:) (as, t)

pIf :: Parser SExpr
pIf = do 
    sp <- getSourcePos
    t1 <- (string "if"   <* notFollowedByAlphaNumSym <* csnl) *> pLexeme <* csnl
    t2 <- (string "then" <* notFollowedByAlphaNumSym <* csnl) *> pLexeme <* csnl
    t3 <- (string "else" <* notFollowedByAlphaNumSym <* csnl) *> pLexeme
    (*:) $ IF t1 t2 t3 (Just sp)
  <?> "if"

-- type definition
pBNF :: Parser SExpr
pBNF = do
    sp <- getSourcePos
    string "data" <* notFollowedByAlphaNumSym <* spaces1
    name <- pUpperId <* spaces1
    string "=" <* notFollowedByAlphaNumSym <* csnl
    tag <- pTag
    tags <- manytry $ (csnl *> char '|' *> spaces1) *> pTag
    (*:) $ BNF name (tag:tags) (Just sp)
  <?> "datatype"
  where
    pTag :: Parser (Name, [Type])
    pTag = do
        name <- pUpperId
        tys <- manytry $ spaces1 *> pType
        (*:) (name, tys)

pCase :: Parser SExpr
pCase = do
    sp <- getSourcePos
    var <- (string "case" <* notFollowedByAlphaNumSym <* spaces1) *> pLexeme <* (spaces1 *> string "of" <* notFollowedByAlphaNumSym <* csnl)
    pair <- pPair
    pairs <- manytry $ csnl *> pPair
    (*:) $ CASE var (pair:pairs) (Just sp)
  <?> "case"
  where
    pPair = do
        pm <- (tryparens pPM <|> pPM) <* spaces1 <* string "->" <* spaces1
        t <- pLexeme
        (*:) (pm, t)

------------------------------------
-- atomic parsers
------------------------------------

pVar :: Parser SExpr
pVar = do
    sp <- getSourcePos
    name <- pLowerId <|> pUpperId
    (*:) $ VAR name (Just sp)
  <?> "variable"
pOprVar :: Parser SExpr
pOprVar = do
    sp <- getSourcePos
    sym <- pSymbolId
    (*:) $ VAR sym (Just sp)
  <?> "symbolic operator"
pInfOpr :: Parser SExpr
pInfOpr = do
    sp <- getSourcePos
    sym <- pSymbolId
    (*:) $ OPR sym (Just sp)
  <?> "infix operator"

pBool :: Parser SExpr
pBool = do
    sp <- getSourcePos
    bool <- (string "True"  <|> string "False") <* notFollowedByAlphaNumSym
    (*:) $ BOOL (read bool :: Bool) (Just sp)
  <?> "bool"
 
pInt :: Parser SExpr
pInt = try pPlus <|> pMinus <?> "integer"
  where
    pPlus = do
        sp <- getSourcePos
        i <- many1 digit
        (*:) $ INT (read i) (Just sp)
    pMinus = do
        sp <- getSourcePos
        char '-'
        INT n _ <- pPlus
        (*:) $ INT (n*(-1)) (Just sp)

pChar :: Parser SExpr
pChar = do 
    sp <- getSourcePos
    char '\''
    c <- noneOf "'"
    char '\''
    (*:) $ CHAR c (Just sp)
  <?> "character" 
pStr :: Parser SExpr
pStr = do
    sp <- getSourcePos
    char '"'
    cs <- seq
    char '"'
    (*:) $ foldr (\x acc -> CONS x acc (Just sp)) SE.nil (SE.char |$> cs)
  <?> "string"
  where
    seq :: Parser String
    seq = do    
        cs <- many (noneOf "\"")
        case cs of
          [] -> (*:) []
          _  -> if last cs == '\\'
                then char '"' >> ((init cs ++ ['"'])++) |$> seq
                else (*:) cs

-- list 
pList :: Parser SExpr
pList = try pNil <|> pCons <?> "list"
  where
    pNil = do
        sp <- getSourcePos
        char '[' *> trycsnl <* char ']'
        (*:) $ NIL (Just sp)
    pCons = do
        sp <- getSourcePos
        braket $ do
            trycsnl
            t <- pLexeme
            ts <- manytry $ trycsnl *> char ',' *> trycsnl *> pLexeme
            try (trycsnl *> char ',' *> trycsnl) <|> trycsnl
            (*:) $ foldr (\x acc -> CONS x acc (Just sp)) SE.nil (t:ts) -- TODO: sp 

-- unit: _
pUnit :: Parser SExpr
pUnit = do
    sp <- getSourcePos
    string "_"
    (*:) $ UNIT (Just sp)
  <?> "_"

-- quote
pQut :: Parser SExpr
pQut = do
    sp <- getSourcePos
    char '{'
    t <- pLexeme
    char '}'
    (*:) $ QUT t (Just sp)
-- quasi-quote
pQQut :: Parser SExpr
pQQut = do
    sp <- getSourcePos
    char '`'
    t <- pLexeme
    (*:) $ QQUT t (Just sp)
-- unquote
pUnQut :: Parser SExpr
pUnQut = do
    sp <- getSourcePos
    char ','
    t <- pVar -- TODO: de Bruijn index で beta-reduction をしているので、 pLexeme とはできない
    (*:) $ UNQUT t (Just sp)


------------------------------------------------------------------------
-- pPM
------------------------------------------------------------------------

pPM :: Parser PM
pPM = try pPMSeq <|> pPMSingle <?> "pattern-match"

pPMSeq :: Parser PM
pPMSeq = try pCons <|> pTagSeq
  where
    pCons :: Parser PM
    pCons = do
        sp <- getSourcePos
        x <- tryparens pPMSeq <|> pPMSingle
        spaces *> char ':' <* spaces
        xs <- try pCons <|> tryparens pPMSeq <|> pPMSingle
        (*:) $ PM.CONS x xs (Just sp)
    pTagSeq :: Parser PM
    pTagSeq = do
        sp <- getSourcePos
        name <- pUpperId
        pms <- many1try $ spaces1 *> (tryparens pPMSeq <|> pPMSingle)
        (*:) $ PM.TAG name pms (Just sp)

pPMSingle :: Parser PM
pPMSingle = (convert |$> (try pBool <|> try pInt <|> try pChar <|> try pTuple <|> try pList <|> pUnit))
            <|> try pVar 
            <|> pTagSingle
  where     
    pVar :: Parser PM
    pVar = do
        sp <- getSourcePos
        name <- pLowerId
        (*:) $ PM.VAR name (Just sp)
    pTagSingle = do
        sp <- getSourcePos
        name <- pUpperId
        (*:) $ PM.TAG name [] (Just sp)
    convert :: SExpr -> PM
    convert (UNIT msp)      = PM.UNIT msp
    convert (VAR name msp)  = PM.VAR name msp
    convert (BOOL bool msp) = PM.BOOL bool msp
    convert (INT n msp)     = PM.INT n msp
    convert (CHAR c msp)    = PM.CHAR c msp
    convert (NIL msp)       = PM.NIL msp
    convert (CONS a d msp)  = PM.CONS (convert a) (convert d) msp
    convert (TUPLE xs msp)  = PM.TUPLE (xs <$| convert) msp

------------------------------------------------------------------------
-- pType
------------------------------------------------------------------------

pType :: Parser Type
pType = do
    ty <- pTyElem <|> pTyVar <|> pTyList <|> try pTyTuple
          <|> pTyData
          <|> parens pType
    mtys <- optionMaybe . try $ spaces *> string "->" *> spaces *> pType
    case mtys of
      Nothing  -> (*:) ty
      Just tys -> (*:) $ ty :-> tys
  <?> "type"
  where
    -- elemental types
    pTyElem :: Parser Type
    pTyElem = do
        str <- choice $ string |$> ["_", "Bool", "Int", "Char", "Str"]
        case str of
          "_"    -> (*:) Ty.UNIT
          "Bool" -> (*:) Ty.BOOL
          "Int"  -> (*:) Ty.INT
          "Char" -> (*:) Ty.CHAR
          "Str"  -> (*:) $ Ty.CONS Ty.CHAR
    pTyVar :: Parser Type
    pTyVar = do
        c <- lower
        cs <- many $ letter <|> digit
        (*:) $ Ty.VAR (c:cs)
    pTyList :: Parser Type
    pTyList = braket $ do
        ty <- pType
        (*:) $ Ty.CONS ty
    pTyTuple :: Parser Type
    pTyTuple = parens $ do
        ty <- pType
        tys <- many1 $ spacelines *> char ',' *> spacelines *> pType
        (*:) $ Ty.TUPLE (ty:tys)
    pTyData :: Parser Type
    pTyData = do
        c <- upper
        cs <- many $ letter <|> digit
        (*:) $ Ty.DATA (c:cs)


------------------------------------------------------------------------
-- other
------------------------------------------------------------------------

-- lower identifier
pLowerId :: Parser String
pLowerId = try $ do
    c <- lower
    cs <- many $ letter <|> idsymbol <|> digit 
    let name = c:cs
    if name `elem` reservedwords
    then never name <?> "lower identifier"
    else (*:) name

-- upper identifier
pUpperId :: Parser String
pUpperId = try $ do
    c <- upper
    cs <- many $ letter <|> idsymbol <|> digit 
    let name = c:cs
    if name `elem` reservedwords
    then never name <?> "upper identifier"
    else (*:) name

-- symbolic identifier
pSymbolId :: Parser String
pSymbolId = try $ do
    cs <- many1 oprsymbol 
    let name = cs
    if name `elem` reservedwords
    then never name <?> "symbol identifier"
    else (*:) name

-- parameter
pParam :: Parser (PM, Type)
pParam = do
    pm <- tryparens pPMSeq <|> pPMSingle
    mty <- optionMaybe . try $ spaces *> string "::" *> spaces *> (tryparens pType <|> pType <?> "type")
    case mty of
      Nothing -> (*:) (pm, Ty.UNIT)
      Just ty -> (*:) (pm, ty)
  <?> "parameter"

-- ascription
pAs :: Parser SExpr 
pAs = do
    sp <- getSourcePos
    id <- pLowerId      -- TODO:
    string "::"
    ty <- pType
    (*:) $ AS (VAR id (Just sp), ty) (Just sp)
  <?> "ascription"

------------------------------------------------------------------------
-- 
------------------------------------------------------------------------


{-
-- tuple-projection 
pTplPrj :: Parser SExpr
pTplPrj = do
    sp <- getSourcePos
    tpl <- pTarget
    char '.'
    n <- many1 $ oneOf "123456789"
    (*:) $ TPLPrj tpl (read n) (Just sp)
  <?> "tuple-projection"
  where
    pTarget = tryparens pApp <|> tryparens pNTarget <|> pNTarget  -- TODO: pApp の他にもある
      where
        pNTarget = try pVar <|> pTuple
-}


