{
module Lexer where

import System.IO
import System.IO.Unsafe
}

%wrapper "posn"

$digit = 0-9      -- digits
$alpha = [a-zA-Z] -- alphabetic characters

tokens :-

  $white+                              ;
  "//".*                               ;

  -- Palavras reservadas

  program                              { \p s -> Program (getLC p)}
  var                                  { \p s -> Var (getLC p)}
  begin                                { \p s -> Begin(getLC p)}
  end                                  { \p s -> End(getLC p)}
  if                                   { \p s -> If(getLC p)}
  else                                 { \p s -> Else(getLC p)}
  while                                { \p s -> While(getLC p)}
  do                                   { \p s -> Do(getLC p)}
  print                                { \p s -> Print(getLC p)}

  -- Tipos primitivos

  int                                  { \p s -> Type s (getLC p)}
  float                                { \p s -> Type s (getLC p)}

  -- Simbolos

  "+"                                  { \p s -> Add (getLC p)}
  "-"                                  { \p s -> Minus (getLC p)}
  "^"                                  { \p s -> Potentiation (getLC p)}
  :                                    { \p s -> Colon (getLC p)}
  ";"                                  { \p s -> SemiColon (getLC p)}
  =                                    { \p s -> Assign (getLC p)}
  ==                                   { \p s -> Equal (getLC p)}
  !=                                   { \p s -> Different (getLC p)}
  "<"                                  { \p s -> Lesser (getLC p)}
  >                                    { \p s -> Greater (getLC p)}

  -- Identificadores

  $digit+                              { \p s -> Int (read s) (getLC p)}  
  $alpha [$alpha $digit \_ \']*        { \p s -> Id s (getLC p)}
  \" $alpha [$alpha $digit ! \_ \']* \"  { \p s -> String s (getLC p)}

{
-- Each action has type :: AlexPosn -> String -> Token

-- The token type:
data Token =
  Program (Int, Int)    |
  Var  (Int, Int)       |
  Begin  (Int, Int)     |
  End  (Int, Int)       |
  Colon  (Int, Int)     |
  SemiColon (Int, Int)  |
  Assign  (Int, Int)    | 
  Equal (Int, Int)      |
  Different (Int, Int)  |
  If (Int, Int)         |
  Else (Int, Int)       |
  While (Int, Int)      |
  Do (Int, Int)         |
  Print (Int, Int)      |
  Add (Int, Int)        |
  Minus (Int, Int)      |
 -- Add (Float, Float)    |
  --Minus (Float, Float)  |
  Potentiation(Int, Int)| 
  Greater (Int, Int)    |
  Lesser (Int, Int)     |
  Type String (Int, Int)|
  Id String (Int, Int)  |
  Int Int (Int, Int)    |
  Float Float (Float, Float)|
  String String (Int, Int)
  deriving (Eq,Show)

getLC (AlexPn _ l c) = (l, c)  

getTokens fn = unsafePerformIO (getTokensAux fn)

getTokensAux fn = do {fh <- openFile fn ReadMode;
                      s <- hGetContents fh;
                      return (alexScanTokens s)}
}