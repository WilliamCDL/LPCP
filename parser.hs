module Main (main) where

import Lexer
import Text.Parsec
import Control.Monad.IO.Class

import System.IO.Unsafe

-- parsers para os tokens

programToken = tokenPrim show update_pos get_token where
  get_token (Program p) = Just (Program p)
  get_token _           = Nothing

idToken = tokenPrim show update_pos get_token where
  get_token (Id x p) = Just (Id x p)
  get_token _        = Nothing

varToken = tokenPrim show update_pos get_token where
  get_token (Var p) = Just (Var p)
  get_token _       = Nothing  

beginToken = tokenPrim show update_pos get_token where
  get_token (Begin p) = Just (Begin p)
  get_token _         = Nothing

endToken = tokenPrim show update_pos get_token where
  get_token (End p) = Just (End p) 
  get_token _       = Nothing

semiColonToken :: ParsecT [Token] st IO (Token)
semiColonToken = tokenPrim show update_pos get_token where
  get_token (SemiColon p) = Just (SemiColon p)
  get_token _             = Nothing

colonToken = tokenPrim show update_pos get_token where
  get_token (Colon p) = Just (Colon p)
  get_token _         = Nothing

assignToken = tokenPrim show update_pos get_token where
  get_token (Assign p) = Just (Assign p)
  get_token _          = Nothing

intToken = tokenPrim show update_pos get_token where
  get_token (Int x p) = Just (Int x p)
  get_token _         = Nothing

floatToken = tokenPrim show update_pos get_token where
  get_token (Float x p) = Just (Float x p)
  get_token _         = Nothing

typeToken = tokenPrim show update_pos get_token where
  get_token (Type x p) = Just (Type x p)
  get_token _          = Nothing 

addToken = tokenPrim show update_pos get_token where
  get_token (Add p) = Just (Add p)
  get_token _       = Nothing 

minusToken = tokenPrim show update_pos get_token where
  get_token (Minus p) = Just (Minus p)
  get_token _       = Nothing 

potentiationToken = tokenPrim show update_pos get_token where
  get_token (Potentiation p) = Just (Potentiation p)
  get_token _       = Nothing 

update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos pos _ (tok:_) = pos -- necessita melhoria
update_pos pos _ []      = pos  

-- parsers para os n??o-terminais

program :: ParsecT [Token] [(Token,Token)] IO ([Token])
program = do
            a <- programToken 
            b <- idToken 
            c <- varToken
            d <- varDecl
            e <- beginToken 
            f <- stmts
            g <- endToken
            eof
            return (a:b:[c] ++ d++ [e] ++ f ++ [g])

varDecl :: ParsecT [Token] [(Token,Token)] IO([Token])
varDecl = do
            a <- idToken
            b <- colonToken
            c <- typeToken
            updateState(symtable_insert (a, get_default_value c))
            s <- getState
            liftIO (print s)
            return (a:b:[c])

stmts :: ParsecT [Token] [(Token,Token)] IO([Token])
stmts = do
          first <- assign
          next <- remaining_stmts
          return (first ++ next)

remaining_stmts :: ParsecT [Token] [(Token,Token)] IO([Token])
remaining_stmts = (do a <- semiColonToken
                      b <- stmts
                      return (a:b)) <|> (return [])

assign :: ParsecT [Token] [(Token,Token)] IO([Token])
assign = do
          a <- idToken
          b <- assignToken
          c <- expression
          s <- getState
          if (not (compatible (get_type a s) c)) then fail "type mismatch"
          else 
            do 
              updateState(symtable_update (a, c))
              s <- getState
              liftIO (print s)
              return (a:b:[c])

-- fun????es para verifica????o de tipos

get_default_value :: Token -> Token
get_default_value (Type "int" (l, c)) = Int 0 (l, c)
--get_default_value (Type "float" (x, c)) = Float 0.0 (x, c)

get_type :: Token -> [(Token, Token)] -> Token
get_type _ [] = error "variable not found"
get_type (Id id1 p1) ((Id id2 _, value):t) = if id1 == id2 then value
                                             else get_type (Id id1 p1) t

compatible :: Token -> Token -> Bool
compatible (Int _ _) (Int _ _) = True
compatible (Float _ _) (Float _ _) = True
compatible (Float _ _) (Int _ _) = True
compatible _ _ = False

-- tentativa de encadeamento expr
termo :: ParsecT [Token] [(Token,Token)] IO(Token)
termo = try bin_termo <|> una_termo

una_termo :: ParsecT [Token] [(Token,Token)] IO(Token)
una_termo = do
              a <- fator
              return (a)

bin_termo :: ParsecT [Token] [(Token,Token)] IO(Token)
bin_termo = do
              n3 <- fator
              result <- eval_remainingT n3
              return (result)

eval_remainingT :: Token -> ParsecT [Token] [(Token,Token)] IO(Token)
eval_remainingT n3= do
                      op <- ope2
                      n4 <- fator
                      result <- eval_remainingT (eval n3 op n4)
                      return (result) 
                      <|> return (n3)

fator :: ParsecT [Token] [(Token,Token)] IO(Token)
fator= try una_fator 

una_fator :: ParsecT [Token] [(Token,Token)] IO(Token)
una_fator = do
                   a <- tipo
                   return (a)

--exp :: ParsecT [Token] [(Token,Token)] IO(Token)
--exp= try bin_exo <|> una_termo

-- fun????es para o avaliador de express??es

expression :: ParsecT [Token] [(Token,Token)] IO(Token)
expression = try bin_expression <|> una_expression

una_expression :: ParsecT [Token] [(Token,Token)] IO(Token)
una_expression = do
                   a <- termo
                   return (a)
   
--- fun????es considerando associatividade ?? esquerda                  
bin_expression :: ParsecT [Token] [(Token,Token)] IO(Token)
bin_expression = do
                   n1 <- termo
                   result <- eval_remaining n1
                   return (result)
eval_remaining :: Token -> ParsecT [Token] [(Token,Token)] IO(Token)
eval_remaining n1 = do
                      op <- ope
                      n2 <- termo
                      result <- eval_remaining (eval n1 op n2)
                      return (result) 
                    <|> return (n1)   

-- opera????es
ope :: ParsecT [Token] [(Token,Token)] IO(Token)
ope = try adicionar <|> diminuir 

adicionar :: ParsecT [Token] [(Token,Token)] IO(Token)
adicionar = do
                op <- addToken
                return(op)

diminuir :: ParsecT [Token] [(Token,Token)] IO(Token)
diminuir = do
                op <- minusToken
                return(op)

ope2 :: ParsecT [Token] [(Token,Token)] IO(Token)
ope2 = try  multiplica               

multiplica :: ParsecT [Token] [(Token,Token)] IO(Token)
multiplica = do
                op <- potentiationToken
                return(op)

--tipos de dados primitivos
tipo :: ParsecT [Token] [(Token,Token)] IO(Token)
tipo = try inteiro <|> racional 

inteiro :: ParsecT [Token] [(Token,Token)] IO(Token)
inteiro = do
                a <- intToken
                return(a)

racional  :: ParsecT [Token] [(Token,Token)] IO(Token)
racional  = do
                a <- floatToken
                return(a)

toInt :: Float -> Int  
toInt = round                   

eval :: Token -> Token -> Token -> Token
eval (Int x p) (Add _ ) (Int y _) = Int (x + y) p 
eval (Int x p) (Minus _ ) (Int y _) = Int (x - y) p
eval (Int x p) (Potentiation _ ) (Int y _) = Int (x ^ y) p

-- fun????es para a tabela de s??mbolos

symtable_insert :: (Token,Token) -> [(Token,Token)] -> [(Token,Token)]
symtable_insert symbol []  = [symbol]
symtable_insert symbol symtable = symtable ++ [symbol]

symtable_update :: (Token,Token) -> [(Token,Token)] -> [(Token,Token)]
symtable_update _ [] = fail "variable not found"
symtable_update (Id id1 p1, v1) ((Id id2 p2, v2):t) = 
                               if id1 == id2 then (Id id1 p2, v1) : t
                               else (Id id2 p2, v2) : symtable_update (Id id1 p1, v1) t

symtable_remove :: (Token,Token) -> [(Token,Token)] -> [(Token,Token)]
symtable_remove _ [] = fail "variable not found"
symtable_remove (id1, v1) ((id2, v2):t) = 
                               if id1 == id2 then t
                               else (id2, v2) : symtable_remove (id1, v1) t                               


-- invoca????o do parser para o s??mbolo de partida 

parser :: [Token] -> IO (Either ParseError [Token])
parser tokens = runParserT program [] "Error message" tokens

main :: IO ()
main = case unsafePerformIO (parser (getTokens "programa.pe")) of
            { Left err -> print err; 
              Right ans -> print ans
            }