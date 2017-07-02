
{-# LANGUAGE OverloadedStrings #-}

module CCGBankReader (
  Feature(..),
  Cat(..),
  Tree(..),
  category,
  readCats,
  terminal,
  nonterminal
  ) where

import Text.Parsec
import Data.List (intercalate)
import qualified Data.Text as T

type FeatureType = T.Text
type FeatureValue = T.Text
newtype Feature = Feature { getValues :: [(FeatureType, FeatureValue)] }

data Cat = S Feature
    | NP Feature
    | Fwd Cat Cat
    | Bwd Cat Cat

data Tree = Node {     rule :: T.Text,
                        cat :: Cat,
                   children :: [Tree] }
          | Leaf {  cat :: Cat,
                   form :: T.Text,
                   surf :: T.Text,
                   base :: T.Text,
                    tag :: T.Text }


instance Show Tree where
    show (Node r c cl) = "{" ++ T.unpack r ++ " " ++ show c ++ " " ++ (intercalate " " $ map show cl) ++ "}"
    show (Leaf c s b t f) = "{" ++ show c ++ " " ++ (T.unpack $ T.intercalate "/" [s, b, t, f]) ++ "}"

instance Show Feature where
    show (Feature feats) = "[" ++ (T.unpack $ T.intercalate "," $ map showKV feats) ++ "]"
        where showKV (ftype, fvalue) = ftype `T.append` "=" `T.append` fvalue

instance Show Cat where
    show (S feat)  = "S" ++ show feat
    show (NP feat) = "NP" ++ show feat
    show (Fwd res arg) = "(" ++ show res ++ "/" ++ show arg ++ ")"
    show (Bwd res arg) = "(" ++ show res ++ "\\" ++ show arg ++ ")"

curly = between (char '{') (char '}')

feature :: Parsec String st Feature
feature = Feature <$> (between (char '[') (char ']') $ keyValuePair `sepBy` (char ','))
    where keyValuePair = (,) <$> term <*> (char '=' *> term)
          term = T.pack <$> many letter

atomic :: Parsec String st Cat
atomic = ((S <$ char 'S') <|> (S <$ string "NP")) <*> feature

slash :: Parsec String st (Cat -> Cat -> Cat)
slash = (Fwd <$ char '/') <|> (Bwd <$ char '\\')

binary :: Parsec String st Cat
binary = between (char '(') (char ')') (apply <$> category <*> slash <*> category)
    where apply res s arg = s res arg

category :: Parsec String st Cat
category = (atomic <|> binary) <* (optional index)

index :: Parsec String st String
index = curly (many $ noneOf "}") -- {I1}, {I2}

category' :: Parsec String st Cat
category' = category <* (many $ noneOf " ") -- discard semantic role dependencies

terminal :: Parsec String st Tree
terminal = curly (Leaf <$> category' <* space <*> token <*> token <*> token <*> token)
    where token = T.pack <$> (many (noneOf "/}") <* (optional $ char '/'))

combinatoryRule :: Parsec String st T.Text
combinatoryRule = T.pack <$> many1 (letter <|> digit <|> oneOf "<>")

nonterminal :: Parsec String st Tree
nonterminal = curly (Node <$> combinatoryRule <* space <*> category' <* space <*> child)
    where child = (++) <$> (node) <*> (option [] (space *> node))
          node = pure <$> ((try nonterminal) <|> terminal)

readCats fileName = do
    contents <- fmap lines $ readFile fileName
    return $ map ((!! 0) . words) contents
