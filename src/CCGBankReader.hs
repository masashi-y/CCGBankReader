
{-# LANGUAGE OverloadedStrings #-}

module CCGBankReader (
  Feature(..),
  feature,
  atomic,
  slash,
  cat,
  readCats
  ) where

import Text.Parsec
import qualified Data.Text as T

type FeatureType = T.Text
type FeatureValue = T.Text
newtype Feature = Feature { getValues :: [(FeatureType, FeatureValue)] }

data Cat = S Feature
    | NP Feature
    | Fwd Cat Cat
    | Bwd Cat Cat

instance Show Feature where
    show (Feature feats) = "[" ++ (T.unpack $ T.intercalate "," $ map showKV feats) ++ "]"
        where showKV (ftype, fvalue) = ftype `T.append` "=" `T.append` fvalue

instance Show Cat where
    show (S feat)  = "S" ++ show feat
    show (NP feat) = "NP" ++ show feat
    show (Fwd res arg) = "(" ++ show res ++ "/" ++ show arg ++ ")"
    show (Bwd res arg) = "(" ++ show res ++ "\\" ++ show arg ++ ")"

feature :: Parsec String st Feature
feature = Feature <$> (between (char '[') (char ']') $ keyValuePair `sepBy` (char ','))
    where keyValuePair = (,) <$> term <*> (char '=' *> term)
          term = T.pack <$> many letter

atomic :: Parsec String st Cat
atomic = toCat <$> (string "S" <|> string "NP") <*> feature
    where toCat "S" = S
          toCat "NP" = NP

slash :: Parsec String st (Cat -> Cat -> Cat)
slash = (Fwd <$ char '/') <|> (Bwd <$ char '\\')

binary :: Parsec String st Cat
binary = between (char '(') (char ')') (apply <$> cat <*> slash <*> cat)
    where apply res s arg = s res arg

cat :: Parsec String st Cat
cat = atomic <|> binary

readCats fileName = do
    contents <- fmap lines $ readFile fileName
    return $ map ((!! 0) . words) contents
