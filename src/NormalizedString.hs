{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module NormalizedString(StringCanonicalizer(Lowercase, CAPSLOCK), CanonicalString,
main -- for running demo as main program
) where

import           Normalized
import Data.Char(toLower, toUpper)

-- Example instance of Normalized, for string canonicalization
data StringCanonicalizer = Lowercase | CAPSLOCK
    deriving (Eq, Ord, Show)
type CanonicalString = NormalizedBy StringCanonicalizer

newLowercase :: String -> CanonicalString
newLowercase = newNormalizedBy Lowercase

newCAPSLOCK :: String -> CanonicalString
newCAPSLOCK = newNormalizedBy CAPSLOCK

type instance FunArg StringCanonicalizer = String

instance Normalizer StringCanonicalizer where
    (<%>) v Lowercase = map toLower v
    (<%>) v CAPSLOCK = map toUpper v


deriving instance Show CanonicalString

 
------- demo
s :: (Show a) => a -> String
s = flip shows "\n"

demo :: [String]
demo = [
      s $ omap2 (++) (newLowercase "Hello") (newLowercase "World"),
      s $ omap2 (++) (newCAPSLOCK "Hello") (newCAPSLOCK "World")
   ]

main :: IO ()
main = mapM_ putStrLn demo