{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeFamilies         #-}
module Modular(
    Mod(Mod), newMod, exponentiate,
    main
    -- note: these are intentionally hidden, in favor of smart constructors/accessors: Modulate
) where

import           Normalized

---------- Section: ModInteger
data Mod = Mod Integer deriving (Eq, Ord, Show)
type ModularInteger = NormalizedBy Mod

newMod n = newNormalizedBy (Mod n)

type instance FunArg Mod = Integer
instance Normalizer Mod where
    (<%>) v (Mod x) = v `mod` x

-- FlexibleInstances
instance Show ModularInteger where
    show (Normalize (Mod n) x) = "(" ++ show x ++ " % " ++ show n ++ ")"


-- As an example, safe and efficient-ish modular exponentiation, that keeps intermediate values in the range
-- (0, modulus^2)
exponentiate ::  Integer -> ModularInteger -> ModularInteger
exponentiate pow base =
  exponentiate' pow base (normalizedLike base (1::Integer))

exponentiate' :: Integer -> ModularInteger -> ModularInteger -> ModularInteger
exponentiate' p base acc   -- base^p * acc
  | p == 0         = acc   -- base^0 == 1
  | p `mod` 2 == 0 =       -- b^2x = (b^2)^x
        exponentiate' (p `div` 2) (omap2 (*) base base) acc
  | otherwise        =     -- b^(p-1) (acc*base)
  exponentiate' (p - 1)     base                       (omap2 (*) acc base)




------- demo
s :: (Show a) => a -> String
s = flip shows "\n"

demo :: [String]
demo = [
      s $ newNormalizedBy (Mod 1000) 10023  -- properly initialized
    , s $ unsafeValue $ newNormalizedBy (Mod 1000) 10023 -- properly initialized and unsafely accessed (OK)
   ]


main = mapM_ putStrLn demo
