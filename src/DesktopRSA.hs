{-# LANGUAGE TypeFamilies #-}
module RSA(Rsa, newRsa, RsaNumber, modulus_, decode, encode, publicKey_,  roundtrip,
privateKey_ -- todo: something clever to make privateKey not visible to callers
, rsaDemo
-- need to export `main` if we want to run this module as a main program (for demo)
-- main
) where


import           Control.Applicative
import           Control.Exception   (assert)
import           Control.Monad
import           Modular
import           Normalized

totient prime1 prime2 = (prime1-1) * (prime2-1)

-- A number mod 'n', assuming 'n' is chosen appropriately as part of an RSA keypair
type RsaNumber = NormalizedBy Mod

data Rsa = Rsa {
    prime1_  :: Integer,
    prime2_  :: Integer,
    publicKey_  :: Integer,
    privateKey_ :: Integer,
    modulus_ :: Integer,
    totient_ :: Integer
}

data RsaPublic = RsaPublic {
       rpKey_ :: Integer,
       rpMod_ :: Integer
}

public :: Rsa -> RsaPublic
public rsa = RsaPublic (publicKey_ rsa) (modulus_ rsa)

newRsa :: Integer -> Integer -> Integer -> Integer -> Rsa
newRsa prime1 prime2 publicKey privateKey
  | (publicKey * privateKey) `mod` theTotient == 1 =
    let modulus = (prime1 * prime2) in
    let norm = newMod modulus in
    Rsa prime1 prime2 publicKey privateKey modulus (totient prime1 prime2)
  | otherwise = error "bogus rsa config"
    where theTotient = totient prime1 prime2

encode rsaPub = exponentiate (rpKey_ rsaPub)

-- todo: export this separately, to simulate security
decode rsa = exponentiate (privateKey_ rsa)

roundtrip :: Rsa -> RsaNumber -> RsaNumber
roundtrip = liftM2 (.) decode (encode . public)


newRsaMessage :: Integer -> RsaPublic -> RsaNumber
newRsaMessage x rsa = 
    let n = rpMod_ rsa in
    assert (x < n) $
    newMod n x


-- demo: specialized to one keypair

-- not very secure, but this is a demo.
myRsa :: Rsa
myRsa = newRsa 42829  43313 7 264995191

myRsaPub :: RsaPublic
myRsaPub = public myRsa

myEncode :: RsaNumber -> RsaNumber
myEncode = encode myRsaPub

myRoundtrip :: RsaNumber -> RsaNumber
myRoundtrip = myDecode . myEncode


myDecode :: RsaNumber -> RsaNumber
myDecode = decode myRsa

rsaDemo = do
     let msg = newRsaMessage 10 myRsaPub
     let m = modulus_ myRsa
     print m
     mapM_ print [
       msg,
       encode myRsaPub msg,
       roundtrip myRsa msg
       ]


-- | The main entry point.
main :: IO ()
main = rsaDemo
