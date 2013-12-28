{-# LANGUAGE RankNTypes #-}
-- | Main entry point to the application.
module Main where

import RSA(rsaDemo)
    
-- | The main entry point.
main :: IO ()
main = rsaDemo

