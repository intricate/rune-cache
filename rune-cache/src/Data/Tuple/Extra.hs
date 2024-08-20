module Data.Tuple.Extra
  ( third
  ) where

third :: (a, b, c) -> c
third (_, _, c) = c
