module Data.Maybe.Utils (singletonToMaybe) where

singletonToMaybe :: [a] -> Maybe a
singletonToMaybe [x] = Just x
singletonToMaybe _   = Nothing
