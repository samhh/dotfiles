module String where

import           Data.Char (isSpace)
import           Data.List (dropWhileEnd)

trimStart :: String -> String
trimStart = dropWhile isSpace

trimEnd :: String -> String
trimEnd = dropWhileEnd isSpace

trim :: String -> String
trim = trimEnd . trimStart
