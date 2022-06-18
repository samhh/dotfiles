module CLI (Opts (..), getOpts) where

import           Options.Applicative
import           Overseer.Hardware   (HardwareItem (..))
import           Prelude

data Opts
  = Hardware HardwareItem

getOpts :: IO Opts
getOpts = execParser (info (opts <**> helper) mempty)

opts :: Parser Opts
opts = subparser . mconcat $
  [ command "hardware" (info (hardware <**> helper) mempty)
  ]

hardware :: Parser Opts
hardware = fmap Hardware . subparser . mconcat $
  [ command "cpu" (info (pure CPU) mempty)
  ]
