module Main where

import           CLI                   (Opts (..), getOpts)
import           Overseer.Hardware     (HardwareItem (..))
import           Overseer.Hardware.CPU (cpuTemp)
import           Prelude

main :: IO ()
main = getOpts >>= \case
  Hardware CPU -> cpuTemp >>= putTextLn
