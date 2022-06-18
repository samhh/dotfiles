{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Overseer.Hardware.CPU where

import           Data.Aeson      (FromJSON, decode)
import qualified Data.Map.Strict as M
import           GHC.List        (maximum)
import           Prelude
import           System.Process

-- Reports the highest temperature between the three measurements reported for a 3900X.
cpuTemp :: IO Text
cpuTemp = maybe e (pretty . maximum . temps) <$> readk10Sensors
  where pretty (Celsius x) = show (ceiling x :: Int) <> "°C"
        e = "Failed to read CPU temps."

type Output = Map Chip (Map Sensor (Map Report Celsius))
type Chip = Text
type Sensor = Text
type Report = Text
newtype Celsius = Celsius Float
  deriving (Eq, Ord, FromJSON)

temps :: Output -> [Celsius]
temps = M.elems <=< M.elems <=< M.elems

readk10Sensors :: IO (Maybe Output)
readk10Sensors = decode . encodeUtf8 <$> readProcess "sensors" ["k10temp-pci-00c3", "-j", "-A"] ""
