module Main where

import qualified Data.ByteString.Builder as Bob
import qualified Data.ByteString.Lazy as Lazy
import Data.Foldable
import Notes
import System.Process
import Text.Printf

type Seconds = Float

type Samples = Float

type Pulse = Float

main :: IO ()
main = do
  saveWave output
  callCommand $ printf "ffplay -showmode 1 -f f32le -ar %f %s" sampleRate output

step :: Float
step = 0.02

volume :: Float
volume = 0.5

sampleRate :: Samples
sampleRate = 48000

duration :: Seconds
duration = 0.5

output :: FilePath
output = "output.bin"

noteToStep :: Hz -> Float
noteToStep note = (note * 2 * pi) / sampleRate

frequency :: Hz -> Seconds -> Float -> [Pulse]
frequency note duration volume = map (* volume) $ map sin $ map (* noteToStep note) [0.0 .. sampleRate * duration]

note :: Semitones -> Seconds -> Float -> [Pulse]
note n = frequency (frequencyCalc n)

majorScale :: [Pulse]
majorScale =
  concat
    [ note 0 duration volume,
      note 2 duration volume,
      note 4 duration volume,
      note 5 duration volume,
      note 7 duration volume,
      note 9 duration volume,
      note 11 duration volume,
      note 12 duration volume
    ]

wave :: [Pulse]
wave = concat [note (2 * i) duration volume | i <- [0 .. 10]]

saveWave :: FilePath -> IO ()
saveWave path = Lazy.writeFile path $ Bob.toLazyByteString $ fold $ map Bob.floatLE wave