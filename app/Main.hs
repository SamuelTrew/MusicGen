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

type Beats = Float

type Note = [Pulse]

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

duration :: Beats
duration = 4.0

output :: FilePath
output = "output.bin"

bpm :: Beats
bpm = 120.0

beatDuration :: Seconds
beatDuration = 60.0 / bpm

attack :: [Pulse]
attack = map (min 1.0) [0.0, 0.001 ..]

noteToStep :: Hz -> Float
noteToStep note = (note * 2 * pi) / sampleRate

frequency :: Hz -> Seconds -> Float -> [Pulse]
frequency note duration volume =
  map (* volume) $ zipWith3 (\x y z -> x * y * z) attack outWave release
  where
    outWave = map sin $ map (* noteToStep note) [0.0 .. sampleRate * duration]

    release :: [Pulse]
    release = reverse $ take (length outWave) attack

note :: Semitones -> Beats -> Float -> [Pulse]
note n beats = frequency (frequencyCalc n) (beats * beatDuration)

chord :: [Note] -> Note
chord notes = foldl' (zipWith (+)) (take (round $ sampleRate * duration) (repeat 0.0)) notes

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
wave =
  concat
    [ --
      note 0 0.25 volume,
      note 0 0.25 volume,
      note 0 0.25 volume,
      note 0 0.25 volume,
      note 0 0.5 volume,
      --
      note 0 0.25 volume,
      note 0 0.25 volume,
      note 0 0.25 volume,
      note 0 0.25 volume,
      note 0 0.25 volume,
      note 0 0.25 volume,
      note 0 0.5 volume,
      --
      note 5 0.25 volume,
      note 5 0.25 volume,
      note 5 0.25 volume,
      note 5 0.25 volume,
      note 5 0.25 volume,
      note 5 0.25 volume,
      note 5 0.5 volume,
      --
      note 3 0.25 volume,
      note 3 0.25 volume,
      note 3 0.25 volume,
      note 3 0.25 volume,
      note 3 0.25 volume,
      note 3 0.25 volume,
      note 3 0.5 volume,
      --
      note (-2) 0.5 volume,
      --
      note 0 0.25 volume,
      note 0 0.25 volume,
      note 0 0.25 volume,
      note 0 0.25 volume,
      note 0 0.5 volume
    ]

saveWave :: FilePath -> IO ()
saveWave path =
  Lazy.writeFile path $
    Bob.toLazyByteString $
      fold $
        map
          Bob.floatLE
          ( chord
              [ note 0 duration volume,
                note 4 duration volume,
                note 7 duration volume,
                note 12 duration volume
              ]
          )