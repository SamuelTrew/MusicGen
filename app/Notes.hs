module Notes where

type Hz = Float

type Semitones = Float

a4 :: Hz
a4 = 440.0

pitchStandard = a4

b4 :: Hz
b4 = 493.88

interval :: Float
interval = 2 ** (1.0 / 12.0)

frequencyCalc :: Semitones -> Hz
frequencyCalc n = pitchStandard * (interval ** n)