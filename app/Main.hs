import Data.ByteString.Builder qualified as Bob
import Data.ByteString.Lazy qualified as Lazy
import Data.Foldable

main :: IO ()
main = z

wave :: [Float]
wave = map sin [0.0 .. 48000]

x = fold $ map Bob.floatLE wave

y = Bob.toLazyByteString x

z = Lazy.writeFile "output.bin" y