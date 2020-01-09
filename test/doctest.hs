import System.FilePath.Glob (glob)
import Test.DocTest

exts :: [String]
exts =
  [ "-XDataKinds"
  , "-XKindSignatures"
  , "-XTypeFamilies"
  , "-XTypeOperators"
  , "-XUndecidableInstances"
  ]

main :: IO ()
main = do
  xs <- glob "src/**/*.hs"
  doctest (exts ++ xs)
