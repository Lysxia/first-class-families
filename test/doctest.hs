{-# LANGUAGE CPP                    #-}

import Test.DocTest

#if __GLASGOW_HASKELL__ >= 902

exts :: [String]
exts =
  [ "-XDataKinds"
  , "-XKindSignatures"
  , "-XTypeFamilies"
  , "-XTypeOperators"
  , "-XUndecidableInstances"
  ]

main :: IO ()
main = doctest $ exts ++ ["src"] 

#else

main :: IO ()
main = pure ()

#endif
