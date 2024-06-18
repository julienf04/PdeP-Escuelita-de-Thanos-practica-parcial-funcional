module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  describe "Test de si un universo es apto para pendex" $ do
    it "Si es apto para pendex" $ do
      esAptoParaPendex universoLoco `shouldBe` True

