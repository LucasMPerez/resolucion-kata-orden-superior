module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  describe "Test de Casas Grandes" $ do
    it "unaCasa es grande" $ do
      casaGrande unaCasa `shouldBe` True
    it "otraCasa es grande" $ do
      casaGrande otraCasa `shouldBe` False
    it "terceraCasa es grande" $ do
      casaGrande terceraCasa `shouldBe` False