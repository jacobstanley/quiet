{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ < 806
main :: IO ()
main =
  putStrLn "Tests disabled for GHC < 8.6"
#else

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}

import           GHC.Generics (Generic(..))

import           Hedgehog
import           Hedgehog.Main (defaultMain)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Data.Ratio ((%))

import           Quiet

import           Text.Read (Read(..), readEither)

data Simple =
  Simple Int String Double Rational
  deriving (Eq, Ord, Read, Show)

data Record =
  Record {
      recordInt :: Int
    , recordString :: String
    , recordDouble :: Double
    , recordRational :: Rational
    } deriving (Eq, Ord, Generic)
      deriving (Read, Show) via (Quiet Record)

data Outrageous =
    Flipper Record
  | Int :! Int
  | Double :@ Double
  | Int `Quux` Double
  | String :# Record
  | Simple :$ Outrageous
  | DontDoThis { outrageousInt :: Int, outrageousString :: String }
    deriving (Eq, Ord, Generic)
    deriving (Read, Show) via (Quiet Outrageous)

genInt :: Gen Int
genInt =
  Gen.int (Range.constantFrom 0 (-100) 100)

genString :: Gen String
genString =
  Gen.list (Range.constant 0 20) Gen.unicode

genDouble :: Gen Double
genDouble =
  Gen.double (Range.constantFrom 0 (-100) 100)

genRational :: Gen Rational
genRational =
  (%)
    <$> Gen.integral (Range.constantFrom 0 (-100) 100)
    <*> Gen.filter (/= 0) (Gen.integral (Range.constantFrom 0 (-100) 100))

genSimple :: Gen Simple
genSimple =
  Simple
    <$> genInt
    <*> genString
    <*> genDouble
    <*> genRational

genRecord :: Gen Record
genRecord =
  Record
    <$> genInt
    <*> genString
    <*> genDouble
    <*> genRational

genOutrageous :: Gen Outrageous
genOutrageous =
  Gen.recursive Gen.choice [
      Flipper <$> genRecord
    , (:!) <$> genInt <*> genInt
    , (:@) <$> genDouble <*> genDouble
    , Quux <$> genInt <*> genDouble
    , (:#) <$> genString <*> genRecord
    , DontDoThis <$> genInt <*> genString
    ] [
      Gen.subtermM genOutrageous (\x -> (:$) <$> genSimple <*> pure x)
    ]

eq :: Simple -> Record -> Bool
eq (Simple x0 y0 z0 w0) (Record x1 y1 z1 w1) =
  and [
      x0 == x1
    , y0 == y1
    , z0 == z1
    , w0 == w1
    ]

prop_simple :: Property
prop_simple =
  property $ do
    x <- forAll genSimple
    tripping x show readEither

prop_record :: Property
prop_record =
  property $ do
    x <- forAll genRecord
    tripping x show readEither

prop_simple_record :: Property
prop_simple_record =
  property $ do
    x <- forAll genSimple
    y <- evalEither . readEither . ("Record" <>) . drop 6 $ show x
    annotateShow y
    assert $ eq x y

prop_outrageous :: Property
prop_outrageous =
  property $ do
    x <- forAll genOutrageous
    tripping x show readEither

main :: IO ()
main =
  defaultMain [checkParallel $$discover]
#endif
