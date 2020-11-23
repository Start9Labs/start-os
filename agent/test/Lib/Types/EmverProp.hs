{-# LANGUAGE TemplateHaskell #-}
module Lib.Types.EmverProp where

import           Startlude               hiding ( Any )

import           Hedgehog                      as Test
import           Lib.Types.Emver
import           Hedgehog.Range
import           Hedgehog.Gen                  as Gen
import qualified Data.Attoparsec.Text          as Atto

versionGen :: MonadGen m => m Version
versionGen = do
    a <- word (linear 0 30)
    b <- word (linear 0 30)
    c <- word (linear 0 30)
    d <- word (linear 0 30)
    pure $ Version (a, b, c, d)

rangeGen :: MonadGen m => m VersionRange
rangeGen = choice [pure None, pure Any, anchorGen, disjGen, conjGen]

anchorGen :: MonadGen m => m VersionRange
anchorGen = do
    c <- element [LT, EQ, GT]
    f <- element [Left, Right]
    Anchor (f c) <$> versionGen

conjGen :: MonadGen m => m VersionRange
conjGen = liftA2 conj rangeGen rangeGen

disjGen :: MonadGen m => m VersionRange
disjGen = liftA2 disj rangeGen rangeGen

prop_conjAssoc :: Property
prop_conjAssoc = property $ do
    a   <- forAll rangeGen
    b   <- forAll rangeGen
    c   <- forAll rangeGen
    obs <- forAll versionGen
    (obs <|| conj a (conj b c)) === (obs <|| conj (conj a b) c)

prop_conjCommut :: Property
prop_conjCommut = property $ do
    a   <- forAll rangeGen
    b   <- forAll rangeGen
    obs <- forAll versionGen
    (obs <|| conj a b) === (obs <|| conj b a)

prop_disjAssoc :: Property
prop_disjAssoc = property $ do
    a   <- forAll rangeGen
    b   <- forAll rangeGen
    c   <- forAll rangeGen
    obs <- forAll versionGen
    (obs <|| disj a (disj b c)) === (obs <|| disj (disj a b) c)

prop_disjCommut :: Property
prop_disjCommut = property $ do
    a   <- forAll rangeGen
    b   <- forAll rangeGen
    obs <- forAll versionGen
    (obs <|| disj a b) === (obs <|| disj b a)

prop_anyIdentConj :: Property
prop_anyIdentConj = property $ do
    a   <- forAll rangeGen
    obs <- forAll versionGen
    obs <|| conj Any a === obs <|| a

prop_noneIdentDisj :: Property
prop_noneIdentDisj = property $ do
    a   <- forAll rangeGen
    obs <- forAll versionGen
    obs <|| disj None a === obs <|| a

prop_noneAnnihilatesConj :: Property
prop_noneAnnihilatesConj = property $ do
    a   <- forAll rangeGen
    obs <- forAll versionGen
    obs <|| conj None a === obs <|| None

prop_anyAnnihilatesDisj :: Property
prop_anyAnnihilatesDisj = property $ do
    a   <- forAll rangeGen
    obs <- forAll versionGen
    obs <|| disj Any a === obs <|| Any

prop_conjDistributesOverDisj :: Property
prop_conjDistributesOverDisj = property $ do
    a   <- forAll rangeGen
    b   <- forAll rangeGen
    c   <- forAll rangeGen
    obs <- forAll versionGen
    obs <|| conj a (disj b c) === obs <|| disj (conj a b) (conj a c)

prop_disjDistributesOverConj :: Property
prop_disjDistributesOverConj = property $ do
    a   <- forAll rangeGen
    b   <- forAll rangeGen
    c   <- forAll rangeGen
    obs <- forAll versionGen
    obs <|| disj a (conj b c) === obs <|| conj (disj a b) (disj a c)

prop_anyAcceptsAny :: Property
prop_anyAcceptsAny = property $ do
    obs <- forAll versionGen
    assert $ obs <|| Any

prop_noneAcceptsNone :: Property
prop_noneAcceptsNone = property $ do
    obs <- forAll versionGen
    assert . not $ obs <|| None

prop_conjBoth :: Property
prop_conjBoth = property $ do
    a   <- forAll rangeGen
    b   <- forAll rangeGen
    obs <- forAll versionGen
    (obs <|| conj a b) === (obs <|| a && obs <|| b)

prop_disjEither :: Property
prop_disjEither = property $ do
    a   <- forAll rangeGen
    b   <- forAll rangeGen
    obs <- forAll versionGen
    (obs <|| disj a b) === (obs <|| a || obs <|| b)

prop_rangeParseRoundTrip :: Property
prop_rangeParseRoundTrip = withShrinks 0 . property $ do
    a   <- forAll rangeGen
    obs <- forAll versionGen
    when (a == None) Test.discard
    -- we do not use 'tripping' here since 'tripping' requires equality of representation
    -- we only want to check equality up to OBSERVATION
    (satisfies obs <$> Atto.parseOnly parseRange (show a)) === Right (satisfies obs a)

prop_anchorLeftIsNegatedRight :: Property
prop_anchorLeftIsNegatedRight = property $ do
    a   <- forAll anchorGen
    neg <- case a of
        Anchor (Right o) v -> pure $ Anchor (Left o) v
        Anchor (Left  o) v -> pure $ Anchor (Right o) v
        _                  -> Test.discard
    obs <- forAll versionGen
    obs <|| a /== obs <|| neg

tests :: IO Bool
tests = checkParallel $ $$discover
