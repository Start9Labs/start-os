{- |
Module : Lib.Types.Emver
Description : Semver with 4th digit extension for Embassy
License : Start9 Non-Commercial
Maintainer : keagan@start9labs.com
Stability : experimental
Portability : portable

This module was designed to address the problem of releasing updates to Embassy Packages where the upstream project was
either unaware of or apathetic towards supporting their application on the Embassy platform. In most cases, the original
package will support <https://semver.org/spec/v2.0.0.html semver2>. This leaves us with the problem where we would like
to preserve the original package's version, since one of the goals of the Embassy platform is transparency. However, on
occasion, we have screwed up and published a version of a package that needed to have its metadata updated. In this
scenario we were left with the conundrum of either unilaterally claiming a version number of a package we did not author
or let the issue persist until the next update. Neither of these promote good user experiences, for different reasons.
This module extends the semver standard linked above with a 4th digit, which is given PATCH semantics.
-}

module Lib.Types.Emver
    ( major
    , minor
    , patch
    , revision
    , satisfies
    , (<||)
    , (||>)
    -- we do not export 'None' because it is useful for its internal algebraic properties only
    , VersionRange(Anchor, Any, None)
    , Version(..)
    , AnyRange(..)
    , AllRange(..)
    , conj
    , disj
    , exactly
    , parseVersion
    , parseRange
    )
where

import           Prelude
import qualified Data.Attoparsec.Text          as Atto
import           Data.Function
import           Data.Functor                   ( (<&>)
                                                , ($>)
                                                )
import           Control.Applicative            ( liftA2
                                                , Alternative((<|>))
                                                )
import           Data.String                    ( IsString(..) )
import qualified Data.Text                     as T

-- | AppVersion is the core representation of the SemverQuad type.
newtype Version = Version { unVersion :: (Word, Word, Word, Word) } deriving (Eq, Ord)
instance Show Version where
    show (Version (x, y, z, q)) =
        let postfix = if q == 0 then "" else '.' : show q in show x <> "." <> show y <> "." <> show z <> postfix
instance IsString Version where
    fromString s = either error id $ Atto.parseOnly parseVersion (T.pack s)
instance Read Version where
    readsPrec i = 

-- | A change in the value found at 'major' implies a breaking change in the API that this version number describes
major :: Version -> Word
major (Version (x, _, _, _)) = x

-- | A change in the value found at 'minor' implies a backwards compatible addition to the API that this version number
-- describes
minor :: Version -> Word
minor (Version (_, y, _, _)) = y

-- | A change in the value found at 'patch' implies that the implementation of the API has changed without changing the
-- invariants promised by the API. In many cases this will be incremented when repairing broken functionality
patch :: Version -> Word
patch (Version (_, _, z, _)) = z

-- | This is the fundamentally new value in comparison to the original semver 2.0 specification. It is given the same
-- semantics as 'patch' above, which begs the question, when should you update this value instead of that one. Generally
-- speaking, if you are both the package author and maintainer, you should not ever increment this number, as it is
-- redundant with 'patch'. However, if you maintain a package on some distribution channel, and you are /not/ the
-- original author, then it is encouraged for you to increment 'quad' instead of 'patch'.
revision :: Version -> Word
revision (Version (_, _, _, q)) = q


-- | 'Operator' is the type that specifies how to compare against the target version. Right represents the ordering,
-- Left negates it
type Operator = Either Ordering Ordering

-- | 'VersionRange' is the algebra of sets of versions. They can be constructed by having an 'Anchor' term which
-- compares against the target version, or can be described with 'Conj' which is a conjunction, or 'Disj', which is a
-- disjunction. The 'Any' and 'All' terms are primarily there to round out the algebra, but 'Any' is also exposed due to
-- its usage in semantic versioning in general. The 'None' term is not useful to the end user as there would be no
-- reasonable usage of it to describe version sets. It is included for its utility as a unit on 'Disj' and possibly as
-- a zero on 'Conj'
--
-- Laws (reflected in implementations of smart constructors):
-- Commutativity of conjunction: Conj a b === Conj b a
-- Commutativity of disjunction: Disj a b === Disj b a
-- Associativity of conjunction: Conj (Conj a b) c === Conj a (Conj b c)
-- Associativity of disjunction: Disj (Disj a b) c === Disj a (Disj b c)
-- Identity of conjunction: Any `Conj` a === a
-- Identity of disjunction: None `Disj` a === a
-- Zero of conjunction: None `Conj` a === None
-- Zero of disjunction: Any `Disj` a === Any
-- Distributivity of conjunction over disjunction: Conj a (Disj b c) === Disj (Conj a b) (Conj a c)
-- Distributivity of disjunction over conjunction: Disj a (Conj b c) === Conj (Disj a b) (Disj a c)
data VersionRange
    = Anchor Operator Version
    | Conj VersionRange VersionRange
    | Disj VersionRange VersionRange
    | Any
    | None
    deriving (Eq)

-- | Smart constructor for conjunctions. Eagerly evaluates zeros and identities
conj :: VersionRange -> VersionRange -> VersionRange
conj Any  b    = b
conj a    Any  = a
conj None _    = None
conj _    None = None
conj a    b    = Conj a b

-- | Smart constructor for disjunctions. Eagerly evaluates zeros and identities
disj :: VersionRange -> VersionRange -> VersionRange
disj Any  _    = Any
disj _    Any  = Any
disj None b    = b
disj a    None = a
disj a    b    = Disj a b

exactly :: Version -> VersionRange
exactly = Anchor (Right EQ)

instance Show VersionRange where
    show (Anchor (  Left  EQ) v           ) = '!' : '=' : show v
    show (Anchor (  Right EQ) v           ) = '=' : show v
    show (Anchor (  Left  LT) v           ) = '>' : '=' : show v
    show (Anchor (  Right LT) v           ) = '<' : show v
    show (Anchor (  Left  GT) v           ) = '<' : '=' : show v
    show (Anchor (  Right GT) v           ) = '>' : show v
    show (Conj   a@(Disj _ _) b@(Disj _ _)) = paren (show a) <> (' ' : paren (show b))
    show (Conj   a@(Disj _ _) b           ) = paren (show a) <> (' ' : show b)
    show (Conj   a            b@(Disj _ _)) = show a <> (' ' : paren (show b))
    show (Conj   a            b           ) = show a <> (' ' : show b)
    show (Disj   a            b           ) = show a <> " || " <> show b
    show Any                                = "*"
    show None                               = "!"
instance Read VersionRange where
    readsPrec _ s = case Atto.parseOnly parseRange (T.pack s) of
        Left  _ -> []
        Right a -> [(a, "")]

paren :: String -> String
paren = mappend "(" . flip mappend ")"

newtype AnyRange = AnyRange { unAnyRange :: VersionRange }
instance Semigroup AnyRange where
    (<>) = AnyRange <<$>> disj `on` unAnyRange
instance Monoid AnyRange where
    mempty = AnyRange None

newtype AllRange = AllRange { unAllRange :: VersionRange }
instance Semigroup AllRange where
    (<>) = AllRange <<$>> conj `on` unAllRange
instance Monoid AllRange where
    mempty = AllRange Any

-- | Predicate for deciding whether the 'Version' is in the 'VersionRange'
satisfies :: Version -> VersionRange -> Bool
satisfies v (Anchor op v') = either (\c x y -> compare x y /= c) (\c x y -> compare x y == c) op v v'
satisfies v (Conj   a  b ) = v `satisfies` a && v `satisfies` b
satisfies v (Disj   a  b ) = v `satisfies` a || v `satisfies` b
satisfies _ Any            = True
satisfies _ None           = False

(<||) :: Version -> VersionRange -> Bool
(<||) = satisfies
{-# INLINE (<||) #-}

(||>) :: VersionRange -> Version -> Bool
(||>) = flip satisfies
{-# INLINE (||>) #-}

(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap
{-# INLINE (<<$>>) #-}

parseOperator :: Atto.Parser Operator
parseOperator =
    (Atto.char '=' $> Right EQ)
        <|> (Atto.string "!=" $> Left EQ)
        <|> (Atto.string ">=" $> Left LT)
        <|> (Atto.string "<=" $> Left GT)
        <|> (Atto.char '>' $> Right GT)
        <|> (Atto.char '<' $> Right LT)

parseVersion :: Atto.Parser Version
parseVersion = do
    major' <- Atto.decimal <* Atto.char '.'
    minor' <- Atto.decimal <* Atto.char '.'
    patch' <- Atto.decimal
    quad'  <- Atto.option 0 $ Atto.char '.' *> Atto.decimal
    pure $ Version (major', minor', patch', quad')

-- >>> Atto.parseOnly parseRange "=2.3.4 1.2.3.4 - 2.3.4.5 (>3.0.0 || <3.4.5)"
-- Right =2.3.4 >=1.2.3.4 <=2.3.4.5 ((>3.0.0 || <3.4.5))
-- >>> Atto.parseOnly parseRange "0.2.6"
-- Right =0.2.6
parseRange :: Atto.Parser VersionRange
parseRange = s <|> (Atto.char '*' *> pure Any) <|> (Anchor (Right EQ) <$> parseVersion)
    where
        sub = Atto.char '(' *> Atto.skipSpace *> parseRange <* Atto.skipSpace <* Atto.char ')'
        s =
            unAnyRange
                .   foldMap AnyRange
                <$> ((p <|> sub) `Atto.sepBy1` (Atto.skipSpace *> Atto.string "||" <* Atto.skipSpace))
        p = unAllRange . foldMap AllRange <$> ((a <|> sub) `Atto.sepBy1` Atto.space)
        a = liftA2 Anchor parseOperator parseVersion <|> caret <|> tilde <|> wildcard <|> hyphen

-- >>> liftA2 satisfies (Atto.parseOnly parseVersion "0.20.1.1") (Atto.parseOnly parseRange "^0.20.1")
-- Right True
caret :: Atto.Parser VersionRange
caret = (Atto.char '^' *> parseVersion) <&> \case
    v@(Version (0, 0, 0, _)) -> Anchor (Right EQ) v
    v@(Version (0, 0, z, _)) -> rangeIE v (Version (0, 0, z + 1, 0))
    v@(Version (0, y, _, _)) -> rangeIE v (Version (0, y + 1, 0, 0))
    v@(Version (x, _, _, _)) -> rangeIE v (Version (x + 1, 0, 0, 0))

-- >>> Atto.parseOnly tilde "~1.2.3.4"
-- Right >=1.2.3.4 <1.2.4
tilde :: Atto.Parser VersionRange
tilde = (Atto.char '~' *> (Atto.decimal `Atto.sepBy1` Atto.char '.')) >>= \case
    [x, y, z, q] -> pure $ rangeIE (Version (x, y, z, q)) (Version (x, y, z + 1, 0))
    [x, y, z]    -> pure $ rangeIE (Version (x, y, z, 0)) (Version (x, y + 1, 0, 0))
    [x, y]       -> pure $ rangeIE (Version (x, y, 0, 0)) (Version (x, y + 1, 0, 0))
    [x]          -> pure $ rangeIE (Version (x, 0, 0, 0)) (Version (x + 1, 0, 0, 0))
    o            -> fail $ "Invalid number of version numbers: " <> show (length o)

range :: Bool -> Bool -> Version -> Version -> VersionRange
range inc0 inc1 v0 v1 =
    let lo = if inc0 then Left LT else Right GT
        hi = if inc1 then Left GT else Right LT
    in  Conj (Anchor lo v0) (Anchor hi v1)

rangeIE :: Version -> Version -> VersionRange
rangeIE = range True False

-- >>> Atto.parseOnly wildcard "1.2.3.x"
-- Right >=1.2.3 <1.2.4
wildcard :: Atto.Parser VersionRange
wildcard = (Atto.many1 (Atto.decimal <* Atto.char '.') <* Atto.char 'x') >>= \case
    [x, y, z] -> pure $ rangeIE (Version (x, y, z, 0)) (Version (x, y, z + 1, 0))
    [x, y]    -> pure $ rangeIE (Version (x, y, 0, 0)) (Version (x, y + 1, 0, 0))
    [x]       -> pure $ rangeIE (Version (x, 0, 0, 0)) (Version (x + 1, 0, 0, 0))
    o         -> fail $ "Invalid number of version numbers: " <> show (length o)

-- >>> Atto.parseOnly hyphen "0.1.2.3 - 1.2.3.4"
-- Right >=0.1.2.3 <=1.2.3.4
hyphen :: Atto.Parser VersionRange
hyphen = liftA2 (range True True) parseVersion (Atto.skipSpace *> Atto.char '-' *> Atto.skipSpace *> parseVersion)
