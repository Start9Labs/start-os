module Lib.Types.Url where

import           Startlude

import           Control.Monad.Fail
import qualified Data.Attoparsec.Text          as A
import qualified GHC.Show                       ( Show(..) )

-- this is a very weak definition of url, it needs to be fleshed out in accordance with https://www.ietf.org/rfc/rfc1738.txt
data Url = Url
    { urlScheme :: Text
    , urlHost   :: Text
    , urlPort   :: Word16
    }
    deriving Eq
instance Show Url where
    show (Url scheme host port) = toS $ scheme <> "://" <> host <> ":" <> show port

parseUrl :: Text -> Either String Url
parseUrl t = A.parseOnly urlParser (toS t)

urlParser :: A.Parser Url
urlParser = do
    (scheme, defPort) <- A.option ("https", 443) $ schemeParser >>= \case
        "http"  -> pure ("http", 80)
        "https" -> pure ("https", 443)
        other   -> fail $ "Invalid Scheme: " <> toS other
    eHost <- fmap Left (untilParser ":") <|> fmap Right (atLeastParser 2)
    case eHost of
        Left  host -> Url scheme host <$> portParser
        Right host -> pure $ Url scheme host defPort

untilParser :: Text -> A.Parser Text
untilParser t = toS <$> A.manyTill A.anyChar (A.string t)

atLeastParser :: Int -> A.Parser Text
atLeastParser n = do
    minLength <- toS <$> A.count n A.anyChar
    rest      <- A.takeText
    pure $ minLength <> rest

portParser :: A.Parser Word16
portParser = do
    port <- A.decimal
    A.atEnd >>= \case
        True  -> pure port
        False -> fail "invalid port"

schemeParser :: A.Parser Text
schemeParser = toS <$> A.manyTill A.anyChar (A.string "://")
