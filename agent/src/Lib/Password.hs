module Lib.Password where

import           Startlude
import           Yesod.Auth.Util.PasswordStore  ( makePassword
                                                , verifyPassword
                                                , passwordStrength
                                                )
import qualified Data.ByteString.Char8         as BS
                                                ( pack
                                                , unpack
                                                )
import           Data.Text                      ( pack
                                                , unpack
                                                )

import           Model

-- Root account identifier
rootAccountName :: Text
rootAccountName = "embassy-root"


-- | Default strength used for passwords (see "Yesod.Auth.Util.PasswordStore"
--   for details).
defaultStrength :: Int
defaultStrength = 17

-- | The type representing account information stored in the database should
--   be an instance of this class.  It just provides the getter and setter
--   used by the functions in this module.
class HasPasswordHash account where
    getPasswordHash :: account -> Text
    setPasswordHash :: Text -> account -> account

    {-# MINIMAL getPasswordHash, setPasswordHash #-}


-- | Calculate a new-style password hash using "Yesod.Auth.Util.PasswordStore".
passwordHash :: MonadIO m => Int -> Text -> m Text
passwordHash strength pwd = do
    h <- liftIO $ makePassword (BS.pack $ unpack pwd) strength
    return $ pack $ BS.unpack h

-- | Set password for account, using the given strength setting. Use this
--   function, or 'setPassword', to produce a account record containing the
--   hashed password.  Unlike previous versions of this module, no separate
--   salt field is required for new passwords (but it may still be required
--   for compatibility while old password hashes remain in the database).
--
--   This function does not change the database; the calling application
--   is responsible for saving the data which is returned.
setPasswordStrength :: (MonadIO m, HasPasswordHash account) => Int -> Text -> account -> m account
setPasswordStrength strength pwd u = do
    hashed <- passwordHash strength pwd
    return $ setPasswordHash hashed u

-- | As 'setPasswordStrength', but using the 'defaultStrength'
setPassword :: (MonadIO m, HasPasswordHash account) => Text -> account -> m account
setPassword = setPasswordStrength defaultStrength

validatePass :: HasPasswordHash u => u -> Text -> Bool
validatePass account password = do
    let h = getPasswordHash account
    -- NB plaintext password characters are truncated to 8 bits here,
    -- and also in passwordHash above (the hash is already 8 bit).
    -- This is for historical compatibility, but in practice it is
    -- unlikely to reduce the entropy of most users' alphabets by much.
    let hash'     = BS.pack $ unpack h
        password' = BS.pack $ unpack password
    if passwordStrength hash' > 0
        -- Will give >0 for valid hash format, else treat as if wrong password
        then verifyPassword password' hash'
        else False

instance HasPasswordHash Account where
    getPasswordHash = accountPassword
    setPasswordHash h u = u { accountPassword = h }
