{-# LANGUAGE RecordWildCards #-}
module Handler.TestLogin where

import Import

import Yesod.Auth
import Yesod.Auth.Account hiding (UserAccount)
import qualified Yesod.Auth.Message as Msg

data Credentials = Credentials
  { credentialsUsername :: Text
  , credentialsPassword :: Text
  }
  deriving (Eq, Show, Read)

instance FromJSON Credentials where
     parseJSON (Object v) = Credentials
                        <$> v .: "username"
                        <*> v .: "password"
     -- A non-Object value is of the wrong type, so fail.
     parseJSON _          = empty

instance ToJSON Credentials where
     toJSON (Credentials uname passwd) = object ["username" .= uname, "password" .= passwd]

instance ToJSON UserAccount where
    toJSON UserAccount{..} = object
      [ "username" .= userAccountUsername ]

postTestLoginR :: Handler Value
postTestLoginR = do
  Credentials uname passwd <- requireJsonBody :: Handler Credentials
  mr  <- getMessageRender
  uac <- runAccountDB $ loadUser uname
  res <- case uac of
           Just u | verifyPassword passwd (userPasswordHash u) && userEmailVerified u -> do
             setCreds False $ Creds "account" (username u) []
             return $ Right u
           _ -> do
             return $ Left [mr Msg.InvalidUsernamePass]
  returnJson (entityVal <$> res)
