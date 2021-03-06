{-# LANGUAGE FlexibleInstances #-}
module Model where

import Yesod
import Data.Text (Text)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import Database.Persist.Quasi
import Data.Typeable (Typeable)
import Data.Time
import Prelude

import RepositoryType
import SubmissionStatus

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")
