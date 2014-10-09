module RepositoryType where

import Prelude
import Database.Persist.TH
import Data.Aeson.TH

data RepositoryType
  = RepoGitHub
  | RepoTrass
  | RepoURL
  deriving (Eq, Show, Read)

deriveJSON defaultOptions ''RepositoryType

derivePersistField "RepositoryType"
