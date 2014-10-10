module SubmissionStatus where

import Prelude
import Database.Persist.TH
import Data.Aeson.TH

data SubmissionStatus
  = SubmissionReceived
  | SubmissionInvalid
  | SubmissionCompilationError
  | SubmissionFailedTests
  | SubmissionPassedTests
  | SubmissionInReview
  | SubmissionRejected
  | SubmissionAccepted
  deriving (Eq, Show, Read)

deriveJSON defaultOptions ''SubmissionStatus

derivePersistField "SubmissionStatus"
