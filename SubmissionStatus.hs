module SubmissionStatus where

import Prelude
import Database.Persist.TH
import Data.Aeson.TH

data SubmissionStatus
  = SubmissionReceived
  | SubmissionStarted
  | SubmissionMalformed
  | SubmissionErroredTests
  | SubmissionFailedTests
  | SubmissionAborted
  | SubmissionPassedTests
  | SubmissionInReview
  | SubmissionRejected
  | SubmissionAccepted
  deriving (Eq, Ord, Show, Read)

deriveJSON defaultOptions ''SubmissionStatus

derivePersistField "SubmissionStatus"
