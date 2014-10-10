{-# LANGUAGE RecordWildCards #-}
module Handler.Submit where

import Import
import System.LXC
import Data.Aeson.TH
import Data.Time
import Data.Text (unpack)

import SubmissionStatus

data SubmissionPayload = SubmissionPayload
  { submissionPayloadSender :: UserId
  , submissionPayloadTask   :: TaskId
  }
  deriving (Eq, Show, Read)

instance FromJSON SubmissionPayload where
     parseJSON (Object v) = SubmissionPayload
                        <$> v .: "sender"
                        <*> v .: "task"
     -- A non-Object value is of the wrong type, so fail.
     parseJSON _          = empty

instance ToJSON SubmissionPayload where
     toJSON (SubmissionPayload sender task) = object ["sender" .= sender, "task" .= task]

createSubmission :: MonadIO m => SubmissionPayload -> m Submission
createSubmission SubmissionPayload{..} = do
  sentAt <- liftIO getCurrentTime
  return $ Submission
    { submissionSender  = submissionPayloadSender
    , submissionTask    = submissionPayloadTask
    , submissionSentAt  = sentAt
    , submissionStatus  = SubmissionReceived
    }

postSubmitR :: TaskId -> Handler Value
postSubmitR taskId = do
  payload       <- requireJsonBody :: Handler SubmissionPayload
  submission    <- createSubmission payload
  submissionId  <- runDB $ insert submission

  let tmpName = unpack $ "submit-" <> toPathPiece submissionId
  lxcBase   <- extraLxcBase <$> getExtra
  Just tmp  <- withContainer lxcBase $ clone (Just tmpName) Nothing [CloneSnapshot] Nothing Nothing Nothing []
  withContainer tmp $ do
    start False []
    attachRunWait defaultAttachOptions "echo" ["echo", "Hello, world!"]
    stop
    destroy

  returnJson $ submissionId
