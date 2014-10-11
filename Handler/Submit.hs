{-# LANGUAGE RecordWildCards #-}
module Handler.Submit where

import Import
import Data.Aeson.TH
import Data.Time
import Data.Text (unpack)

import Control.Concurrent
import Control.Monad
import System.Exit
import System.LXC

import Yesod.Auth

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

setSubmissionStatus :: SubmissionId -> SubmissionStatus -> Handler ()
setSubmissionStatus id status = runDB $ do
  currentStatus <- submissionStatus <$> getJust id
  when (currentStatus < status) $ do
    updateWhere [SubmissionId ==. id] [SubmissionStatus =. status]

checkSubmission :: SubmissionId -> Submission -> Handler ()
checkSubmission submissionId Submission{..} = do
  let tmpName = unpack $ "submit-" <> toPathPiece submissionId
  lxcBase   <- extraLxcBase <$> getExtra
  Just tmp  <- withContainer lxcBase $ clone (Just tmpName) Nothing [CloneSnapshot] Nothing Nothing Nothing []

  runInnerHandler <- handlerToIO
  let checkpoint status = do
        liftIO $ threadDelay 2000000
        runInnerHandler $ setSubmissionStatus submissionId status

  withContainer tmp $ do
    start False []
    checkpoint SubmissionStarted
    exitCode <- attachRunWait defaultAttachOptions "echo" ["echo", "Hello, world!"]
    case exitCode of
      Nothing           -> checkpoint SubmissionAborted
      Just ExitSuccess  -> checkpoint SubmissionPassedTests
      Just _            -> checkpoint SubmissionFailedTests
    stop
    destroy

  return ()

postSubmitR :: TaskId -> Handler Value
postSubmitR taskId = do
  authId        <- requireAuth
  payload       <- requireJsonBody :: Handler SubmissionPayload
  submission    <- createSubmission payload
  submissionId  <- runDB $ insert submission

  forkHandler
    (\_ -> setSubmissionStatus submissionId SubmissionAborted)
    (checkSubmission submissionId submission)

  returnJson $ submissionId
