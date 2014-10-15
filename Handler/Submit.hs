{-# LANGUAGE RecordWildCards #-}
module Handler.Submit where

import Import
import Data.Time

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Yaml

import Control.Monad
import System.Exit

import Yesod.Auth

import SubmissionStatus

import Trass
import Trass.Config
import Trass.Config.Options
import System.LXC

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

checkSubmission :: SubmissionId -> Submission -> FilePath -> Handler ()
checkSubmission submissionId Submission{..} submitFile = do
  -- runInnerHandler <- handlerToIO
  -- let checkpoint = runInnerHandler . setSubmissionStatus submissionId

  liftIO $ do
    Just cfg     <- decodeFile "/home/fizruk/trass-lxc/examples/.trass.yml"
    Just section <- decodeFile "/home/fizruk/trass-lxc/examples/.section.yml"
    let Right c = applyConfiguration cfg section
    submit (Container "test-1" Nothing) submitFile ["/home/fizruk/trass-lxc/examples/common_task/", "/home/fizruk/trass-lxc/examples/task-1/"] c

  return ()

postSubmitR :: TaskId -> Handler Value
postSubmitR taskId = do
  authId  <- requireAuthId
  mfile   <- lookupFile "filedata"
  case mfile of
    Nothing -> invalidArgs ["filedata"]
    Just fileInfo -> do
      let payload    = SubmissionPayload authId taskId
          submitFile = "/home/fizruk/temp/submission.payload"
      liftIO $ fileMove fileInfo submitFile
      submission    <- createSubmission payload
      submissionId  <- runDB $ insert submission

      forkHandler
        (\_ -> setSubmissionStatus submissionId SubmissionAborted)
        (checkSubmission submissionId submission submitFile)

      returnJson $ submissionId

