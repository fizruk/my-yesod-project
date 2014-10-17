{-# LANGUAGE RecordWildCards #-}
module Handler.Submit where

import Import
import Data.Time

import qualified Data.ByteString.Lazy as L
import Data.Text (Text)
import qualified Data.Text as Text

import Data.Aeson
import qualified Data.Yaml as Yaml

import Data.Maybe

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

getSectionConfig :: Maybe SectionId -> Handler (ConfigWithOptions TrassConfig)
getSectionConfig Nothing = return mempty
getSectionConfig (Just sectionId) = do
  section <- runDB $ get404 sectionId
  case sectionConfig section of
    Nothing   -> getSectionConfig (sectionParent section)
    Just cfg  -> return . fromMaybe mempty . decodeStrict $ cfg

checkSubmission :: SubmissionId -> ConfigWithOptions TrassConfig -> FilePath -> FilePath -> Handler ()
checkSubmission submissionId sectionCfg taskPath submitFile = do
  -- runInnerHandler <- handlerToIO
  -- let checkpoint = runInnerHandler . setSubmissionStatus submissionId

  cfg <- getTrassConfig
  liftIO $ do
    let Right c = applyConfiguration cfg sectionCfg
    submit (Container "test-1" Nothing) submitFile [taskPath] c

  return ()

postSubmitR :: TaskId -> Handler Value
postSubmitR taskId = do
  authId      <- requireAuthId
  task        <- runDB $ get404 taskId
  sectionCfg  <- getSectionConfig (Just $ taskSection task)
  mfile       <- lookupFile "filedata"
  case mfile of
    Nothing -> invalidArgs ["filedata"]
    Just fileInfo -> do
      let payload    = SubmissionPayload authId taskId
          submitFile = "/home/fizruk/temp/submission.payload" -- TODO: withTempFile
      liftIO $ fileMove fileInfo submitFile
      submission    <- createSubmission payload
      submissionId  <- runDB $ insert submission

      forkHandler
        (\_ -> setSubmissionStatus submissionId SubmissionAborted)
        (checkSubmission submissionId sectionCfg (taskPath task) submitFile)

      returnJson $ submissionId

