module Handler.Task where

import Import

import Handler.Submit

import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy as L

import Text.Markdown
import Text.Highlighting.Kate

import Yesod.Auth

import SubmissionStatus

-- | Render markdown with code highlighting.
renderMarkdown :: Text -> Html
renderMarkdown = markdown def {msBlockCodeRenderer = kateBlockCodeRenderer} . L.fromStrict

-- | Highlighting code with Kate Highlighter.
kateBlockCodeRenderer :: Maybe Text -> (Text, Html) -> Html
kateBlockCodeRenderer lang (src, _) = formatHtmlBlock defaultFormatOpts $ highlightAs (maybe "text" Text.unpack lang) $ Text.unpack src

getTaskR :: TaskId -> Handler TypedContent
getTaskR taskId = do
  task <- runDB $ get404 taskId
  selectRep $ do
    provideRep $ defaultLayout $ toWidget
      [hamlet|
        <p> #{renderMarkdown (taskDescription task)}
        <form method=post action=@{TaskR taskId} role=form>
          <button .btn .btn-primary type="submit"> Submit
          <textarea name="inPageCodeInput" #codeMirrorTextArea .form-control>
      |]
    provideJson $ taskDescription task

postTaskR :: TaskId -> Handler TypedContent
postTaskR taskId = do
  authId      <- requireAuthId
  task        <- runDB $ get404 taskId
  sectionCfg  <- getSectionConfig (Just $ taskSection task)
  solution    <- lookupPostParam "inPageCodeInput"
  case solution of
    Nothing -> invalidArgs ["inPageCodeInput"]
    Just text -> do
      let payload    = SubmissionPayload authId taskId
          submitFile = "/home/fizruk/temp/submission.payload" -- TODO: withTempFile
      liftIO $ Text.writeFile submitFile text
      submission    <- createSubmission payload
      submissionId  <- runDB $ insert submission

      forkHandler
        (\_ -> setSubmissionStatus submissionId SubmissionAborted)
        (checkSubmission submissionId sectionCfg (taskPath task) submitFile)

      selectRep $ do
        provideRep $ defaultLayout $ toWidget
          [hamlet|
            <p> Submission #{toPathPiece submissionId} sent.
          |]
        provideJson $ submissionId
