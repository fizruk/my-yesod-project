module Handler.TaskSubmission where

import Import

getTaskSubmissionR :: SubmissionId -> Handler Value
getTaskSubmissionR submissionId = do
  submission <- runDB $ get404 submissionId
  returnJson submission
