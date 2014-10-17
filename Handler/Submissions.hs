module Handler.Submissions where

import Data.List

import Import

import Yesod.Auth

getSubmissionsR :: Handler Html
getSubmissionsR = do
  authId <- requireAuthId
  submissions <- runDB $ selectList [ SubmissionSender ==. authId ] [ Desc SubmissionSentAt ]
  let taskIds = map (submissionTask . entityVal) submissions
  tasks <- mapM (runDB . get) taskIds

  let contents = zip3 [1..] submissions tasks

  defaultLayout $ toWidget
    [hamlet|
      <table .table>
        <thead>
          <tr>
            <th> #
            <th> Sent at
            <th> Task
            <th> Status
        <tbody>
          $forall (n, Entity submissionId submission, task) <- contents
            <tr>
              <td> #{show n}
              <td> #{show $ submissionSentAt submission}
              <td>
                $maybe t <- task
                  <a href=@{TaskR (submissionTask submission)}> #{taskTitle t}
                $nothing
                  Unknown
              <td> #{show $ submissionStatus submission}
    |]
