module Handler.NewSection where

import Import

postNewSectionR :: Handler Value
postNewSectionR = do
  section <- requireJsonBody :: Handler Section
  sectionId <- runDB $ insert section
  returnJson $ sectionId
