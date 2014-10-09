module Handler.ListSections where

import Import

getListSectionsR :: Handler Value
getListSectionsR = do
  sections <- runDB $ selectList [ SectionParent ==. Nothing ] []
  returnJson sections
