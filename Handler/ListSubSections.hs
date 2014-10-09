module Handler.ListSubSections where

import Import

getListSubSectionsR :: SectionId -> Handler Value
getListSubSectionsR parentId = do
  sections <- runDB $ selectList [ SectionParent ==. Just parentId ] []
  returnJson sections
