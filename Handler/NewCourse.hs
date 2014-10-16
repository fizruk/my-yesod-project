{-# LANGUAGE RecordWildCards #-}
module Handler.NewCourse where

import Import

import Data.Aeson
import Data.ByteString.Lazy (toStrict)
import qualified Data.Text as Text

import System.Exit
import System.FilePath
import System.Process

import qualified Trass.Course as C

import Yesod.Auth

importSection :: CourseId -> Maybe SectionId -> C.Section -> Handler SectionId
importSection courseId sectionId C.Section{..} = runDB $ insert $ Section
  sectionPath
  sectionTitle
  sectionSummary
  sectionOverview
  sectionTheory
  (toStrict . encode <$> sectionTrassConfig)
  courseId
  sectionId

importTask :: CourseId -> SectionId -> C.Task -> Handler TaskId
importTask courseId sectionId C.Task{..} = runDB $ insert $ Task
  taskPath
  taskTitle
  taskDescription
  sectionId
  courseId

importCourse' :: CourseId -> Maybe SectionId -> C.Course -> Handler SectionId
importCourse' courseId sectionId C.Course{..} = do
  newSectionId <- importSection courseId sectionId courseSection
  mapM_ (importTask    courseId newSectionId) courseTasks
  mapM_ (importCourse' courseId $ Just newSectionId) courseSubsections
  return newSectionId

importCourse :: CourseId -> Handler ()
importCourse courseId = do
  course <- runDB $ get404 courseId
  let coursePath = "/home/fizruk/.trass/courses" </> Text.unpack (toPathPiece courseId)

  liftIO $ callProcess "rm" ["-rf", coursePath]

  case courseSourceRepo course of
    Nothing -> return ()
    Just repo -> liftIO $ do
      callProcess "git" ["clone", "--depth", "1", "--single-branch", Text.unpack repo, coursePath]

  c <- liftIO $ C.readCourse coursePath

  runDB $ do
    deleteWhere [ SectionCourse ==. courseId ]
    deleteWhere [ TaskCourse ==. courseId ]

  rootSectionId <- importCourse' courseId Nothing c
  runDB $ update courseId [ CourseRootSection =. rootSectionId ]

postNewCourseR :: Handler Value
postNewCourseR = do
  authId <- requireAuthId
  course <- requireJsonBody :: Handler Course
  courseId <- runDB $ insert course
  importCourse courseId
  returnJson courseId

