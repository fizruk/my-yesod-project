module Handler.UpdateCourse where

import Import
import Handler.NewCourse

postUpdateCourseR :: CourseId -> Handler Value
postUpdateCourseR courseId = do
  importCourse courseId
  returnJson ("ok" :: Text)
