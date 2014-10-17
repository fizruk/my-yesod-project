module Handler.Section where

import Import

import qualified Data.Text as Text
import qualified Data.Text.Lazy as L

import Text.Markdown
import Text.Highlighting.Kate

-- | Render markdown with code highlighting.
renderMarkdown :: Text -> Html
renderMarkdown = markdown def {msBlockCodeRenderer = kateBlockCodeRenderer} . L.fromStrict

-- | Highlighting code with Kate Highlighter.
kateBlockCodeRenderer :: Maybe Text -> (Text, Html) -> Html
kateBlockCodeRenderer lang (src, _) = formatHtmlBlock defaultFormatOpts $ highlightAs (maybe "text" Text.unpack lang) $ Text.unpack src

getSectionR :: SectionId -> Handler TypedContent
getSectionR sectionId = do
  section <- runDB $ get404 sectionId
  parent  <- case sectionParent section of
               Nothing       -> return Nothing
               Just parentId -> runDB $ get parentId
  tasks       <- runDB $ selectList [ TaskSection   ==. sectionId ] []
  subsections <- runDB $ selectList [ SectionParent ==. Just sectionId ] []
  selectRep $ do
    provideRep $ defaultLayout $ toWidget
      [hamlet|
        <h1>
          #{sectionTitle section}
          $maybe summary <- sectionSummary section
            <br>
            <small> #{summary}

        $maybe parentId <- sectionParent section
          $maybe title <- sectionTitle <$> parent
            <a href=@{SectionR parentId}> #{title}

        <hr>

        $forall Entity subsectionId subsection <- subsections
          <a href=@{SectionR subsectionId}> #{sectionTitle subsection}

        <hr>

        $forall Entity taskId task <- tasks
          <a href=@{TaskR taskId}> #{taskTitle task}

        $maybe overview <- sectionOverview section
          <p> #{renderMarkdown overview}
        $nothing
          <p> No overview.

        $maybe theory <- sectionTheory section
          <p> #{renderMarkdown theory}
        $nothing
          <p> No theory.
      |]
    provideJson $ sectionOverview section
