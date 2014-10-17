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
  selectRep $ do
    provideRep $ return
      [shamlet|
        $maybe overview <- sectionOverview section
          <p> #{renderMarkdown overview}
        $nothing
          <p> No overview.
      |]
    provideJson $ sectionOverview section
