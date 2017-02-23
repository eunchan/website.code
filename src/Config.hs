module Config where

--------------------------------------------------------------------------------
import           Text.Pandoc
import           Hakyll



--------------------------------------------------------------------------------
-- Make output directories as hidden
config :: Configuration
config = defaultConfiguration
    { destinationDirectory  = ".site"
    , storeDirectory        = ".cache"
    , tmpDirectory          = ".cache/tmp"
    }

--------------------------------------------------------------------------------
feedCtx :: Context String
feedCtx = mconcat
    [ bodyField "description"
    , defaultContext
    ]

feedConfiguration :: String -> FeedConfiguration
feedConfiguration title = FeedConfiguration
    { feedTitle       = title
    , feedDescription = "Everything"
    , feedAuthorName  = "Eunchan"
    , feedAuthorEmail = "me@eunchan.kim"
    , feedRoot        = "https://www.eunchan.kim"
    }

-- Default Writer Options + HTML5,
-- Detailed option is described in Text.Pandoc.Options
ekWriterOptions :: WriterOptions
ekWriterOptions = defaultHakyllWriterOptions
    { writerHtml5           = True
    }
