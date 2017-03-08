--------------------------------------------------------------------------------
-- | This module containing specialized function to generage sky contents.
--
{-# LANGUAGE OverloadedStrings          #-}
module Sky
    ( getResourceBodyIfExist
    , skyRules
    , skytagsRules
    , skylogCtx
    , buildSky
    ) where

--------------------------------------------------------------------------------
import           Control.Monad                   (forM)
import           Data.Monoid                     ((<>))
import           Data.Maybe                      (catMaybes, fromMaybe)
import           Data.List                       (intersperse)
import           Text.Blaze.Html                 (toHtml, toValue, (!))
import           Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as A

import           Hakyll

import           Post
--------------------------------------------------------------------------------
-- | Sky Rules to create sky pages
skyRules :: Rules ()
skyRules = do
    -- Build Celestial Bodies, Observation Sites, and Equiptment
    skybodies <- buildSky "skybody" "sky/log/*.md" (fromCapture "sky/body/*.md")
    skysites <- buildSky "place" "sky/log/*.md" (fromCapture "sky/site/*.md")
    skyequips <- buildSky "equip" "sky/log/*.md" (fromCapture "sky/equip/*.md")

    match "sky/log/**.md" $ post "skylog" (dateRoute "sky/log/") (skylogCtx skybodies skysites skyequips)
    match "sky/*.md" $ post "skylog" idRoute (skylogCtx skybodies skysites skyequips)

    create ["sky/body.html"] $ do
        route idRoute
        compile $ do
            cloud <- renderTagCloud 90 220 skybodies
            makeItem cloud
                >>= templateAndUrl "_tpl/post.html" ekCtx

    create ["sky/index.html"] $ do
        route idRoute
        compile $ do
            logs <- recentFirst =<< loadAll "sky/log/*.md"
            let ctx = constField "title" "Observation Logs" <>
                      listField "posts" (skylogCtx skybodies skysites skyequips) (return logs) <>
                      constField "mapindex" "on" <>
                      defaultContext
            makeItem ""
                >>= templateAndUrl "_tpl/posts.html" ctx

    tagsRules skybodies (skytagsRules "skybody")

    create ["sky/site.html"] $ do
        route idRoute
        compile $ do
            cloud <- renderTagCloud 90 220 skysites
            makeItem cloud
                >>= templateAndUrl "_tpl/post.html" defaultContext

    tagsRules skysites (skytagsRules "skysite")

    tagsRules skyequips (skytagsRules "skyequip")

    -- create sites.json
    create ["sky/sites.json"] $ do
        route idRoute
        compile $ do
            sites <- loadAllSnapshots "sky/site/*.md" "skysite"

            let ctx = listField "sites" defaultContext (return sites) <>
                      defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "_tpl/sites.json" ctx

--------------------------------------------------------------------------------
-- Post Contexts
skylogCtx :: Tags -> Tags -> Tags -> Context String
skylogCtx skybodies skysite skyequips= mconcat
    [ modificationTimeField "mtime" "%U"
    , dateField "date" "%B %e, %Y"
    , skybodyField "skybody" skybodies
    , skysiteField "skysite" skysite
    , skyequipField "skyequip" skyequips
    , defaultContext
    ]

--------------------------------------------------------------------------------
-- tagsRules function
skytagsRules :: String              -- snapshot
             -> String              -- Actual Tags
             -> Pattern             -- Pattern that includes these tags
             -> Rules ()            -- Return TagRule
skytagsRules snapshot _ pat = do

    route $ setExtension ".html"
    compile $ do
        logs <- recentFirst =<< loadAll pat

        let ctx = listField "posts" defaultContext (return logs) <>
                  defaultContext

        body <- getResourceBodyIfExist
        links <- loadBody "links.md"
        item <- makeItem (itemBody body ++ "\n\n" ++ links)
        postCompile item snapshot "_tpl/posts.html" ctx

--------------------------------------------------------------------------------
-- | Integrated function (buildBodies, buildSites)
getSky :: MonadMetadata m
       => String
       -> Identifier
       -> m [String]
getSky metafield identifier = do
    metadata <- getMetadata identifier
    return $ maybe [] (map trim . splitAll ",") $ lookupString metafield metadata

buildSky :: MonadMetadata m
         => String                  -- "skybody" or "place"
         -> Pattern                 -- "sky/log/*.md"
         -> (String -> Identifier)  -- fromCapture
         -> m Tags
buildSky meta = buildTagsWith (getSky meta)


-- | Wrapping tag with <li> and concatenating
wrapWithLi :: [H.Html] -> H.Html
wrapWithLi = mconcat . intersperse "\n      " . map H.li

-- | Render skybody with links
skybodyField :: String     -- ^ Destination key
          -> Tags       -- ^ Tags
          -> Context a  -- ^ Context
skybodyField =
    tagsAsTitleFieldWith (getSky "skybody") simpleRenderLink wrapWithLi

skysiteField :: String
             -> Tags
             -> Context a
skysiteField =
    tagsAsTitleFieldWith (getSky "place") simpleRenderLink wrapWithLi

skyequipField :: String        -- Destination Field
              -> Tags          -- Tags
              -> Context a     -- Output Context
skyequipField =
    tagsAsTitleFieldWith (getSky "equip") simpleRenderLink wrapWithLi

-- | Render one tag link
simpleRenderLink :: String -> Maybe FilePath -> Maybe H.Html
simpleRenderLink _   Nothing         = Nothing
simpleRenderLink tag (Just filePath) =
  Just $ H.a ! A.href (toValue $ toUrl filePath) $ toHtml tag

--------------------------------------------------------------------------------
-- | Render tags with links with custom functions to get tags and to
-- render links
tagsAsTitleFieldWith :: (Identifier -> Compiler [String])
              -- ^ Get the tags
              -> (String -> Maybe FilePath -> Maybe H.Html)
              -- ^ Render link for one tag
              -> ([H.Html] -> H.Html)
              -- ^ Concatenate tag links
              -> String
              -- ^ Destination field
              -> Tags
              -- ^ Tags structure
              -> Context a
              -- ^ Resulting context
tagsAsTitleFieldWith getTags' renderLink cat key tags = field key $ \item -> do
    tags' <- getTags' $ itemIdentifier item
    links <- forM tags' $ \tag -> do
        route' <- getRoute $ tagsMakeId tags tag
        title <- getMetadataField (tagsMakeId tags tag) "title"
        return $ renderLink (fromMaybe tag title) route'

    return $ renderHtml $ cat $ catMaybes links
