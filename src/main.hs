--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Main where

--------------------------------------------------------------------------------
import           Control.Monad   (forM_)
import           Data.Monoid     ((<>))
import           Prelude         hiding (id)

import           GHC.IO.Encoding
--------------------------------------------------------------------------------
import           Hakyll

import           Blog
import           Config
import           Post
import           Sky

--------------------------------------------------------------------------------
-- | Entry point
main :: IO ()
main = do
    setLocaleEncoding utf8  -- this is necessory for running in Windows
    hakyllWith config $ do
        -- CSS files (using sass)
        match "static/css/*.scss" $ compile getResourceBody

        -- Add dependency for extra scss files. If these files are changed,
        -- hakyll will update main css.
        scssDependencies <- makePatternDependency "static/css/*.scss"
        rulesExtraDependencies [scssDependencies] $
            create ["static/css/eunchan.css"] $ do
                route idRoute
                compile $ loadBody (fromFilePath "static/css/eunchan.scss")
                    >>= makeItem
                    >>= withItemBody (unixFilter "sassc" ["-s", "-Istatic/css"])
                    >>= return . fmap compressCss

        -- Static files except scss
        match ("static/**" .&&. complement "static/css/*.scss") $ do
            route   idRoute
            compile copyFileCompiler

        -- Render the 404 page, don't relativize URL's here
        match "404.md" $ do
            route   $ setExtension ".html"
            compile $ pandocCompiler
                >>= loadAndApplyTemplate "_tpl/post.html" defaultContext
                >>= loadAndApplyTemplate "_tpl/default.html" defaultContext

        -- Build slug list [(Slug, Identifier)]
        slugs <- buildSlugs ("page/**.md" .||. "sky/**.md" .||. "blog/**.md")

        -- Include link file. This file's content will be included in all markdown files.
        -- 'links.md' is compiled with slug list so that the other post compile
        -- function doesn't need to be modified.
        match "links.md" $ compile $ do
            body <- getResourceBody
            sls <- renderSlugs slugs
            makeItem (itemBody body ++ "\n\n" ++ sls)

        -- Blog Rules (See Blog.hs)
        blogRules

        -- Sky Rules (See Sky.hs)
        skyRules

        -- Render Posts (See Post.hs)
        matchMetadata "page/**.md" postIsPublic $ post "post" moveToUpper defaultContext -- moveToUpper

        -- | Yearly Archive
        years <- buildYears "page/**.md"

        -- Create Front page
        create ["index.md"] $ do
            route $ setExtension ".html"
            compile $ do
                recentposts <- fmap (take 7) . publicOnly . recentFirst =<< loadEverything
                let ctx = listField "posts" ekCtx (return recentposts) <>
                          field "years" (\_ -> renderYears years) <>
                          defaultContext
                body <- getResourceBodyIfExist
                links <- loadBody "links.md"
                makeItem (itemBody body ++ "\n" ++ links)
                    >>= renderPandocWith defaultHakyllReaderOptions ekWriterOptions
                    >>= templateAndUrl "_tpl/posts.html" ctx

        -- | TODO: Pull the posts in the year.
        forM_ years $ \(year, _) ->
            create [yearId year] $ do
                route idRoute
                compile $ do
                    posts <- publicOnly . recentFirst =<< loadPostYear year "page/**.md"
                    let postsCtx = mconcat
                                   [ listField "posts" ekCtx (return posts)
                                   , constField "title" ("Posts published in " ++ year)
                                   , field "years" (\_ -> renderYears years)
                                   , defaultContext
                                   ]
                    makeItem ""
                        >>= templateAndUrl "_tpl/posts.html" postsCtx

        create ["rss.xml"] $ do
            route idRoute
            compile $
                loadEverything
                    >>= fmap (take 10) . publicOnly . recentFirst
                    >>= renderRss (feedConfiguration "Posts") feedCtx
        -- Read templates
        match "_tpl/*" $ compile templateCompiler

allGlob :: Pattern
allGlob = postsGlob .||. "page/**.md"
