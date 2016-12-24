--------------------------------------------------------------------------------
-- | This module is designed for implementing blog.
{-# LANGUAGE OverloadedStrings #-}

module Blog
    ( blogRules
    ) where

--------------------------------------------------------------------------------
import           Data.Maybe                      (isNothing)
import           Data.Monoid                     ((<>))

import           Control.Applicative             (Alternative (..))
import           Data.List                       (findIndex, intercalate,
                                                  isPrefixOf, sortBy, tails)

import           Control.Monad                   (filterM, zipWithM_)

import           Data.Time.Clock                 (UTCTime)
import           Data.Time.Format                (defaultTimeLocale, parseTimeM)

import           System.FilePath                 (takeFileName)

import           Text.Blaze.Html                 (toValue, (!))
import           Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as A
import           Text.Blaze.Internal             (preEscapedString)

import           Hakyll

import           Config
import           Post

postsGlob :: Pattern
postsGlob = "blog/**.md" .&&. complement "blog/index.md"

blogPageForPageIdx :: Int -> String
blogPageForPageIdx index = "blog/" ++ (if index==1 then "" else show index ++ "/") ++ "index.html"

blogRules :: Rules ()
blogRules = do
    tags <- buildTags postsGlob (fromCapture "blog/tag/*.html")

    matchMetadata postsGlob postIsPublic $ blogPost "blog"            -- snapshot name
                                                    (dateRoute "blog/") -- Route
                                                    defaultContext    -- context

    create ["blog/posts.html"] $ do
        route idRoute
        compile $ do
            blogposts <- recentFirst =<< loadAllSnapshots postsGlob "blog"
            let ctx = constField "title" "Archive" <>
                      listField "posts" defaultContext (return blogposts) <>
                      defaultContext
            item <- makeItem ""
            postCompile item "blogss" "_tpl/posts.html" ctx

    create ["blog/rss.xml"] $ do
        route idRoute
        compile $
            loadAllSnapshots postsGlob "blog"
                >>= fmap (take 10) . publicOnly . recentFirst
                >>= renderAtom (feedConfiguration "Blog Posts") feedCtx

    paginate 7 $ \index maxIndex itemsForPage -> do
        let bId = fromFilePath $ blogPageForPageIdx index
        create [bId] $ do
            route idRoute
            compile $ do
                let allCtx =
                        field "title" (\_ -> return "Journal") `mappend`
                        defaultContext
                    loadTeaser id' = loadSnapshot id' "blog"
                        >>= loadAndApplyTemplate "_tpl/teaser.html" (teaserCtx tags)
                items <- mapM loadTeaser itemsForPage
                let itembodies = map itemBody items
                    postsCtx =
                        constField "posts" (concat itembodies) `mappend`
                        field "navlinkolder" (\_ -> return $ indexNavLink index 1 maxIndex) `mappend`
                        field "navlinknewer" (\_ -> return $ indexNavLink index (-1) maxIndex) `mappend`
                        tagCloudField "taglist" 80 200 tags `mappend`
                        -- 카테고리는 blog/{category}/YYYY-MM-DD-title.md 형식일 때
                        -- buildCategories로 만들어짐. 현재 블로그에서는 사용하지 않음.
                        --field "categorylist" (\_ -> renderTagListLines categories) `mappend`
                        defaultContext

                makeItem ""
                    >>= loadAndApplyTemplate "_tpl/blogpage.html" postsCtx
                    >>= loadAndApplyTemplate "_tpl/default.html" allCtx
                    >>= relativizeUrls

--------------------------------------------------------------------------------
-- | post function
-- ss : snapshot name
-- r  : route, if you want to use default, hand over r as idRoute
-- ctx: Context that is used while generating pages
blogPost :: String          -- snapshot name
     -> Routes          -- additional Routes
     -> Context String  -- Post Context
     -> Rules ()
blogPost ss r ctx = do
    -- Adding default extention as html after all Routes are handled.
    route $ r `composeRoutes` setExtension ".html"
    compile $ do
        -- Adding Prev/Next posts on current post context
        -- 현재 코드는 일단 URL만 추가함. 추후 Title까지 추가하면 더욱 좋을 듯
        let blogCtx =
                field "nextPost" (nextPostUrl postsGlob) `mappend`
                field "prevPost" (previousPostUrl postsGlob) `mappend`
                ctx
        -- Include links.md content. In order to include this file, it should be
        -- loaded into cache by 'getResourceBody' function.
        body <- getResourceBody
        links <- loadBody "links.md"
        item <- makeItem (itemBody body ++ "\n\n" ++ links)
        postCompile item ss "_tpl/post.html" blogCtx

-- BEGIN PREV/NEXT LINK----------------------------------------------
-- | from rgoulter
-- See commmit
-- https://github.com/rgoulter/my-hakyll-blog/commit/a4dd0513553a77f3b819a392078e59f461d884f9
previousPostUrl :: Pattern
                -> Item String
                -> Compiler String
previousPostUrl glob p = do
    posts <- getMatches glob
    let ident = itemIdentifier p
        sortedPosts = sortIdentifiersByDate posts
        ident' = itemBefore sortedPosts ident
    case ident' of
        Just i  -> (fmap (maybe empty toUrl) . getRoute) i
        Nothing -> empty

nextPostUrl :: Pattern
            -> Item String
            -> Compiler String
nextPostUrl glob p = do
    posts <- getMatches glob
    let ident = itemIdentifier p
        sortedPosts = sortIdentifiersByDate posts
        ident' = itemAfter sortedPosts ident
    case ident' of
        Just i  -> (fmap (maybe empty toUrl) . getRoute) i
        Nothing -> empty

itemAfter :: Eq a => [a] -> a -> Maybe a
itemAfter xs x =
    lookup x $ zip xs (tail xs)

itemBefore :: Eq a => [a] -> a -> Maybe a
itemBefore xs x =
    lookup x $ zip (tail xs) xs

sortIdentifiersByDate :: [Identifier] -> [Identifier]
sortIdentifiersByDate =  sortBy (flip byDate)

byDate :: Identifier -> Identifier -> Ordering
byDate id1 id2 =
    let fn1 = takeFileName $ toFilePath id1
        fn2 = takeFileName $ toFilePath id2
        parseTime' fn = parseTimeM True defaultTimeLocale "%Y-%m-%d" $ intercalate "-" $ take 3 $ splitAll "-" fn
    in compare (parseTime' fn1 :: Maybe UTCTime) (parseTime' fn2 :: Maybe UTCTime)
-- END OF PREV/NEXT LINKS--------------------------------------------

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagsField "tags" tags `mappend` postCtx

teaserCtx :: Tags -> Context String
teaserCtx tags =
    field "teaser" teaserBody `mappend` postCtxWithTags tags

teaserBody :: Item String -> Compiler String
teaserBody item = do
    let body = itemBody item
    return $ extractTeaser . maxLengthTeaser . compactTeaser $ body
  where
    extractTeaser :: String -> String
    extractTeaser [] = []
    extractTeaser xs@(x : xr)
        | "<!-- more -->" `isPrefixOf` xs = []
        | otherwise                       = x : extractTeaser xr

    maxLengthTeaser :: String -> String
    maxLengthTeaser s = if isNothing (findIndex (isPrefixOf "<!-- more -->") (tails s))
                            then unwords (take 60 (words s))
                            else s

    compactTeaser :: String -> String
    compactTeaser =
        replaceAll "<iframe [^>]*>" (const "") .
        replaceAll "<img [^>]*>"    (const "") .
        replaceAll "<p>"            (const "") .
        replaceAll "</p>"           (const "") .
        replaceAll "<blockquote>"   (const "") .
        replaceAll "</blockquote>"  (const "") .
        replaceAll "<strong>"       (const "") .
        replaceAll "</strong>"      (const "") .
        replaceAll "<ol>"           (const "") .
        replaceAll "</ol>"          (const "") .
        replaceAll "<ul>"           (const "") .
        replaceAll "</ul>"          (const "") .
        replaceAll "<li>"           (const "") .
        replaceAll "</li>"          (const "") .
        replaceAll "<h[0-9][^>]*>"  (const "") .
        replaceAll "</h[0-9]>"      (const "") .
        replaceAll "<pre.*"         (const "") .
        replaceAll "<a [^>]*>"      (const "") .
        replaceAll "</a>"           (const "") .
        replaceAll "<figure>"       (const "") .
        replaceAll "</figure>"      (const "") .
        replaceAll "<figcaption>"   (const "") .
        replaceAll "</figcaption>"  (const "")

-- | Identifier로 해당 글이 public post인지 판독
idPublic :: MonadMetadata m
         => Identifier
         -> m Bool
idPublic identifier = do
    public <- getMetadataField identifier "public"
    return $ public == Just "true"

-- | Pattern에 들어있는 Posts 중 public 인 것만 모아서 Identifer 리스트로 넘기는 함수
filterPublic :: MonadMetadata m
             => Pattern
             -> m [Identifier]
filterPublic glob = do
    identifiers <- getMatches glob
    filterM idPublic identifiers

paginate:: Int -> (Int -> Int -> [Identifier] -> Rules ()) -> Rules ()
paginate itemsPerPage rules = do
    -- public만 가져오도록 수정.
    -- TODO: 다만 코드가 Post.hs에 있는 publicOnly 와 일정부분 중복이 됨.
    -- 이를 해결하도록 수정할 것
    -- TODO: Internal date metafield를 이용해서 sorting하도록 변경할 것.
    --       nodate 일경우 또는 date metafield가 없을 경우, 리스트에서 제외할 것.
    identifiers <- filterPublic postsGlob

    let sorted = sortBy (flip byDate) identifiers
        chunks = chunk itemsPerPage sorted
        maxIndex = length chunks
        pageNumbers = take maxIndex [1..]
        process i = rules i maxIndex
    zipWithM_ process pageNumbers chunks

--------------------------------------------------------------------------------
-- | Split list into equal sized sublists.
-- https://github.com/ian-ross/blog
chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = ys : chunk n zs
    where (ys,zs) = splitAt n xs

--------------------------------------------------------------------------------
-- | Generate navigation link HTML for stepping between index pages.
-- https://github.com/ian-ross/blog
--
indexNavLink :: Int -> Int -> Int -> String
indexNavLink n d maxn = renderHtml ref
  where ref = if refPage == "" then ""
              else H.a ! A.href (toValue $ toUrl refPage) $
                   preEscapedString lab
        lab = if d > 0 then "Older Entries &raquo;" else "&laquo; Newer Entries"
        refPage = if n + d < 1 || n + d > maxn then ""
                  else blogPageForPageIdx (n + d)
