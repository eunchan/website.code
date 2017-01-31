{-# LANGUAGE Arrows            #-}
{-# LANGUAGE OverloadedStrings #-}
module Post
    ( post
    , postCompile
    , loadDisqus
    , loadEverything
    , moveToUpper
    , getResourceBodyIfExist
    , postIsPublic
    , postIsPublicOrDraft
    , publicOnly
    , isPublic
    , ekCtx
    , dateRoute
    , mediaUrls
    , slashIndexUrls
    , buildYears
    , renderYears
    , loadPostYear
    , yearId
    , buildSlugs
    , renderSlugs
    ) where
--------------------------------------------------------------------------------
import           Control.Monad                   (filterM, forM)
import           Data.Binary                     (Binary)
import           Data.List                       (intercalate, isPrefixOf, sort)
import qualified Data.Map                        as M
import           Data.Time.Clock                 (UTCTime)
import           Data.Time.Format                (defaultTimeLocale, formatTime,
                                                  parseTimeOrError)
import           Data.Typeable

import           System.Directory                (doesFileExist)
import           System.FilePath                 (joinPath, splitPath,
                                                  takeFileName, (</>))

import           Text.Blaze.Html                 (toHtml, toValue, (!))
import           Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as A

import           Config
import           Hakyll
--------------------------------------------------------------------------------
-- | post function
-- ss : snapshot name
-- r  : route, if you want to use default, hand over r as idRoute
-- ctx: Context that is used while generating pages
post          :: String          -- snapshot name
              -> Routes          -- additional Routes
              -> Context String  -- Post Context
              -> Rules ()
post ss r ctx = do
    -- Adding default extention as html after all Routes are handled.
    route $ r `composeRoutes` (setExtension ".html")

    -- Include links.md content. In order to include this file, it should be
    -- loaded into cache by 'getResourceBody' function.
    compile $ do
        body  <- getResourceBody
        links <- loadBody "links.md"
        item  <- makeItem (itemBody body ++ "\n\n" ++ links)
        postCompile item ss "_tpl/post.html" ctx

postCompile :: Item String      -- itemBody
            -> String           -- snapshot
            -> Identifier       -- firstTemplate
            -> Context String   -- Context
            -> Compiler (Item String)
postCompile item ss tpl ctx = do
    return item
        >>= renderPandocWith defaultHakyllReaderOptions ekWriterOptions
        >>= saveSnapshot ss
        >>= loadDisqus ctx
        >>= loadAndApplyTemplate tpl ctx
        >>= loadAndApplyTemplate "_tpl/default.html" ctx
        >>= slashIndexUrls
        >>= relativizeUrls

-- | loadEverything that returns every post
loadEverything :: (Binary a, Typeable a) => Compiler [Item a]
loadEverything = do
    posts <- loadAll "page/**.md"
    skylog <- loadAllSnapshots ("sky/log/*.md" .||. "sky/*.md") "skylog"
    blog <- loadAllSnapshots ("blog/**.md" .&&. complement "blog/index.md") "blog"
    return $ posts ++ skylog ++ blog

-- | If disqus is defined as "on" in meta, the page will show disqus comments.
--   Or, just return item as is.
loadDisqus :: Context String
           -> Item String
           -> Compiler (Item String)
loadDisqus ctx item = do
    disqus <- getMetadataField (itemIdentifier item) "disqus" -- Maybe String
    case disqus of
        -- YAML parse "on" as true
        Just "true" -> loadAndApplyTemplate "_tpl/disqus.html" ctx item
        Just "on"   -> loadAndApplyTemplate "_tpl/disqus.html" ctx item -- for compatibility
        _           -> return $ item

isPublic :: MonadMetadata m
         => Item a
         -> m Bool
isPublic item = do
    public <- getMetadataField (itemIdentifier item) "public"
    return $ public == Just "true"

-- | filter public
-- Code derived from nurpax(http://github.com/nurpax/blog/)
publicOnly :: MonadMetadata m
           => m [Item a]
           -> m [Item a]
publicOnly i = i >>= \lst ->
    filterM isPublic lst

-- | Filter Public for match funciton
-- Usage : matchMetadata "pattern/**" postIsPublic
metadataFieldIs :: String -> String -> Metadata -> Bool
metadataFieldIs key value metadata =
    case lookupString key metadata of
        Just v  -> value == v
        Nothing -> False

postIsPublic :: Metadata -> Bool
postIsPublic = metadataFieldIs "public" "true"

postIsPublicOrDraft :: Metadata -> Bool
postIsPublicOrDraft md = (postIsPublic md) || (metadataFieldIs "public" "draft" md)

moveToUpper :: Routes
moveToUpper = customRoute stripTopDir
  where
    stripTopDir = joinPath . drop 1 . splitPath . toFilePath

-- | return body content if file exists. If not, return empty string.
getResourceBodyIfExist :: Compiler (Item String)
getResourceBodyIfExist = do
    id' <- getUnderlying
    b <- unsafeCompiler (doesFileExist $ toFilePath id')
    if b
        then getResourceBody
        else makeItem ""

ekCtx :: Context String
ekCtx = mconcat
    [ modificationTimeField "mtime" "%U"
    , dateField "date" "%B %e, %Y"
    , defaultContext
    ]

-- | Route based on metadata field 'date' -------------------------------------
dateRoute :: FilePath -> Routes
dateRoute prefix = metadataRoute (f prefix)
  where
    f p md = customRoute $ pullDateToFilePath p md

-- | Add prefix then compose YYYY/MM/DD/post.html format ----------------------
pullDateToFilePath :: FilePath -> Metadata -> Identifier -> FilePath
pullDateToFilePath p m i = p </> (convertDateToFilePath m i)

convertDateToFilePath :: Metadata -> Identifier -> FilePath
convertDateToFilePath md id' = convertLocalTimetoISO (getDate md) $ toFilePath id'
  where
    -- convertLocalTimetoISO :: String -> FilePath -> FilePath
    convertLocalTimetoISO d fp = toISO d </> chopDayFromFileName fp
    chopDayFromFileName fp' = replaceAll "[0-9]{4}-[0-9]{2}-[0-9]{2}-" (const "") $ takeFileName fp'
    toISO dateString = formatTime defaultTimeLocale "%Y/%m/%d" $ readTimeFromMetadataString dateString

-- TODO: Make more format
readTimeFromMetadataString :: String -> UTCTime
readTimeFromMetadataString dateString = parseTimeOrError False defaultTimeLocale "%B %e, %Y" dateString

getDate :: Metadata -> String
getDate md = case lookupString "date" md of
    Just v  -> v
    Nothing -> ""

-- let timeFromString = readTime defaultTimeLocale "%d %b %Y %l:%M %p" dateString :: UTCTime
--            Format YYYY/MM/DD HH:MM
-- formatTime defaultTimeLocale "%Y/%m/%d %H:%M" timeFromString

-- | Change /media/ to //media.eunchan.kim/ -----------------------------------
--------------------------------------------------------------------------------
-- | Compiler form of 'relativizeUrls' which automatically picks the right root
-- path
mediaUrls :: Item String -> Compiler (Item String)
mediaUrls item = do
    r <- getRoute $ itemIdentifier item
    return $ case r of
        Nothing -> item
        Just _  -> fmap (prefixUrlsWith "//media.eunchan.kim/" "/media/") item


--------------------------------------------------------------------------------
-- | Relativize URL's in HTML
prefixUrlsWith :: String  -- ^ New Path to switch
               -> String  -- ^ Prefix to search
               -> String  -- ^ HTML to relativize
               -> String  -- ^ Resulting HTML
prefixUrlsWith new pf = withUrls rel
  where
    isRel x = pf `isPrefixOf` x && not ("//" `isPrefixOf` x)
    rel x   = if isRel x then new ++ (replaceAll pf (const "") x) else x

-- | slashIndexUrls chops `index.html` to prettify URL address on the browser
slashIndexUrls :: Item String
               -> Compiler (Item String)
slashIndexUrls item = do
    r <- getRoute $ itemIdentifier item
    return $ case r of
        Nothing -> item
        Just _  -> fmap chopIndexHtml item

chopIndexHtml :: String
              -> String
chopIndexHtml = withUrls ind
  where
    ind x = if isIndexHtml x then (replaceAll "index.html" (const "") x) else x
    isIndexHtml x = "index.html" == takeFileName x

--------------------------------------------------------------------------------
-- | Yearly Archive
type Year = String

buildYears :: MonadMetadata m => Pattern -> m [(Year, Int)]
buildYears pattern = do
    --ids <- getMatches pattern
    idmds <- getAllMetadata pattern
    return . frequency . (filter isNotEmpty) .(map getYear) $ idmds
  where
    frequency xs = M.toList (M.fromListWith (+) [(x,1) | x <- xs])
    isNotEmpty x =  x /= ""

-- getYear :: Identifier -> Year
-- getYear id = case getMetadataField id "date" of --maybeString
--     Just v  -> toYear v
--     Nothing -> ""       -- will Filter out later

getYear :: (Identifier, Metadata) -> Year
getYear idmd = case lookupString "date" (snd idmd) of
    Just v  -> toYear v
    Nothing -> ""

toYear :: String -> Year
toYear ds = formatTime defaultTimeLocale "%Y" $ readTimeFromMetadataString ds

yearPath :: Year -> FilePath
yearPath year = "archive/" ++ year ++ "/index.html"

yearId :: Year -> Identifier
yearId = fromFilePath . yearPath

renderYears :: [(Year, Int)] -> Compiler String
renderYears years = do
    years' <- forM (reverse . sort $ years) $ \(year, count) -> do
        -- route' <- getRoute $ yearId year
        return (year, count) -- (year, route', count)
    return . intercalate ", " $ map makeLink years'
  where
    makeLink (year, count) = -- (year, route', count) =
      (renderHtml (H.a ! A.href (yearUrl year) $ toHtml year)) ++
      " (" ++ show count ++ ")"
    yearUrl = toValue . toUrl . yearPath

loadPostYear :: (Binary a, Typeable a) => Year -> Pattern -> Compiler [Item a]
loadPostYear year pattern = do
    posts <- loadAll pattern    -- Compiler [Item String]
    filterM (isYear year) posts

isYear :: MonadMetadata m => Year -> Item a -> m Bool
isYear year item = do
    metadata <- getMetadata $ itemIdentifier item
    return $ case lookupString "date" metadata of
        Just v  -> year == (toYear v)
        Nothing -> False

--------------------------------------------------------------------------------
-- | slug to link format
type Slug = String

buildSlugs :: MonadMetadata m => Pattern -> m [(Slug, Identifier)]
buildSlugs pattern = do
    idmds <- getAllMetadata pattern
    return . (filter isNotEmpty). (map getSlug) $ idmds
  where
    -- TODO: Build based on filename when 'slug' doesn't exist
    getSlug idmd = case lookupString "slug" (snd idmd) of
        Just v  -> (v, fst idmd)
        Nothing -> ("","")
    isNotEmpty x = x /= ("","")

renderSlugs :: [(Slug, Identifier)] -> Compiler String
renderSlugs slugs = do
    links <- forM slugs $ \(slug, id') -> do
        idroute <- getRoute id'
        return $ "[" ++ slug ++ "]: /" ++ (getJustRoute idroute)
    return . intercalate "\n" $ links
  where
    getJustRoute filepath = case filepath of
        Just v  -> v
        Nothing -> ""
