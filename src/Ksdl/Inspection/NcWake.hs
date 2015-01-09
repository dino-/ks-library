-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Ksdl.Inspection.NcWake
   where

import Data.List ( intercalate, isPrefixOf, tails, zip4 )
import Data.Maybe ( fromMaybe )
import qualified Data.Text as T
import Network.HTTP
import Text.HTML.TagSoup
import Text.Printf ( printf )

import Ksdl.Facility
import Ksdl.Inspection.Common


urlPrefix :: String
urlPrefix = "http://wake.digitalhealthdepartment.com/"


download :: Downloader
download dir pageLimit = do
   allPageUrls <- getPageUrls
   let pageCount = length allPageUrls
   printf "Downloading %d of %d pages\n\n"
      (fromMaybe pageCount pageLimit) pageCount

   let pageUrls = maybe allPageUrls (\n -> take n allPageUrls) pageLimit

   let getters = map getFacilities pageUrls  -- [IO [Facility]]
   mapM_ (\ml -> ml >>= mapM_ (saveFacility dir)) getters


-- Get all (4) facilities from a page at the supplied URL
getFacilities :: String -> IO [Facility]
getFacilities url = do
   printf "Retrieving %s\n" url

   tags <- parseTags `fmap` (openURL . getRequest $ urlPrefix ++ url)

   let titles =
         [ t | a:TagText t:_ <- tails tags
         , a ~== "<a href>"
         , isPrefixOf "facilities" $ fromAttrib "href" a
         , not . elem '&' $ fromAttrib "href" a
         ]
   let scores =
         [ (read t) :: Double | bs:l:_:TagText t:_ <- tails tags
         , bs ~== "<b>"
         , l ~== "Score:"
         ]
   let locations = trim
         [ t | bs:l:_:TagText t:_ <- tails tags
         , bs ~== "<b>"
         , l ~== "Location:"
         ]
   let dates = trim
         [ t | bs:l:_:TagText t:_ <- tails tags
         , bs ~== "<b>"
         , l ~== "Inspection Date:"
         ]

   mapM setId $ map (\(t,s,l,d) ->
      Facility "" (T.pack t) s (T.pack l) (parseDate d))
      $ zip4 titles scores locations dates

   where trim = map (dropWhile (== ' '))


-- Get the URLs of all search result pages
getPageUrls :: IO [String]
getPageUrls = do
   tags <- parseTags `fmap` openURL mkPost
   return $ map (fromAttrib "href" . head) .
      sections (~== "<a class=teaser>") $ tags


-- Used for debugging to store the search results page locally
-- for inspection
savePage :: IO ()
savePage = do
   src <- openURL mkPost
   writeFile "temp.html" src


-- Retrieve a page
openURL :: HStream b => Request b -> IO b
openURL p = getResponseBody =<< simpleHTTP p


{- These things are for the first form post to get all of the page URLs
-}

mkPost :: Request_String
mkPost = postRequestWithBody
   (urlPrefix ++ "reports.cfm")
   "application/x-www-form-urlencoded"
   $ intercalate "&" searchParams


searchParams :: [String]
searchParams =
   [ "f=search"
   , "strSearch1="
   , "relevance1=fName"
   , "strSearch2="
   , "relevance2=fName"
   , "strSearch3="
   , "relevance3=fName"
   , "lscore="
   , "hscore="
   , "ftype=Restaurant"
   , "fzipcode=Any"
   , "rcritical=Any"
   , "sMonth=8"
   , "sDay=20"
   , "sYear=2014"
   , "eMonth=12"
   , "eDay=20"
   , "eYear=2014"
   , "func=Search"
   ]
