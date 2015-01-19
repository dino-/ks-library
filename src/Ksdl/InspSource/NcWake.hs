-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Ksdl.InspSource.NcWake
   where

import Data.List ( intercalate, isPrefixOf )
import Data.Maybe ( fromMaybe )
import qualified Data.Text as T
--import Debug.Trace ( trace )
import Network.HTTP
import Text.HTML.TagSoup
import Text.Printf ( printf )

import qualified Ksdl.Inspection as I
import Ksdl.InspSource.Common


urlPrefix :: String
urlPrefix = "http://wake.digitalhealthdepartment.com/"


inspectionSrc :: String
inspectionSrc = "nc_wake"


download :: Downloader
download dir pageLimit = do
   allPageUrls <- getPageUrls
   let pageCount = length allPageUrls
   printf "Downloading %d of %d pages\n\n"
      (fromMaybe pageCount pageLimit) pageCount

   let pageUrls = maybe allPageUrls (\n -> take n allPageUrls) pageLimit

   let getters = map getFacilities pageUrls  -- [IO [Inspection]]
   mapM_ (\ml -> ml >>= mapM_ (I.saveInspection dir)) getters


-- Get all (4) facilities from a page at the supplied URL
getFacilities :: String -> IO [I.Inspection]
getFacilities url = do
   printf "Retrieving %s\n" url

   tags <- parseTags `fmap` (openURL . getRequest $ urlPrefix ++ url)

   let itags = isolateInspTags tags
   let insps = map extractInsp itags
   return $ map I.setId insps


-- Extract the block of tags containing each separate facility
isolateInspTags :: [Tag String] -> [[Tag String]]
isolateInspTags= partitions isFacAnchor
   where isFacAnchor e =
            (e ~== "<a href>") &&
            (isPrefixOf "facilities" $ fromAttrib "href" e) &&
            (not . elem '&' $ fromAttrib "href" e)


-- Extract the Inspection data from a facility's tags
extractInsp :: [Tag String] -> I.Inspection
extractInsp tags = I.Inspection
   ""
   inspectionSrc
   (T.pack name)
   (T.pack . trim $ addr)
   (I.parseDate date)
   (read . trim $ score)
   (reinspToBool reinspection)
   (urlPrefix ++ detail)

   where
      name = innerText . (takeWhile (not . isTagClose)) $ tags
      TagText addr = (dropWhile (~/= "Location:") tags) !! 2
      TagText date = (dropWhile (~/= "Inspection Date:") tags) !! 2
      TagText score = (dropWhile (~/= "Score:") tags) !! 2
      detailTag = head . (dropWhile (~/= "<input type=button>")) $ tags
      reinspection = fromAttrib "value" detailTag
      detail = extractDetailUrl . fromAttrib "onclick" $ detailTag

      trim = unwords . words

      reinspToBool "Inspection" = False
      reinspToBool _            = True

      extractDetailUrl =
         takeWhile (/= '\'') . tail . dropWhile (/= '\'')


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
