-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

import Data.Aeson ( encode )
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List ( intercalate, isPrefixOf, tails, zip4 )
import Data.Maybe ( fromMaybe, listToMaybe )
import Network.HTTP
import System.Environment ( getArgs )
import System.FilePath
import System.IO
   ( BufferMode ( NoBuffering )
   , hSetBuffering, stdout, stderr
   )
import Text.HTML.TagSoup
import Text.Printf ( printf )

import Ksdl.Facility


urlPrefix :: String
urlPrefix = "http://wake.digitalhealthdepartment.com/"


main :: IO ()
main = do
   -- No buffering, it messes with the order of output
   mapM_ (flip hSetBuffering NoBuffering) [ stdout, stderr ]

   args <- getArgs
   let dir = head args

   {- A page contains 4 facilities.
      Invoking with no number means get all pages.
      Careful, takes a while!
   -}
   let pageLimit = read `fmap` (listToMaybe . tail $ args)

   allPageUrls <- getPageUrls
   let pageCount = length allPageUrls
   printf "Downloading %d of %d pages\n\n"
      (fromMaybe pageCount pageLimit) pageCount

   let pageUrls = maybe allPageUrls (\n -> take n allPageUrls) pageLimit

   let getters = map getFacilities pageUrls  -- [IO [Facility]]
   mapM_ (\ml -> ml >>= mapM_ (saveFacility dir)) getters


saveFacility :: FilePath -> Facility -> IO ()
saveFacility dir fac = BL.writeFile (dir </> (_id fac)) $ encode fac


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

   mapM setId $ map (\(t,s,l,d) -> Facility "" t s l (parseDate d))
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
