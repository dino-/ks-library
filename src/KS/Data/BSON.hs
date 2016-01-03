-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

{- |
   This module is for working with data in MongoDB
-}

module KS.Data.BSON ( combineId, separateId ) where

import Data.Bson ( Document, Label, exclude, include, merge )


-- This is a fixed value that MongoDB expects. Do not change it. Ever.
idKey :: Label
idKey = "_id"


{- Separate the _id portion of a Document, returning it and the
   rest separately
-}
separateId :: Document -> (Document, Document)
separateId wholeDoc = (include [idKey] wholeDoc, exclude [idKey] wholeDoc)


{- Combine two documents, intended to be used as the inverse of
   the above separateId function
-}
combineId :: (Document, Document) -> Document
combineId (docId, docRest) = merge docId docRest

