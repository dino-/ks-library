- Do something with that scrubName business
- Can we factor out the formatTime stuff we're doing at least twice in this code? Make it part of the ks-library
- Add function:
   - KS.Data.BSON.bsonToDoc :: BSON.Document -> D.Document
- KS.Data.Inspection
   - Should we really be using Data.ByteString.Lazy.Char8 for
     saveInspection? Try with just Data.ByteString.Lazy or even
     not lazy.
