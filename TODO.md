- Set time zone env for testsuite/Inspection.hs. This is failing on AWS where the machines are UTC
- Add function:
   - KS.Data.BSON.bsonToDoc :: BSON.Document -> D.Document
- KS.Data.Inspection
   - Should we really be using Data.ByteString.Lazy.Char8 for
     saveInspection? Try with just Data.ByteString.Lazy or even
     not lazy.
