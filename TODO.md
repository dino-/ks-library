- Triangle potential population of Android users
- Add use case and caching info to the new wiki
   - use cases:
      - what's around a specific location
      - search for a specific restaurant
   - caching on the client:
      - ??
- Add msmtp installation notes to wiki

- Fill out more of README.md
- It may be time to put info in changelog.md
- Look into migrating to MongoDB
- Is there a different url encoding API than what's in Network.HTTP?
- Ks.Inspection
   - Should we really be using Data.ByteString.Lazy.Char8 for
     saveInspection? Try with just Data.ByteString.Lazy or even
     not lazy.
- ks-dlinsp
   - Failure to access the URL kills the whole process. Should we handle this with retrying somehow? Not sure
   - Should have much more/better logging
- Ks.DlInsp.Source.*
   - Replace usage of Network.HTTP with Network.HTTP.Conduit
     (http-conduit). See Ksdl.Geocoding/Places