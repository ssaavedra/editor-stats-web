module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")




instance ToJSON a => ToJSON (Entity a) where
  toJSON (Entity key s) = object ["id" .= key
                                , "sub" .= s
                                ]
instance ToJSON Stats where
  toJSON Stats {..} = object [ "editor" .= statsEditor
                             , "timestamp" .= statsTimestamp
                             , "statType" .= statsStype
                             , "statInfo" .= statsInfo
                             ]

instance ToJSON Session where
  toJSON Session {..} = object ["agent" .= sessionAgent
                               , "token" .= sessionToken
                               , "createdAt" .= sessionStartTime
                               , "owner" .= sessionOwner
                               ]

instance ToJSON Document where
  toJSON Document {..} = object $ [ "dochash" .= (decodeUtf8 documentDochash)
                                  , "owner" .= documentOwner
                                  ] ++ maybeCreationTime ++ maybeFilepath ++ maybeGitrepo
    where maybeFilepath = toList $ ("filepath" .=) `fmap` documentFilepath
          maybeGitrepo = toList $ ("gitRepo" .=) `fmap` documentGitRepo
          maybeCreationTime = toList $ ("creationTime" .=) `fmap` documentCreationTime



