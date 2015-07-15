module Handler.Session where

import Import
import Database.Persist.Sql (fromSqlKey)
import Handler.Stats (checkOwningSession)

getSessionR :: Text -> Handler TypedContent
getSessionR token = do
  Entity sessionId session <- runDB $ getBy404 $ UniqueSessionToken token
  session <- checkOwningSession sessionId

  selectRep $ do
    provideRep $ defaultLayout
      [whamlet|
       <h2>Session ##{show $ fromSqlKey sessionId} (#{sessionAgent session})
       <p>
         <a href=@{SessionStatsR $ sessionId}>Show statistics for this session


       <p>
         <strong>Token ID:
         <pre>#{sessionToken session}</pre>

       <p>Additional data:
         <pre>#{show session}

       <form action=@{SessionR token}>
         <input type=hidden name=action value=delete>
         <input type=submit value=Delete session>
      |]

    provideRep $ return $ toJSON session


postSessionR :: Text -> Handler Html
postSessionR token = do  Entity sessionId session <- runDB $ getBy404 $ UniqueSessionToken token
                         session <- checkOwningSession sessionId
                         action <- runInputPost $ ireq textField "action"

                         if action == "delete"
                           then deleteSessionR token
                           else defaultLayout [whamlet|
                                                <h2>Action not understood
                                              |]


deleteSessionR :: Text -> Handler Html
deleteSessionR token = do  Entity sessionId session <- runDB $ getBy404 $ UniqueSessionToken token
                           session <- checkOwningSession sessionId
                           runDB $ delete sessionId
                           redirect SessionsR
  
