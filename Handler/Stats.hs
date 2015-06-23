module Handler.Stats where

import Import
import Handler.Documents (ListingAPI(..))
import qualified Data.Text as T
import Text.Shakespeare.Text

checkOwningSession sessionId = do
  session <- runDB $ get404 sessionId
  Entity uid user <- requireAuth

  if sessionOwner session == uid
    then
    return session
    else
    permissionDenied [st|"You (#{userIdent user}) are not authorized to handle this session"|]


emacsForm = runInputGet $ ireq textField "stats"

getSessionStatsR :: SessionId -> Handler TypedContent
getSessionStatsR sessionId = do
  session <- checkOwningSession sessionId
  stats <- runDB $ selectList [StatsEditor ==. sessionId] []

  let token = sessionToken session
      agent = sessionAgent session

  multiReprT stats $ do
    setTitle "Statistics for agent"
    [whamlet|
             <h1>Statistics involving&nbsp;
               <a href=@{SessionR token}>#{agent}
             <div class="alert alert-warning">
               <strong>You must set your editor to POST statistics to the following URL
               <pre>@{SessionStatsR sessionId}
               <strong>With the following Token
               <div>
                 <code>Authorization:
                 <kbd>Bearer #{sessionToken session}
            |]


postSessionStatsR :: SessionId -> Handler Html
postSessionStatsR sessionId = do
  session <- checkOwningSession sessionId
  doc <- emacsForm
  error $ show doc


