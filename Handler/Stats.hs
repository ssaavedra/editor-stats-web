module Handler.Stats where

import Import
import Handler.Documents (ListingAPI(..))

import Text.Shakespeare.Text
import Language.Sexp.Parser (parseExn, Sexp(..))
import qualified Data.ByteString.Lazy
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Encoding as TL (decodeUtf8, encodeUtf8)


checkOwningSession sessionId = do
  session <- runDB $ get404 sessionId
  Entity uid user <- requireAuth

  if sessionOwner session == uid
    then
    return session
    else
    permissionDenied [st|"You (#{userIdent user}) are not authorized to handle this session"|]


postedStats = runInputPost $ ireq textField "stats"

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

             <div>
               <form action=@{SessionStatsR sessionId} method=POST>
            |]


-- TODO: Implement MonadPlus? of StatsTransaction which is a [Stats]
-- with a "current-active-doc" state to carry over the ">>" operation.(1441131169.2925935 (:checkpoint 0))


postSessionStatsR :: SessionId -> Handler TypedContent
postSessionStatsR sessionId = do
  session <- checkOwningSession sessionId
  stats <- postedStats

  -- let sexp = parseExn . L.fromChunks . return . encodeUtf8 =<< return $ stats
  --     stats' = mapMaybe (parseStat sessionId) sexp

  let sexp = parseExn . Data.ByteString.Lazy.fromChunks . return . encodeUtf8 =<< return stats
      stats' = mapMaybe (parseStat sessionId) sexp

  -- TODO Trim sequences already on DB
  newStats <- filterM statSelector stats'

  insertedIds <- runDB $ sequence $ map insert newStats
  r <- runDB $ sequence $ map get insertedIds
  multiRepr $ object [ "length" .= length insertedIds
                     , "ids" .= insertedIds
                     , "status" .= ("ok" :: Text)
                     ]


statSelector :: Stats -> Handler Bool
statSelector stat = do
  existing <- runDB $ selectList [ StatsEditor ==. statsEditor stat
                                 , StatsTimestamp ==. statsTimestamp stat
                                 , StatsStype ==. statsStype stat] [LimitTo 1]
  return $ null existing




parseStat sid sexp = case sexp of
  List [(Atom time),
        List ((Atom stype):info)
       ] ->
    let time' = decodeUtf8 $ toStrict time
        stype' = decodeUtf8 $ toStrict stype
        info' = map sexpToText info
    in
      Just $ Stats sid time' stype' info'

  _ -> Nothing

sexpToText (Atom i) = Data.Text.Lazy.toStrict $ TL.decodeUtf8 i
sexpToText (List l) = concat ["[\"", intercalate "\", \"" $ map sexpToText l, "\"]"]




