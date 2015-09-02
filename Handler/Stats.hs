module Handler.Stats where

import Import
import Handler.Documents (ListingAPI(..))

import qualified Control.Monad.State as SM

import Text.Shakespeare.Text
import Language.Sexp.Parser (parseExn, Sexp(..))
import qualified Data.ByteString.Lazy
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Encoding as TL (decodeUtf8, encodeUtf8)

import Database.Persist.Sql (toSqlKey)


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
-- with a "current-active-doc" state to carry over the ">>" operation.

topLevelDecl = undefined

pairStatsWithDocuments sid statList = SM.evalState pairStats (statList, Nothing)
  where pairStats :: SM.State ([Stats], Maybe Document) [Stats]
        pairStats = do
          curDoc <- SM.gets snd
          case curDoc of
            Nothing -> do let fromDoc = getFromInitialDocument statList
                          elts <- SM.gets fst
                          theDoc <- retrieveOrCreateDoc $ head fromDoc
                          SM.evalState pairStats (fromDoc, Just theDoc)
            Just doc -> do elts <- SM.gets fst
                           let (related, rest) = span (docDoesNotChange doc) elts
                               related' = relateToDoc doc related
                           related'

        docDoesNotChange doc stat = docId == Nothing &&
                                    stype /= ":buffer-switch" &&
                                    stype /= ":buffer-inactive" &&
                                    stype /= ":buffer-active"
          where docId = statsDocument stat
                stype = statsStype stat
        
        docEquals doc stat = statsDocument stat == Just doc

        getFromInitialDocument = dropWhile (not isBufferSwitch)
        relateToDoc = map $ id
        isBufferSwitch a = True

        retrieveOrCreateDoc stat = do existing <- runDB $ selectList [ DocumentOwner ==. sid
                                                                     , DocumentDochash ==. hash
                                                                     , DocumentFilepath ==. path
                                                                     , DocumentGitRepo ==. repo
                                                                     ] [LimitTo 1]
                                      if null existing then
                                        runDB $ insert $ Document sid hash path repo time
                                        else
                                        return $ entityKey $ head existing
          where info = assert (statsStype stat == ":buffer-switch" || statsStype stat == ":buffer-active") $ statsInfo stat
                infoH = parseJSON info
                hash = get ":buffer-path-sha256" infoH
                path = get ":buffer-name" infoH
                repo = get ":buffer-projectile-dir" infoH
                time = Nothing






postSessionStatsR :: SessionId -> Handler TypedContent
postSessionStatsR sessionId = do
  session <- checkOwningSession sessionId
  stats <- postedStats

  -- let sexp = parseExn . L.fromChunks . return . encodeUtf8 =<< return $ stats
  --     stats' = mapMaybe (parseStat sessionId) sexp

  let sexp = parseExn . Data.ByteString.Lazy.fromChunks . return . encodeUtf8 =<< return stats
      stats' = mapMaybe (parseStat sessionId Nothing) sexp

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


someSid  :: SessionId
someSid = undefined

someSid2 :: SessionId
someSid2 = toSqlKey 5

someDocId :: Maybe DocumentId
someDocId = Just $ toSqlKey 5 :: Maybe DocumentId

sampleStatStr = "(1435234819.2160487 (:buffer-change 3931 3931 144))"
sampleStatSexp = parseExn . Data.ByteString.Lazy.fromChunks . return . encodeUtf8 =<< [sampleStatStr]
sampleStats = mapMaybe (parseStat someSid2 someDocId) sampleStatSexp


parseStat sid docId sexp = case sexp of
  List [(Atom time),
        List ((Atom stype):info)
       ] ->
    let time' = decodeUtf8 $ toStrict time
        stype' = decodeUtf8 $ toStrict stype
        info' = map sexpToText info
    in
      Just $ Stats sid docId time' stype' info'

  _ -> Nothing

sexpToText (Atom i) = Data.Text.Lazy.toStrict $ TL.decodeUtf8 i
sexpToText (List l) = concat ["[\"", intercalate "\", \"" $ map sexpToText l, "\"]"]




