{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Handler.Documents where

import Import
import Prelude (read)

import Yesod.Form.Jquery


instance YesodJquery App

class ListingAPI t where
  multiReprT :: t -> Widget -> Handler TypedContent
  multiReprT a htmlT = selectRep $ do
    provideRep $ defaultLayout $ do
      htmlT
      toWidget $ htmlRepr a

    provideRep $ return $ jsonRepr a

  multiRepr :: t -> Handler TypedContent
  multiRepr = flip multiReprT $ return ()

  htmlRepr :: t -> Html
  jsonRepr :: t -> Value

instance ToJSON a => ListingAPI a where
  jsonRepr a = toJSON a
  htmlRepr a = hr $ toJSON a
    where hr (Array elts) = [shamlet|
                             $if null elts
                               <div>No elements.
                             $else
                               <ul>
                                 $forall el <- elts
                                   <li>#{hr el}
                             |]
          hr (String t) = [shamlet|<span>#{t} |]
          hr (Number n) = [shamlet|<span>#{show n} |]
          hr (Bool True) = [shamlet|<span>true</span> |]
          hr (Bool False) = [shamlet|<span>false</span> |]
          hr Null = [shamlet|- |]
          hr (Object v) = [shamlet|
                              <dl>
                                #{map hrMap $ mapToList v}
                            |]

          hrMap (k, v) = [shamlet|
                          <dt>#{show k}
                          <dl>#{hr v}
                         |]


bytestringField = convertField encodeUtf8 decodeUtf8 textField
sessionIdField = convertField id id textField
utcTimeField = convertField show read textField

documentMetaForm :: SessionId -> Html -> MForm Handler (FormResult Document, Widget)
documentMetaForm sessionId = renderDivs $ Document
    <$> areq (selectField sessions) "session id" (Just sessionId)
    <*> areq bytestringField "Document Name" Nothing
    <*> aopt textField "File path" Nothing
    <*> aopt textField "Git repo path" Nothing
    <*> aopt textField "Creation Time" Nothing

  where sessions = do
          entities <- runDB $ selectList [SessionId ==. sessionId] []
          optionsPairs $ map (\s -> (sessionToken $ entityVal s, entityKey s)) entities


-- getDocumentsR :: SessionId -> Handler TypedContent
-- getDocumentsR sessionId = do
--                           docs <- runDB $ selectList [DocumentOwner ==. sessionId] [Asc DocumentCreationTime]
--                           (widget, enctype) <- generateFormPost $ documentMetaForm sessionId
--                           multiReprT docs $ do
--                             setTitle "Document list"
--                             [whamlet|<h1>Document list
--                              <form method=post action=@{DocumentsR sessionId} enctype=#{enctype}>
--                                ^{widget}
--                                <button>Create
--                             |]


-- postDocumentsR :: SessionId -> Handler TypedContent
-- postDocumentsR sessionId = putDocumentR sessionId docId



