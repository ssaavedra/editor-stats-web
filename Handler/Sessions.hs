{-# LANGUAGE RecordWildCards #-}
module Handler.Sessions where

import Import
import System.Random (randomRIO)
import qualified Data.Text as T


getSessionsR :: Handler TypedContent
getSessionsR = do
               authId <- requireAuthId
               time <- liftIO getCurrentTime
               sessions <- runDB $ selectList [SessionOwner ==. authId] [Asc SessionId]
               formId <- newIdent
               jsonButton <- newIdent

               selectRep $ do
                 provideRep $ defaultLayout $ do
                   addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.6.2/jquery.min.js"
                   toWidget $
                     [whamlet|
                      $if null sessions
                          <h2>No open sessions at #{show time}
                      $else
                          <h2>You have #{show $ length sessions} open sessions
                          <h5>Info updated at #{show time}
                          <ul>
                            $forall entity <- sessions
                              $case entity
                                $of Entity sessionId session
                                  <li>
                                    <a href=@{SessionR $ sessionToken session}>#{sessionAgent session}
                      <hr>
                      <form ##{formId} method=post action=@{SessionsR}>
                          <h2>Create new session
                          <label for=agent>Agent name
                          <input type=text id=agent name=agent>
                          <input type=submit value=Create>
                          <button ##{jsonButton}>Toggle JSON
                    
                     |]
                   toWidget [julius|
                     $(function() {
                       var re = /_accept/i;
                       var form = $("#" + #{toJSON formId});
                       $("#" + #{toJSON jsonButton}).click(function() {
                         if(re.test(form.attr("action"))) {
                           form.attr("action", "@{SessionsR}");
                         } else {
                           form.attr("action", "@{SessionsR}?_accept=application/json");
                       } } )});
                   |]

                 provideRep $ return $ array sessions


postSessionsR :: Handler TypedContent
postSessionsR = do
                authId <- requireAuthId
                agentName <- runInputPost $ ireq textField "agent"
                currentTime <- liftIO getCurrentTime
                tokenGen <- liftIO $ sequence $ take 32 $ repeat $ randomRIO ('a', 'z')

                let newToken = T.pack tokenGen
                    newSession = Session agentName newToken currentTime authId

                sessionId <- runDB $ insert newSession

                redirect $ SessionR newToken
                selectRep $ do
                  provideRep $ defaultLayout
                    [whamlet|
                     <h2>Session (#{show sessionId}) for #{agentName} created
                     <pre>#{show newSession}
                    |]

                  provideRep $ return $ toJSON newSession
