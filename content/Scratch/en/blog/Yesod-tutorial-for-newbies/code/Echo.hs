module Handler.Echo where

import Import

getEchoR :: Text -> Handler RepHtml
getEchoR theText = do
    defaultLayout $ do
        [whamlet|<h1>#{theText}|]
