module Handler.Wall where

import Import
import State

data Post = Post Text

postForm :: Html -> MForm Handler (FormResult Post, Widget)
postForm = renderDivs $ Post
    <$> areq textField "post" Nothing

getWallR :: Text -> Handler Html
getWallR key = do
    -- TODO: validate key length
    (widget, enctype) <- generateFormPost postForm

    yesod <- getYesod

    pl <- liftIO $ getPosts (posts yesod) key

    defaultLayout $ do
        aDomId <- newIdent
        $(widgetFile "wall")

postWallR :: Text -> Handler Html
postWallR key = do
    ((result, _), _) <- runFormPost postForm

    yesod <- getYesod

    let FormSuccess (Post post) = result

    liftIO $ addPost (posts yesod) key post

    redirect (WallR key)
