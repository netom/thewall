module Handler.Wall where

import Import
import State

postForm :: Maybe Post -> Html -> MForm Handler (FormResult Post, Widget)
postForm post = renderDivs $ Post
    <$> areq textField "Nick" (postNick <$> post)
    <*> areq textareaField "Post" (postBody <$> post)

getWallR :: Text -> Handler Html
getWallR key = do
    -- TODO: validate key length
    maybeNick <- lookupSession "nick"
    let formNick = case maybeNick of
                   Nothing -> ""
                   Just nick -> nick

    let post = Post formNick $ Textarea ""

    (widget, enctype) <- generateFormPost $ postForm $ Just post

    yesod <- getYesod

    PostList expire ps <- liftIO $ getPosts (posts yesod) key

    defaultLayout $ do
        $(widgetFile "wall")

postWallR :: Text -> Handler Html
postWallR key = do
    ((result, _), _) <- runFormPost $ postForm Nothing

    yesod <- getYesod

    let FormSuccess post = result
    let Post nick _ = post

    setSession "nick" nick

    liftIO $ addPost (posts yesod) key post

    redirect (WallR key)
