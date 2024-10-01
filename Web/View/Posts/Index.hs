module Web.View.Posts.Index where
import Web.View.Prelude

import Data.UUID (UUID, nil)

-- data IndexView = IndexView { posts :: [Post] }
data IndexView = IndexView { postsWithDetails :: [(Post, [Reaction], [Comment])] }

instance View IndexView where
    html IndexView { .. } = [hsx|
        <head>
            <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0-beta3/css/all.min.css">
        </head>
        <body>
        <div style="display: flex; flex-direction: row; justify-content: space-between;">
            {breadcrumb}
            <a class="js-delete js-delete-no-confirm" href={DeleteSessionAction}>Logout</a>
        </div>
        <div style="display: flex; justify-content: center;">
            <h1 style="font-size: 5rem;">STaBLit</h1>
        </div>
        <h1>Home<a href={pathTo NewPostAction} class="btn btn-primary ms-4">+ New Post</a></h1>
        <hr>
        <div>{forEach postsWithDetails renderPost}</div>
        </body>
        |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Posts" PostsAction
                ]

renderPost :: (Post, [Reaction], [Comment]) -> Html
renderPost postwithdetails = 
    case postwithdetails of 
        (post, reactions, comments) -> [hsx|
    <div>
            <div style="display: flex; flex-direction: row; justify-content: space-between;">
                <div style="display: flex; flex-direction: row;">
                    <p style="font-weight: bold;">Author: {post.author}</p>
                    <p style="margin-left: 1rem;">{post.createdAt |> timeAgo}</p>
                </div>
                {renderEditDeleteButtons post}
            </div>
            <a href={ShowPostAction post.id} style="font-size: 1.5rem;">{post.title}</a>
            <p style="margin-top: 1rem;">{post.body}</p>

            <div style="display: flex; flex-direction: row; align-items: center;">

                <p style="margin-top: 1rem;">{post.likecount}</p>
                {renderLikeButton post.id}

                <p style="margin-top: 1rem;">{post.dislikecount}</p>
                {renderDislikeButton post.id}

                <a href={NewCommentAction post.id} style="align-items: center;">
                    <p style="margin-top: 1rem;">Comment</p>
                </a>

                <div class="emoji-reactions" style="margin-left: 2rem;">
                    {renderReactionButtons post.id reactions}
                </div>

            </div>

            <div>
                {renderComments (buildCommentTree Nothing comments)}
            </div>

            <hr style="width: 90%; margin: auto; margin-bottom: 2rem;">
    </div>
|]
        _ -> mempty


renderEditDeleteButtons :: Post -> Html
renderEditDeleteButtons post =
    if currentUser.username == post.author
    then [hsx|
        <div style="display: flex; flex-direction: row;">
            <a href={EditPostAction post.id} class="text-muted">Edit</a>
            <a href={DeletePostAction post.id} class="js-delete text-muted" style="margin-left: 2rem;">Delete</a>
        </div>
    |]
    else mempty

renderLikeButton postId = [hsx| 
    <form method="POST" action={LikePostAction postId}>
        <button type="submit" class="btn btn-primary" style="margin-right: 1rem; margin-left: 1rem"><i class="fas fa-thumbs-up"></i> Like</button>
    </form>
|]


renderDislikeButton postId = [hsx| 
    <form method="POST" action={DislikePostAction postId}>
        <button type="submit" class="btn btn-primary" style="margin-right: 1rem; margin-left: 1rem"><i class="fas fa-thumbs-up"></i> Dislike</button>
    </form>
|]

renderReactionButtons postId reactions = [hsx|
    <form method="POST" action={CreateReactionAction (Just postId) Nothing}>
        <button type="submit" name="emoji" value="😊" style="background: none; border: none; font-size: 1.5rem; margin-right: 0.5rem;"><span style="font-size: 1rem;">{renderCount (countreactions reactions "😊")}</span> 😊</button>
        <button type="submit" name="emoji" value="👍" style="background: none; border: none; font-size: 1.5rem; margin-right: 0.5rem;"><span style="font-size: 1rem;">{renderCount (countreactions reactions "👍")}</span> 👍</button>
        <button type="submit" name="emoji" value="❤️" style="background: none; border: none; font-size: 1.5rem; margin-right: 0.5rem;"><span style="font-size: 1rem;">{renderCount (countreactions reactions "❤️")}</span> ❤️</button>
        <button type="submit" name="emoji" value="🤣" style="background: none; border: none; font-size: 1.5rem; margin-right: 0.5rem;"><span style="font-size: 1rem;">{renderCount (countreactions reactions "🤣")}</span> 🤣</button>
    </form>
|]



countreactions :: [Reaction] -> Text -> Int 
countreactions reactions emoji = 
    let relevantReactions = filter (\reaction -> get #emoji reaction == emoji) reactions
    in length relevantReactions 
    

renderCount :: Int -> Html
renderCount count = 
    if count > 0
    then [hsx| ({count}) |]
    else mempty



renderComments :: [CommentWithReplies] -> Html
renderComments comments = 
    if not (null comments)
    then [hsx| 
        <p style="font-weight: bold;">Replies:</p>
        <div style="background-color: #B8B8B8; padding: 2rem;">
            {forEach comments renderComment}
        </div>

    |]
    else mempty

renderComment :: CommentWithReplies -> Html
renderComment commentwithreplies = 
    let actualcomment = comment commentwithreplies
    in
    [hsx| 
   <div style="margin-left: 2rem; background-color: #F0F0F0; padding: 1rem; margin-bottom: 1rem; width: 95%;">
        <div style="display: flex; flex-direction: row;">
            <p style="font-weight: bold;">{actualcomment.author}</p>
            <p style="margin-left: 1rem;">{actualcomment.createdAt |> timeAgo}</p>
        </div>
        <p>{actualcomment.body}</p>
        <form method="POST" action={CreateCommentAction2 (actualcomment.postId) }>
            <input type="text" style="border-radius: 10px; border-width: 0.1rem; padding: 0.2rem;" name="body" placeholder="Reply..." required />
            <input type="hidden" name="parentId" value={get #id actualcomment} />
            <button type="submit" class="btn btn-primary" style="margin-left: 1rem;">Reply</button>
        </form>
        {renderComments (replies commentwithreplies)}
    </div>

|]


buildCommentTree :: Maybe (Id' "comments") -> [Comment] -> [CommentWithReplies]
buildCommentTree parentId comments =
    let replies = filter (\c -> touuid2 (get #parentid c) == touuid (toid parentId)) comments
    in map (\reply -> CommentWithReplies reply (buildCommentTree (Just (get #id reply)) comments)) replies

touuid :: Id' "comments" -> UUID
touuid id = do
    case id of
        Id uuid -> uuid

toid :: Maybe (Id' "comments") -> (Id' "comments")
toid id = do
    case id of
        Just id1 -> id1
        Nothing -> Id nil

touuid2 :: Maybe UUID -> UUID
touuid2 id = do
    case id of
        Just uuid -> uuid
        Nothing -> nil

data CommentWithReplies = CommentWithReplies
    { comment :: Comment
    , replies :: [CommentWithReplies]
    }
    deriving (Eq, Show)
