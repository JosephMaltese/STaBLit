module Web.View.Posts.Index where
import Web.View.Prelude



data IndexView = IndexView { posts :: [Post], reactions:: [Reaction] }

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
        <div>{forEach posts ( \p -> renderPost p reactions)}</div>
        </body>
        |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Posts" PostsAction
                ]

renderPost :: Post -> [Reaction] -> Html
renderPost post reactions = [hsx|
    <div style="margin-bottom: 2rem;">
            <div style="display: flex; flex-direction: row; justify-content: space-between;">
                <p>Author: {post.author}</p>
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

                <div class="emoji-reactions">
                    {renderReactionButtons post.id}
                </div>

                <div class="reactions-display">
                    {renderReactions post reactions}
                </div>

                
            </div>
            <hr>
    </div>
|]


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

{--
renderReactionButtons postId = [hsx|
    <div>
        <a name="emoji" href={CreateReactionAction postId "😊"}>😊</a>
        <a name="emoji" href={CreateReactionAction postId "👍"}>👍</a>
        <a name="emoji" href={CreateReactionAction postId "❤️"}>❤️</a>
    </div>
|]
--}

renderReactionButtons postId = [hsx|
    <div style="display: flex; flex-direction: row;">
        <form method="POST" action={CreateReactionAction postId "😊"}>
            <button type="submit" style="margin-right: 1rem; margin-left: 1rem">😊</button>
        </form>
        <form method="POST" action={CreateReactionAction postId "👍"}>
            <button type="submit" style="margin-right: 1rem; margin-left: 1rem">👍</button>
        </form>
        <form method="POST" action={CreateReactionAction postId "❤️"}>
            <button type="submit" style="margin-right: 1rem; margin-left: 1rem">❤️</button>
        </form>
    </div>
|]


{--
renderReactions :: Post -> [Reaction] -> Html
renderReactions post reactions = do
    let relevantreactions = filter (\reaction -> (touuid (get #postid reaction)) == (touuid (get #id post))) reactions
    [hsx|
    <div>
        {length reactions}
    </div>
|]
--}

renderReactions :: Post -> [Reaction] -> Html
renderReactions post reactions = do
    [hsx|
    <div>
    </div>
|]

renderReaction reaction = do
    [hsx|
    <div>
        <span>{reaction.text}</span>
    </div>
|]
   

{--
touuid :: Id' "posts" -> UUID
touuid postid = do
    case postid of
        Id uuid -> uuid
--}