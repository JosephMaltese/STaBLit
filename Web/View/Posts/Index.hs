module Web.View.Posts.Index where
import Web.View.Prelude

data IndexView = IndexView { posts :: [Post] }

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
        <div>{forEach posts renderPost}</div>
        </body>
        |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Posts" PostsAction
                ]

renderPost :: Post -> Html
renderPost post = [hsx|
    <div>
            <div style="display: flex; flex-direction: row; justify-content: space-between;">
                <p>Author: {post.author}</p>
                {renderEditDeleteButtons post}
            </div>
            <a href={ShowPostAction post.id} style="font-size: 1.5rem;">{post.title}</a>
            <p style="margin-top: 1rem;">{post.body}</p>

            <div style="display: flex; flex-direction: row; align-items: center;">

                <p style="margin-top: 1rem;">0</p>
                <button class="btn btn-like">
                    <i class="fas fa-thumbs-up"></i> Like
                </button>

                <p style="margin-top: 1rem;">0</p>
                <button class="btn btn-dislike">
                    <i class="fas fa-thumbs-down"></i> Dislike
                </button>

                <a href={NewCommentAction post.id} style="align-items: center;">
                    <p style="margin-top: 1rem;">Comment</p>
                </a>
            </div>
    </div>
|]


renderEditDeleteButtons :: Post -> Html
renderEditDeleteButtons post =
    if currentUser.email == post.author
    then [hsx|
        <div style="display: flex; flex-direction: row;">
            <a href={EditPostAction post.id} class="text-muted">Edit</a>
            <a href={DeletePostAction post.id} class="js-delete text-muted" style="margin-left: 2rem;">Delete</a>
        </div>
    |]
    else mempty