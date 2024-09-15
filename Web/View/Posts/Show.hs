module Web.View.Posts.Show where
import qualified Text.MMark as MMark
import Web.View.Prelude

data ShowView = ShowView { post :: Include "comments" Post }

instance View ShowView where
    html ShowView { .. } = [hsx|
        {breadcrumb}
        <h1>{post.title}</h1>
        <p>{post.createdAt |> timeAgo}</p>
        <p>{post.body |> renderMarkdown}</p>

        <a href={NewCommentAction post.id}>Add Comment</a>
        <div>{forEach post.comments renderComment}</div>

    |]
        where
            breadcrumb = renderBreadcrumb
                            [ breadcrumbLink "Posts" PostsAction
                            , breadcrumbText "Show Post"
                            ]
            renderMarkdown text = 
                case text |> MMark.parse "" of
                    Left error -> "Something went wrong"
                    Right markdown -> MMark.render markdown |> tshow |> preEscapedToHtml
            renderComment comment = [hsx|   
                        <div class="mt-4">
                            <h5>{comment.author}</h5>
                            <p>{comment.body}</p>
                        </div>
                    |]