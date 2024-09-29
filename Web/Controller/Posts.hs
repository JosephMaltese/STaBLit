module Web.Controller.Posts where

import qualified Text.MMark as MMark
import Web.Controller.Prelude
import Web.View.Posts.Index
import Web.View.Posts.New
import Web.View.Posts.Edit
import Web.View.Posts.Show


instance Controller PostsController where

    action PostsAction = do
        posts <- query @Post
            |> orderByDesc #createdAt
            |> fetch
        postsWithDetails <- forM posts \post -> do
            let postid1 = (get #id post)
            let postuuid = getpostuuid postid1
            reactions <- query @Reaction
                |> filterWhere (#postid, postuuid)
                |> fetch
            comments <- query @Comment
                |> filterWhere (#postId, postid1)
                |> fetch
            pure (post, reactions, comments)
        render IndexView { postsWithDetails }


    action NewPostAction = do
        let post = newRecord
        render NewView { .. }

    action ShowPostAction { postId } = do
        post <- fetch postId
            >>= pure . modify #comments (orderByDesc #createdAt)
            >>= fetchRelated #comments
        render ShowView { .. }

    action EditPostAction { postId } = do
        post <- fetch postId
        render EditView { .. }

    action UpdatePostAction { postId } = do
        post <- fetch postId
        post
            |> buildPost
            |> ifValid \case
                Left post -> render EditView { .. }
                Right post -> do
                    post <- post |> updateRecord
                    setSuccessMessage "Post updated"
                    redirectTo EditPostAction { .. }

    action CreatePostAction = do
        let post = newRecord @Post
        post
            |> buildPost
            |> set #author currentUser.username
            --- Optional --- UPDATE: SEEMS TO WORK FOR NOW
            |> set #likecount 0
            |> set #dislikecount 0
            |> set #likes []
            |> set #dislikes []
            |> ifValid \case
                Left post -> render NewView { .. } 
                Right post -> do
                    post <- post |> createRecord
                    setSuccessMessage "Post created"
                    redirectTo PostsAction

    action DeletePostAction { postId } = do
        post <- fetch postId
        deleteRecord post
        setSuccessMessage "Post deleted"
        redirectTo PostsAction

    action LikePostAction { postId } = do
        ensureIsUser
        post <- fetch postId

        let id :: Id' "users" = currentUserId
        let userId = getuuid id
        let hasLiked = userId `elem` post.likes
        let hasDisliked = userId `elem` post.dislikes

        let likes = if hasLiked
            then filter (/= userId) post.likes
            else (userId : post.likes)
        let dislikes = if hasDisliked 
            then filter (/= userId) post.dislikes
            else post.dislikes
        
        let likeCount = length likes
        let dislikeCount = length dislikes

        updatePost postId likes dislikes likeCount dislikeCount
        redirectTo PostsAction





    action DislikePostAction { postId } = do
        ensureIsUser
        post <- fetch postId

        let id = currentUserId
        let userId = getuuid id
        let hasLiked = userId `elem` post.likes
        let hasDisliked = userId `elem` post.dislikes

        let likes = if hasLiked
            then filter (/= userId) post.likes
            else post.likes
        let dislikes = if hasDisliked 
            then filter (/= userId) post.dislikes
            else (userId : post.dislikes)
            
        let likeCount = length likes
        let dislikeCount = length dislikes

        updatePost postId likes dislikes likeCount dislikeCount
        redirectTo PostsAction
    


    

buildPost post = post
    |> fill @'["title", "body"]
    |> validateField #title nonEmpty
    |> validateField #body nonEmpty
    |> validateField #body isMarkdown


isMarkdown :: Text -> ValidatorResult
isMarkdown text = 
    case MMark.parse "" text of
        Left _ -> Failure "Please provide valid Markdown"
        Right _ -> Success




updatePost postId likes dislikes likeCount dislikeCount = do
    post :: Post <- fetch postId
    post
        |> set #likes likes
        |> set #dislikes dislikes
        |> set #likecount likeCount
        |> set #dislikecount dislikeCount
        |> updateRecord

    setSuccessMessage "Post liked/disliked"

getuuid :: Id' "users" -> UUID
getuuid id = do
    case id of
        Id uuid -> uuid

getpostuuid :: Id' "posts" -> UUID
getpostuuid id = do
    case id of
        Id uuid -> uuid