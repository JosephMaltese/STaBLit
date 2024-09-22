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
        render IndexView { .. }

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
        post :: Post <- fetch postId

        let userId = currentUser.id
        let hasLiked = userId `elem` post.likes
        let hasDisliked = userId `elem` post.dislikes

        let updatedLikes = if hasLiked
            then filter (/= userId) post.likes
            else (userId : post.likes)
        let updatedDislikes = if hasDisliked 
            then filter (/= userId) post.dislikes
            else post.dislikes
        
        let likeCount = length updatedLikes
        let dislikeCount = length updatedDislikes

        post
            |> set #likes updatedLikes
            |> set #dislikes updatedDislikes
            |> set #likecount likeCount
            |> set #likecount dislikeCount
            |> updateRecord

    action DislikePostAction { postId } = do
        ensureIsUser
        post :: Post <- fetch postId
        let userId = currentUser.id
        let hasLiked = userId `elem` post.likes
        let hasDisliked = userId `elem` post.dislikes

        let updatedLikes = if hasLiked
            then filter (/= userId) post.likes
            else post.likes
        let updatedDislikes = if hasDisliked 
            then filter (/= userId) post.dislikes
            else (userId : post.dislikes)
            
        let likeCount = length updatedLikes
        let dislikeCount = length updatedDislikes

        post
            |> set #likes updatedLikes
            |> set #dislikes updatedDislikes
            |> set #likecount likeCount
            |> set #likecount dislikeCount
            |> updateRecord
    


    

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
