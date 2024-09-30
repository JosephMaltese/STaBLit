module Web.Controller.Comments where

import Web.Controller.Prelude
import Web.View.Comments.Index
import Web.View.Comments.New
import Web.View.Comments.Edit
import Web.View.Comments.Show

instance Controller CommentsController where
    action CommentsAction = do
        comments <- query @Comment |> fetch
        render IndexView { .. }

    action NewCommentAction { postId } = do
        let comment = newRecord
                |> set #postId postId
                |> set #author currentUser.username
        post <- fetch postId
        render NewView { .. }

    action ShowCommentAction { commentId } = do
        comment <- fetch commentId
        render ShowView { .. }

    action EditCommentAction { commentId } = do
        comment <- fetch commentId
        render EditView { .. }

    action UpdateCommentAction { commentId } = do
        comment <- fetch commentId
        comment
            |> buildComment
            |> ifValid \case
                Left comment -> render EditView { .. }
                Right comment -> do
                    comment <- comment |> updateRecord
                    setSuccessMessage "Comment updated"
                    redirectTo EditCommentAction { .. }

    action CreateCommentAction = do
        let comment = newRecord @Comment
        comment
            |> buildComment
            |> ifValid \case
                Left comment -> do
                    post <- fetch comment.postId -- <---- NEW
                    render NewView { .. } 
                Right comment -> do
                    comment <- comment |> createRecord
                    setSuccessMessage "Comment created"
                    redirectTo ShowPostAction { postId = comment.postId }
    
    action CreateCommentAction2 { postId } = do
        let bodyText = param "body"
        let parentId = param "parentId"
        let comment = newRecord @Comment
        comment
            |> set #postId postId
            |> set #author currentUser.username
            |> set #body bodyText
            |> set #parentid (if isNothing parentId then Nothing else parentId )
            |> createRecord
        setSuccessMessage "Comment created"
        redirectTo PostsAction


    action DeleteCommentAction { commentId } = do
        comment <- fetch commentId
        deleteRecord comment
        setSuccessMessage "Comment deleted"
        redirectTo CommentsAction

buildComment comment = comment
    |> fill @'["postId", "body"]
    |> set #author currentUser.username
