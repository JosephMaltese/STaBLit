module Web.Controller.Reactions where

import Web.Controller.Prelude
import Web.View.Reactions.Index
import Web.View.Reactions.New
import Web.View.Reactions.Edit
import Web.View.Reactions.Show
import Application.Script.Prelude (Comment'(postId))
import Generated.ActualTypes (Reaction'(emoji))

instance Controller ReactionsController where
    action ReactionsAction = do
        reactions <- query @Reaction |> fetch
        render IndexView { .. }

    action NewReactionAction = do
        let reaction = newRecord
        render NewView { .. }

    action ShowReactionAction { reactionId } = do
        reaction <- fetch reactionId
        render ShowView { .. }

    action EditReactionAction { reactionId } = do
        reaction <- fetch reactionId
        render EditView { .. }

    action UpdateReactionAction { reactionId } = do
        reaction <- fetch reactionId
        reaction
            |> buildReaction
            |> ifValid \case
                Left reaction -> render EditView { .. }
                Right reaction -> do
                    reaction <- reaction |> updateRecord
                    setSuccessMessage "Reaction updated"
                    redirectTo EditReactionAction { .. }

    {--
    action CreateReactionAction = do
        let reaction = newRecord @Reaction
        reaction
            |> buildReaction
            |> ifValid \case
                Left reaction -> render NewView { .. } 
                Right reaction -> do
                    reaction <- reaction |> createRecord
                    setSuccessMessage "Reaction created"
                    redirectTo ReactionsAction
    --}

    action CreateReactionAction {postId} = do
        emoji <- param @Text "emoji"
        userId <- currentUserId
        let emojitext = case emoji of
            "1" -> "😊"
            "2" -> "👍"
            "3" -> "❤️"
            _ -> " "
        let useruuid :: UUID = getuuid userId
        let postuuid :: UUID = getuuid postId
        let reaction = newRecord @Reaction
        reaction
            |> set #postid postId
            |> set #emoji emojitext
            |> set #userid useruuid
            |> createRecord
        setSuccessMessage "Reaction created"
        redirectTo PostsAction

    action DeleteReactionAction { reactionId } = do
        reaction <- fetch reactionId
        deleteRecord reaction
        setSuccessMessage "Reaction deleted"
        redirectTo ReactionsAction



buildReaction reaction = reaction
    |> fill @'["postid", "userid", "emoji"]


getuuid :: Id' "users" -> UUID
getuuid id = do
    case id of
        Id uuid -> uuid
