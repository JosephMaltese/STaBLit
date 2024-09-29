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


    action CreateReactionAction { postId } = do
        let emoji = param "emoji"
        let userId = currentUserId
        let useruuid = getuseruuid userId
        let postuuid :: UUID = getpostuuid postId

        existingReaction <- query @Reaction
            |> filterWhere (#userid, useruuid)
            |> filterWhere (#postid, postuuid)
            |> fetchOneOrNothing

        case existingReaction of
            Just reaction -> deleteRecord reaction
            Nothing -> pure ()

        let newReaction = newRecord @Reaction
        newReaction
            |> set #postid postuuid
            |> set #emoji emoji
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


getuseruuid :: Id' "users" -> UUID
getuseruuid id = do
    case id of
        Id uuid -> uuid

getpostuuid :: Id' "posts" -> UUID
getpostuuid id = do
    case id of
        Id uuid -> uuid
