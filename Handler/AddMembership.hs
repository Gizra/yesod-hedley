module Handler.AddMembership where

import Import
import State (GroupMembershipState(..))
import qualified Database.Esqueleto   as E
import           Database.Esqueleto      ((^.), (?.), (&&.))
import Handler.SseReceive

membershipForm :: UserId -> Maybe GroupMembership -> Form GroupMembership
membershipForm userId mGroupMembership = renderSematnicUiDivs $ GroupMembership
    <$> areq (selectField optionsEnum) (selectSettings "State") (Just State.Active)
    <*> lift (liftIO getCurrentTime)
    <*> pure userId
    <*> areq (selectField companies) (selectSettings "Company") Nothing
    where
        selectSettings label =
          FieldSettings
            { fsLabel = label
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs = [("class", "ui fluid dropdown")]
            }
        companies = do
          entities <- getGroupsUserIsNotMemberOf userId
          optionsPairs $ map (\company -> (companyTitle $ entityVal company, entityKey company)) entities


-- Return the companies a user is not a member of.
getGroupsUserIsNotMemberOf :: UserId -> Handler [Entity Company]
getGroupsUserIsNotMemberOf userId = do
    runDB
        . E.select
        . E.from $ \(company `E.LeftOuterJoin` groupMembership) -> do
            E.on $ E.just (company ^. CompanyId) E.==. (groupMembership ?. GroupMembershipCompanyId) &&.
                   (groupMembership ?. GroupMembershipUserId) E.==. E.just (E.val userId)
            E.where_ $ E.isNothing (groupMembership ?. GroupMembershipCompanyId)
            return company

getAddMembershipR :: Handler Html
getAddMembershipR =  do
    (userId, _) <- requireAuthPair
    companies <- getGroupsUserIsNotMemberOf userId

    if (null companies)
      then do
        -- User has no other groups to subscribe to.
        defaultLayout
            [whamlet|
              You are already subscribed to all groups!
            |]
      else do
        -- Generate the form to be displayed.
        (widget, enctype) <- generateFormPost $ membershipForm userId Nothing
        defaultLayout
            [whamlet|
              <form class="ui form" method=post action=@{AddMembershipR} enctype=#{enctype}>
                  ^{widget}
                  <button.ui.primary.button>Create membership
            |]

postAddMembershipR :: Handler Html
postAddMembershipR = do
    (userId, _) <- requireAuthPair
    ((result, widget), enctype) <- runFormPost $ membershipForm userId Nothing
    case result of
        FormSuccess membership -> do
          mid <- runDB $ insert membership
          sendMessage "AddMembershipR" "1" (Entity mid membership)

          setMessage "Membership saved"
          redirect AddMembershipR
        _ -> defaultLayout
            [whamlet|
                <p>Invalid input, let's try again.
                <form method=post action=@{AddMembershipR} enctype=#{enctype}>
                    ^{widget}
                    <button>Submit
            |]


--
renderSematnicUiDivs = renderSematnicUiDivsMaybeLabels True

-- Only difference here is that we add a ".field" class on the wrapper div.
renderSematnicUiDivsMaybeLabels :: Monad m => Bool -> FormRender m a
renderSematnicUiDivsMaybeLabels withLabels aform fragment = do
    (res, views') <- aFormToForm aform
    let views = views' []
    let widget = [whamlet|
$newline never
\#{fragment}
$forall view <- views
    <div.field :fvRequired view:.required :not $ fvRequired view:.optional>
        $if withLabels
                <label for=#{fvId view}>#{fvLabel view}
        $maybe tt <- fvTooltip view
            <div .tooltip>#{tt}
        ^{fvInput view}
        $maybe err <- fvErrors view
            <div .errors>#{err}
|]
    return (res, widget)
