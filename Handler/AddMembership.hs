module Handler.AddMembership where

import Import
import State (GroupMembershipState(..))

membershipForm :: UserId -> Maybe GroupMembership -> Form GroupMembership
membershipForm userId mGroupMembership = renderSematnicUiDivs $ GroupMembership
    <$> areq (selectField optionsEnum) stateSettings (Just State.Active)
    <*> lift (liftIO getCurrentTime)
    <*> pure userId
    <*> areq (selectField companies) companySettings Nothing
    where
        stateSettings = FieldSettings
          { fsLabel = "State"
          , fsTooltip = Nothing
          , fsId = Nothing
          , fsName = Nothing
          , fsAttrs = [("class", "ui fluid dropdown")]
          }
        companySettings = FieldSettings
          { fsLabel = "Company"
          , fsTooltip = Nothing
          , fsId = Nothing
          , fsName = Nothing
          , fsAttrs = [("class", "ui fluid dropdown")]
          }
        companies = do
          entities <- runDB $ selectList [] [Asc CompanyTitle]
          optionsPairs $ map (\company -> (companyTitle $ entityVal company, entityKey company)) entities



getAddMembershipR :: Handler Html
getAddMembershipR =  do
    (userId, _) <- requireAuthPair
    -- Generate the form to be displayed
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
          _ <- runDB $ insert membership
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
