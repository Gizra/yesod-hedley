module Handler.AddMembership where

import Import
import State (GroupMembershipState(..))

membershipForm :: UserId -> Maybe GroupMembership -> Form GroupMembership
membershipForm userId mGroupMembership = renderDivs $ GroupMembership
    <$> areq (selectField optionsEnum) "State" (Just State.Active)
    <*> lift (liftIO getCurrentTime)
    <*> pure userId
    <*> areq (selectField companies) "Company" Nothing
    where
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
          <form method=post action=@{AddMembershipR} enctype=#{enctype}>
              ^{widget}
              <p>It also doesn't include the submit button.
              <button>Submit
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
