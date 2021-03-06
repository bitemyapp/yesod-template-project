module Handler.Auth where

import Import

import Handler.Sessions
import Helpers.Forms
import Helpers.Views

loginForm :: Form (Text, Text)
loginForm =
  renderDivs $
  (,) <$> areq textField (named "email" (placeheld "Email")) Nothing
      <*> areq passwordField (named "password" (placeheld "Password")) Nothing

redirectIfLoggedIn :: (RedirectUrl App r) => r -> Handler ()
redirectIfLoggedIn r = do
  maybeUser <- getUser
  case maybeUser of
    Nothing -> return ()
    (Just _) -> redirect r

requireUser :: Handler (Entity User)
requireUser = do
  maybeUser <- getUser
  case maybeUser of
    Nothing -> redirect LoginR
    (Just user) -> return user

renderLogin :: Widget -> Handler Html
renderLogin widget = do
  baseLayout Nothing $ do
    setTitle "Login"
    [whamlet|
<div .box .wide>
  <div .legend>
    Login
  <form accept-charset="UTF-8" action="/login" method="post">
    ^{widget}
|]

getLoginR :: Handler Html
getLoginR = do
  redirectIfLoggedIn HomeR
  (loginFormWidget, _) <- generateFormPost loginForm
  renderLogin loginFormWidget

postLoginR :: Handler Html
postLoginR = do
  redirectIfLoggedIn HomeR
  ((result, widget), _) <- runFormPost loginForm
  case result of
    FormSuccess (email, password) -> do
      maybeUP <- runDB (getUserPassword email)
      case maybeUP of
        Nothing ->
          notFound
        (Just ((Entity dbUserKey _), (Entity _ dbPass))) -> do
          let success = passwordMatches (passwordHash dbPass) password
          case success of
            False ->
              notAuthenticated
            True -> do
              setUserSession dbUserKey True
              redirect HomeR
    _ -> renderLogin widget


signupForm :: Form (Text, Text)
signupForm = loginForm

renderSignup :: Widget -> Handler Html
renderSignup widget = do
  baseLayout Nothing $ do
    setTitle "Signup"
    [whamlet|
<div .row #content>
  <div .medium-8 .columns>
    <hr>
<div .row #content>
  <div .medium-8 .columns>
    <h3>Signup for an account!
    <form method="POST" action="@{SignupR}">
      ^{widget}
      <input .button type="submit" value="Submit">
|]

getSignupR :: Handler Html
getSignupR = do
  redirectIfLoggedIn HomeR
  (signupFormWidget, _) <- generateFormPost signupForm
  renderSignup signupFormWidget

postSignupR :: Handler Html
postSignupR = do
  redirectIfLoggedIn HomeR
  ((result, widget), _) <- runFormPost signupForm
  case result of
    FormSuccess (email, password) -> do
      -- Check to see if a user with this email already exists
      maybeUP <- runDB (getUserEntity email)
      case maybeUP of
        -- If it does, render the form again (?)
        (Just _) -> do
          renderSignup widget
        -- If not, create a user
        Nothing -> do
          (Entity dbUserKey _) <- runDB $ createUser email password
          setUserSession dbUserKey True
          redirect HomeR
    _ -> renderSignup widget

getSignoutR :: Handler Html
getSignoutR = do
  deleteLoginData
  redirect HomeR
