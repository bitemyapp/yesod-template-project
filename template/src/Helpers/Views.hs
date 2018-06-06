module Helpers.Views where

import Import

baseLayout :: Maybe (Entity User)
           -> WidgetFor App ()
           -> HandlerFor App Html
baseLayout _ content =
  defaultLayout $ do
    addStylesheet (StaticR css_app_css)
    [whamlet|
<div #wrapper>
  <div #header>
    <div>
      <a #homelogo
         href="@{HomeR}"
         title="PROJECTNAME">
      <span .headerlinks>
        <a href="@{HomeR}">
          Home
    <div #headerright>
      <span .headerlinks>
        <a href="@{LoginR}">
          Login
    <div .clear>
  <div #inside>
    ^{content}
    <div #footer>
      <a href="@{AboutR}">
        About
    <div .clear>
|]
