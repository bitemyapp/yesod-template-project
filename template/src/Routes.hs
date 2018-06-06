{-# OPTIONS_GHC -fno-warn-orphans #-}

module Routes where

import Import.NoFoundation

import AppType

mkYesodData "App" [parseRoutes|
/static StaticR Static appStatic

/ HomeR GET
/about AboutR GET

-- Auth
/login      LoginR   GET POST
/signup     SignupR  GET POST
/signout    SignoutR GET
|]
