module Model.Types where

import           ClassyPrelude.Yesod

type ControlIO m = (MonadIO m)

type DBM m a =
  (ControlIO m) => SqlPersistT m a

type DB a = forall m. DBM m a

type DBVal val =
  ( PersistEntity val
  , PersistEntityBackend val ~ SqlBackend
  , PersistStore (PersistEntityBackend val))

fetchThingByField
  :: (PersistField typ, DBVal val)
  => EntityField val typ -> typ -> DB (Maybe (Entity val))
fetchThingByField field u =
  selectFirst [field ==. u] []
