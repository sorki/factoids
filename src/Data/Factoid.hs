{-# LANGUAGE OverloadedStrings          #-}

module Data.Factoid where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLogger, NoLoggingT)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Resource
import Data.Text (Text)
import Data.Time
import Database.Esqueleto
import qualified Database.Persist.Sqlite

import Data.Factoid.Schema
import Text.Pretty.Simple

runner
  :: (MonadUnliftIO m)
  => Text
  -> ReaderT SqlBackend (NoLoggingT (ResourceT m)) a
  -> m a
runner con act = Database.Persist.Sqlite.runSqlite con act

runDefault
  :: (MonadUnliftIO m)
  => Text
  -> SqlPersistT (NoLoggingT (ResourceT m)) a
  -> m a
runDefault fdb act = runner
  fdb
  (runMigration migrateAll >> act)

--  "factoids.sqlite"

queryFactoids
  :: (MonadIO m, MonadLogger m)
  => SqlPersistT m ([Entity Factoid])
queryFactoids = select $ from return

queryFactoid
  :: (MonadIO m, MonadLogger m)
  => Text
  -> SqlPersistT m ([Entity Factoid])
queryFactoid k = select $ from $ \f -> do
  where_ (f ^. FactoidName ==. val k)
  limit 1
  pure f

getFact
  :: (MonadIO m, MonadLogger m)
  => Text
  -> SqlPersistT m (Maybe (Entity Factoid))
getFact k = do
  sup <- queryFactoid k
  pure $ case sup of
    [] -> Nothing
    [x] -> pure x

queryFactoidHistory
  :: (MonadIO m, MonadLogger m)
  => FactoidId
  -> SqlPersistT m ([Entity FactoidHistory])
queryFactoidHistory f = select $ from $ \(fh `InnerJoin` factoid) -> do
  on (fh ^. FactoidHistoryRef ==. factoid ^. FactoidId)
  where_ (fh ^. FactoidHistoryRef ==. val f)
  return fh

getFactHist
  :: (MonadIO m, MonadLogger m)
  => Text
  -> SqlPersistT m (Maybe [Entity FactoidHistory])
getFactHist k = do
  mf <- getFact k
  case mf of
    Nothing -> pure Nothing
    Just f -> do
      h <- queryFactoidHistory (entityKey f)
      pure $ pure $ h

setFact
  :: (MonadIO m, MonadLogger m)
  => Text
  -> Text
  -> Text
  -> SqlPersistT m ()
setFact k v by = do
  now <- liftIO $ Data.Time.getCurrentTime

  mf <- getFact k
  case mf of
    Just f -> do
      update $ \x -> do
        set x [FactoidValue =. val v]
        where_ (x ^. FactoidName ==. val k)

      void $ insert $ FactoidHistory (entityKey f) v by now
    Nothing -> do
      newFact <- insert $ Factoid k v False
      insert $ FactoidHistory newFact v by now
      return ()

forgetFact
  :: (MonadIO m, MonadLogger m)
  => Text
  -> SqlPersistT m Bool
forgetFact k = do
  mf <- getFact k
  case mf of
    Just f -> do
      deleteCascade $ entityKey f
      return True
    Nothing -> do
      return False

-- hoist

setFactIO
  :: Text
  -> Text
  -> Text
  -> Text
  -> IO ()
setFactIO db k v by = runDefault db $ setFact k v by

getFactIO
  :: Text
  -> Text
  -> IO (Maybe Factoid)
getFactIO db k = fmap entityVal <$> (runDefault db . getFact $ k)

getFactHistIO
  :: Text
  -> Text
  -> IO (Maybe [FactoidHistory])
getFactHistIO db k = fmap (map entityVal) <$> (runDefault db . getFactHist $ k)

forgetFactIO
  :: Text
  -> Text
  -> IO Bool
forgetFactIO db = runDefault db . forgetFact

test :: IO ()
test = runner ":memory:" $ do
  runMigration migrateAll

  factA <- insert $ Factoid "fa" "fa-val" False
  factB <- insert $ Factoid "fb" "fb-val" True

  now <- liftIO $ Data.Time.getCurrentTime
  insert $ FactoidHistory factA "fa-val" "jane" now
  insert $ FactoidHistory factB "fb-val" "joe" now

  setFact "set" "toSomeValue" "by-user"
  setFact "set" "toSomeOtherValue" "by-another-ser"
  setFact "set" "reset" "by-res"
  Just _ <- getFact "set"

  setFact "ephemeral" "toSomeValue" "by-user"
  forgetFact "ephemeral"
  Nothing <- getFact "ephemeral"

  facts <- queryFactoids
  histSet <- getFactHist "set"
  liftIO $ do
    pPrint facts
    pPrint histSet
