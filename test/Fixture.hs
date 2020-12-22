module Fixture where

import ClassyPrelude
    ( ($), IO, error, asks, MonadIO(..), MonadReader )


unimplemented :: a
unimplemented = error "unimplemented"

dispatch0 :: (MonadIO m, MonadReader r m)
         => (r -> IO b)
         -> m b
dispatch0 getter  = do
  func <- asks getter
  liftIO func 

dispatch :: (MonadIO m, MonadReader r m)
         => (r -> a -> IO b)
         -> (a -> m b)
dispatch getter param = do
  func <- asks getter
  liftIO $ func param
dispatch2 :: (MonadIO m, MonadReader r m)
          => (r -> a -> b -> IO c)
          -> (a -> b -> m c)
dispatch2 getter param1 param2 = do
        func <- asks getter
        liftIO $ func param1 param2