module Fixture where

import ClassyPrelude
import Bot.Error
import Control.Monad.Except




unimplemented :: a
unimplemented = error "unimplemented"




-- dispatch0 ::(r -> Either Error b)
--          -> Either Error b
-- dispatch0 getter  = asks getter  >>= liftEither
  
--   -- do
--   -- func <- asks getter
--   -- return func
--   -- liftIO func 
-- -- asks _sendMsg mess >>= liftEither
-- dispatch :: (MonadIO m, MonadReader r m)
--          => (r -> a -> IO b)
--          -> (a -> m b)
-- dispatch getter param = do
--   func <- asks getter
--   liftIO $ func param
-- dispatch2 :: (MonadIO m, MonadReader r m)
--           => (r -> a -> b -> IO c)
--           -> (a -> b -> m c)
-- dispatch2 getter param1 param2 = do
--         func <- asks getter
--         liftIO $ func param1 param2