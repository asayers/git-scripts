{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Trans.Control
import Data.Maybe
import Git
import Git.Review.Routes
import Git.Libgit2
import Snap.Core
import Snap.Http.Server
import System.Environment

instance MonadThrow Snap where throwM e = liftBase (throwM e)
instance MonadCatch Snap where
    catch act handleE = control $ \runInBase ->
        catch (runInBase act) (\e -> runInBase (handleE e))
instance MonadMask Snap where
    mask mkAct = control $ \runInBase ->
        mask $ \restore -> runInBase $ mkAct (liftBaseOp_ restore)
    uninterruptibleMask mkAct = control $ \runInBase ->
        uninterruptibleMask $ \restore -> runInBase $ mkAct (liftBaseOp_ restore)

main :: IO ()
main = do
    repo <- fromMaybe "." . listToMaybe <$> getArgs
    httpServe (setPort 8080 mempty) $ withRepository lgFactory repo (webApp repo)
