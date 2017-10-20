{-# LANGUAGE  BangPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Logger
-- Copyright   :  (c) carbolymer
-- License     :  Apache-2.0
--
-- Stability   :  experimental
-- Portability :  POSIX
--
-- Dead simple logger
--
-----------------------------------------------------------------------------
module Logger (getLogger) where

import Control.Concurrent
import Control.Monad.RWS
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Time (defaultTimeLocale, formatTime, getZonedTime)

logMessage :: (MonadIO m) => String -> String -> String -> m ()
logMessage !moduleName !level !message = liftIO $ do
  currentTime <- message `seq` getZonedTime
  let formattedTime = formatTime defaultTimeLocale "%F %T" currentTime
  putStrLn $! formattedTime ++ " " ++ level ++ " [" ++ moduleName ++ "] " ++ message

-- | Creates logging functions which are returning log messages in `MonadIO`
getLogger :: (MonadIO m) => String            -- ^ Logger name
          -> (String -> m (), String -> m ()) -- ^ Logging functions: (info logger, error logger)
getLogger moduleName = (logMessage moduleName "INFO", logMessage moduleName "ERROR")
