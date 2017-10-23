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
module Logger (Level(..), getLogger) where

import Control.Concurrent
import Control.Monad.RWS
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Time (defaultTimeLocale, formatTime, getZonedTime)


logMessage :: (MonadIO m) => String -> Level -> String -> m ()
logMessage !moduleName !level !message = liftIO $ do
  currentTime <- message `seq` getZonedTime
  let formattedTime = formatTime defaultTimeLocale "%F %T" currentTime
  putStrLn $! formattedTime ++ " " ++ (show level) ++ " [" ++ moduleName ++ "] " ++ message


-- | Logging levels
data Level = INFO     -- ^ Info
           | WARNING  -- ^ Warning
           | ERROR    -- ^ Error
           deriving (Show, Eq)


-- | Creates logging functions which are returning log messages in `MonadIO`
getLogger :: (MonadIO m) => String  -- ^ Logger name
          -> [Level]                -- ^ Logging levels
          -> [String -> m ()]       -- ^ Logging functions for particular levels
getLogger moduleName = map (logMessage moduleName)
