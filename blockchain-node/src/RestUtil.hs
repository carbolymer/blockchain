{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module RestUtil where

import Servant (Handler, NoContent(..))
import Control.Monad.IO.Class (liftIO)

class ConvertibleToApiHandler s t where
  convert :: s -> t

instance ConvertibleToApiHandler (IO s) (Handler s) where
  convert = liftIO

instance ConvertibleToApiHandler (IO ()) (Handler NoContent) where
  convert _ = return NoContent

instance (ConvertibleToApiHandler s t) => ConvertibleToApiHandler (a -> s) (a -> t) where
  convert function = convert . function
