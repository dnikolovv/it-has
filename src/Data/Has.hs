{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE FlexibleInstances     #-}

{-|
Module      : Data.Has
Description : Automatically derivable Has instances.
Copyright   : (c) Dobromir Nikolov, 2020
License     : BSD3
Maintainer  : dnikolovv@hotmail.com
Stability   : experimental
Portability : PORTABLE

This is a drop-in replacement of <http://hackage.haskell.org/package/data-has data-has>. The only difference with the original package is that this one uses `Generic` for its default implementation.

This allows you to automatically derive instances for `Has`, e.g.

@
 data Config =
  Config
    { configLogEnv :: LogEnv
    , configDbConn :: DbConnection
    } deriving (Generic, Has LogEnv, Has DbConnection)
@

For more documentation and examples, please refer to the <http://hackage.haskell.org/package/data-has original package>.
-}
module Data.Has where

import           Data.Generics.Internal.VL.Lens (over)
import           Data.Generics.Product.Typed

-- | A type class for an extensible product.
class Has a b where
  getter :: b -> a
  modifier :: (a -> a) -> b -> b

  default getter :: HasType a b => b -> a
  getter = getTyped @a

  default modifier :: HasType a b => (a -> a) -> b -> b
  modifier = over $ typed @a

instance Has a a where
  getter = id
  modifier = id
