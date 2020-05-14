![Haskell CI](https://github.com/dnikolovv/it-has/workflows/Haskell%20CI/badge.svg)

# it-has

This is a (nearly) drop-in replacement of [data-has](http://hackage.haskell.org/package/data-has). The differences with the original package are that this one misses `hasLens` and uses `Generic` for its default implementation. Your initial reaction may be to start mourning the loss of `hasLens`, but first take a look at the cool things you can do without it!

Reduce boilerplate! You can trim down this:

```haskell
data Config =
  Config
    { configLogEnv      :: !LogEnv
    , configJwtSettings :: !JWTSettings
    , configMetrics     :: !Metrics
    , configEkgStore    :: !EKG.Store }

-- Heavy manual instances, data-has only has default implementation for tuples
instance Has LogEnv Config where
  getter = configLogEnv
  modifier f v = v { configLogEnv = f (configLogEnv v) }

instance Has JWTSettings Config where
  getter = configJwtSettings
  modifier f v = v { configJwtSettings = f (configJwtSettings v) }

instance Has Metrics where
  getter = configMetrics
  modifier f v = v { configMetrics = f (configJwtSconfigMetricsettings v) }

instance Has EKG.Store Config where
  getter = configEkgStore
  modifier f v = v { configEkgStore = f (configEkgStore v) }
```

To this:

```haskell
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

data Config =
  Config
    { configLogEnv      :: !LogEnv
    , configJwtSettings :: !JWTSettings
    , configMetrics     :: !Metrics
    , configEkgStore    :: !EKG.Store
    } deriving (Generic, Has LogEnv, Has JWTSettings, Has Metrics, Has EKG.Store)
```

Another trick is that you can "force" a sum type to have a specific field defined.

E.g. you may want to define an `Error` type and enforce that it always has an `ErrorText` attached to it.

```haskell
newtype ErrorText =
 ErrorText Text
 
data Error =
 ValidationError |
 NotFound |
 Critical |
 Unauthorized
```

You can do that by deriving `Has ErrorText`. The compiler will error until you have added an `ErrorText` field to each representation.

```haskell
data Error =
 ValidationError ErrorText |
 NotFound ErrorText |
 Critical ErrorText |
 Unauthorized ErrorText
 deriving (Generic, Has ErrorText)
```

For more documentation and examples, please refer to the [original package](http://hackage.haskell.org/package/data-has).
