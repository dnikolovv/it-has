# it-has

This is a drop-in replacement of [data-has](http://hackage.haskell.org/package/data-has). The only difference with the original package is that this one uses `Generic` for its default implementation.

This allows you to automatically derive instances for `Has`, e.g.

```haskell
 data Config =
  Config
    { configLogEnv :: LogEnv
    , configDbConn :: DbConnection
    } deriving (Generic, Has LogEnv, Has DbConnection)
```

For more documentation and examples, please refer to the [original package](http://hackage.haskell.org/package/data-has).