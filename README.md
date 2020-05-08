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

Another thing you can do with `it-has` which you cannot with `data-has` is a trick to "force" a sum type to have a specific field type defined.

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
