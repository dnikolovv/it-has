{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE TemplateHaskell    #-}

import           Control.Monad (void)
import           Data.Has
import           GHC.Generics    (Generic)
import           Test.Hspec
import           Test.QuickCheck (Arbitrary, quickCheckAll)

newtype LogConfig =
  LogConfig String
  deriving (Show, Eq)
  deriving (Arbitrary) via (String)

newtype SomeUrl =
  SomeUrl String
  deriving (Show, Eq)
  deriving (Arbitrary) via (String)

newtype ConnectionString =
  ConnectionString String
  deriving (Show, Eq)
  deriving (Arbitrary) via (String)

newtype DatabaseConfig =
  DatabaseConfig String
  deriving (Show, Eq)
  deriving (Arbitrary) via (String)

data SomeConfig =
  SomeConfig
    { someConfigLogConfig      :: LogConfig
    , someConfigDatabaseConfig :: DatabaseConfig
    , someConfigSomeUrl        :: SomeUrl
    } deriving
      ( Generic
      , Has LogConfig
      , Has DatabaseConfig
      , Has SomeUrl )

newtype ErrorText =
  ErrorText String
  deriving (Show, Eq)
  deriving (Arbitrary) via (String)

data Error =
  ValidationError ErrorText |
  Unauthorized ErrorText
  deriving (Generic, Has ErrorText)

prop_returnsSetValuesForRecord :: LogConfig -> DatabaseConfig -> SomeUrl -> Bool
prop_returnsSetValuesForRecord logConfig dbConfig someUrl =
  let config = SomeConfig logConfig dbConfig someUrl
  in getter config == logConfig &&
     getter config == dbConfig &&
     getter config == someUrl

prop_returnsForcedFieldsForSumType :: ErrorText -> ErrorText -> Bool
prop_returnsForcedFieldsForSumType firstErrorText secondErrorText =
  let firstError = ValidationError firstErrorText
      secondError = Unauthorized secondErrorText
  in getter firstError == firstErrorText &&
     getter secondError == secondErrorText

return []

main :: IO ()
main = void runTests

runTests = $quickCheckAll