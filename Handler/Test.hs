{-# LANGUAGE RecordWildCards #-}
module Handler.Test where

import Import

data Person = Person
    { name :: Text
    , age  :: Int
    }

instance ToJSON Person where
    toJSON Person {..} = object
        [ "name" .= name
        , "age"  .= age
        ]

instance FromJSON Person where
  parseJSON (Object o) = Person
    <$> o .: "name"
    <*> o .: "age"
  parseJSON _ = empty

getTestR :: Handler Value
getTestR = returnJson $ Person "Machete" 42

postTestR :: Handler Value
postTestR = do
  person <- requireJsonBody :: Handler Person
  returnJson $ person { name = "Machete" }
