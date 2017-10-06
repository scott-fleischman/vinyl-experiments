{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Data.Vinyl
import Data.Vinyl.CoRec

data MyField = MyFieldCount | MyFieldName

type family MyFieldType (f :: MyField) :: * where
  MyFieldType 'MyFieldCount = Integer
  MyFieldType 'MyFieldName = String
newtype MyFieldTypeN f = MyFieldTypeN (MyFieldType f)

myRec :: Rec MyFieldTypeN '[ 'MyFieldCount, 'MyFieldName ]
myRec
  =  (MyFieldTypeN @'MyFieldCount 20)
  :& (MyFieldTypeN @'MyFieldName "large")
  :& RNil

data MyVariant = CoRecError | CoRecValue
type family MyVariantType (f :: MyVariant) :: * where
  MyVariantType 'CoRecError = String
  MyVariantType 'CoRecValue = Integer
newtype MyVariantTypeN f = MyVariantTypeN (MyVariantType f)

myCoRec :: CoRec MyVariantTypeN '[ 'CoRecError, 'CoRecValue ]
myCoRec = CoRec (MyVariantTypeN @'CoRecError "The ship is sinking")

main :: IO ()
main = do
  putStrLn "hello world"
