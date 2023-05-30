module Data.AddressBook where

import Prelude

import Control.Plus (empty)
import Data.List (List(..), filter, head, nubByEq)
import Data.Maybe (Maybe, isJust)

type Address =
  { street :: String
  , city   :: String
  , state  :: String
  }

type Entry =
  { firstName :: String
  , lastName  :: String
  , address   :: Address
  }

type AddressBook = List Entry

showAddress :: Address -> String
showAddress addr = addr.street <> ", " <>
                   addr.city <> ", " <>
                   addr.state


showEntry :: Entry -> String
showEntry entry = entry.lastName <> ", " <>
                  entry.firstName <> ": " <>
                  showAddress entry.address

emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons


firstEntryMatchingPredicate :: (Entry -> Boolean) -> AddressBook -> Maybe Entry
firstEntryMatchingPredicate predicate = filter predicate >>> head



findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry firstName lastName = firstEntryMatchingPredicate matchesName
  where
    matchesName entry = entry.firstName == firstName && entry.lastName == lastName


findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet street = firstEntryMatchingPredicate matchesAddress
  where
    matchesAddress entry = entry.address.street == street


isInBook :: String -> String -> AddressBook -> Boolean
isInBook firstName lastName = findEntry firstName lastName >>> isJust



removeDuplicatesManual :: AddressBook -> AddressBook
removeDuplicatesManual Nil = Nil
removeDuplicatesManual (Cons head tail) = 
   if isInBook head.firstName head.lastName tail
   then Cons head $ removeDuplicatesManual tail
   else removeDuplicatesManual tail


removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubByEq nameMatches
  where
    nameMatches a b = a.firstName == b.firstName && a.lastName == b.lastName