module Data.AddressBook where

import Prelude

import Control.Plus (empty)
import Data.List (List(..), filter, head, nubBy, null)
import Data.Maybe (Maybe)
import Data.Monoid.Additive (Additive(..))
import Main (fn_takes_fn)

type Entry = {
    first_name :: String,
    last_name :: String,
    address :: Address
}

type Address = {
    street :: String,
    city :: String,
    state :: String
}

type AddressBook = List Entry

show_entry :: Entry -> String
show_entry entry =
    entry.last_name <> ", " <>
    entry.first_name <> ": " <>
    show_address entry.address

show_address :: Address -> String
show_address addr =
    addr.street <> ", " <>
    addr.city <> ", " <>
    addr.state

empty_book :: AddressBook
empty_book = empty

fake_addr :: Address
fake_addr = { street: "123 St.", city: "Faketown", state: "FK" }

fake_addr2 :: Address
fake_addr2 = { street: "456 St.", city: "Fakopolis", state: "FK" }

fake_entry :: Entry
fake_entry = { first_name: "Jonas", last_name: "Brothers", address: fake_addr }

fake_entry2 :: Entry
fake_entry2 = { first_name: "Jonas", last_name: "Brothers", address: fake_addr }

fake_book :: AddressBook
fake_book = insert_entry fake_entry $ insert_entry fake_entry empty_book

insert_entry :: Entry -> AddressBook -> AddressBook
-- insert_entry entry book = Cons entry book
-- insert_entry entry = Cons entry
insert_entry = Cons

-- `$` is the `apply` operator.
find_entry :: String -> String -> AddressBook -> Maybe Entry
find_entry first_name last_name =
    -- head $ filter filter_entry book
    -- head <<< filter filter_entry
    filter filter_entry >>> head
    where
        filter_entry :: Entry -> Boolean
        filter_entry entry = entry.first_name == first_name && entry.last_name == last_name

find_by_address :: Address -> AddressBook -> Maybe Entry
find_by_address addr =
    filter filter_entry >>> head
    where
        filter_entry :: Entry -> Boolean
        filter_entry entry = entry.address == addr

has_address :: Address -> AddressBook -> Boolean
has_address addr =
    filter filter_entry >>> not null
    where
        filter_entry :: Entry -> Boolean
        filter_entry entry = entry.address == addr

remove_duplicates :: AddressBook -> AddressBook
remove_duplicates =
    nubBy same_name
    where
        same_name :: Entry -> Entry -> Boolean
        same_name a b = a.first_name == b.first_name && a.last_name == b.last_name


