{-# LANGUAGE MultiParamTypeClasses #-}
module HaskellGame.Utils.Dictionary where

import Prelude ( Eq(..), Show(..), String(),
                 Bool(..),
                 (.), ($), fst, snd )

import Data.Maybe ( Maybe(..) )

import qualified Data.List as List

{- A dictionary type -}

class Eq k => Dictionary d k v where
  insert :: k -> v -> d k v -> d k v
  lookup :: k -> d k v -> Maybe v
  delete :: k -> d k v -> d k v
  empty :: d k v -> Bool
  fromList :: [(k, v)] -> d k v
  toList :: d k v -> [(k, v)]

  delete k dict = fromList [ (x, y) | (x, y) <- toList dict, x /= k ]

data SimpleDict k v = SimpleDict [(k, v)]
                    deriving (Eq, Show)

instance (Eq v) => Dictionary SimpleDict String v where
  insert k v (SimpleDict dict) =
    let old = List.find ((==k) . fst) dict
    in
      case old of
        Nothing -> SimpleDict $ (k, v):dict
        Just (ok, ov) ->  SimpleDict $ (k, v):(List.delete (ok, ov) dict)

  lookup k (SimpleDict dict) =
    let pair = List.find ((==k) . fst) dict
    in
      case pair of
        Nothing -> Nothing
        Just (_, v) -> Just v

  fromList = SimpleDict

  toList (SimpleDict x) = x

  empty (SimpleDict d) = List.null d
