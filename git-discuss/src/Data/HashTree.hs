{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.HashTree
    ( HashTree(..)
    , lookupValue
    , lookupParent
    , traverseMaybeWithKey
    , foldTree
    ) where

import Data.Function
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMS
import Data.Hashable
import Data.List

newtype HashTree k v = HashTree
    { nodes :: HashMap k (Maybe k, v)
    } deriving (Eq, Show, Monoid)

lookupValue :: (Eq k, Hashable k) => k -> HashTree k v -> Maybe v
lookupValue k = fmap snd . HMS.lookup k . nodes

lookupParent :: (Eq k, Hashable k) => k -> HashTree k v -> Maybe (Maybe k)
lookupParent k = fmap fst . HMS.lookup k . nodes

traverseMaybeWithKey
    :: (Applicative f)
    => (k -> v -> f (Maybe v')) -> HashTree k v -> f (HashTree k v')
traverseMaybeWithKey fn =
    fmap (HashTree . HMS.mapMaybe id) .
    HMS.traverseWithKey (\k (p, v) -> fmap ((,) p) <$> fn k v) .
    nodes

-------------------------------------------------------------------------------
-- Traversing the comment tree

-- The roots are in-order, whereas children are traversed in reverse order.
-- This is weird, but it makes sense in the context of the web UI.
foldTree
    :: Eq k
    => (v -> v -> Ordering)  -- ^ Order siblings
    -> (k -> v -> [a] -> a)  -- ^ Process a node, plus its processed children
    -> HashTree k v
    -> [a]
foldTree ord fn tree =
    map go (sortBy ord' $ getRoots tree)
  where
    ord' = ord `on` (snd . snd)
    go (k, (_p, v)) = fn k v (map go (sortBy ord' $ getChildren k tree))

getRoots :: Eq k => HashTree k v -> [(k, (Maybe k, v))]
getRoots = HMS.toList . HMS.filter (\(parent,_) -> parent == Nothing) . nodes

getChildren :: Eq k => k -> HashTree k v -> [(k, (Maybe k, v))]
getChildren k = HMS.toList . HMS.filter (\(parent,_) -> parent == Just k) . nodes
