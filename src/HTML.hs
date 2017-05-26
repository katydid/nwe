{-# LANGUAGE FlexibleInstances #-}

module HTML (parse) where

import Text.HTML.TagSoup

parse :: String -> [String]
parse s = concatMap toStrings (parseTags s)

toStrings :: Tag String -> [String]
toStrings (TagOpen s as) = (renderTags [TagOpen s []]): (map (\(k, v) -> k ++ "=" ++ v) as)
toStrings t = [renderTags [t]]
