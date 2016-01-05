#!/bin/env runhaskell
{- See README.md
 - TODO: handle annotations
 -}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Main where

import           Data.Default    (def)
import           Data.List       (zipWith4)
import qualified Data.Text       as T
import qualified Data.Yaml       as YML
import           Prelude         hiding (readFile)
import           Prelude.Unicode
import           Text.XML        (ParseSettings, readFile)
import           Text.XML.Cursor

{- This record matches the table's headers, without a few useless or
 - difficult fields
 -}
data Emoji = Emoji { codes :: T.Text -- a hexadecimal string
                   -- emojis are either a single unicode character, or can
                   -- contain a modifier in the case of skin tone, flags, etc.
                   , chars :: Either (Char, Char) Char
                   , name  :: T.Text -- human readable, "CAR", etc.
                   -- TODO: use native PNG: https://goo.gl/o5U8JR
                   , image :: T.Text -- png formatted image in base64 text
                   -- , annotations :: [String] -- categories, like "light"
                   } deriving (Eq, Show, Read)

instance YML.ToJSON Emoji where
    toJSON emoji =
        YML.object [ "codes" YML..= codes emoji
                   -- transform the chars into strings of length 1 or 2
                   , "chars" YML..= case chars emoji of
                       Left (c1, c2) -> [c1, c2]
                       Right c1 -> [c1]
                   , "name" YML..= name emoji
                   -- To include the image in the encoding, just uncomment this
                   -- , "image" YML..= image emoji
                   ]

getCodes :: Cursor -> [T.Text]
getCodes tbody = map (T.replace "U+" "") $ rawCodes
    -- these include a prefix of U+ and are Texts
    where rawCodes = tbody $// attributeIs "class" "code" &// content

getChars :: Cursor -> [Either (Char, Char) Char]
getChars tbody =
    let txts = tbody $// attributeIs "class" "chars" &// content
        -- construct an Either from the Texts returned above
        foldFunc t lst = case T.length t of
                            2 -> Left (T.index t 0, T.index t 1) : lst
                            -- TODO: unsafe, avoid len 0
                            _ -> Right (T.index t 0) : lst
    in foldr foldFunc [] txts

-- this one is a bit more complex because both the "name" and "attribute" fields
-- have class == "name", but we can filter them out because they all have links.
getNames :: Cursor -> [T.Text]
getNames tbody = let rawNms :: [T.Text]
                     rawNms = tbody $// laxElement "td"
                                    >=> attributeIs "class" "name"
                                    >=> check (not . containsLinks)
                                    &/ content
                 in filter (not . parenthesized) rawNms
    where containsLinks cursor = not ∘ null $ (cursor $// laxElement "a")
          parenthesized txt = "(" `T.isPrefixOf` txt && ")" `T.isSuffixOf` txt

-- the image data itself is stored in the "src" attribute
getImages :: Cursor -> [T.Text]
getImages tbody = let txts = tbody $// laxElement "img"  &| laxAttribute "src"
                  in map (T.intercalate " ") $ txts

main :: IO ()
main = do
    -- def here is the default ParseSettings
    doc ← readFile (def ∷ ParseSettings) "res/emoji-data-table.xhtml"
    let tbody = (fromDocument doc $// laxElement "tbody") !! 0 -- TODO: unsafe
        rows = tbody $/ laxElement "tr"
        -- better hope they're the same length! and that the traversal order is
        -- deterministic!
        emoji = zipWith4 Emoji (getCodes tbody)
                               (getChars tbody)
                               (getNames tbody)
                               (getImages tbody)
        -- emoji = map emojiFromTR rows
    YML.encodeFile "emoji.yml" emoji
