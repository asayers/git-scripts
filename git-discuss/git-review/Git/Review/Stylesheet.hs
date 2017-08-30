{-# LANGUAGE OverloadedStrings #-}

module Git.Review.Stylesheet
    ( stylesheet
    ) where

import Prelude hiding (div, span)

import Clay
import qualified Clay.Font as F

stylesheet :: Css
stylesheet = do
    div # byClass "topic-title" ? do
        h1 ? marginBottom (px 0)
        a ? float floatRight
        borderBottomStyle solid
        marginBottom (em 1)

    div # byClass "comment" ? do
        borderStyle solid
        borderWidth $ px 1
        margin (em 0.2) (em 0.2) (em 0.2) (em 0.2)

    div # byClass "comment-header" ? do
        display flex
        flexDirection row
        justifyContent spaceBetween
        backgroundColor "#f0f0f0"
        paddingLeft (em 0.3)
        paddingRight (em 0.3)
    div # byClass "comment-body" ?
        padding (em 0.3) (em 0.3) (em 0.3) (em 0.3)
    div # byClass "comment-footer" ? do
        display flex
        flexDirection row
        justifyContent spaceBetween
        backgroundColor "#f0f0f0"
        paddingLeft (em 0.3)
        paddingRight (em 0.3)

    span # byClass "timestamp" ? do
        fontSizeCustom F.small
        before & content (stringContent "[")
        after & content (stringContent "]")

    span # byClass "uuid" ? do
        fontSizeCustom F.xxSmall
        fontFamily [] [monospace]
        fontColor grey
        marginTop auto
        marginBottom auto

    span # byClass "author-name" ?
        marginRight (em 0.3)

    span # byClass "author-email" ? do
        fontSizeCustom F.small
        fontFamily [] [monospace]
        before & content (stringContent "<")
        after & content (stringContent ">")

    div # byClass "comment-body" |> p ?
        margin (em 0.5) (em 0.5) (em 0.5) (em 0.5)

    div # byClass "tree" |> ul ? paddingLeft (px 0)
    div # byClass "tree" ? ul ? listStyleType none

    div # byClass "series" ? do
        fontFamily [] [monospace]
        margin (em 1) (em 1) (em 1) (em 1)
        div <? margin (em 1) (em 0) (em 1) (em 0)

        div # byClass "cover" <? do
            sideBar bisque

        div # byClass "notes" <? do
            pre ? marginBottom (px 0)
            form ? display inline

        div # byClass "diffstat" <? do
            table ? borderCollapse collapse
            td ? padding (px 0) (px 0) (px 0) (px 0)

        div # byClass "shortlog" <? do
            div # byClass "commit-group" ? marginLeft (em 4)
            label # byClass "shortlog" ? do
                margin (px 0) (px 0) (px 0) (px 0)

        div # byClass "diffs" <? do
            div # byClass "diff" ? do
                marginTop (em 1)
                overflow auto
                transition "max-height" (sec 0.3) easeOut (sec 0)
            div # byClass "commit"   ? sideBar lavender
            div # byClass "filediff" ? sideBar lightblue

    star # byClass "boxed" ? do
        padding (em 1) (em 1) (em 1) (em 1)
        margin (px 0) (px 0) (px 0) (px 0)
        borderStyle solid
        borderWidth (px 1)
        maxHeight (em 30)
        overflow auto

    star # byClass "delete-form" ? display inline

    div # byClass "container" ? do
        width (px 980)
        margin (px 0) auto (px 0) auto

    textarea # byClass "new-root-box" ? do
        width (px 974)
        height (em 8)
        fontFamily [] [monospace]
    textarea # byClass "reply-box" ? do
        width (px 800)
        height (em 8)
        fontFamily [] [monospace]
    textarea # byClass "edit-box" ? do
        width (px 800)
        height (em 8)
        fontFamily [] [monospace]

    star # byClass "start-hidden" ? do
        display none
        -- maxHeight (px 0)
        -- overflow hidden
    let tc = input # byClass "toggle-controller"
    tc ? display none
    tc # checked |~ star # byClass "start-hidden" <? do
        display inherit
        -- maxHeight (px 300)
        -- overflow inherit
    tc # checked |~ star # byClass "start-shown" <? display none

    label # byClass "collapser" ? width (em 1)

    star # byClass "link" ? do
        textDecoration underline
        fontColor blue
        cursor pointer
    star # byClass "pre-link" ? do
        cursor pointer
        hover & textDecoration underline

    span # byClass "kw" ? fontColor darkorchid -- diff...
    span # byClass "dt" ? fontColor blue       -- @@ ...
    span # byClass "va" ? fontColor green      -- + ...
    span # byClass "st" ? fontColor red        -- - ...

sideBar :: Color -> Css
sideBar sideColor = do
    paddingLeft (em 1)
    borderLeftStyle solid
    borderLeftWidth (px 12)
    borderLeftColor sideColor
