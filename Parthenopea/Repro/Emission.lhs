> {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
> {-# HLINT ignore "Unused LANGUAGE pragma" #-}
>
> {-# LANGUAGE RecordWildCards #-}
> {-# LANGUAGE UnicodeSyntax #-}

Emission
William Clements
November 9, 2023

> module Parthenopea.Repro.Emission
>        (  bracks
>         , comma
>         , commaOrNot
>         , Emission(..)
>         , emitComment
>         , emitDefault
>         , emitLine
>         , emitNextComment
>         , emitShowL
>         , emitShowR
>         , fillFieldL
>         , fillFieldR
>         , gmId
>         , parens
>         , reapEmissions
>         )
>         where
>
> import Data.List ( singleton )

Emission capability ===================================================================================================

> data Emission                            = 
>   ToFieldL String Int
>   | ToFieldR String Int
>   | Unblocked String
>   | Blanks Int
>   | Empty 
>   | EndOfLine deriving Show
>
> makeString             :: Emission → String
> makeString em                            =
>   case em of
>     ToFieldL str sz    → if len > sz then error $ unwords ["overflowL", show sz, show len, show str]
>                                      else fillFieldL sz str
>       where
>         len                              = length str
>     ToFieldR str sz    → if len > sz then error $ unwords ["overflowR", show sz, show len, show str]
>                                      else fillFieldR sz str
>       where
>         len                              = length str
>     Unblocked str      → str
>     Blanks sz          → replicate sz ' '
>     Empty              → ""
>     EndOfLine          → "\n"
>
> emitLine               :: [Emission] → [Emission]
> emitLine ex                              = singleton literate ++ ex ++ singleton EndOfLine
>
> commaOrNot             :: Int → Emission
> commaOrNot nth                           =
>   if nth == 0
>     then ToFieldL ""  2
>     else ToFieldL "," 2
>
> parens                 :: [Emission] → [Emission]
> parens ex                                = [Unblocked "("] ++ ex ++ [Unblocked ")"]
>
> bracks                 :: [Emission] → [Emission]
> bracks ex                                = [Unblocked "["] ++ ex ++ [Unblocked "]"]
>
> comma, literate        :: Emission
> comma                                    = Unblocked ", "
> literate                                 = ToFieldL ">" 2
>
> emitComment            :: [Emission] → [Emission]
> emitComment ex                           = [EndOfLine] ++ ex ++ [EndOfLine, EndOfLine]
>
> emitNextComment        :: [Emission] → [Emission]
> emitNextComment ex                       = ex ++ [EndOfLine, EndOfLine]
>
> emitShowL              :: (Show a) ⇒ a → Int → Emission
> emitShowL item                           = ToFieldL (show item)
>
> emitShowR              :: (Show a) ⇒ a → Int → Emission
> emitShowR item                           = ToFieldR (show item)
>
> emitDefault            :: (Show a) ⇒ a → Emission
> emitDefault item                         = Unblocked (show item)
>
> gmId                   :: (Show a) ⇒ a → Emission
> gmId i                                   = emitShowL i 22
>
> reapEmissions          :: [Emission] → String
> reapEmissions                            = concatMap makeString
>
> fillFieldL             :: Int → String → String
> fillFieldL fieldSz str                   = str ++ safeReplicate (length str) fieldSz ' '
>
> fillFieldR             :: Int → String → String
> fillFieldR fieldSz str                   = safeReplicate (length str) fieldSz ' ' ++ str
>
> safeReplicate          :: Int → Int → Char → String
> safeReplicate sz maxSz                   = replicate (maxSz - sz)
