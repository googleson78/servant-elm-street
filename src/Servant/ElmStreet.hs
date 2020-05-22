{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Servant.ElmStreet
  ( LangElm
  , generateElm, BaseUrl(..)
  ) where

import Control.Arrow (Arrow((&&&)))
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Proxy (Proxy(..))
import Data.String (IsString(fromString))
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (dquotes, Pretty, viaShow, rbrace, lbrace, encloseSep, list, nest, rparen, comma, lparen, equals, emptyDoc, vsep, parens, hsep, pretty, (<+>), line, colon, Doc)
import Elm.Ast (isEnum, ElmConstructor(..), definitionToRef, TypeName(..), TypeRef(..), ElmAlias(..), ElmType(..), ElmPrim(..), ElmDefinition(..))
import Elm.Generate (Settings(..))
import Elm.Generic (Elm(..))
import Elm.Print.Common (showDoc, qualifiedTypeWithVarsDoc, wrapParens)
import Lens.Micro (each, (^..), (^.))
import Lens.Micro (to)
import Servant.Foreign
import System.FilePath ((<.>), (</>))
import Text.Casing (toCamel, toPascal, fromKebab)
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.IO as Text


data LangElm

instance Elm a => HasForeignType LangElm ElmDefinition a where
  typeFor _ _ proxyA = toElmDefinition proxyA

newtype BaseUrl = BaseUrl {getBaseUrl :: Text}
  deriving newtype Pretty

generateRequests :: BaseUrl -> Req ElmDefinition -> Doc ann
generateRequests baseUrl req =
  typeDef req <>
  line <>
  funDef baseUrl req

-- N.B.: order of args here must match order of args in 'funDef'
typeDef :: Req ElmDefinition -> Doc ann
typeDef req =
  hsep
    [ funName $ req ^. reqFuncName
    , colon
    , arrSep $ concat
        [ req ^.. reqUrl . path . each . to unSegment . _Cap . argType . to defName -- TODO: think about using a record here
        , req ^.. reqUrl . queryStr . each . queryArgName . argType . to defName -- TODO: think about using a record here
        , req ^.. reqHeaders . each . headerArg . argType . to defName -- TODO: think about using a record here
        , [defName reqBody' | Just reqBody' <- [req ^. reqBody]]
        , [parens $ hsep
            [ "Result"
            , "Http.Error"
            , assertReturnType req defName
            , arrow
            , "msg"
            ]
          ]
        , ["Cmd msg"]
        ]
    ]
    where
      defName :: ElmDefinition -> Doc ann
      defName = \case
        DefAlias (ElmAlias {elmAliasName}) -> pretty elmAliasName
        DefType (ElmType {elmTypeName}) -> pretty elmTypeName
        DefPrim prim -> elmPrimDoc prim

funDef :: BaseUrl -> Req ElmDefinition -> Doc ann
funDef baseUrl req =
  hsep $ concat
  [ [funName $ req ^. reqFuncName]
  , captureNames req
  , map fst $ queryNames req
  , map fst $ headerNames req
  , [body | Just _ <- [req ^. reqBody]]
  , [expectMsg]
  , [equals]
  , [nest 2 $ line <> funBody baseUrl req]
  ]

funBody :: BaseUrl -> Req ElmDefinition -> Doc ann
funBody baseUrl req =
  nest 2 $ vsep $
    [ "Http.request"
    , encloseSep lbrace rbrace comma $
        [ "method" <+> equals <+> viaShow (req ^. reqMethod)
        , "headers" <+>
          equals <+>
          "List.filterMap" <+>
          "identity" <+>
          nest 2 (list $ map applyHeaderifier $ headerNames req)
        , "url" <+> equals <+> crossOrigin baseUrl (req ^. reqUrl . path) (queryNames req)
        , body <+> equals <+> encodeBody (req ^. reqBody)
        , "expect" <+> equals <+> "Http.expectJson" <+> expectMsg <+> assertReturnType req (parens . typeRefDecoder . definitionToRef)
        , "timeout" <+> equals <+> nothing
        , "tracker" <+> equals <+> nothing
        ]
    ]
  where
    applyHeaderifier :: (Doc ann, HeaderArg ElmDefinition) -> Doc ann
    applyHeaderifier (valueName, queryArg) =
      hsep
        [ defHeaderify (queryArg ^. headerArg . argType)
        , dquotes (queryArg ^. headerArg . argName . to unPathSegment . to pretty)
        , valueName
        ]
    encodeBody :: Maybe ElmDefinition -> Doc ann
    encodeBody Nothing = "Http.emptyBody"
    encodeBody (Just toEncode) = "Http.jsonBody <|" <+> parens (typeRefEncoder (definitionToRef toEncode) <+> body)

crossOrigin :: BaseUrl -> [Segment ElmDefinition] -> [(Doc ann, QueryArg ElmDefinition)] -> Doc ann
crossOrigin baseUrl segments queryParams =
  hsep
    [ "Url.crossOrigin"
    , dquotes $ pretty baseUrl
    , nest 2 $ list $ map segmentToString segments
    , "<|"
    , "List.filterMap"
    , "identity"
    ] <+>
    nest 2 (list (map applyQuerifier queryParams))
  where
    applyQuerifier :: (Doc ann, QueryArg ElmDefinition) -> Doc ann
    applyQuerifier (valueName, queryArg) =
      hsep
        [ defQuerify (queryArg ^. queryArgName . argType)
        , dquotes (queryArg ^. queryArgName . argName . to unPathSegment . to pretty)
        , valueName
        ]

segmentToString :: Segment ElmDefinition -> Doc ann
segmentToString (Segment seg) =
  case seg of
    Static (PathSegment pathSegment) -> dquotes $ pretty pathSegment
    Cap arg -> defCapturify (arg ^. argType) <+> nameCapture arg

-- We don't fetch ElmDefinitions because it's hard to reuse them.
-- The reason is we only want capture arguments here, and not static path segments,
-- whereas when building the url we need to keep the order of static and capture arguments.
captureNames :: Req ElmDefinition -> [Doc ann]
captureNames req = req ^.. reqUrl . path . each . to unSegment . _Cap . to nameCapture

nameCapture :: Arg ElmDefinition -> Doc ann
nameCapture = ("capture" <>) . pascaliseSegment . _argName

queryNames :: Req ElmDefinition -> [(Doc ann, QueryArg ElmDefinition)]
queryNames req =
  map
    do (("query" <>) . pascaliseSegment . _argName . _queryArgName) &&& id
    do req ^.. reqUrl . queryStr . each

headerNames :: Req ElmDefinition -> [(Doc ann, HeaderArg ElmDefinition)]
headerNames req = map ((("header" <>) . pascaliseSegment . _argName . _headerArg) &&& id) $ req ^.. reqHeaders . each

-- headers and query params are often kebab-cased, and elm doesn't allow for -s in names
pascaliseSegment :: PathSegment -> Doc ann
pascaliseSegment = fromString . toPascal . fromKebab . Text.unpack . unPathSegment

-- segments are often kebab-cased, and elm doesn't allow for -s in names
funName :: FunctionName -> Doc ann
funName = pretty . Text.concat . capAllButFirst . map (Text.pack . toCamel . fromKebab . Text.unpack) . unFunctionName
  where
    capAllButFirst :: [Text] -> [Text]
    capAllButFirst [] = error "Somehow servant gave me an empty FunctionName!"
    capAllButFirst (x:xs) = x : map Text.toTitle xs

assertReturnType :: Req ElmDefinition -> (ElmDefinition -> a) -> a
assertReturnType req f = maybe (error "Servant gave me no return type!") f $ req ^. reqReturnType

nothing :: Doc ann
nothing = "Nothing"

expectMsg :: Doc ann
expectMsg = "expectMsg"

body :: Doc ann
body = "body"

arrSep :: [Doc ann] -> Doc ann
arrSep xs =
  hsep $ List.intersperse arrow xs

arrow :: Doc ann
arrow = "->"

-- TODO: stolen from elm-street
elmTypeRefDoc :: TypeRef -> Doc ann
elmTypeRefDoc = \case
  RefPrim elmPrim -> elmPrimDoc elmPrim
  RefCustom (TypeName typeName) -> pretty typeName

elmTypeParenDoc :: TypeRef -> Doc ann
elmTypeParenDoc = wrapParens . elmTypeRefDoc

elmPrimDoc :: ElmPrim -> Doc ann
elmPrimDoc = \case
  ElmUnit         -> "()"
  ElmNever        -> "Never"
  ElmBool         -> "Bool"
  ElmChar         -> "Char"
  ElmInt          -> "Int"
  ElmFloat        -> "Float"
  ElmString       -> "String"
  ElmTime         -> "Posix"
  ElmMaybe t      -> "Maybe" <+> elmTypeParenDoc t
  ElmResult l r   -> "Result" <+> elmTypeParenDoc l <+> elmTypeParenDoc r
  ElmPair a b     -> lparen <> elmTypeRefDoc a <> comma <+> elmTypeRefDoc b <> rparen
  ElmTriple a b c -> lparen <> elmTypeRefDoc a <> comma <+> elmTypeRefDoc b <> comma <+> elmTypeRefDoc c <> rparen
  ElmList l       -> "List" <+> elmTypeParenDoc l

typeRefDecoder :: TypeRef -> Doc ann
typeRefDecoder (RefCustom TypeName{unTypeName}) = "decode" <> pretty (Text.takeWhile (/= ' ') unTypeName)
typeRefDecoder (RefPrim elmPrim) = case elmPrim of
  ElmUnit         -> "D.map (always ()) (D.list D.string)"
  ElmNever        -> "D.fail \"Never is not possible\""
  ElmBool         -> "D.bool"
  ElmChar         -> "elmStreetDecodeChar"
  ElmInt          -> "D.int"
  ElmFloat        -> "D.float"
  ElmString       -> "D.string"
  ElmTime         -> "Iso.decoder"
  ElmMaybe t      -> "nullable"
    <+> wrapParens (typeRefDecoder t)
  ElmResult l r   -> "elmStreetDecodeEither"
    <+> wrapParens (typeRefDecoder l)
    <+> wrapParens (typeRefDecoder r)
  ElmPair a b     -> "elmStreetDecodePair"
    <+> wrapParens (typeRefDecoder a)
    <+> wrapParens (typeRefDecoder b)
  ElmTriple a b c -> "elmStreetDecodeTriple"
    <+> wrapParens (typeRefDecoder a)
    <+> wrapParens (typeRefDecoder b)
    <+> wrapParens (typeRefDecoder c)
  ElmList l       -> "D.list" <+> wrapParens (typeRefDecoder l)

typeRefEncoder :: TypeRef -> Doc ann
typeRefEncoder (RefCustom TypeName{unTypeName}) = "encode" <> pretty (Text.takeWhile (/= ' ') unTypeName)
typeRefEncoder (RefPrim elmPrim) = case elmPrim of
    ElmUnit         -> "always <| E.list identity []"
    ElmNever        -> "never"
    ElmBool         -> "E.bool"
    ElmChar         -> "E.string << String.fromChar"
    ElmInt          -> "E.int"
    ElmFloat        -> "E.float"
    ElmString       -> "E.string"
    ElmTime         -> "Iso.encode"
    ElmMaybe t      -> "elmStreetEncodeMaybe"
        <+> wrapParens (typeRefEncoder t)
    ElmResult l r   -> "elmStreetEncodeEither"
        <+> wrapParens (typeRefEncoder l)
        <+> wrapParens (typeRefEncoder r)
    ElmPair a b     -> "elmStreetEncodePair"
        <+> wrapParens (typeRefEncoder a)
        <+> wrapParens (typeRefEncoder b)
    ElmTriple a b c -> "elmStreetEncodeTriple"
        <+> wrapParens (typeRefEncoder a)
        <+> wrapParens (typeRefEncoder b)
        <+> wrapParens (typeRefEncoder c)
    ElmList l       -> "E.list" <+> wrapParens (typeRefEncoder l)

defCapturify :: ElmDefinition -> Doc ann
defCapturify = typeRefStringifier . definitionToRef

defHeaderify :: ElmDefinition -> Doc ann
defHeaderify = typeHeaderifier . definitionToRef

defQuerify :: ElmDefinition -> Doc ann
defQuerify = typeQuerifier . definitionToRef

typeHeaderifier :: TypeRef -> Doc ann
typeHeaderifier = \case
  RefPrim elmPrim -> primHeaderifier elmPrim
  RefCustom typeName -> "pureHeaderifier" <+> stringifierName typeName

primHeaderifier :: ElmPrim -> Doc ann
primHeaderifier = \case
  ElmInt -> "intHeader"
  ElmString -> "((<<) Just) << Http.header"
  ElmNever -> "never"
  ElmMaybe ref -> "overMaybe" <+> wrapParens (typeHeaderifier ref)
  ElmChar -> error "Using a Char as a header is not supported!" -- TODO: decide how to show/read bools
  ElmBool -> error "Using a Bool as a header is not supported!" -- TODO: decide how to show/read bools
  ElmFloat -> error "Using a Float as a header is not supported!"
  ElmTime -> error "Using a Time as a header is not supported!"
  ElmUnit -> error "Using a () as a header is not supported!"
  ElmResult _ _ -> error "Using an Either as a header is not supported!"
  ElmPair _ _ -> error "Using a 2-tuple as a header is not supported!"
  ElmTriple _ _ _ -> error "Using a 3-tuple as a header is not supported!"
  ElmList _ -> error "Using a list as a header is not supported!"

pureHeaderifier :: Doc ann
pureHeaderifier =
  vsep
    [ "pureHeaderifier : (a -> String) -> String -> a -> Maybe Http.Header"
    , "pureHeaderifier f name x = Just <| Http.header name (f x)"
    ]

intHeader :: Doc ann
intHeader =
  vsep
    [ "intHeader : String -> Int -> Maybe Http.Header"
    , "intHeader name = Just << Http.header name << String.fromInt"
    ]

typeQuerifier :: TypeRef -> Doc ann
typeQuerifier = \case
  RefPrim elmPrim -> primQuerifier elmPrim
  RefCustom typeName -> "pureQuerifier" <+> stringifierName typeName

primQuerifier :: ElmPrim -> Doc ann
primQuerifier = \case
  ElmInt -> "((<<) Just) << Url.int"
  ElmString -> "((<<) Just) << Url.string"
  ElmNever -> "never"
  ElmMaybe ref -> "overMaybe" <+> wrapParens (typeQuerifier ref)
  ElmChar -> error "Using a Char as a query param/header is not supported!"
  ElmBool -> error "Using a Bool as a query param/header is not supported!"
  ElmFloat -> error "Using a Float as a query param/header is not supported!"
  ElmTime -> error "Using a Time as a query param/header is not supported!"
  ElmUnit -> error "Using a () as a query param/header is not supported!"
  ElmResult _ _ -> error "Using an Either as a query param/header is not supported!"
  ElmPair _ _ -> error "Using a 2-tuple as a query param/header is not supported!"
  ElmTriple _ _ _ -> error "Using a 3-tuple as a query param/header is not supported!"
  ElmList _ -> error "Using a list as a query param/header is not supported!"

pureQuerifier :: Doc ann
pureQuerifier =
  vsep
    [ "pureQuerifier : (a -> String) -> String -> a -> Maybe Url.QueryParameter"
    , "pureQuerifier f name x = Just <| Url.string name (f x)"
    ]

overMaybe :: Doc ann
overMaybe =
  vsep
    [ "overMaybe : (String -> a -> Maybe b) -> String -> Maybe a -> Maybe b"
    , "overMaybe f name = Maybe.andThen (f name)"
    ]

prettyShowStringifier :: ElmDefinition -> Text
prettyShowStringifier def =
  showDoc case def of
    DefAlias elmAlias -> "Using a type alias \"" <> pretty (show elmAlias) <> "\" as a query param/header is not supported!"
    DefType elmType   -> typeStringifier elmType
    DefPrim _         -> emptyDoc

-- Generate a way to stringify the given ElmType
typeStringifier :: ElmType -> Doc ann
typeStringifier ty@ElmType{..} =
  stringifierDef ty <>
  line <>
  stringifierName (TypeName elmTypeName) <+> equals <+>
  if isEnum ty
  then enumStringifier
  else
    if elmTypeIsNewtype
    then newtypeStringifier
    else error "Using a sum type with fields as a query param/header is not supported!"
  where
    enumStringifier :: Doc ann
    enumStringifier = "T.show" <> pretty elmTypeName

    newtypeStringifier :: Doc ann
    newtypeStringifier =
        fieldStringifier <+> "<< T.un" <> pretty elmTypeName
      where
        fieldStringifier :: Doc ann
        fieldStringifier =
          case elmTypeConstructors of
            ElmConstructor _ []:|[] -> error $ Text.unpack elmTypeName <> " is flagged as a newtype, but has no fields!"
            ElmConstructor _ [f]:|[] -> wrapParens (typeRefStringifier f)
            ElmConstructor _ _:|[] -> error $ Text.unpack elmTypeName <> " is flagged as a newtype, buth has more than one field!"
            _ -> error $ Text.unpack elmTypeName <> " is flagged as a newtype, buth has more than one constructor!"

stringifierDef :: ElmType -> Doc ann
stringifierDef ElmType {elmTypeName, elmTypeVars} =
  hsep
    [ stringifierName (TypeName elmTypeName)
    , colon
    , wrapParens (qualifiedTypeWithVarsDoc elmTypeName elmTypeVars)
    , "->"
    , "String"
    ]

-- | Create the name of the stringifier function.
stringifierName :: TypeName -> Doc ann
stringifierName TypeName {unTypeName} = "stringify" <> pretty unTypeName

-- | Converts the reference to the existing type to the corresponding stringifier.
typeRefStringifier :: TypeRef -> Doc ann
typeRefStringifier (RefCustom TypeName{..}) = "stringify" <> pretty (Text.takeWhile (/= ' ') unTypeName)
typeRefStringifier (RefPrim elmPrim) = case elmPrim of
  ElmInt -> "String.fromInt"
  ElmString -> "identity"
  ElmNever -> "never"
  ElmMaybe _ -> error "Using a Maybe \"for stringification\" (as a base for a query parameter/capture/header/) is not supported!"
  ElmChar -> error "Using a Char \"for stringification\" (as a base for a query parameter/capture/header/) is not supported!"
  ElmBool -> error "Using a Bool \"for stringification\" (as a base for a query parameter/capture/header/) is not supported!"
  ElmFloat -> error "Using a Float \"for stringification\" (as a base for a query parameter/capture/header/) is not supported!"
  ElmTime -> error "Using a Time \"for stringification\" (as a base for a query parameter/capture/header/) is not supported!"
  ElmUnit -> error "Using a () \"for stringification\" (as a base for a query parameter/capture/header/) is not supported!"
  ElmResult _ _ -> error "Using an Either \"for stringification\" (as a base for a query parameter/capture/header/) is not supported!"
  ElmPair _ _ -> error "Using a 2-tuple \"for stringification\" (as a base for a query parameter/capture/header/) is not supported!"
  ElmTriple _ _ _ -> error "Using a 3-tuple \"for stringification\" (as a base for a query parameter/capture/header/) is not supported!"
  ElmList _ -> error "Using a list \"for stringification\" (as a base for a query parameter/capture/header/) is not supported!"

class RenderStringifier (types :: [Type]) where
  renderStringifier :: [Text]

instance RenderStringifier '[] where
  renderStringifier = []

instance (Elm t, RenderStringifier ts) => RenderStringifier (t ': ts) where
  renderStringifier = "" : prettyShowStringifier (toElmDefinition $ Proxy @t) : renderStringifier @ts

-- TODO: mostly copy pasted from elm-street, try to reuse somehow
generateElm :: forall (ts :: [Type]) . RenderStringifier ts => BaseUrl -> [Req ElmDefinition] -> Settings -> IO ()
generateElm baseUrl reqs Settings{..} = do
    writeElm "Stringifier" $ stringifierHeader : renderStringifier @ts
    writeElm "Servant" $ servantHeader : map (showDoc . generateRequests baseUrl) reqs

    writeElm "ServantElmStreet" servantElmStreetDefinitions
  where
    moduleDir, fullPath :: FilePath
    moduleDir = foldr (</>) "" settingsModule
    fullPath  = settingsDirectory </> moduleDir

    writeElm :: FilePath -> [Text] -> IO ()
    writeElm file defs = Text.writeFile (fullPath </> file <.> "elm") (Text.unlines defs)

    joinModule :: [String] -> Text
    joinModule = Text.pack . List.intercalate "."

    typesModule, encoderModule, decoderModule, streetModule, servantModule, servantElmStreetModule, stringifierModule :: Text -- TODO: expose typesModule from elm-street?
    typesModule   = joinModule $ settingsModule ++ [settingsTypesFile]
    encoderModule = joinModule $ settingsModule ++ [settingsEncoderFile]
    decoderModule = joinModule $ settingsModule ++ [settingsDecoderFile]
    streetModule  = joinModule $ settingsModule ++ ["ElmStreet"]

    servantElmStreetModule  = joinModule $ settingsModule ++ ["ServantElmStreet"] -- TODO: somehow merge this with the other utils?
    servantModule = joinModule $ settingsModule ++ ["Servant"] -- TODO: not hardcoded?
    stringifierModule = joinModule $ settingsModule ++ ["Stringifier"] -- TODO: not hardcoded?

    servantHeader :: Text
    servantHeader = Text.unlines
      [ "module " <> servantModule <> " exposing (..)"
      , ""
      , "import String"
      , "import List"
      , "import Http"
      , "import Url.Builder as Url"
      , "import Json.Encode as E exposing (..)"
      , "import Json.Decode as D exposing (..)"
      , "import Json.Decode.Pipeline as D exposing (required)"
      , "import Json.Decode.Pipeline as D exposing (required)"
      , "import " <> streetModule <> " exposing (..)"
      , "import " <> servantElmStreetModule <> " exposing (..)"
      , "import " <> stringifierModule <> " exposing (..)"
      , "import " <> encoderModule <> " exposing (..)"
      , "import " <> decoderModule <> " exposing (..)"
      ]

    stringifierHeader :: Text
    stringifierHeader = Text.unlines
      [ "module " <> stringifierModule <> " exposing (..)"
      , ""
      , "import String"
      , "import " <> typesModule <> " as T"
      ]

    servantElmStreetDefinitions :: [Text]
    servantElmStreetDefinitions =
      [ "module " <> servantElmStreetModule <> " exposing (..)"
      , ""
      , "import Http"
      , "import String"
      , "import Maybe"
      , "import Url.Builder as Url"
      , ""
      , ""
      , showDoc pureHeaderifier
      , showDoc pureQuerifier
      , showDoc overMaybe
      , showDoc intHeader
      ]
