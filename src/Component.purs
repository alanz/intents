module Component
  ( State
  , Hash(..)
  , Intent(..)
  , IntentArray(..)
  , HoloEntryArray(..)
  , HoloEntry(..)
  , Query(..)
  , ui
  ) where

import Control.Monad.Aff (Aff)
import Control.Monad.Except (ExceptT(..))
import DOM (DOM)
import DOM.Event.Event (Event, preventDefault)
import Data.Argonaut ((.?), (:=), (~>))
import Data.Argonaut as AG
import Data.Bifunctor (lmap)
import Data.Foreign (F, ForeignError(..))
import Data.Generic (class Generic, gShow)
import Data.Maybe (Maybe(..), maybe)
import Data.MediaType.Common (applicationJSON)
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax as AX
import Network.HTTP.Affjax.Response as AXR
import Prelude

-- ---------------------------------------------------------------------


-- The state of everything on the rendered page.  Used in the 'render'
-- function to produce the final page.
type State =
  { loading         :: Boolean
  , intentText      :: String
  , intentType      :: String
  , createdHash     :: Maybe Hash
  , retrievedIntent :: Maybe Intent
  , allIntents      :: HoloEntryArray Intent
  , result          :: Maybe String
  }


-- Actions that can originate from the page.
-- The SetXXX ones relate to fields changing through user interaction,
-- and map the change through to the underlying State.
-- The MakeRequestXXX ones trigger an AJAX call to the holochain backend.
-- PreventDefault is used to lock the page while an AJAX call is running
data Query a
  = SetIntentText  String a
  | SetIntentType  String a
  | SetCreatedHash String a

  | MakeRequestCreate a
  | MakeRequestRetrieve a
  | MakeRequestGetAll a
  | PreventDefault Event (Query a)

-- ---------------------------------------------------------------------

-- Provide a description of the application, so that the event loop can run the application
ui :: forall eff. H.Component HH.HTML Query Unit Void (Aff (dom :: DOM, ajax :: AX.AJAX | eff))
ui =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = { loading: false
                 , intentText: "This text will be saved in Holochain"
                 , intentType: "Offer"
                 , createdHash: Nothing
                 , retrievedIntent: Nothing
                 , allIntents: HoloEntryArray []
                 , result: Nothing }

  render :: State -> H.ComponentHTML Query
  render st =
    HH.div_
      [
      HH.form
        [ HE.onSubmit (HE.input $ \e a -> PreventDefault e (MakeRequestCreate a))]
        [ HH.h3_ [ HH.text "Post an intent" ]

        , HH.p_ [ HH.text "This will be stored in Holochain and the hash key for the new intent will be shown" ]
        , HH.p_ [ HH.text "in the Hash text box below." ]
        , HH.label_
            [ HH.div_ [ HH.text "Intent:" ]
            , HH.input
                [ HP.value st.intentText
                , HP.attr (HH.AttrName "size") "50"
                , HE.onValueInput (HE.input SetIntentText)
                ]
            , HH.input
                [ HP.value st.intentType
                , HP.attr (HH.AttrName "size") "50"
                , HE.onValueInput (HE.input SetIntentType)
                ]
            ]
        , HH.p_ []
        , HH.input
            [ HP.type_ HP.InputSubmit
            , HP.value "Submit"
            , HP.disabled st.loading
            ]
        , HH.p_
            [ HH.text (if st.loading then "Working..." else "") ]
        ]
  -- -----------------------------------
    , HH.form
        [ HE.onSubmit (HE.input $ \e a -> PreventDefault e (MakeRequestRetrieve a))]
        [ HH.h3_ [ HH.text "Retrieve an intent" ]

        , HH.p_ [ HH.text "Press the Read button and the hash key will be used to retrieve the intent" ]
        , HH.label_
            [ HH.div_ [ HH.text "Hash:" ]
            , HH.input
                [ HP.value (maybe "" getHash st.createdHash)
                , HP.attr (HH.AttrName "size") "50"
                , HE.onValueInput (HE.input SetCreatedHash)
                ]
            ]
        , HH.p_ []
        , HH.input
            [ HP.type_ HP.InputSubmit
            , HP.value "Read"
            , HP.disabled st.loading
            ]
        , HH.p_
            [ HH.text (if st.loading then "Working..." else "") ]
        , HH.div_
            case st.retrievedIntent of
              Nothing -> []
              Just res ->
                [ HH.h3_
                    [ HH.text "Read:" ]
                , HH.pre_
                    [ HH.code_ [ HH.text (show res) ] ]
                ]
        , HH.div_
            case st.result of
              Nothing -> []
              Just res ->
                [ HH.h3_
                    [ HH.text "Response:" ]
                , HH.pre_
                    [ HH.code_ [ HH.text res ] ]
                ]
        ]
      -- onClick  :: forall r i. (MouseEvent -> Maybe i) -> IProp (onClick :: MouseEvent | r) i
      -- onSubmit :: forall r i. (Event      -> Maybe i) -> IProp (onSubmit :: Event | r)     i
      , HH.button
        [ HE.onClick (HE.input_ MakeRequestGetAll) ]
        [ HH.text "get all intents" ]
      , HH.div_
        [ HH.text "All intents"
        , HH.table_ (map renderHoloEntryIntent (getHoloEntries st.allIntents))
          ]
      ]

  renderHoloEntryIntent :: forall a b. HoloEntry Intent -> HH.HTML a b
  renderHoloEntryIntent (HoloEntry { entry:he }) = renderIntent he

  renderIntent :: forall a b. Intent -> HH.HTML a b
  renderIntent (Intent { content:c, typ:t, timestamp:ts })
    = HH.tr_ [HH.td_ [HH.text c], HH.td_ [HH.text t], HH.td_ [HH.text ts]   ]

  eval :: Query ~> H.ComponentDSL State Query Void (Aff (dom :: DOM, ajax :: AX.AJAX | eff))
  eval = case _ of
    SetIntentText text next -> do
      H.modify (_ { intentText = text, result = Nothing :: Maybe String })
      pure next
    SetIntentType typ next -> do
      H.modify (_ { intentType = typ, result = Nothing :: Maybe String })
      pure next
    SetCreatedHash hash next -> do
      H.modify (_ { createdHash = Just (Hash hash), result = Nothing :: Maybe String })
      pure next
    MakeRequestCreate next -> do
      st <- H.get
      H.modify (_ { loading = true })
      let intent = Intent
                     { content: st.intentText
                     , typ:     st.intentType
                     , timestamp: "1525534245"
                     }
      response <- H.liftAff (intentCreate intent)
      -- response <- H.liftAff getAllIntents
      H.modify (_ { loading = false
                  , createdHash = Just response.response
                  , result      = Just (getHash response.response)
                  })
      pure next
    MakeRequestRetrieve next -> do
      mhash <- H.gets _.createdHash
      case mhash of
        Just hash -> do
          H.modify (_ { loading = true })
          response <- H.liftAff (intentRead hash)
          H.modify (_ { loading = false
                      , retrievedIntent = Just response.response
                      , result          = Just ("got hash result:" <> show response.status)
                      })
          pure next
        Nothing -> do
          H.modify (_ { loading = false
                      , result          = Just "no hash value"
                      })
          pure next

    MakeRequestGetAll next -> do
      response <- H.liftAff getAllIntents
      H.modify (_ { loading = false
                  , allIntents = response.response
                  , result     = Just ("getAllIntents result:" <> show response.status)
                  })
      pure next

    PreventDefault e query -> do
      H.liftEff $ preventDefault e
      eval query

-- ---------------------------------------------------------------------

-- From http://codingstruggles.com/purescript/post-request-with-purescript-affjax-and-argonaut.html
data Hash = Hash String

getHash :: Hash -> String
getHash (Hash s) = s

derive instance genericHash :: Generic Hash

instance showHash :: Show Hash where
    show = gShow

instance decodeJsonHash :: AG.DecodeJson Hash where
  decodeJson json = do
    obj <- AG.decodeJson json
    pure $ Hash obj

instance encodeJsonHash :: AG.EncodeJson Hash where
  encodeJson (Hash hash) = AG.encodeJson hash

-- -------------------------------------

-- A Purescript representation of an 'Intent' as described in the DNA
data Intent = Intent
                { content    :: String
                , typ       :: String
                , timestamp :: String
                }

derive instance genericIntent :: Generic Intent

instance showIntent :: Show Intent where
    show = gShow

instance decodeJsonIntent :: AG.DecodeJson Intent where
  decodeJson json = do
    obj <- AG.decodeJson json
    content   <- obj .? "content"
    typ       <- obj .? "type"
    timestamp <- obj .? "timestamp"
    pure $ Intent { content, typ, timestamp }

instance encodeJsonIntent :: AG.EncodeJson Intent where
  encodeJson (Intent intent)
    =  "content"   := intent.content
    ~> "type"      := intent.typ
    ~> "timestamp" := intent.timestamp
    ~> AG.jsonEmptyObject

-- ---------------------------------------------------------------------

data IntentArray = IntentArray (Array Intent)

getIntents :: IntentArray -> (Array Intent)
getIntents (IntentArray arr) = arr

derive instance genericIntentArray :: Generic IntentArray

instance showIntentArray :: Show IntentArray where
    show = gShow

instance decodeJsonIntentArray :: AG.DecodeJson IntentArray where
  decodeJson json = do
    obj <- AG.decodeJson json
    pure $ IntentArray obj

instance encodeJsonIntentArray :: AG.EncodeJson IntentArray where
  encodeJson (IntentArray arr) = AG.encodeJson arr

-- ---------------------------------------------------------------------

data HoloEntryArray a = HoloEntryArray (Array (HoloEntry a))

getHoloEntries :: forall a. HoloEntryArray a -> (Array (HoloEntry a))
getHoloEntries (HoloEntryArray arr) = arr

derive instance genericHoloEntryArray :: Generic a => Generic (HoloEntryArray a)

instance showHoloEntryArray :: (Show a, Generic a) => Show (HoloEntryArray a) where
    show = gShow

instance decodeJsonHoloEntryArray :: AG.DecodeJson (HoloEntry a) => AG.DecodeJson (HoloEntryArray a) where
  decodeJson json = do
    obj <- AG.decodeJson json
    pure $ HoloEntryArray obj

instance encodeJsonHoloEntryArray :: (AG.EncodeJson (HoloEntry a)) => AG.EncodeJson (HoloEntryArray a) where
  encodeJson (HoloEntryArray arr) = AG.encodeJson arr

-- ---------------------------------------------------------------------

{-

[{"Entry":{"content":"This text will be saved in Holochain","timestamp":"2018-05-06T18:15:33.125Z","type":"Offer"}
 ,"EntryType":"intent"
 ,"Hash":"QmbtZRp6ibckQLJDGcp4umwL6K4iAPbbbi4n6QEBk37h8L"
 ,"Source":"QmS85jqpzwsUaAnwjowbhkXvA9EfZAD6L3ToP85qw8S2BG"}
]
-}

data HoloEntry a
  = HoloEntry
     { entry :: a
     , entryType :: String
     , entryHash :: Hash
     , entrySource :: Hash
     }

derive instance genericHoloEntry :: Generic a => Generic (HoloEntry a)

instance decodeJsonHoloEntry :: AG.DecodeJson a => AG.DecodeJson (HoloEntry a) where
  decodeJson json = do
    obj <- AG.decodeJson json
    e    <- obj .? "Entry"
    typ  <- obj .? "EntryType"
    hash <- obj .? "Hash"
    src  <- obj .? "Source"
    pure $ HoloEntry { entry:e, entryType:typ, entryHash:hash, entrySource:src }

instance encodeJsonHoloEntry :: AG.EncodeJson a => AG.EncodeJson (HoloEntry a) where
  encodeJson (HoloEntry {entry:e, entryType:typ, entryHash:hash, entrySource:src})
    =  "Entry"     := e
    ~> "EntryType" := typ
    ~> "Hash"      := hash
    ~> "Source"    := src
    ~> AG.jsonEmptyObject

-- ---------------------------------------------------------------------

instance responsableJsonIntent :: AXR.Respondable Intent where
  responseType = Tuple (Just applicationJSON) AXR.JSONResponse
  fromResponse = decodeJsonResponse <=< AXR.fromResponse

instance responsableJsonHash :: AXR.Respondable Hash where
  responseType = Tuple (Just applicationJSON) AXR.JSONResponse
  fromResponse = decodeJsonResponse <=< AXR.fromResponse

instance responsableJsonHoloEntry :: AG.DecodeJson (HoloEntry a) => AXR.Respondable (HoloEntry a) where
  responseType = Tuple (Just applicationJSON) AXR.JSONResponse
  fromResponse = decodeJsonResponse <=< AXR.fromResponse

instance responsableJsonHoloEntryArray :: AG.DecodeJson (HoloEntry a) => AXR.Respondable (HoloEntryArray a) where
  responseType = Tuple (Just applicationJSON) AXR.JSONResponse
  fromResponse = decodeJsonResponse <=< AXR.fromResponse

-- ---------------------------------------------------------------------
-- See
-- https://stackoverflow.com/questions/42927827/purescript-reuse-argonaut-json-decoding-for-affjax-respondeable
-- It appears to be *way* more complicated than is should be
decodeJsonResponse :: forall a. AG.DecodeJson a => AG.Json -> F a
decodeJsonResponse =
  ExceptT <<< pure <<< lmap (pure <<< ForeignError) <<< AG.decodeJson

-- ---------------------------------------------------------------------

-- The DNA intentCreate call
intentCreate :: forall e . Intent -> AX.Affjax e Hash
intentCreate intent = do
  AX.post "/fn/Intents/intentCreate" (AG.encodeJson intent)

-- The DNA intentRead call
intentRead :: forall e . Hash -> AX.Affjax e Intent
intentRead (Hash hash) = AX.post "/fn/Intents/intentRead" (AG.encodeJson hash)

-- The DNA getAllIntents call
getAllIntents :: forall e . AX.Affjax e (HoloEntryArray Intent)
getAllIntents = AX.post "/fn/Intents/getAllIntents" ""

-- ---------------------------------------------------------------------
