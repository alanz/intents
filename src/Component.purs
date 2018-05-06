module Component (State, Hash(..), Intent(..), Query(..), ui) where

import Control.Monad.Aff (Aff)
import Control.Monad.Except (ExceptT(..))
import DOM (DOM)
import DOM.Event.Event (Event, preventDefault)
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, encodeJson, decodeJson, jsonEmptyObject, (.?), (:=), (~>))
-- import Data.Argonaut.Generic.Argonaut as GA
import Data.Bifunctor (lmap)
import Data.Foreign (F, ForeignError(..))
-- import Data.Foreign.Generic as FG
import Data.Generic (class Generic, gShow)
import Data.Maybe (Maybe(..), maybe)
import Data.MediaType.Common (textPlain)
-- import Data.MediaType.Common as MediaType
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax as AX
import Network.HTTP.Affjax.Response as AXR
import Prelude
-- import Simple.JSON as SJ
-- import Unsafe.Coerce (unsafeCoerce)

-- ---------------------------------------------------------------------


type State =
  { loading :: Boolean
  , intentText :: String
  , intentType :: String
  , createdHash :: Maybe Hash
  , retrievedIntent :: Maybe Intent
  , username :: String
  , result :: Maybe String
  }

data Query a
  = SetUsername String a
  | SetIntentText  String a
  | SetIntentType  String a
  | SetCreatedHash String a

  | MakeRequestCreate a
  | MakeRequestRetrieve a
  | PreventDefault Event (Query a)

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
                 , username: ""
                 , result: Nothing }

  render :: State -> H.ComponentHTML Query
  render st =
    HH.div_
      [
      HH.form
        [ HE.onSubmit (HE.input $ \e a -> PreventDefault e (MakeRequestCreate a))]
        [ HH.h1_ [ HH.text "Post an intent" ]

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
        , HH.p_ [ HH.text "hello world" ]
    , HH.form
        [ HE.onSubmit (HE.input $ \e a -> PreventDefault e (MakeRequestRetrieve a))]
        [ HH.h1_ [ HH.text "Retrieve an intent" ]

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
              -- Nothing -> []
              Nothing ->
                [ HH.h2_
                    [ HH.text "Read: Nothing" ]
                ]
              Just res ->
                [ HH.h2_
                    [ HH.text "Read:" ]
                , HH.pre_
                    [ HH.code_ [ HH.text (show res) ] ]
                ]
        , HH.div_
            case st.result of
              Nothing -> []
              Just res ->
                [ HH.h2_
                    [ HH.text "Response:" ]
                , HH.pre_
                    [ HH.code_ [ HH.text res ] ]
                ]
        ]
      ]

  eval :: Query ~> H.ComponentDSL State Query Void (Aff (dom :: DOM, ajax :: AX.AJAX | eff))
  eval = case _ of
    SetUsername username next -> do
      H.modify (_ { username = username, result = Nothing :: Maybe String })
      pure next
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
      -- H.modify (_ { loading = false
      --             , result          = Just ("hash value:" <> show mhash)
      --             })
      -- pure next
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

instance decodeJsonHash :: DecodeJson Hash where
  decodeJson json = do
    obj <- decodeJson json
    pure $ Hash obj

instance encodeJsonHash :: EncodeJson Hash where
  encodeJson (Hash hash) = encodeJson hash

-- -------------------------------------

data Intent = Intent
                { content    :: String
                 , typ       :: String
                 , timestamp :: String
                 }

derive instance genericIntent :: Generic Intent

instance showIntent :: Show Intent where
    show = gShow

instance decodeJsonIntent :: DecodeJson Intent where
  decodeJson json = do
    obj <- decodeJson json
    content   <- obj .? "content"
    typ       <- obj .? "type"
    timestamp <- obj .? "timestamp"
    pure $ Intent { content, typ, timestamp }

instance encodeJsonIntent :: EncodeJson Intent where
  encodeJson (Intent intent)
    =  "content"   := intent.content
    ~> "type"      := intent.typ
    ~> "timestamp" := intent.timestamp
    ~> jsonEmptyObject

-- ---------------------------------------------------------------------
-- See
-- https://stackoverflow.com/questions/42927827/purescript-reuse-argonaut-json-decoding-for-affjax-respondeable
-- It appears to be *way* more complicated than is should be
instance responsableJsonIntent :: AXR.Respondable Intent where
  -- responseType = Tuple (Just applicationJSON) JSONResponse
  responseType = Tuple (Just textPlain      ) AXR.JSONResponse -- holo is broken
  fromResponse = decodeJsonResponse <=< AXR.fromResponse

instance responsableJsonHash :: AXR.Respondable Hash where
  -- responseType = Tuple (Just applicationJSON) JSONResponse
  responseType = Tuple (Just textPlain      ) AXR.JSONResponse -- holo is broken
  fromResponse = decodeJsonResponse <=< AXR.fromResponse

decodeJsonResponse :: forall a. DecodeJson a => Json -> F a
decodeJsonResponse =
  ExceptT <<< pure <<< lmap (pure <<< ForeignError) <<< decodeJson

-- ---------------------------------------------------------------------

intentCreate :: forall e . Intent -> AX.Affjax e Hash
intentCreate intent = do
  AX.post "http://localhost:4141/fn/Intents/intentCreate" (encodeJson intent)

intentRead :: forall e . Hash -> AX.Affjax e Intent
intentRead (Hash hash) = AX.post "/fn/Intents/intentRead" (encodeJson hash)

-- post :: forall e a b. Requestable a => Respondable b => URL -> a -> Affjax e b

getAllIntents :: forall e b. AXR.Respondable b =>  AX.Affjax e b
getAllIntents = AX.post "/fn/Intents/getAllIntents" ""
