module Component (State, Query(..), ui) where

import Prelude
import Control.Monad.Aff (Aff)
import Data.Maybe (Maybe(..))
import DOM (DOM)
import DOM.Event.Event (Event, preventDefault)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax as AX
import Network.HTTP.Affjax.Response (class Respondable)

type State =
  { loading :: Boolean
  , username :: String
  , result :: Maybe String
  }

data Query a
  = SetUsername String a
  | MakeRequest a
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
  initialState = { loading: false, username: "", result: Nothing }

  render :: State -> H.ComponentHTML Query
  render st =
    HH.form
      [ HE.onSubmit (HE.input $ \e a -> PreventDefault e (MakeRequest a))]
      [ HH.h1_ [ HH.text "Lookup GitHub user" ]
      , HH.label_
          [ HH.div_ [ HH.text "Enter username:" ]
          , HH.input
              [ HP.value st.username
              , HE.onValueInput (HE.input SetUsername)
              ]
          ]
      , HH.input
          [ HP.type_ HP.InputSubmit
          , HP.value "Fetch info"
          , HP.disabled st.loading
          ]
      , HH.p_
          [ HH.text (if st.loading then "Working..." else "") ]
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

  eval :: Query ~> H.ComponentDSL State Query Void (Aff (dom :: DOM, ajax :: AX.AJAX | eff))
  eval = case _ of
    SetUsername username next -> do
      H.modify (_ { username = username, result = Nothing :: Maybe String })
      pure next
    MakeRequest next -> do
      username <- H.gets _.username
      H.modify (_ { loading = true })
      -- response <- H.liftAff $ AX.get ("https://api.github.com/users/" <> username)
      -- response <- H.liftAff $ AX.get ("/fn/Intents/getAllIntents")
      -- response <- H.liftAff $ AX.post "http://localhost:4141/fn/Intents/getAllIntents" ""
      req :: AX.AffjaxResponse String <- H.liftAff intentCreate
      response <- H.liftAff getAllIntents
      H.modify (_ { loading = false, result = Just response.response })
      pure next
    PreventDefault e query -> do
      H.liftEff $ preventDefault e
      eval query

intentCreate :: forall e b. Respondable b => AX.Affjax e b
intentCreate = do
  let intent = "{\"content\":\"This text will be saved in Holochain\",\"type\":\"Offer\",\"timestamp\":1525534245}"

  AX.post "http://localhost:4141/fn/Intents/intentCreate" intent

-- post :: forall e a b. Requestable a => Respondable b => URL -> a -> Affjax e b

getAllIntents :: forall e b. Respondable b =>  AX.Affjax e b
getAllIntents = AX.post "/fn/Intents/getAllIntents" ""
