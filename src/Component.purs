module Component where

import Prelude

import DOM.Node.Document (doctype)
import Data.Eq ((==), (/=))
import Data.Functor.Coproduct.Nested (in1)
import Data.HeytingAlgebra ((&&))
import Data.Int (even)
import Data.Maybe (Maybe(..))
import FFI.Util (property, setProperty)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

data Query a = Printing String a | Restart a
type Input = Unit
type Output = Void

winner :: Array String
winner = [ "" ]

restart :: Array String
restart = [ "" ]

count :: Array Int
count = [ 0 ]

--update :: forall e. Int -> State
update arr ren =
    let (counter :: Int) = property count "0"
        _ = if (even $ counter) then
                setProperty turnXO "0" "X"
            else
                setProperty turnXO "0" "O"

        _ = if ((property winner "0") == "" && (property arr ren) == "") then
                setProperty count "0" (counter + 1)
            else
                setProperty arr ren (property arr ren)
        _ = if ((property winner "0") == "" && (property arr ren) == "") then
                setProperty arr ren (property turnXO "0")
            else
                setProperty arr ren (property arr ren) in
        pure initialState

drawCon arr =
    let (counter :: Int) = property count "0"
        _ = if (counter == 9) then
                setProperty winner "0" "Match Drawn"
            else
                setProperty winner "0" (property winner "0") in
        pure initialState


turnXO :: Array String
turnXO = [ "X" ]


rowCheck arr =
        let (place :: String) = property turnXO "0"
            _ = if( arr.a == arr.b && arr.b == arr.c && arr.a /= "" ) then
                     setProperty winner "0" (arr.a <> " Won the game")
                else
                     if( arr.d == arr.e && arr.e == arr.f && arr.d /= "" ) then
                              setProperty winner "0" (arr.d <> " Won the game")
                      else
                               if( arr.g == arr.h && arr.h == arr.i && arr.g /= "" ) then
                                        setProperty winner "0" (arr.g <> " Won the game")
                                else
                                        if( arr.a == arr.d && arr.d == arr.g && arr.a /= "" ) then
                                                 setProperty winner "0" (arr.a <> " Won the game")
                                            else
                                                 if( arr.b == arr.e && arr.e == arr.h && arr.b /= "" ) then
                                                          setProperty winner "0" (arr.b <> " Won the game")
                                                  else
                                                           if( arr.c == arr.f && arr.f == arr.i && arr.c /= "" ) then
                                                                    setProperty winner "0" (arr.c <> " Won the game")
                                                            else
                                                                    if( arr.a == arr.e && arr.e == arr.i && arr.a /= "" ) then
                                                                             setProperty winner "0" (arr.a <> " Won the game")
                                                                        else
                                                                             if( arr.c == arr.e && arr.e == arr.g && arr.c /= "" ) then
                                                                                      setProperty winner "0" (arr.c <> " Won the game")
                                                                              else
                                                                                      setProperty winner "0" "" in
        pure initialState




type State = { a :: String, b :: String, c :: String, d :: String, e :: String, f :: String, g :: String, h :: String, i :: String, w :: String }

initialState :: State
initialState = { a : "", b : "", c : "", d : "", e : "", f : "", g : "", h : "", i : "", w : "" }

component :: forall m. H.Component HH.HTML Query Input Output m
component = H.component
    { initialState: const initialState
    , render : render
    , eval : eval
    , receiver: const Nothing
    }

render :: State -> H.ComponentHTML Query
render state = HH.div_
      [ HH.table_
        [ HH.tr_
          [ HH.td
              [ HE.onClick (HE.input_ $ Printing "a") ]
              [ HH.text state.a]
          , HH.td
              [ HP.class_ $ ClassName "vert"
              , HE.onClick (HE.input_ $ Printing "b") ]
              [ HH.text state.b ]
          , HH.td
              [ HE.onClick (HE.input_ $ Printing "c") ]
              [ HH.text state.c ]
          ]
      , HH.tr_
          [ HH.td
              [ HP.class_ $ ClassName "hori"
              , HE.onClick (HE.input_ $ Printing "d") ]
              [ HH.text state.d ]
          , HH.td
              [ HP.class_ $ ClassName "verthori"
              , HE.onClick (HE.input_ $ Printing "e") ]
              [ HH.text state.e ]
          , HH.td
              [ HP.class_ $ ClassName "hori"
              , HE.onClick (HE.input_ $ Printing "f") ]
              [ HH.text state.f ]
          ]
      , HH.tr_
          [ HH.td
              [ HE.onClick (HE.input_ $ Printing "g") ]
              [ HH.text state.g]
          , HH.td
              [ HP.class_ $ ClassName "vert"
              , HE.onClick (HE.input_ $ Printing "h") ]
              [ HH.text state.h ]
          , HH.td
              [ HE.onClick (HE.input_ $ Printing "i") ]
              [ HH.text state.i]
          ]
       ]
       ,
       HH.p_
         [ HH.text state.w ]
       ,
       HH.p
         [ HE.onClick (HE.input_ $ Restart) ]
         [ HH.text "RESTART" ]
      ]



eval :: forall m a. Query a -> H.ComponentDSL State Query Void m a
eval = case _ of
        Printing turn next -> do
          curState <- H.get
          _ <- update curState turn
          _ <- rowCheck curState
          _ <- drawCon curState
          H.modify (\state -> { a : state.a, b : state.b, c : state.c, d : state.d, e : state.e, f : state.f, g : state.g, h : state.h, i : state.i, w : property winner "0" })
          pure next
        Restart next -> do
          let _ = setProperty count "0" 0
          let _ = setProperty winner "0" ""
          H.modify (\state -> { a : "", b : "", c : "", d : "", e : "", f : "", g : "", h : "", i : "", w : "" } )
          pure next
