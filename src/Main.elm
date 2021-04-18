module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Url
import Url.Parser as Parser exposing (Parser, (</>), custom, fragment, map, oneOf, s, top)
import Debug


-- MAIN


main : Program () Model Msg
main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClicked
    }



-- MODEL


type alias Model =
  { key : Nav.Key
  , url : Url.Url
  , extra : String
  }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
  ( Model key url "initial", Cmd.none )



-- UPDATE


type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          ( model, Nav.pushUrl model.key (Url.toString url) )

        Browser.External href ->
          ( model, Nav.load href )

    UrlChanged url ->
      stepUrl url model

-- ROUTER

stepUrl : Url.Url -> Model -> (Model, Cmd Msg)
stepUrl url model =
  let
    parser =
      oneOf
        [ route (Parser.s "elmNav")
            ( { model | url = url, extra = "This is top."}, Cmd.none
            )
        , route (Parser.s "elmNav/home")
        ( { model | url = url, extra = "home."}, Cmd.none
        )
        , route (Parser.s "elmNav/reviews" </> Parser.string)
        ( \bookName -> ({ model | url = url, extra = bookName}, Cmd.none)
        )
        , route (Parser.s "elmNav/add" </> Parser.int </> Parser.int)
        ( \arg1 arg2 ->
          ({ model | url = url
                   , extra = String.fromInt (Debug.log "arg" (arg1 + arg2))}
          , Cmd.none)
        )
        -- , route (s "help")
        --     ({ model | page=Hello })
        --     -- (stepHelp model (Help.init session "Design Guidelines" "/assets/help/design-guidelines.md"))
        -- , route (s "help" </> s "documentation-format")
        --     (stepHelp model (Help.init session "Documentation Format" "/assets/help/documentation-format.md"))
        ]
  in
  case Parser.parse parser url of
    Just answer ->
      answer

    Nothing ->
      ( { model | url = url }
      , Cmd.none
      )


route : Parser a b -> a -> Parser (b -> c) c
route parser handler =
  Parser.map handler parser




-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
  { title = "URL Interceptor"
  , body =
      [ text "The current URL is: "
      , b [] [ text (Url.toString model.url) ]
      , ul []
          [ viewLink "/elmNav/home"
          , viewLink "/elmNav/profile"
          , viewLink "/elmNav/reviews/the-century-of-the-self"
          , viewLink "/elmNav/reviews/public-opinion"
          , viewLink "/elmNav/reviews/shah-of-shahs"
          , viewLink "/elmNav/add/11/10"
          ]
      , text model.extra
      ]
  }


viewLink : String -> Html msg
viewLink path =
  li [] [ a [ href path ] [ text path ] ]
