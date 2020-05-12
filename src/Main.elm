module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Http
import Json.Decode exposing (field, string)
import Bible

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , view = view
    }

init : () -> (Model, Cmd Msg)
init _ =
  let model = { book = "Genesis", chapter = 1, data = Loading } in ( model, getScripture model )

-- TYPES
type ScriptureText
  = Loading
  | Success String
  | Failure

type Msg
  = ChangeBook String
  | ChangeChapter String
  | ScriptureChanged (Result Http.Error String)

-- ALIASES
type alias Model =
  { 
    book : String,
    chapter: Int,
    data: ScriptureText
   }

-- ACTIONS
getScripture : Model -> Cmd Msg
getScripture model =
  Http.get
    { url = "https://bible-api.com/" ++ model.book ++ String.fromInt model.chapter ++ "?verse_numbers=true"
    , expect = Http.expectJson ScriptureChanged (field "text" string)
    }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChangeBook book ->
       let newModel = { model | book = book, chapter = 1, data = Loading } in
      (newModel, getScripture newModel)

    ChangeChapter chapter ->
      let newModel = { model | chapter = Maybe.withDefault 1 (String.toInt chapter), data = Loading } in
      (newModel, getScripture newModel)
    
    ScriptureChanged result ->
      case result of
        Ok text ->
          ({ model | data = Success text }, Cmd.none)

        Err _ ->
          ({ model | data = Failure }, Cmd.none)

-- VIEW
view : Model -> Html Msg
view model =
  div []
    [
    div [] [
      select [ onInput ChangeBook ] (createBooks Bible.getBooks)
      , select [ value (String.fromInt model.chapter), onInput ChangeChapter ] (createChapters (Bible.getChapters model.book))
    ]
    , h1 [] [ text "The Bible"]
    , viewScripture model.data
    ]

viewScripture: ScriptureText -> Html Msg
viewScripture scripture =
  case scripture of
    Failure ->
      createDiv "Sorry something did go wrong. Text could not be loaded."

    Loading ->
      createDiv "Loading..."

    Success data ->
      div [] (List.map createDiv (String.split "\n" data))

createBooks : List String -> List (Html Msg)
createBooks books =
  List.map createOption books

createChapters : Int -> List (Html Msg)
createChapters amountOfChapter =
 List.range 1 amountOfChapter
   |> List.map String.fromInt
   |> List.map createOption

createOption : String -> Html Msg
createOption t =
  option [value t ] [ text t ]

createDiv : String -> Html Msg
createDiv t =
  div [] [ text t ]