module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Bible
import Types exposing (..)

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , view = view
    }

init : () -> (Model, Cmd Msg)
init _ =
  let model = { book = "Genesis", chapter = 1, data = Loading } in
  ( model, Bible.loadScripture model.book model.chapter )

type alias Model =
  { 
    book : String,
    chapter: Int,
    data: Call
   }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChangeBook book ->
       let newModel = { model | book = book, chapter = 1, data = Loading } in
      (newModel, Bible.loadScripture newModel.book newModel.chapter)

    ChangeChapter chapter ->
      let newModel = { model | chapter = Maybe.withDefault 1 (String.toInt chapter), data = Loading } in
      (newModel, Bible.loadScripture newModel.book newModel.chapter)
    
    ScriptureChanged result ->
      case result of
        Ok text ->
          ({ model | data = Success text }, Cmd.none)

        Err _ ->
          ({ model | data = Failure }, Cmd.none)

view : Model -> Html Msg
view model =
  div []
    [
    div [] [
      select [ onInput ChangeBook ] (createBooks Bible.getBooks)
      , select [ value (String.fromInt model.chapter), onInput ChangeChapter ] (createChapters (Bible.getChapters model.book))
    ]
    , h1 [] [ text "The Bible"]
    , h2 [] [ text (model.book ++ " " ++ String.fromInt model.chapter)]
    , viewScripture model.data
    ]

viewScripture: Types.Call -> Html Msg
viewScripture scripture =
  case scripture of
    Types.Failure ->
      createDiv "Sorry something did go wrong. Text could not be loaded."

    Types.Loading ->
      createDiv "Loading..."

    Types.Success data ->
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