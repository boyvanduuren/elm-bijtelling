-- Small program to calculate monthly net added tax value when leasing a car

import Html exposing (Html, Attribute, beginnerProgram, text, div, input)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Result exposing (withDefault)
import String


main =
  beginnerProgram { model = model, view = view, update = update }

-- LIB

type alias Schaal =
  { start : Float
  , eind : Float
  , percentage: Float
  }

verschuldigd : Float -> Float
verschuldigd bedrag =
    let
        -- the different Dutch tax brackets since 2018
        schaalEen = Schaal 0.0 20142 18.65
        schaalTwee = Schaal 20142 34404 22.95
        schaalDrie = Schaal 34404 68507 40.85
        -- note that the last bracket has no upper bound so we set it
        -- to the lower bound and account for it in the calculation
        schaalVier = Schaal 68507 68507 51.95
        berekening jaarSalaris schaal =
          let verschil = jaarSalaris - schaal.start in
            if jaarSalaris > schaal.start
            then if verschil < schaal.eind || schaal.start == schaal.eind
              then verschil * schaal.percentage / 100
              else (schaal.eind - schaal.start) * schaal.percentage / 100
            else 0
     in
        berekening bedrag schaalEen + berekening bedrag schaalTwee + berekening bedrag schaalDrie + berekening bedrag schaalVier

-- Parse a string as float, or return 0 if we can't
parseInputField : String -> Float
parseInputField input = Result.withDefault 0 (String.toFloat input)
-- MODEL

type alias Model =
  { jaarSalaris : Float
  , catalogusWaarde : Float
  , bijtelling : Float
  }

model : Model
model =
  Model 0.0 0.0 0.0

-- UPDATE

type Msg
    = Jaarsalaris String
    | Cataloguswaarde String
    | Bijtelling String


update : Msg -> Model -> Model
update msg model =
  case msg of
    Jaarsalaris jaarsalaris ->
      { model | jaarSalaris = parseInputField jaarsalaris }

    Cataloguswaarde cataloguswaarde ->
      { model | catalogusWaarde = parseInputField cataloguswaarde }

    Bijtelling bijtelling ->
      { model | bijtelling = parseInputField bijtelling }


-- VIEW

view content =
  div []
    [ input [ placeholder "Jaarsalaris", onInput Jaarsalaris ] []
    , input [ placeholder "Cataloguswaarde", onInput Cataloguswaarde ] []
    , input [ placeholder "Bijtelling", onInput Bijtelling ] []
    , berekenBijtelling content
    ]
    
berekenBijtelling : Model -> Html msg
berekenBijtelling model =
  let
    belasting = verschuldigd model.jaarSalaris
    bijtelling = verschuldigd (model.jaarSalaris + (model.catalogusWaarde * model.bijtelling / 100))
    maandelijks = (bijtelling - belasting) / 12
  in
    div [] [ text (toString (round maandelijks)) ]

