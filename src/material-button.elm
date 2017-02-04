{- This file re-implements the Elm Counter example (1 counter) with elm-mdl
   buttons. Use this as a starting point for using elm-mdl components in your own
   app.
-}


module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (href, class, style, placeholder)
import Html.Events as Events
import Material
import Material.Scheme
import Material.Button as Button
import Material.Options as Options exposing (css)
import Material.Textfield as Textfield
import String



-- MODEL




type alias Model =
    { count : Int
    , riverSand : Field
    , mdl :
        Material.Model
        -- Boilerplate: model store for any and all Mdl components you use.
    }

type alias Field = 
    { value : String
    , error : Maybe String
    , required : Bool
    , valid : Bool
    , pristine : Bool
    , validators : List (String -> Bool)
    }


validateRequired : String -> Bool
validateRequired value = 
    if value == "" then
        False
    else
        True

fieldInit = Field "" Nothing True False True [validateRequired]

model : Model
model =
    { count = 0
    , riverSand = fieldInit
    , mdl =
        Material.model
        -- Boilerplate: Always use this initial Mdl model store.
    }




-- ACTION, UPDATE


type Msg
    = Increase
    | Reset
    | SetRiverSand String
    | Mdl (Material.Msg Msg)



-- Boilerplate: Msg clause for internal Mdl messages.


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increase ->
            ( { model | count = model.count + 1 }
            , Cmd.none
            )

        Reset ->
            ( { model | count = 0 }
            , Cmd.none
            )

        SetRiverSand input ->
            let 
                riverSand = model.riverSand
                
                validate number = 
                    case String.toFloat number of
                        Err msg->
                            { riverSand |  
                                pristine = False
                                , error = Just "Por Favor ingrese un Número"
                                , valid = False
                                , value = number
                                
                            }
                        Ok val->
                            { riverSand |  
                                pristine = False
                                , error = Nothing
                                , valid = True
                                , value = number
                                
                            }
            in
                ({model | riverSand = (validate input) }, Cmd.none)
                        
            
        -- Boilerplate: Mdl action handler.
        Mdl msg_ ->
            Material.update Mdl msg_ model



-- VIEW


type alias Mdl =
    Material.Model


view : Model -> Html Msg
view model =
    div []
    [
        div
            [ style [ ( "padding", "2rem" ) ] ]
            [ text ("Current count: " ++ toString model.count)
              {- We construct the instances of the Button component that we need, one
                 for the increase button, one for the reset button. First, the increase
                 button. The first three arguments are:

                   - A Msg constructor (`Mdl`), lifting Mdl messages to the Msg type.
                   - An instance id (the `[0]`). Every component that uses the same model
                     collection (model.mdl in this file) must have a distinct instance id.
                   - A reference to the elm-mdl model collection (`model.mdl`).

                 Notice that we do not have to add fields for the increase and reset buttons
                 separately to our model; and we did not have to add to our update messages
                 to handle their internal events.

                 Mdl components are configured with `Options`, similar to `Html.Attributes`.
                 The `Options.onClick Increase` option instructs the button to send the `Increase`
                 message when clicked. The `css ...` option adds CSS styling to the button.
                 See `Material.Options` for details on options.
              -}
            , Button.render Mdl
                [ 0 ]
                model.mdl
                [ Options.onClick Increase
                , css "margin" "0 24px"
                ]
                [ text "Increase" ]
            , Button.render Mdl
                [ 1 ]
                model.mdl
                [ Options.onClick Reset ]
                [ text "Reset" ]
            ]
        , div []
        [
        Textfield.render Mdl [2] model.mdl
            [ Textfield.label "Arena de Río"
            , Options.onInput SetRiverSand
            , Textfield.error (Maybe.withDefault "Error" model.riverSand.error)
                |> Options.when (not model.riverSand.valid && (not model.riverSand.pristine))
            , Textfield.value model.riverSand.value
            ] []
        ]
    ]    
        |> Material.Scheme.top



-- Load Google Mdl CSS. You'll likely want to do that not in code as we
-- do here, but rather in your master .html file. See the documentation
-- for the `Material` module for details.


main : Program Never Model Msg
main =
    Html.program
        { init = ( model, Cmd.none )
        , view = view
        , subscriptions = always Sub.none
        , update = update
        }