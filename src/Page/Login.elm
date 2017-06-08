module Page.Login exposing (view, update, Model, Msg, initialModel, ExternalMsg(..), subscriptions)

{-| The login page.
-}

import Animation exposing (percent, px)
import Animation.Messenger
import Data.Session as Session exposing (Session)
import Data.User as User exposing (User)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder, decodeString, field, string)
import Json.Decode.Pipeline as Pipeline exposing (decode, optional)
import Request.User exposing (storeSession)
import Route exposing (Route)
import Util exposing ((=>))
import Validate exposing (..)
import Views.Form as Form
import Views.Page exposing (modal)


-- MODEL --


type alias Model =
    { errors : List Error
    , email : String
    , password : String
    , modalActive : Bool
    , modalStyle : Animation.Messenger.State Msg
    , modalBackgroundStyle : Animation.Messenger.State Msg
    }


initialModel : Model
initialModel =
    { errors = []
    , email = ""
    , password = ""
    , modalActive = False
    , modalStyle = styleYAxisAndOpacity -100 0
    , modalBackgroundStyle = styleOpacity 0
    }



-- VIEW --


view : Session -> Model -> Html Msg
view session model =
    div [ class "auth-page" ]
        [ div [ class "container page" ]
            [ div [ class "row" ]
                [ div [ class "col-md-6 offset-md-3 col-xs-12" ]
                    [ h1 [ class "text-xs-center" ] [ text "Sign in" ]
                    , p [ class "text-xs-center" ]
                        [ a [ Route.href Route.Register ]
                            [ text "Need an account?" ]
                        ]
                    , Form.viewErrors model.errors
                    , viewForm
                    ]
                ]
            ]
        , viewModal model
        ]


viewModal : Model -> Html Msg
viewModal model =
    modal
        { title = text "Hello Modal"
        , content = text "I am  a modal"
        , cardAttrs = (Animation.render model.modalStyle)
        , isActive = model.modalActive
        , backgroundAttrs = (Animation.render model.modalBackgroundStyle)
        }
        (MyAnimationMsg HideModal)


viewForm : Html Msg
viewForm =
    div []
        [ Form.input
            [ class "form-control-lg"
            , placeholder "Email"
            , onInput SetEmail
            ]
            []
        , Form.password
            [ class "form-control-lg"
            , placeholder "Password"
            , onInput SetPassword
            ]
            []
        , button [ onClick <| MyAnimationMsg ShowModal, class "btn btn-lg btn-primary pull-xs-right" ]
            [ text "Sign in" ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Animation.subscription (MyAnimationMsg << AnimateModal) [ model.modalStyle ]
        , Animation.subscription (MyAnimationMsg << AnimateModalBackground) [ model.modalBackgroundStyle ]
        ]



-- UPDATE --


type Msg
    = SubmitForm
    | SetEmail String
    | SetPassword String
    | LoginCompleted (Result Http.Error User)
    | MyAnimationMsg MyAnimation
    | DeactivateModal


type MyAnimation
    = AnimateModal Animation.Msg
    | AnimateModalBackground Animation.Msg
    | ShowModal
    | HideModal


type ExternalMsg
    = NoOp
    | SetUser User


update : Msg -> Model -> ( ( Model, Cmd Msg ), ExternalMsg )
update msg model =
    case Debug.log "msg" msg of
        MyAnimationMsg animMsg ->
            updateAnimation model animMsg
                => NoOp

        DeactivateModal ->
            { model | modalActive = False }
                => Cmd.none
                => NoOp

        SubmitForm ->
            case validate model of
                [] ->
                    { model | errors = [] }
                        => Http.send LoginCompleted (Request.User.login model)
                        => NoOp

                errors ->
                    { model | errors = errors }
                        => Cmd.none
                        => NoOp

        SetEmail email ->
            { model | email = email }
                => Cmd.none
                => NoOp

        SetPassword password ->
            { model | password = password }
                => Cmd.none
                => NoOp

        LoginCompleted (Err error) ->
            let
                errorMessages =
                    case error of
                        Http.BadStatus response ->
                            response.body
                                |> decodeString (field "errors" errorsDecoder)
                                |> Result.withDefault []

                        _ ->
                            [ "unable to process registration" ]
            in
                { model | errors = List.map (\errorMessage -> Form => errorMessage) errorMessages }
                    => Cmd.none
                    => NoOp

        LoginCompleted (Ok user) ->
            model
                => Cmd.batch [ storeSession user, Route.modifyUrl Route.Home ]
                => SetUser user


updateAnimation : Model -> MyAnimation -> ( Model, Cmd Msg )
updateAnimation model animation =
    case Debug.log "animation" animation of
        AnimateModal animMsg ->
            Animation.Messenger.update animMsg model.modalStyle
                |> Tuple.mapFirst (\newStyle -> { model | modalStyle = newStyle })

        AnimateModalBackground animMsg ->
            Animation.Messenger.update animMsg model.modalBackgroundStyle
                |> Tuple.mapFirst (\newStyle -> { model | modalBackgroundStyle = newStyle })

        ShowModal ->
            let
                startStyle =
                    fadeInDown <| styleYAxisAndOpacity -100 0

                backgroundStartStyle =
                    fadeIn <| styleOpacity 0
            in
                { model
                    | modalActive = True
                    , modalStyle = startStyle
                    , modalBackgroundStyle = backgroundStartStyle
                }
                    => Cmd.none

        HideModal ->
            let
                startStyle =
                    fadeOutUpWithMessage DeactivateModal <| styleYAxisAndOpacity 1 1

                backgroundStartStyle =
                    fadeOut <| styleOpacity 1
            in
                { model
                    | modalStyle = startStyle
                    , modalBackgroundStyle = backgroundStartStyle
                }
                    => Cmd.none



-- VALIDATION --


type Field
    = Form
    | Email
    | Password


{-| Recording validation errors on a per-field basis facilitates displaying
them inline next to the field where the error occurred.

I implemented it this way out of habit, then realized the spec called for
displaying all the errors at the top. I thought about simplifying it, but then
figured it'd be useful to show how I would normally model this data - assuming
the intended UX was to render errors per field.

(The other part of this is having a view function like this:

viewFormErrors : Field -> List Error -> Html msg

...and it filters the list of errors to render only the ones for the given
Field. This way you can call this:

viewFormErrors Email model.errors

...next to the `email` field, and call `viewFormErrors Password model.errors`
next to the `password` field, and so on.

-}
type alias Error =
    ( Field, String )


validate : Model -> List Error
validate =
    Validate.all
        [ .email >> ifBlank (Email => "email can't be blank.")
        , .password >> ifBlank (Password => "password can't be blank.")
        ]


errorsDecoder : Decoder (List String)
errorsDecoder =
    decode (\email username password -> List.concat [ email, username, password ])
        |> optionalError "email"
        |> optionalError "username"
        |> optionalError "password"


optionalError : String -> Decoder (List String -> a) -> Decoder a
optionalError fieldName =
    let
        errorToString errorMessage =
            String.join " " [ fieldName, errorMessage ]
    in
        optional fieldName (Decode.list (Decode.map errorToString string)) []



-- ANIMATIONS --


fadeIn : Animation.Messenger.State msg -> Animation.Messenger.State msg
fadeIn =
    Animation.interrupt
        [ Animation.to
            [ Animation.opacity 1
            ]
        ]


fadeInDown : Animation.Messenger.State msg -> Animation.Messenger.State msg
fadeInDown =
    Animation.interrupt
        [ Animation.to
            [ Animation.translate3d (px 0) (percent 1) (px 0)
            , Animation.opacity 1
            ]
        ]


fadeOut : Animation.Messenger.State msg -> Animation.Messenger.State msg
fadeOut =
    Animation.interrupt
        [ Animation.to
            [ Animation.opacity 0
            ]
        ]


fadeOutUpWithMessage : msg -> Animation.Messenger.State msg -> Animation.Messenger.State msg
fadeOutUpWithMessage func =
    Animation.interrupt
        [ Animation.to
            [ Animation.translate3d (px 0) (percent -100) (px 0)
            , Animation.opacity 0
            ]
        , Animation.Messenger.send func
        ]


styleOpacity : Float -> Animation.Messenger.State msg
styleOpacity opacity =
    Animation.style
        [ Animation.opacity opacity
        ]


styleYAxisAndOpacity : Float -> Float -> Animation.Messenger.State msg
styleYAxisAndOpacity y opacity =
    Animation.style
        [ Animation.translate3d (px 0) (percent y) (px 0)
        , Animation.opacity opacity
        ]
