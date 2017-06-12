module Util.Modal
    exposing
        ( Msg
        , State
        , init
        , subscriptions
        , update
        , view
        , show
        )

import Animation exposing (percent, px)
import Animation.Messenger
import Util exposing ((=>))
import Html exposing (Html, text)
import Views.Page exposing (modal)


type Msg
    = AnimateModal Animation.Msg
    | AnimateModalBackground Animation.Msg
    | ShowModal
    | HideModal
    | DeactivateModal



-- Selectively expose constructors your consumer needs


show : Msg
show =
    ShowModal


type State
    = State
        { modalActive : Bool
        , modalStyle : Animation.Messenger.State Msg
        , modalBackgroundStyle : Animation.Messenger.State Msg
        }


init : State
init =
    State
        { modalActive = False
        , modalStyle = styleYAxisAndOpacity -100 0
        , modalBackgroundStyle = styleOpacity 0
        }


subscriptions : (Msg -> msg) -> State -> Sub msg
subscriptions toMsg (State model) =
    Sub.batch
        [ Animation.subscription (toMsg << AnimateModal) [ model.modalStyle ]
        , Animation.subscription (toMsg << AnimateModalBackground) [ model.modalBackgroundStyle ]
        ]


update : State -> Msg -> (Msg -> msg) -> ( State, Cmd msg )
update (State model) animation toMsg =
    case Debug.log "animation" animation of
        DeactivateModal ->
            State { model | modalActive = False }
                => Cmd.none

        AnimateModal animMsg ->
            Animation.Messenger.update animMsg model.modalStyle
                |> Tuple.mapFirst (\newStyle -> State { model | modalStyle = newStyle })
                |> Tuple.mapSecond (Cmd.map toMsg)

        AnimateModalBackground animMsg ->
            Animation.Messenger.update animMsg model.modalBackgroundStyle
                |> Tuple.mapFirst (\newStyle -> State { model | modalBackgroundStyle = newStyle })
                |> Tuple.mapSecond (Cmd.map toMsg)

        ShowModal ->
            let
                startStyle =
                    fadeInDown <| styleYAxisAndOpacity -100 0

                backgroundStartStyle =
                    fadeIn <| styleOpacity 0
            in
                State
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
                State
                    { model
                        | modalStyle = startStyle
                        , modalBackgroundStyle = backgroundStartStyle
                    }
                    => Cmd.none


view : State -> (Msg -> msg) -> Html msg
view (State model) toMsg =
    modal
        { title = text "Hello Modal"
        , content = text "I am  a modal"
        , cardAttrs = (Animation.render model.modalStyle)
        , isActive = model.modalActive
        , backgroundAttrs = (Animation.render model.modalBackgroundStyle)
        }
        (HideModal)
        |> Html.map toMsg


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
