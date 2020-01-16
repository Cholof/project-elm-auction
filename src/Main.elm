module Main exposing (main)

import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Navbar as Navbar
import Bootstrap.Utilities.Spacing as Spacing
import Browser exposing (UrlRequest)
import Browser.Navigation as Navigation
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Url exposing (Url)
import Url.Parser as UrlParser exposing ((</>), Parser, s, top)



-- Model > View > Msg > Update > Model ....
-- aTODO: preventDefault / Search disabled specific a


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = ClickedLink
        , onUrlChange = UrlChange
        }



-- Types


type alias Flags =
    {}


type alias Model =
    { navKey : Navigation.Key
    , page : Page
    , navState : Navbar.State
    }


type alias User =
    { username : String
    , firstName : String
    , surname : String
    , email : String
    }


type Page
    = Home
    | AddItem
    | ExpiredAuctions
    | Auctions
    | About
    | Profile
    | NotFound
    | Signin
    | Signout
    | Signup


init : Flags -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( navState, navCmd ) =
            Navbar.initialState NavMsg

        ( model, urlCmd ) =
            urlUpdate url { navKey = key, navState = navState, page = Home }
    in
    ( model, Cmd.batch [ urlCmd, navCmd ] )



-- Msg


type Msg
    = UrlChange Url
    | ClickedLink UrlRequest
    | NavMsg Navbar.State



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Navbar.subscriptions model.navState NavMsg



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedLink req ->
            case req of
                Browser.Internal url ->
                    ( model
                    , if url.fragment == Just "" then
                        Cmd.none

                      else
                        Navigation.pushUrl model.navKey <| Url.toString url
                    )

                Browser.External href ->
                    ( model, Navigation.load href )

        UrlChange url ->
            urlUpdate url model

        NavMsg state ->
            ( { model | navState = state }
            , Cmd.none
            )



-- Url update


urlUpdate : Url -> Model -> ( Model, Cmd Msg )
urlUpdate url model =
    case decode url of
        Nothing ->
            ( { model | page = NotFound }, Cmd.none )

        Just route ->
            ( { model | page = route }, Cmd.none )


decode : Url -> Maybe Page
decode url =
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
        |> UrlParser.parse routeParser



-- Routeparser // Controller


routeParser : Parser (Page -> a) a
routeParser =
    UrlParser.oneOf
        [ UrlParser.map Home top
        , UrlParser.map Auctions (UrlParser.s "currentauctions")
        , UrlParser.map AddItem (UrlParser.s "sell")
        , UrlParser.map ExpiredAuctions (UrlParser.s "expiredauctions")
        , UrlParser.map About (UrlParser.s "about")
        , UrlParser.map Profile (UrlParser.s "profile")
        , UrlParser.map Signin (UrlParser.s "signin")
        , UrlParser.map Signout (UrlParser.s "signout")
        , UrlParser.map Signup (UrlParser.s "signup")
        ]



-- View


view : Model -> Browser.Document Msg
view model =
    { title = "Antique Auction"
    , body =
        [ div []
            [ menu model
            , mainContent model
            ]
        ]
    }



-- Navigation Bar // Links


menu : Model -> Html Msg
menu model =
    Navbar.config NavMsg
        |> Navbar.withAnimation
        -- |> Navbar.fixTop
        |> Navbar.collapseMedium
        |> Navbar.light
        |> Navbar.attrs
            myNavbarBorderStyle
        |> Navbar.brand [ href "#" ]
            [ img
                [ src "src/assets/images/museum.svg"
                , class "d-inline-block align-top"
                , style "width" "60px"
                , style "height" "80px"
                ]
                []
            ]
        |> Navbar.items
            [ Navbar.itemLinkActive
                [ href "#"
                , style "padding" "12px"
                , style "color" "OrangeRed"
                , style "font-size" "20px"
                ]
                [ text "Home" ]
            , Navbar.dropdown
                { id = "mydropdown"
                , toggle =
                    Navbar.dropdownToggle
                        [ style "font-size" "20px"
                        , style "padding" "12px"
                        ]
                        [ text "Auctions" ]
                , items =
                    [ Navbar.dropdownItem
                        [ href "#currentauctions" ]
                        [ text "Current Auctions" ]
                    , Navbar.dropdownItem
                        [ href "#sell" ]
                        [ text "Sell item" ]
                    , Navbar.dropdownDivider
                    , Navbar.dropdownItem
                        [ href "#expiredauctions" ]
                        [ text "Expired Items" ]
                    ]
                }
            , Navbar.dropdown
                { id = "profiledropdown"
                , toggle =
                    Navbar.dropdownToggle
                        [ style "font-size" "20px"
                        , style "padding" "12px"
                        ]
                        [ text "Profile" ]
                , items =
                    [ Navbar.dropdownItem
                        [ href "#profile" ]
                        [ text "My profile" ]
                    , Navbar.dropdownItem
                        [ href "#signin" ]
                        [ text "Sign in" ]
                    , Navbar.dropdownDivider
                    , Navbar.dropdownItem
                        [ href "#signout" ]
                        [ text "Sign out" ]
                    ]
                }
            , Navbar.itemLink
                [ href "#about"
                , style "font-size" "20px"
                , style "padding" "12px"
                ]
                [ text "About" ]
            ]
        |> Navbar.customItems
            [ Navbar.formItem []
                [ Input.text [ Input.attrs [ placeholder "Search your item" ] ]
                , Button.button
                    [ Button.secondary
                    , Button.attrs [ Spacing.ml2Sm ]
                    ]
                    [ text "Search" ]
                ]
            , Navbar.formItem []
                [ Button.linkButton
                    [ Button.secondary
                    , Button.attrs [ Spacing.ml2Sm ]
                    , Button.attrs [ href "#signup" ]
                    ]
                    [ text "Signup" ]
                ]
            ]
        |> Navbar.view model.navState


mainContent : Model -> Html Msg
mainContent model =
    div []
        (case model.page of
            Home ->
                pageHome

            Auctions ->
                pageAuctions

            AddItem ->
                pageAddItem

            ExpiredAuctions ->
                pageExpired

            About ->
                pageAbout

            Profile ->
                pageProfile

            Signin ->
                pageSignin

            Signout ->
                pageSignout

            Signup ->
                pageSignup

            NotFound ->
                pageNotFound
        )



-- Homepage


pageHome : List (Html Msg)
pageHome =
    [ img
        [ src "src/assets/images/home.jpg"
        , style "width" "100%"
        , style "height" "850px"
        ]
        []
    , Grid.row []
        [ Grid.col []
            [ Card.config [ Card.outlineLight ]
                |> Card.headerH4 [ class "text-center" ] [ text "Home" ]
                |> Card.block []
                    [ Block.text [ class "text-center" ]
                        [ text "Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum" ]
                    ]
                |> Card.view
            ]
        ]
    ]



-- Auctions Page
-- 1. add list of Items
-- 2. Move InputGroup and Button distinct item


produceColumn : Grid.Column msg
produceColumn =
    Grid.col
        [ Col.xs3 ]
        [ Card.config [ Card.outlineLight ]
            |> Card.headerH4 [ class "text-center" ]
                [ img [ src "src/assets/images/market.jpg", style "width" "10rem" ] [] ]
            |> Card.block []
                [ Block.text [ class "text-center" ]
                    [ text "Item name" ]
                ]
            |> Card.block []
                [ Block.text [ class "text-center" ]
                    [ text "Blablala" ]
                ]
            |> Card.view
        ]


pageAuctions : List (Html Msg)
pageAuctions =
    [ h1 [ class "text-center" ] [ text "Auctions" ]
    , Grid.container []
        [ Grid.row
            [ Row.topXs ]
            (List.repeat
                5
                produceColumn
            )
        ]
    ]


pageAddItem : List (Html Msg)
pageAddItem =
    [ h1 [ class "text-center" ] [ text "Add Item / Sell" ]
    , Grid.container []
        [ Grid.row
            [ Row.topXs ]
            [ Grid.col
                [ Col.xs3 ]
                [ Card.config [ Card.outlineLight ]
                    |> Card.headerH4 [ class "text-center" ] [ text "About" ]
                    |> Card.block []
                        [ Block.text [ class "text-center" ]
                            [ text "Item 1" ]
                        ]
                    |> Card.block []
                        [ Block.text [ class "text-center" ]
                            [ text "Blablala" ]
                        ]
                    |> Card.view
                ]
            ]
        ]
    ]


pageExpired : List (Html Msg)
pageExpired =
    [ h1 [ class "text-center" ] [ text "Expired Item" ]
    , Grid.container []
        [ Grid.row
            [ Row.topXs ]
            [ Grid.col
                [ Col.xs3 ]
                [ Card.config [ Card.outlineLight ]
                    |> Card.headerH4 [ class "text-center" ] [ text "About" ]
                    |> Card.block []
                        [ Block.text [ class "text-center" ]
                            [ text "Item 1" ]
                        ]
                    |> Card.block []
                        [ Block.text [ class "text-center" ]
                            [ text "Blablala" ]
                        ]
                    |> Card.view
                ]
            ]
        ]
    ]



-- User Page


pageProfile : List (Html Msg)
pageProfile =
    [ h1 [] [ text "Profile" ]
    , p [] [ text "Todo: Render Type - User" ]
    ]


pageSignin : List (Html Msg)
pageSignin =
    [ Form.form [ style "position" "absolute", style "left" "41%", style "top" "50%" ]
        [ Form.group []
            [ Form.label
                [ for "username"
                , style "font-size" "25px"
                ]
                [ text "Username" ]
            , Input.text [ Input.id "username", Input.placeholder "emailname@domain" ]
            ]
        , Form.group []
            [ Form.label
                [ for "password"
                , style "font-size" "25px"
                ]
                [ text "Password" ]
            , Input.password [ Input.id "password", Input.placeholder "minimum 8 tecken" ]
            ]
        , Button.button [ Button.secondary ] [ text "Sign in" ]
        ]
    , div []
        [ img
            [ src "src/assets/images/signin.jpg"
            , style "width" "100%"
            , style "height" "750px"
            ]
            []
        ]
    ]


pageSignout : List (Html Msg)
pageSignout =
    [ h1 [ style "position" "absolute", style "left" "43%", style "top" "44%" ] 
    [ text "You have sign out" ]
    , div []
        [ img
            [ src "src/assets/images/signin.jpg"
            , style "width" "100%"
            , style "height" "750px"
            ]
            []
        ]
    ]


pageSignup : List (Html Msg)
pageSignup =
    [ h1 [ style "position" "absolute", style "left" "53%", style "top" "20%" ] [ text "Create Profile" ]
    , Form.form [ style "position" "absolute", style "left" "53%", style "top" "30%" ]
        [ Form.group []
            [ Form.label
                [ for "username"
                , style "font-size" "15px"
                , style "color" "black"
                ]
                [ text "Username" ]
            , Input.text [ Input.id "username", Input.placeholder "Username" ]
            ]
        , Form.group []
            [ Form.label
                [ for "surname"
                , style "font-size" "15px"
                , style "color" "black"
                ]
                [ text "Surname" ]
            , Input.password [ Input.id "surname", Input.placeholder "Surname" ]
            ]
        , Form.group []
            [ Form.label
                [ for "address"
                , style "font-size" "15px"
                , style "color" "black"
                ]
                [ text "Address" ]
            , Input.password [ Input.id "address", Input.placeholder "Address" ]
            ]
        , Form.group []
            [ Form.label
                [ for "email"
                , style "font-size" "15px"
                , style "color" "black"
                ]
                [ text "Email" ]
            , Input.password [ Input.id "email", Input.placeholder "Email" ]
            ]
        , Form.group []
            [ Form.label
                [ for "password"
                , style "font-size" "15px"
                , style "color" "black"
                ]
                [ text "Password" ]
            , Input.password [ Input.id "password", Input.placeholder "Password" ]
            ]
        , Form.group []
            [ Form.label
                [ for "repeatPassword"
                , style "font-size" "15px"
                , style "color" "black"
                ]
                [ text "Repeat password" ]
            , Input.password [ Input.id "repeatPassword", Input.placeholder "Repeat password" ]
            ]
        , Button.button [ Button.secondary ] [ text "Create account" ]
        ]
    , div []
        [ img
            [ src "src/assets/images/signup.jpg"
            , style "right" "50%"
            , style "width" "50%"
            , style "height" "1050px"
            ]
            []
        ]
    ]



-- About Page


pageAbout : List (Html Msg)
pageAbout =
    [ img
        [ src "src/assets/images/about.jpg"
        , style "width" "100%"
        , style "height" "750px"
        ]
        []
    , Grid.row []
        [ Grid.col []
            [ Card.config [ Card.outlineLight ]
                |> Card.headerH4 [ class "text-center" ] [ text "About" ]
                |> Card.block []
                    [ Block.text [ class "text-center" ]
                        [ text "Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum" ]
                    ]
                |> Card.view
            ]
        ]
    ]



-- Just not found


pageNotFound : List (Html Msg)
pageNotFound =
    [ h1 [] [ text "Not found" ]
    , text "Sorry couldn't find that page"
    ]



-- Style functions


myNavbarBorderStyle : List (Attribute Msg)
myNavbarBorderStyle =
    [ style "height" "80px"
    , style "padding" "15px"
    , style "border-bottom-style" "solid"
    , style "border-bottom-color" "grey"
    ]
