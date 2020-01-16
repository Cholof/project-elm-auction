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



-- Model=


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


type alias Item =
    { name : String
    , description : String
    , image : String
    , startPrice : Int
    }


items : List Item
items =
    [ { name = "Gustaviansk spegel", description = "55 x 90 cm", image = "src/assets/images/gustmirror.jpg", startPrice = 4999 }
    , { name = "Tete-a-tete stolar", description = "Från 1900-talet", image = "src/assets/images/armchair.png", startPrice = 100000 }
    , { name = "Gammaldags Oljelampa", description = "Gammaldags från 1800 talet", image = "src/assets/images/oillamp.jpg", startPrice = 999 }
    ]


init : Flags -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( navState, navCmd ) =
            Navbar.initialState NavMsg

        ( model, urlCmd ) =
            urlUpdate url { navKey = key, navState = navState, page = Home }
    in
    ( model, Cmd.batch [ urlCmd, navCmd ] )



-- Msg=


type Msg
    = UrlChange Url
    | ClickedLink UrlRequest
    | NavMsg Navbar.State



-- Subscriptions=


subscriptions : Model -> Sub Msg
subscriptions model =
    Navbar.subscriptions model.navState NavMsg



-- Update=


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



-- Route=


routeParser : Parser (Page -> a) a
routeParser =
    UrlParser.oneOf
        [ UrlParser.map Home top
        , UrlParser.map Auctions (UrlParser.s "currentauctions")
        , UrlParser.map AddItem (UrlParser.s "additem")
        , UrlParser.map ExpiredAuctions (UrlParser.s "expiredauctions")
        , UrlParser.map About (UrlParser.s "about")
        , UrlParser.map Profile (UrlParser.s "profile")
        , UrlParser.map Signin (UrlParser.s "signin")
        , UrlParser.map Signout (UrlParser.s "signout")
        , UrlParser.map Signup (UrlParser.s "signup")
        ]



-- View=


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



-- Navbar=


menu : Model -> Html Msg
menu model =
    Navbar.config NavMsg
        |> Navbar.withAnimation
        -- |> Navbar.fixTop
        |> Navbar.collapseMedium
        |> Navbar.light
        |> Navbar.attrs
            myNavbarBorderStyle
        |> Navbar.brand [ href "/" ]
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
                [ href "/"
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
                        [ href "#additem" ]
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



-- Home=


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


produceColumn : Item -> Grid.Column msg
produceColumn item =
    Grid.col
        [ Col.xs3 ]
        [ Card.config [ Card.outlineLight ]
            |> Card.headerH4 [ class "text-center" ]
                [ img [ src item.image, style "height" "10rem" ] [] ]
            |> Card.block []
                [ Block.text [ class "text-center" ]
                    [ text ("Name: " ++ item.name) ]
                ]
            |> Card.block []
                [ Block.text [ class "text-center" ]
                    [ text ("Description: " ++ item.description) ]
                ]
            |> Card.block []
                [ Block.text [ class "text-center" ]
                    [ text ("Start price: " ++ String.fromInt item.startPrice ++ " SEK") ]
                ]
            |> Card.view
        ]



-- Auctions=


pageAuctions : List (Html Msg)
pageAuctions =
    [ h1 [ class "text-center" ] [ text "Auctions" ]
    , Grid.container []
        [ Grid.row
            [ Row.topXs ]
            (List.map produceColumn items)
        ]
    ]



-- Additem=


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



-- Expired=


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



-- Profile=


pageProfile : List (Html Msg)
pageProfile =
    [ Card.config [ Card.attrs [ style "width" "20rem", style "height" "800px " ] ]
        |> Card.header [ class "text-center" ]
            [ img [ src "./src/assets/images/profile.png", style "width" "15rem" ] []
            , h3 [ Spacing.mt2 ] [ text "Profile" ]
            ]
        |> Card.block []
            [ Block.titleH4 [ style "padding" "10px" ] [ text "Menu" ]
            , Block.custom <|
                Button.linkButton
                    [ Button.outlineDark
                    , Button.attrs [ style "width" "12rem", style "margin" "10px" ]
                    , Button.attrs [ href "#profile" ]
                    ]
                    [ text "My Profile" ]
            , Block.custom <|
                Button.linkButton
                    [ Button.outlineDark
                    , Button.attrs [ style "width" "12rem", style "margin" "10px" ]
                    , Button.attrs [ href "#currentauctions" ]
                    ]
                    [ text "Your items" ]
            , Block.custom <|
                Button.linkButton
                    [ Button.outlineDark
                    , Button.attrs [ style "width" "12rem", style "margin" "10px" ]
                    , Button.attrs [ href "#additem" ]
                    ]
                    [ text "Sell item" ]
            , Block.custom <|
                Button.linkButton
                    [ Button.outlineDark
                    , Button.attrs [ style "width" "12rem", style "margin" "10px" ]
                    , Button.attrs [ href "#signout" ]
                    ]
                    [ text "Sign out" ]
            ]
        |> Card.view
    , Grid.container
        [ style "position" "absolute"
        , style "left" "20%"
        , style "top" "15%"
        ]
        [ h1 [] [ text "My profile" ] ]
    , Grid.container
        [ style "position" "absolute"
        , style "left" "20%"
        , style "top" "25%"
        ]
        [ Form.form []
            [ Form.row []
                [ Form.colLabel [ Col.sm2 ] [ text "Name : " ]
                , Form.col [ Col.xs3 ]
                    [ Input.text [ Input.id "name", Input.value "Chi" ] ]
                ]
            , Form.row []
                [ Form.colLabel [ Col.sm2 ] [ text "Surname : " ]
                , Form.col [ Col.xs3 ]
                    [ Input.text [ Input.id "surename", Input.value "Trinh" ] ]
                ]
            , Form.row []
                [ Form.colLabel [ Col.sm2 ] [ text "Personnumber : " ]
                , Form.col [ Col.xs3 ]
                    [ Input.text [ Input.id "personNumber", Input.value "220222-2259" ] ]
                ]
            , Form.row []
                [ Form.colLabel [ Col.sm2 ] [ text "Alias : " ]
                , Form.col [ Col.xs3 ]
                    [ Input.text [ Input.id "alias", Input.value "Chipstick" ] ]
                ]
            , Form.row []
                [ Form.colLabel [ Col.sm2 ] [ text "Email : " ]
                , Form.col [ Col.xs3 ]
                    [ Input.text [ Input.id "email", Input.value "Chi@email.se", Input.placeholder "example: name@domain" ] ]
                ]
            , Form.row []
                [ Form.colLabel [ Col.sm2 ] [ text "Address : " ]
                , Form.col [ Col.xs3 ]
                    [ Input.text [ Input.id "address", Input.value "Vasagatan 19", Input.placeholder "example: Sagagatan 11" ] ]
                ]
            , Form.row []
                [ Form.colLabel [ Col.sm2 ] [ text "Postal : " ]
                , Form.col [ Col.xs3 ]
                    [ Input.text [ Input.id "Zip", Input.value "411 11", Input.placeholder "example: 411 11" ] ]
                , Form.col [ Col.xs3 ]
                    [ Input.text [ Input.id "state", Input.value "Göteborg", Input.placeholder "What city?" ] ]
                ]
            , Form.row []
                [ Form.colLabel [ Col.sm2 ] [ text "Country : " ]
                , Form.col [ Col.xs3 ]
                    [ Input.text [ Input.id "country", Input.value "Sweden", Input.placeholder "What country?" ] ]
                ]
            , Form.row [ Row.leftSm ]
                [ Form.col [ Col.sm2 ]
                    [ Button.button
                        [ Button.primary, Button.attrs [ class "float-left" ] ]
                        [ text "Update Profile" ]
                    ]
                ]
            ]
        , Grid.container
            [ style "position" "absolute"
            , style "left" "70%"
            , style "top" "-15%"
            ]
            [ h1 [] [ text "Change Password" ] ]
        , Grid.container
            [ style "position" "absolute"
            , style "left" "70%"
            , style "top" "1px"
            ]
            [ Form.row []
                [ Form.colLabel [ Col.sm2 ] [ text "Old password : " ]
                , Form.col [ Col.xs3 ]
                    [ Input.password [ Input.id "password", Input.value "*******", Input.placeholder "At least 8 char" ] ]
                ]
            , Form.row []
                [ Form.colLabel [ Col.sm2 ] [ text "New password : " ]
                , Form.col [ Col.xs3 ]
                    [ Input.password [ Input.id "change password", Input.placeholder "New password" ] ]
                ]
            , Form.row []
                [ Form.colLabel [ Col.sm2 ] [ text "Repeat new password : " ]
                , Form.col [ Col.xs3 ]
                    [ Input.password [ Input.id "change password", Input.placeholder "Repeat new password" ] ]
                ]
            , Form.row [ Row.leftSm ]
                [ Form.col [ Col.sm2 ]
                    [ Button.button
                        [ Button.primary, Button.attrs [ class "float-left" ] ]
                        [ text "Change password" ]
                    ]
                ]
            ]
        ]
    ]



-- Signin=


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



-- Signout=


pageSignout : List (Html Msg)
pageSignout =
    [ h1 [ style "position" "absolute", style "left" "40%", style "top" "45%" ]
        [ text "You have been sign out" ]
    , div []
        [ img
            [ src "src/assets/images/signin.jpg"
            , style "width" "100%"
            , style "height" "750px"
            ]
            []
        ]
    ]



--Signup=


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
            , Input.text [ Input.id "surname", Input.placeholder "Surname" ]
            ]
        , Form.group []
            [ Form.label
                [ for "address"
                , style "font-size" "15px"
                , style "color" "black"
                ]
                [ text "Address" ]
            , Input.text [ Input.id "address", Input.placeholder "Address" ]
            ]
        , Form.group []
            [ Form.label
                [ for "email"
                , style "font-size" "15px"
                , style "color" "black"
                ]
                [ text "Email" ]
            , Input.email [ Input.id "email", Input.placeholder "Email" ]
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



-- About=


pageAbout : List (Html Msg)
pageAbout =
    [ img
        [ src "src/assets/images/about.jpg"
        , style "width" "100%"
        , style "height" "850px"
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
