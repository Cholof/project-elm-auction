module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Browser.Navigation as Navigation
import Browser exposing (UrlRequest)
import Url exposing (Url)
import Url.Parser as UrlParser exposing ((</>), Parser, s, top)
import Bootstrap.CDN exposing (..)
import Bootstrap.Navbar as Navbar
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Button as Button
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Form.Input as Input
import Bootstrap.ListGroup as Listgroup

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
    { 
      username : String
    , firstName : String
    , surname : String
    , age : Int
    }

type Page
    = Home
    | Auctions
    | About
    | Profile
    | NotFound



init : Flags -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( navState, navCmd ) =
            Navbar.initialState NavMsg

        ( model, urlCmd ) =
            urlUpdate url { navKey = key, navState = navState, page = Home }
    in
        ( model, Cmd.batch [ urlCmd, navCmd ] )
-- Msg return states
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
                     ( model, Navigation.pushUrl model.navKey <| Url.toString url )

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
        , UrlParser.map Auctions (UrlParser.s "auctions")
        , UrlParser.map About (UrlParser.s "about")
        , UrlParser.map Profile (UrlParser.s "profile")
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
        |> Navbar.container 
        |> Navbar.brand [ href "#" ] [ text "Antinque Auction" ]
        |> Navbar.items
            [ Navbar.itemLink [ href "#auctions" ] [ text "Auctions" ]
            , Navbar.itemLink [ href "#about" ] [ text "About" ]
            , Navbar.itemLink [ href "#profile" ] [ text "Profile" ]
            ]
        |> Navbar.view model.navState

mainContent : Model -> Html Msg
mainContent model =
    Grid.container [] <|
        case model.page of
            Home ->
                pageHome model

            Auctions ->
                pageAuctions model

            About ->
                pageAbout model

            Profile ->
                pageProfile model

            NotFound ->
                pageNotFound

-- Homepage
pageHome : Model -> List (Html Msg)
pageHome modelHome =
    [ h1 [] [ text "Home" ]
    , Grid.row []
        [ Grid.col []
            [ Card.config [ Card.outlineDanger ]
                |> Card.headerH4 [class "text-center" ] [ text "Home" ]
                |> Card.block []
                    [ Block.text [] [ text "Todo: Image responsive" ]
            
                    ]
                |> Card.view
            ]
        ]
    ]

-- Auctions Page
-- 1. add list of Items 
-- 2. Move InputGroup and Button distinct item
pageAuctions : Model -> List (Html Msg)
pageAuctions modelAuctions =
    [ h2 [] [ text "Auctions" ]    
    , div [] [InputGroup.config (InputGroup.text [ Input.placeholder "Amount"])
        |> InputGroup.successors
            [ InputGroup.span [] [text " kr"]]
            |> InputGroup.view]
    , Button.button [Button.primary]
        [ text "Bid Item" ]
    ]

        
-- User Page
pageProfile : Model -> List (Html Msg)
pageProfile modelProfile = 
     [ h1 [] [ text "Profile" ]
     , Button.button [] [ text "Login" ]
     ]


-- About Page
pageAbout : Model -> List (Html Msg)
pageAbout modelAbout =
    [ h1 [] [ text "About" ]
    , Listgroup.ul
        [ Listgroup.li [Listgroup.warning] [ text "Chi" ]
        , Listgroup.li [Listgroup.warning] [ text "Olof" ]
        , Listgroup.li [Listgroup.warning] [ text "Todo: write about us?" ]
        ]
    ]

-- Just not found
pageNotFound : List (Html Msg)
pageNotFound =
    [ h1 [] [ text "Not found" ]
    , text "SOrry couldn't find that page"
    ]

    