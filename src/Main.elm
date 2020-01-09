module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Browser.Navigation as Navigation
import Browser exposing (UrlRequest)
import Url exposing (Url)
import Url.Parser as UrlParser exposing ((</>), Parser, s, top)
import Bootstrap.Navbar as Navbar
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Utilities.Spacing as Spacing
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
        |> Navbar.brand [ href "#" ]
            [ img
                [ src "src/assets/images/museum.svg"
                , class "align-top"
                , style "width" "40px"
                , style "height" "40px"
                ]
                []
            , text "Home"
            ]
        |> Navbar.items
            [ 
            Navbar.dropdown 
                { id = "mydropdown"
                , toggle = Navbar.dropdownToggle [] [ text "Auctions" ]
                , items =
                    [ Navbar.dropdownHeader [ text "Select" ]
                    , Navbar.dropdownItem
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
            ,  
            Navbar.dropdown 
                { id = "profiledropdown"
                , toggle = Navbar.dropdownToggle [] [ text "Profile" ]
                , items =
                    [ Navbar.dropdownHeader [ text "Select" ]
                    , Navbar.dropdownItem
                        [ href "#profile" ]
                        [ text "My profile" ]
                    , Navbar.dropdownItem
                        [ href "#signin" ]
                        [ text "Signin" ]
                    , Navbar.dropdownDivider
                    , Navbar.dropdownItem
                        [ href "#signout" ]
                        [ text "Signout" ]
                    ]
                }
            
            ,Navbar.itemLink [ href "#about" ] [ text "About" ]
            ]
        |> Navbar.customItems
            [ Navbar.formItem []
                [ Input.text [ Input.attrs [placeholder "Search your item" ]]
                , Button.button
                    [ Button.success
                    , Button.attrs [ Spacing.ml2Sm]
                    ]
                    [ text "Search"]
                ]
             , Navbar.formItem []
                [ Button.linkButton
                    [ Button.success
                    , Button.attrs [ Spacing.ml2Sm]
                    ]
                    [text "Signup"]]
            ]
        |> Navbar.view model.navState
    
mainContent : Model -> Html Msg
mainContent model =
    div [] (  
        case model.page of
            Home ->
                pageHome model 

            Auctions ->
                pageAuctions model

            AddItem ->
                pageAddItem model
            
            ExpiredAuctions ->
                pageExpired model

            About ->
                pageAbout model

            Profile ->
                pageProfile model

            Signin  ->
                pageSigin model

            Signout  ->
                pageSignout model

            Signup ->
                pageSignup model

            NotFound ->
                pageNotFound)

-- Homepage
pageHome : Model -> List (Html Msg)
pageHome modelHome =
     
    [img [src "src/assets/images/home.jpg"
                , style "width" "100%"
                , style "height" "750px"] []
        ,
        Grid.row [] 
        [ Grid.col []
            [ Card.config [ Card.outlineLight ]
                
                |> Card.headerH4 [class "text-center" ] [ text "Home" ]
                |> Card.block []
                    [ Block.text [class "text-center"]
                     [ text "Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum" ]
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
    , div [] [InputGroup.config (InputGroup.text [ Input.placeholder "Amount" ])
        |> InputGroup.successors
            [ InputGroup.span [] [text " kr"]]
            |> InputGroup.view]
    , Button.button [Button.primary]
        [ text "Bid Item" ]
    ]

pageAddItem : Model -> List (Html Msg)
pageAddItem modelSell =
    [ h1 [] [ text "Add Item / Sell" ]
    , Grid.row []
        [ Grid.col [ ]
            [ Card.config [ Card.outlineDanger ]
                |> Card.headerH4 [class "text-center" ] [ text "Sell" ]
                |> Card.block [ ]
                    [ Block.text [] [ text "Todo: Add Item" ]
                    
                    ]
                |> Card.view
            ]
        ]
    ]

pageExpired : Model -> List (Html Msg)
pageExpired modelExpired =
    [ h1 [] [ text "Expired" ]
    , Listgroup.ul
        [ Listgroup.li [Listgroup.info] [ text "Chi" ]
        , Listgroup.li [Listgroup.info] [ text "Olof" ]
        , Listgroup.li [Listgroup.info] [ text "Todo: write about us?" ]
        ]
    ]
-- User Page
pageProfile : Model -> List (Html Msg)
pageProfile modelProfile = 
     [ h1 [] [ text "Profile" ]
     ,p[] [ text "Todo: Render Type - User" ]
     ]

pageSigin : Model -> List (Html Msg)
pageSigin modelSignin = 
     [ h1 [] [ text "Signin" ]
     ,p[] [ text "Todo: Signin" ]
     ]

pageSignout : Model -> List (Html Msg)
pageSignout modelSignout = 
     [ h1 [] [ text "signout" ]
     ,p[] [ text "Todo: Signout" ]
     ]

pageSignup : Model -> List (Html Msg)
pageSignup modelSignout = 
     [ h1 [] [ text "Sign Up" ]
     ,p[] [ text "Todo: Sign Up" ]
     ]

-- About Page
pageAbout : Model -> List (Html Msg)
pageAbout modelAbout =
     [img [src "src/assets/images/about.jpg"
                , style "width" "100%"
                , style "height" "750px"] []
        ,
        Grid.row [] 
        [ Grid.col []
            [ Card.config [ Card.outlineLight ]
                
                |> Card.headerH4 [class "text-center" ] [ text "About" ]
                |> Card.block []
                    [ Block.text [class "text-center"]
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

    