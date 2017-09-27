module Main exposing (..)

import Html exposing (Html, div, text)
import GraphQL.Request.Builder exposing (..)
import GraphQL.Request.Builder.Arg as Arg
import GraphQL.Request.Builder.Variable as Var
import GraphQL.Client.Http as GraphQLClient
import Task exposing (Task)


type alias Photo =
    { url : String
    , caption : String
    }


type alias User =
    { name : String
    , photos : List Photo
    }


userQuery : Document Query User { vars | userID : String }
userQuery =
    let
        userIDVar =
            Var.required "userID" .userID Var.id

        photo =
            object Photo
                |> with (field "url" [] string)
                |> with (field "caption" [] string)

        user =
            object User
                |> with (field "name" [] string)
                |> with (field "photos" [] (list photo))

        queryRoot =
            extract
                (field "User"
                    [ ( "id", Arg.variable userIDVar ) ]
                    user
                )
    in
        queryDocument queryRoot


userQueryRequest : Request Query User
userQueryRequest =
    userQuery
        |> request { userID = "cj819wp6xkneb01431kvknvhq" }


type alias UserResponse =
    Result GraphQLClient.Error User


type alias Model =
    Maybe UserResponse


type Msg
    = ReceiveQueryResponse UserResponse


sendQueryRequest : Request Query a -> Task GraphQLClient.Error a
sendQueryRequest request =
    GraphQLClient.sendQuery "https://api.graph.cool/simple/v1/cj8199ll30g820118lpgcgj9z" request


sendUserQuery : Cmd Msg
sendUserQuery =
    sendQueryRequest userQueryRequest
        |> Task.attempt ReceiveQueryResponse


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    ( Nothing, sendUserQuery )


view : Model -> Html Msg
view model =
    div []
        [ model |> toString |> text ]


update : Msg -> Model -> ( Model, Cmd Msg )
update (ReceiveQueryResponse response) model =
    ( Just response, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
