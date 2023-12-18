module Rasper exposing (..)


import Browser
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (disabled)


-- Model

type alias Character =
    { name : String
    , health : Int
    , attack : Int
    , defense : Int
    }

type alias Model =
    { player : Character
    , enemy : Character
    , message : String
    , isPlayerTurn : Bool
    }

initialPlayer : Character
initialPlayer =
    { name = "Player"
    , health = 100
    , attack = 10
    , defense = 5
    }

initialEnemy : Character
initialEnemy =
    { name = "Enemy"
    , health = 80
    , attack = 8
    , defense = 3
    }

initialModel : Model
initialModel =
    { player = initialPlayer
    , enemy = initialEnemy
    , message = ""
    , isPlayerTurn = True
    }

-- Msg

type Msg
    = PlayerAttack
    | EnemyAttack
    | NewGame

-- Update

update : Msg -> Model -> Model
update msg model =
    case msg of
        PlayerAttack ->
            let
                enemy = model.enemy
                updatedEnemyHealth =
                    max 0 (model.enemy.health - (calculateDamage model.player model.enemy))
                updatedEnemy =
                    { enemy | health = updatedEnemyHealth }
                updatedModel =
                    { model | enemy = updatedEnemy }
            in
            if updatedEnemyHealth <= 0 then
                { updatedModel | message = "Player wins!" }
            else
                { updatedModel | message = "Player attacks! Enemy's turn." , isPlayerTurn = False }

        EnemyAttack ->
            let
                player = model.player
                updatedPlayerHealth =
                    max 0 (model.player.health - (calculateDamage model.enemy model.player))
                updatedPlayer =
                    { player | health = updatedPlayerHealth }
                updatedModel =
                    { model | player = updatedPlayer }
            in
            if updatedPlayerHealth <= 0 then
                { updatedModel | message = "Enemy wins!" }
            else
                { updatedModel | message = "Enemy attacks! Player's turn." , isPlayerTurn = True }

        NewGame ->
            initialModel

calculateDamage : Character -> Character -> Int
calculateDamage attacker defender =
    let
        damage =
            max 0 (attacker.attack - defender.defense)
    in
    damage

-- View

view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "RPG Combat System" ]
        , div [] [ text model.message ]
        , div [] [ text ("Player Health: " ++ String.fromInt model.player.health) ]
        , div [] [ text ("Enemy Health: " ++ String.fromInt model.enemy.health) ]
        , button [ onClick PlayerAttack, disabled (not model.isPlayerTurn || model.player.health <= 0 || model.enemy.health <= 0) ] [ text "Attack" ]
        , button [ onClick EnemyAttack, disabled (model.isPlayerTurn || model.player.health <= 0 || model.enemy.health <= 0) ] [ text "Continue" ]
        , button [ onClick NewGame ] [ text "New Game" ]
        ]
