module Main exposing (main)

import Browser
import Form exposing (..)
import Form.View
import Html exposing (Html)

type Msg
    = FormChanged (Form.View.Model InputValues)
    | Signup UserDetails


type alias Model =
    Form.View.Model InputValues


type Plan
    = Basic
    | Pro
    | Enterprise


type Email
    = Email String


type Password
    = Password String


type alias InputValues =
    { name : String
    , email : String
    , password : String
    , repeatPassword : String
    , plan : String
    , agreedToTerms : Bool
    }


type alias UserDetails =
    { name : Maybe String
    , email : Email
    , password : Password
    , plan : Plan
    }


init =
    Form.View.idle
        { name = ""
        , email = ""
        , password = ""
        , repeatPassword = ""
        , plan = "Pro"
        , agreedToTerms = False
        }


nameField =
    Form.textField
        { parser = Ok
        , value = .name
        , update = \value values -> { values | name = value }
        , error = always Nothing
        , attributes =
            { label = "Name"
            , placeholder = "Your name"
            }
        }


parseEmail s =
    if String.contains "@" s then
        Ok <| Email s

    else
        Err "Invalid email"


emailField =
    Form.emailField
        { parser = parseEmail
        , value = .email
        , update = \value values -> { values | email = value }
        , error = always Nothing
        , attributes =
            { label = "Email"
            , placeholder = "you@example.com"
            }
        }


parsePassword s =
    if String.length s >= 6 then
        Ok <| Password s

    else
        Err "Password must be at least 6 characters"


passwordField =
    Form.passwordField
        { parser = parsePassword
        , value = .password
        , update = \value values -> { values | password = value }
        , error = always Nothing
        , attributes =
            { label = "Password"
            , placeholder = "Your password"
            }
        }


repeatPasswordField =
    Form.meta
        (\values ->
            Form.passwordField
                { parser =
                    \value ->
                        if value == values.password then
                            Ok ()

                        else
                            Err "The passwords must match"
                , value = .repeatPassword
                , error = always Nothing
                , update =
                    \newValue values_ ->
                        { values_ | repeatPassword = newValue }
                , attributes =
                    { label = "Repeat password"
                    , placeholder = "Repeat password"
                    }
                }
        )


parsePlan s =
    case s of
        "Basic" ->
            Ok Basic

        "Pro" ->
            Ok Pro

        "Enterprise" ->
            Ok Enterprise

        _ ->
            Err "Invalid plan"


planSelector =
    Form.selectField
        { parser = parsePlan
        , value = .plan
        , update = \value values -> { values | plan = value }
        , error = always Nothing
        , attributes =
            { label = "Choose a plan"
            , placeholder = "Choose a plan"
            , options =
                [ ( "Basic", "Basic" )
                , ( "Pro", "Pro" )
                , ( "Enterprise", "Enterprise" )
                ]
            }
        }


termsCheckbox =
    Form.checkboxField
        { parser =
            \value ->
                if value then
                    Ok ()

                else
                    Err "You must accept the terms"
        , value = .agreedToTerms
        , update = \value values -> { values | agreedToTerms = value }
        , error = always Nothing
        , attributes =
            { label = "I agree to terms and conditions" }
        }


form : Form InputValues UserDetails
form =
    Form.succeed
        (\name email password plan _ ->
            UserDetails name email password plan
        )
        |> Form.append (Form.optional nameField)
        |> Form.append emailField
        |> Form.append
            (Form.succeed (\password _ -> password)
                |> Form.append passwordField
                |> Form.append repeatPasswordField
                |> Form.group
            )
        |> Form.append planSelector
        |> Form.append termsCheckbox


update : Msg -> Model -> Model
update msg model =
    case msg of
        FormChanged newForm ->
            newForm

        Signup _ ->
            model


view : Model -> Html Msg
view model =
    Form.View.asHtml
        { onChange = FormChanged
        , action = "Sign up"
        , loading = "Signing up"
        , validation = Form.View.ValidateOnSubmit
        }
        (Form.map Signup form)
        model


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }