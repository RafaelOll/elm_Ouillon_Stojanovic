module Main exposing (..)
import Browser
import Http
import Html exposing (..)
import Html.Events exposing (onInput,onClick)
import Html.Attributes exposing (..)
import Http
import Random
import Task
import Json.Decode as Decode exposing  (..)


-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }



-- MODEL


type R
  = Failure String
  | Loading
  | Success (String, List Donnes,String)

type alias Model =
   {mot: String, 
   items:List String, 
   mottr: String,
   content: String,
   texto: String,
   compteur: Int,
   trouver: Bool,
   sucess:R
   }

init : () -> (Model, Cmd Msg)
init _ =
  ( Model "" []  "" "" "Text to reverse" 0 False Loading 
  , Http.get
      { url = "http://localhost:8000/mots.txt"
      , expect = Http.expectString GotText
      }
  )



-- UPDATE
type alias Donnes=
  { word:String
    , meaning: List Meaning
  }
type alias Meaning=
   { partOfSpeech : String
    , definitions : List Definition
    }
type alias Definition=
  {
    definition:String
  }

type Msg
  = GotText (Result Http.Error String)
  | Num Int
  | GotWord (Result Http.Error (List Donnes))
  | Affichermot String
  | Change String
  | Nouveau 
 

type Error
    = BadUrl String
    | Timeout
    | NetworkError
    | BadStatus Int
    | BadBody String
    


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotText result ->
      case result of
        Ok fullText ->
          let
            items = String.split " " fullText
          in
            ({ model | items = items  }, Random.generate Num (Random.int 1 999))
        Err _ ->
          ({model|sucess=Failure "error message"}, Cmd.none)
    Num num ->  
      let mot=Maybe.withDefault "" (List.head (List.drop num model.items))
      in ({model|mot=mot}, Http.get
          { url = "https://api.dictionaryapi.dev/api/v2/entries/en/" ++ mot
          , expect = Http.expectJson GotWord recupereJson})
      
    GotWord result ->
      case result of
        Ok web ->
          ({model | sucess = Success (model.mot, web, "")}, Cmd.none)

        Err error  ->
          ({model|sucess=Failure (errorToString error)}, Cmd.none)

    Affichermot r->
      let mottr=model.mot
      in ({model|mottr=mottr}, Cmd.none)
    
    Change newContent ->
      if String.toLower newContent/=String.toLower model.mot then
        ({model | content = newContent},  Cmd.none)
      else 
        ({model | content = newContent,compteur=(model.compteur+1)},  Cmd.none)


         
    Nouveau -> ({model|mottr="", content=""}, Random.generate Num (Random.int 1 999))

   





errorToString : Http.Error -> String
errorToString error =
    case error of
        Http.BadUrl url ->
            "The URL " ++ url ++ " was invalid"
        Http.Timeout ->
            "Unable to reach the server, try again"
        Http.NetworkError ->
            "Unable to reach the server, check your network connection"
        Http.BadStatus 500 ->
            "The server had a problem, try again later"
        Http.BadStatus 400 ->
            "Verify your information and try again"
        Http.BadStatus _ ->
            "Unknown error"
        Http.BadBody errorMessage ->
            errorMessage

recupereJson : Decode.Decoder (List Donnes)
recupereJson = Decode.list motDecoder

motDecoder : Decode.Decoder Donnes
motDecoder =
  map2 Donnes
        (field "word" string)
        (field "meanings" (Decode.list meaningDecoder))
meaningDecoder : Decode.Decoder Meaning
meaningDecoder =
    map2 Meaning
        (field "partOfSpeech" string)
        (field "definitions" (Decode.list definitionDecoder))
definitionDecoder : Decode.Decoder Definition
definitionDecoder =
    Decode.map Definition
        (field "definition" string)
-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW


view : Model -> Html Msg
view model =
  case model.sucess of
    Failure error ->
      text ("I was unable to load your book."++error)

    Loading ->
      text "Loading..."

    Success (mot, result, r) ->
      div [] [
        h1 [] [ greetingtitle ],
        div [] ( convertData result),
        div [] [ text (model.mottr)],
        div []
          [ input [ placeholder model.texto, Html.Attributes.value model.content, onInput Change ] []
          , 
          if String.toLower model.content/=String.toLower model.mot then 
            greetingpastrouve

          else 
            reponseJuste

          ],
        div [] [convert(button [ onClick (Affichermot)] [ text "Afficher la réponse car je ne suis pas très fort" ])],
        div [] [button [ onClick (Nouveau)] [ text "Nouveau mot" ] ],
        text ("Nombre de mot trouvé: "++(String.fromInt model.compteur))
        


      ]

reponseJuste : Html Msg
reponseJuste =
  div
    [ style "color" "green"
    , style "height" "20px"
    , style "width" "100%"
    ]
    [ text "Mot trouvé"
    ]

greetingtitle : Html Msg
greetingtitle =
  div
    [ style "color" "pink"
    , style "height" "50px"
    , style "width" "100%"
    , style "text-align" "center"
    ]
    [ text "Play with Rafael & Maëlle!  <3 <3 <3"
    ]

greetingpastrouve : Html Msg
greetingpastrouve =
  div
    [ style "color" "red"
    , style "height" "20px"
    , style "width" "100%"
    ]
    [ text "Pas encore trouvé, tu vas y arriver <3"
    ]

greetingtrouve : Html Msg
greetingtrouve =
  div
    [ style "color" "green"
    , style "height" "20px"
    , style "width" "100%"
    ]
    [ text "Pas encore trouvé, tu vas y arriver <3"
    ]

convert : Html (String -> Msg) -> Html Msg
convert html =
    Html.map (\toMsg -> toMsg "") html

convertData : List Donnes -> List (Html Msg)
convertData donnees =
    List.map (\donne -> convertDonne donne) donnees

convertDonne : Donnes -> Html Msg
convertDonne donne =
    let
        word = donne.word
        meanings = donne.meaning
    in
        div []
            [ --h2 [] [text word]
             ul [] (List.map convertMeaning meanings)
            ]

convertMeaning : Meaning -> Html Msg
convertMeaning meaning =
    let
        partOfSpeech = meaning.partOfSpeech
        definitions = meaning.definitions
    in
        li []
            [ h3 [] [text partOfSpeech]
            , ul [] (List.map convertDefinition definitions)
            ]

convertDefinition : Definition -> Html Msg
convertDefinition definition =
    li [] [text definition.definition]
    

