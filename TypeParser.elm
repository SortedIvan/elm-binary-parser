module TypeParser exposing (..)
import Html exposing (Html)
import Html.Attributes
import Html exposing (Html, text)
import Html exposing (s)
import Html.Attributes exposing (list)
import Html exposing (b)
import Tuple exposing (..)
import String exposing (fromFloat)
import Html.Events exposing (custom)
import Bitwise exposing (or)
import Html exposing (a)
import Set
import Parser exposing (..)
import Char
import Json.Decode as Decode exposing (Decoder)
import Parser exposing (..)
import Set
import String


type Type
  = Var String
  | Implies Type Type
  | Or Type Type
  | And Type Type
  | Not Type
  | Equal Type Type
  | Tuple (List Type)
  | Type String (List Type)
  | Record (List (String, Type)) (Maybe String)


decoder : Decoder Type
decoder =
  Decode.andThen decoderHelp Decode.string


decoderHelp : String -> Decoder Type
decoderHelp string =
  case parse string of
    Err error ->
      Decode.fail "TODO"

    Ok actualType ->
      Decode.succeed actualType



-- PARSE TYPES


parse : String -> Result (List DeadEnd) Type
parse source =
  Parser.run tipe source



-- FUNCTIONS


tipe : Parser Type
tipe =
  lazy <| \_ ->
    andThen tipeHelp tipeTerm


tipeHelp : Type -> Parser Type
tipeHelp t =
  oneOf
    [ map (Implies t) implesType,
      map (And t) andType,
      map (Or t) orType,
      map (Equal t) equalType
    , succeed t
    ]

--remove if necessary
andType : Parser Type
andType =
  succeed identity
    |. backtrackable spaces
    |. and
    |. spaces
    |= tipe

orType : Parser Type
orType =
  succeed identity
    |. backtrackable spaces
    |. or
    |. spaces
    |= tipe

equalType : Parser Type
equalType =
  succeed identity
    |. backtrackable spaces
    |. equal
    |. spaces
    |= tipe


implesType : Parser Type
implesType =
  succeed identity
    |. backtrackable spaces
    |. arrow
    |. spaces
    |= tipe


notType : Parser Type
notType =
  succeed identity
    |. backtrackable spaces
    |. not
    |. spaces
    |= tipe



arrow : Parser ()
arrow =
  symbol "->"

and : Parser ()
and =
  symbol "&"

or : Parser ()
or =
  symbol "||"

not : Parser ()
not =
  symbol "N"

equal : Parser ()
equal =
  symbol "="

tipeTerm : Parser Type
tipeTerm =
  oneOf
    [ map Var lowVar
    , succeed Type
        |= qualifiedCapVar
        |= loop [] chompArgs
    , record
    , propositionsParser
    ]

--MAGIC
chompArgs : List Type -> Parser (Step (List Type) (List Type))
chompArgs revArgs =
  oneOf
    [ succeed identity
        |. backtrackable spaces
        |= term
        |> map (\arg -> Loop (arg :: revArgs))
    , map (\_ -> Done (List.reverse revArgs)) (succeed ())
    ]


term : Parser Type
term =
  oneOf
    [ map Var lowVar
    , map (\name -> Type name []) qualifiedCapVar
    , record
    , propositionsParser
    ]



-- RECORDS


record : Parser Type
record =
  succeed (\ext fs -> Record fs ext)
    |. symbol "{"
    |. spaces
    |= extension
    |= recordEnd


extension : Parser (Maybe String)
extension =
  oneOf
    [ succeed Just
        |= backtrackable lowVar
        |. backtrackable spaces
        |. symbol "|"
        |. spaces
    , succeed Nothing
    ]



field : Parser (String, Type)
field =
  succeed Tuple.pair
    |= lowVar
    |. spaces
    |. symbol ":"
    |. spaces
    |= tipe


type alias Fields = List (String, Type)


recordEnd : Parser Fields
recordEnd =
  oneOf
    [ field
        |. spaces
        |> andThen (\f -> loop [f] recordEndHelp)
    , succeed []
        |. symbol "}"
    ]


recordEndHelp : Fields -> Parser (Step Fields Fields)
recordEndHelp revFields =
  oneOf
    [ succeed (\f -> Loop (f :: revFields))
        |. comma
        |. spaces
        |= field
        |. spaces
    , succeed (\_ -> Done (List.reverse revFields))
        |= symbol "}"
    ]



-- TUPLE


propositionsParser : Parser Type
propositionsParser =
  map tuplize <|
    sequence
      { start = "("
      , separator = ","
      , end = ")"
      , spaces = spaces
      , item = tipe
      , trailing = Forbidden
      }



tuplize : List Type -> Type
tuplize args =
  case args of
    [arg] ->
      arg

    _ ->
      Tuple args



-- VAR HELPERS


lowVar : Parser String
lowVar =
  var Char.isLower


capVar : Parser String
capVar =
  var Char.isUpper


isInnerVarChar : Char -> Bool
isInnerVarChar char =
  Char.isAlphaNum char || char == '_'


qualifiedCapVar : Parser String
qualifiedCapVar =
  getChompedString <|
    capVar
      |. loop () qualifiedCapVarHelp

qualifiedCapVarHelp : () -> Parser (Step () ())
qualifiedCapVarHelp _ =
  oneOf
    [ succeed (Loop ())
        |. symbol "."
        |. capVar
    , succeed (Done ())
    ]


var : (Char -> Bool) -> Parser String
var isFirst =
  variable
    { start = isFirst
    , inner = isInnerVarChar
    , reserved = Set.empty
    }



-- HELPERS


spaces : Parser ()
spaces =
  chompWhile (\char -> char == ' ')


comma : Parser ()
comma =
  symbol "," 

        
my_results: List String
my_results =
    [ 
      "_________AND____PARSER____TEST__________",
      pr <| Parser.run propositionsParser "(a & b)",
      "_________IMPLIES____PARSER____TEST__________",  
      pr <| Parser.run propositionsParser "(a -> b)",
      "_________OR____PARSER____TEST__________",  
      pr <| Parser.run propositionsParser "(a || b)",
      "_________EQUAL____PARSER____TEST__________",  
      pr <| Parser.run propositionsParser "(a = b)",
      "_________((a -> (b || c)) = (d & e))__________",  
      pr <| Parser.run propositionsParser "((a -> (b || c)) = (d & e))",
      "___________(a = (a = ( a = (a = b))))",
      pr <| Parser.run propositionsParser "(a = (a = ( a = (a = b))))" 

    ] 
    
-- Boiler-plate below:
-- update this values for long output lines, or small browser windows
page_width : number
page_width = 1000

to_wrap: String -> String
to_wrap my_value =
    if (String.length my_value <= page_width) then
        (String.left page_width my_value)
    else
        (String.left page_width my_value) ++ ("\n") ++ to_wrap (String.dropLeft page_width my_value)

to_div: String -> Html msg
to_div my_value = 
    Html.div [] [(to_wrap my_value) |> Html.text]

pr : a -> String
pr = Debug.toString

main: Html msg
main = Html.pre 
        []
        (List.map to_div my_results)