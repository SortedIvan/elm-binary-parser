module BinaryParser exposing (..)
import Debug exposing (..)
import Html exposing (Html)
import Parser exposing (..)
import Set exposing (..)

type Tree
    = Nil
    | Node Int Tree Tree
 
swapElement : Tree -> Int -> Tree -> Tree
swapElement left val right = 
    Node val left right

nilElement : Int -> Tree
nilElement val = 
    Node val Nil Nil

treeParser : Parser Tree
treeParser =
  oneOf
    [
    succeed swapElement
        |. symbol "["
        |= lazy (\_ -> treeParser)
        |. symbol ","
        |= int
        |. symbol ","
        |= lazy (\_ -> treeParser)
        |. symbol "]"
    ,succeed nilElement
        |= int
    ,succeed Nil
        |. keyword "x"
    ]


userInputParser : Parser Tree
userInputParser =
    succeed identity
        |= treeParser
        |. end
        
binary = Parser.run userInputParser "[1,9,3]"
binary2 = Parser.run userInputParser "[1,9,x]"
binary3 = Parser.run userInputParser "[[1,2,3],4,[5,6,7]]"
binary4 = Parser.run userInputParser "9"
binary5 = Parser.run userInputParser "[1,3,5]"

myTree: Tree
myTree =
    Node 1 Nil Nil


my_results: List String
my_results =
    [
        pr <| binary,
        pr <| binary2,
        pr <| binary3,
        pr <| binary4,
        pr <| binary5,
        pr <| myTree
    ] 
    
-- Boiler-plate below:

-- update this values for long output lines, or small browser windows
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

pr = Debug.toString

main: Html msg
main = Html.pre 
        []
        (List.map to_div my_results)
    
