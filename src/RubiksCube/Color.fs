module RubiksCube.Color

open System

type Color =
    | Red
    | Orange
    | White
    | Yellow
    | Green
    | Blue

let consoleColor(color: Color) =
    match color with
    | Red    -> ConsoleColor.Red
    | Orange -> ConsoleColor.DarkYellow
    | White  -> ConsoleColor.White
    | Yellow -> ConsoleColor.Yellow
    | Green  -> ConsoleColor.Green
    | Blue   -> ConsoleColor.Blue

let backgroundColor(color: Color) =
    match color with
    | Red    -> "red"
    | Orange -> "orange"
    | White  -> "white"
    | Yellow -> "yellow"
    | Green  -> "green"
    | Blue   -> "blue"

let colorChar(color: Color) =
    match color with
    | Red    -> "R"
    | Orange -> "O"
    | White  -> "W"
    | Yellow -> "Y"
    | Green  -> "G"
    | Blue   -> "B"