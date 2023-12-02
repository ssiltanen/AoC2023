#time

open System

let parseGame (game: string) =
    let id :: sets =
        game[5..].Split([| ": "; ", "; "; " |], StringSplitOptions.RemoveEmptyEntries)
        |> Array.toList

    int id, sets |> List.toArray |> Array.map _.Replace(" ", "")

let games = IO.File.ReadAllLines "data/day02.txt" |> Array.map parseGame

let count = Seq.takeWhile Char.IsDigit >> String.Concat >> int
let color = Seq.skipWhile Char.IsDigit >> String.Concat

let limits = dict [| "red", 12; "green", 13; "blue", 14 |]

let part1 (id: int, game: string[]) =
    game
    |> Array.forall (fun cubes -> count cubes <= limits.Item(color cubes))
    |> fun isPossible -> if isPossible then id else 0

let part2 =
    Array.groupBy color
    >> Array.map (snd >> Array.map count >> Array.max)
    >> Array.reduce (*)

games |> Array.sumBy part1 |> printfn "Part 1: %i"
games |> Array.sumBy (snd >> part2) |> printfn "Part 2: %i"

#time
