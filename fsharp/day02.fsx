#time

let parseGame (game: string) =
    let [| id; subsets |] = game[5..].Replace(" ", "").Split(":")
    int id, subsets.Split ";"

let games = System.IO.File.ReadAllLines "data/day02.txt" |> Array.map parseGame

let takeCubes = Seq.takeWhile System.Char.IsDigit >> System.String.Concat >> int
let takeColor = Seq.skipWhile System.Char.IsDigit >> System.String.Concat

let limits = [| "red", 12; "green", 13; "blue", 14 |]

let part1 (id: int, game: string[]) =
    game
    |> Array.forall (fun subset ->
        subset.Split ","
        |> Array.forall (fun cubeColor ->
            let color = takeColor cubeColor
            let cubes = takeCubes cubeColor

            limits |> Array.find (fst >> (=) color) |> snd |> (<=) cubes))
    |> fun isPossible -> if isPossible then id else 0

let part2: int * string[] -> int =
    snd
    >> Array.collect (fun subset ->
        let colors = subset.Split(",")

        let tryGetCubes color =
            colors
            |> Array.tryFind (takeColor >> (=) color)
            |> Option.map (fun cubes -> color, takeCubes cubes)

        [| tryGetCubes "red"; tryGetCubes "green"; tryGetCubes "blue" |]
        |> Array.choose id)
    >> Array.groupBy fst
    >> Array.map (snd >> Array.map snd >> Array.max)
    >> Array.reduce (*)

games |> Array.sumBy part1 |> printfn "Part 1: %i"
games |> Array.sumBy part2 |> printfn "Part 2: %i"

#time
