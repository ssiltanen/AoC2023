let data = System.IO.File.ReadAllLines "data/day03.txt"
let height = Array.length data
let width = data[0].Length

let adjacentToSymbol y x length =
    (([ x - 1 .. x + length ] |> List.where (fun i -> i >= 0 && i < width)),
     ([ y - 1 .. y + 1 ] |> List.where (fun i -> i >= 0 && i < height)))
    ||> List.allPairs
    |> List.exists (fun (x, y) ->
        let c = data[y][x]
        c <> '.' && System.Char.IsDigit c |> not)

let rec part1 (acc: int) (cursor: int) (y: int) (row: string) =
    if row = "" then
        acc
    elif System.Char.IsDigit row[0] then
        let num = row |> Seq.takeWhile System.Char.IsDigit |> System.String.Concat
        let adj = adjacentToSymbol y cursor num.Length
        part1 (if adj then acc + int num else acc) (cursor + num.Length) y row[num.Length ..]
    else
        let nonNum =
            row |> Seq.takeWhile (System.Char.IsDigit >> not) |> System.String.Concat

        part1 acc (cursor + nonNum.Length) y row[nonNum.Length ..]

data |> Array.mapi (part1 0 0) |> Array.sum |> printfn "Part 1: %i"
