let data = System.IO.File.ReadAllLines "data/day03.txt"

#time

let height = Array.length data
let width = data[0].Length

let adjacentToSymbol y x length =
    (([ x - 1 .. x + length ] |> List.where (fun i -> i >= 0 && i < width)),
     ([ y - 1 .. y + 1 ] |> List.where (fun i -> i >= 0 && i < height)))
    ||> List.allPairs
    |> List.exists (fun (x, y) ->
        let c = data[y][x]
        c <> '.' && System.Char.IsDigit c |> not)

let adjacent (x, y) =
    ([ x - 1 .. x + 1 ], [ y - 1 .. y + 1 ])
    ||> List.allPairs
    |> List.where ((<>) (x, y))
    |> set

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

let rec parse (acc: ((int * int) Set * int) list) (cursor: int) (y: int) (row: string) =
    if row = "" then
        acc
    elif System.Char.IsDigit row[0] then
        let num = row |> Seq.takeWhile System.Char.IsDigit |> System.String.Concat

        let coords =
            [ cursor .. cursor + num.Length - 1 ] |> List.map (fun x -> x, y) |> set

        parse ((coords, int num) :: acc) (cursor + num.Length) y row[num.Length ..]

    else
        let nonNum =
            row |> Seq.takeWhile (System.Char.IsDigit >> not) |> System.String.Concat

        parse acc (cursor + nonNum.Length) y row[nonNum.Length ..]

let numbers = data |> Array.mapi (parse [] 0)

let part2 =
    data
    |> Array.mapi (fun y row -> row |> Seq.indexed |> Seq.where (snd >> (=) '*') |> Seq.map (fun (x, _) -> x, y))
    |> Array.collect Seq.toArray
    |> Array.sumBy (fun (astX, astY) ->
        let adj = adjacent (astX, astY)

        numbers
        |> Array.map (
            List.choose (fun (c, num) ->
                if Set.intersect adj c |> Set.isEmpty |> not then
                    Some num
                else
                    None)
        )
        |> Array.collect List.toArray
        |> fun arr -> if Array.length arr = 2 then Array.reduce (*) arr else 0)

data |> Array.mapi (part1 0 0) |> Array.sum |> printfn "Part 1: %i"
part2 |> printfn "Part 2: %i"

#time
