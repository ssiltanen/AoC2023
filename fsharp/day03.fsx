let data = System.IO.File.ReadAllLines "data/day03.txt"

#time

let adjacent (x, y) =
    ([ x - 1 .. x + 1 ], [ y - 1 .. y + 1 ])
    ||> List.allPairs
    |> List.where ((<>) (x, y))
    |> set

type Symbol = { value: char; coord: int * int }

type Number = { value: int; coords: (int * int) Set }

let rec parse (numbers: Number list, symbols: Symbol list) (cursor: int) (y: int) (row: string) =
    if row = "" then
        numbers, symbols
    elif System.Char.IsDigit row[0] then
        let num = row |> Seq.takeWhile System.Char.IsDigit |> System.String.Concat

        let coords =
            [ cursor .. cursor + num.Length - 1 ] |> List.map (fun x -> x, y) |> set

        parse ({ value = int num; coords = coords } :: numbers, symbols) (cursor + num.Length) y row[num.Length ..]
    elif row[0] = '.' then
        let dots = row |> Seq.takeWhile ((=) '.') |> Seq.length
        parse (numbers, symbols) (cursor + dots) y row[dots..]
    else
        parse (numbers, { value = row[0]; coord = cursor, y } :: symbols) (cursor + 1) y row[1..]

let parsed = data |> Array.mapi (parse ([], []) 0)
let numbers = parsed |> Array.collect (fst >> List.toArray)
let symbols = parsed |> Array.collect (snd >> List.toArray)
let symbolCoords = symbols |> Array.map _.coord |> set

let part1 =
    numbers
    |> Array.where (
        _.coords
        >> Set.exists (adjacent >> Set.intersect symbolCoords >> Set.isEmpty >> not)
    )
    |> Array.sumBy _.value

let part2 =
    symbols
    |> Array.where (_.value >> (=) '*')
    |> Array.sumBy (
        _.coord
        >> adjacent
        >> fun adj ->
            let adjNum =
                numbers
                |> Array.where (_.coords >> Set.intersect adj >> Set.isEmpty >> not)
                |> Array.map _.value

            if Array.length adjNum = 2 then
                Array.reduce (*) adjNum
            else
                0
    )

part1 |> printfn "Part 1: %i"
part2 |> printfn "Part 2: %i"

#time
