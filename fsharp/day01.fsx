let data = System.IO.File.ReadAllLines "data/day01.txt"

#time

let inline toInt (c: char) = int c - int '0'

let part1 str =
    let first = Seq.find System.Char.IsAsciiDigit >> toInt
    let last = Seq.findBack System.Char.IsAsciiDigit >> toInt
    10 * (first str) + (last str)

let numbers =
    [| "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine" |]

let part2 (str: string) =
    let matches =
        str
        |> Seq.indexed
        |> Seq.choose (function
            | _, c when System.Char.IsAsciiDigit c -> Some(toInt c)
            | i, _ ->
                let substr = str.Substring(i)

                numbers |> Array.tryFindIndex substr.StartsWith |> Option.map ((+) 1))

    10 * (Seq.head matches) + (Seq.last matches)

data |> Array.sumBy part1 |> printfn "Part 1: %i"
data |> Array.sumBy part2 |> printfn "Part 2: %i"

#time
