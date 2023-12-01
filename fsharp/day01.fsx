open System
open System.Text.RegularExpressions

let toInt (c: char) = int c - int '0'

let data = IO.File.ReadAllLines "data/day01.txt"

#time "on"

data
|> Array.sumBy (fun chars ->
    let first = chars |> Seq.find Char.IsNumber |> toInt
    let last = chars |> Seq.findBack Char.IsNumber |> toInt
    10 * first + last)
|> printfn "Part 1: %i"

let numbers =
    [| "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine" |]

data
|> Array.sumBy (fun line ->
    let numbers =
        seq line
        |> Seq.indexed
        |> Seq.choose (function
            | i, c when Char.IsNumber(c) -> Some(toInt c)
            | i, c ->
                let substr = line.Substring(i)

                numbers
                |> Array.tryFindIndex (fun num -> substr.StartsWith(num))
                |> Option.map ((+) 1))

    10 * (Seq.head numbers) + (Seq.last numbers))
|> printfn "Part 2: %i"

#time "off"
