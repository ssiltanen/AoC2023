open System

#time

let hits =
    System.IO.File.ReadAllLines "data/day04.txt"
    |> Array.map (fun game ->
        let num = game.Split([| "Card"; ":"; " " |], StringSplitOptions.RemoveEmptyEntries)
        let lhs, rhs = num |> Array.splitAt (Array.IndexOf(num, "|"))
        Set.intersect (set (Array.tail lhs)) (set rhs) |> Set.count)

hits |> Array.sumBy (fun count -> pown 2 (count - 1)) |> printfn "Part 1: %i"

hits
|> Array.indexed
|> Array.fold
    (fun acc (i, count) ->
        [ i + 1 .. i + count ]
        |> List.iter (fun j -> Array.tryItem j acc |> Option.iter (fun inst -> acc[j] <- inst + acc[i]))

        acc)
    (Array.create (Array.length hits) 1)
|> Array.sum
|> printfn "Part 2: %i"

#time
