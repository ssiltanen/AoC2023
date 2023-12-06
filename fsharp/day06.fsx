#time

let [| times; distances |] =
    System.IO.File.ReadAllLines "data/day06.txt"
    |> Array.map (_.Split([| "Time:"; "Distance:"; " " |], System.StringSplitOptions.RemoveEmptyEntries))

let waysToBeat (time: int64, dist: int64) =
    let test hold = dist / hold + hold < time
    let low = seq { 1L .. dist - 1L } |> Seq.find test
    let high = seq { low .. dist - 1L } |> Seq.find (test >> not)
    high - low

(Array.map int64 times, Array.map int64 distances)
||> Array.zip
|> Array.map waysToBeat
|> Array.reduce (*)
|> printfn "Part 1: %u"

(System.String.Concat times |> int64, System.String.Concat distances |> int64)
|> waysToBeat
|> printfn "Part 2: %u"

#time
