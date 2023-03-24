open System.Diagnostics
open System.IO
open System.Runtime.Intrinsics.X86

let inline createBitVector64 [|x0; x1; x2; x3; x4; x5|] =
    (1uL <<< x0) ||| (1uL <<< x1) ||| (1uL <<< x2) ||| (1uL <<< x3) ||| (1uL <<< x4) ||| (1uL <<< x5)

let timer = Stopwatch.StartNew()
let tickets = File.ReadAllLines (__SOURCE_DIRECTORY__ + "/../Tickets1M.csv")
              |> Array.Parallel.map (fun line -> line.Split ',' |> Array.map int32)
timer.Stop()
printfn $"%i{Array.length tickets} rows parsed in %i{timer.ElapsedMilliseconds} milliseconds\n"

let inline countWinningNumbers draw ticket =
    let mask = createBitVector64 ticket
    let winningNumbers = draw &&& mask
    Popcnt.X64.PopCount winningNumbers |> uint8

timer.Restart()
let draw = createBitVector64 [|5; 10; 20; 34; 42; 57|]
let matches = tickets
              |> Array.map (countWinningNumbers draw)
              |> Array.filter ((<=) 3uy)
              |> Array.countBy id
timer.Stop()

matches
    |> Array.sortByDescending fst
    |> Array.iter (fun (matches, count) -> printfn $"tickets with %i{matches} winning number(s): %i{count}")

printfn $"\n%i{Array.length tickets} tickets checked in %i{timer.ElapsedMilliseconds} milliseconds\n"

let inline combinations (xs: int32[]) =
    let length = Array.length xs
    [| for i in 0..(length - 3) do
       for j in (i + 1)..(length - 2) do
       for k in (j + 1)..(length - 1) -> (1uL <<< xs[i]) ||| (1uL <<< xs[j]) ||| (1uL <<< xs[k]) |]

timer.Restart()
let combinationCounts =
    tickets
    |> Array.Parallel.collect combinations
    |> Array.countBy id
timer.Stop()

combinationCounts
|> Array.filter (fun (_, count) -> count >= 700)
|> Array.sortByDescending snd
|> Array.iter (fun (combination, count) -> printfn $"%064B{combination} occurred %i{count} times")

printfn $"\nmost common combinations calculated in %i{timer.ElapsedMilliseconds} milliseconds"
