open System.Collections.Concurrent
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Runtime.Intrinsics.X86
open System.Threading.Tasks;

module Array =
    module Parallel =
        let collectn n fn arr =
            let length = Array.length arr
            let result = Array.zeroCreate (length * n)
            Parallel.For(0, length, (fun i ->
                fn arr[i] |> Seq.iteri (fun j value -> result[(i * n) + j] <- value)
            )) |> ignore
            result

        let countById (arr: uint32[]) =
            let map = ConcurrentDictionary<uint32, int32>()
            Parallel.ForEach(arr, fun x ->
                map.AddOrUpdate(x, 1, (fun _ value -> value + 1)) |> ignore
            ) |> ignore
            map
            |> Seq.map (fun kv -> (kv.Key, kv.Value))
            |> Array.ofSeq


let inline createBitVector64 [|x0; x1; x2; x3; x4; x5|] =
    (1uL <<< int x0) ||| (1uL <<< int x1) ||| (1uL <<< int x2) ||| (1uL <<< int x3) ||| (1uL <<< int x4) ||| (1uL <<< int x5)

let timer = Stopwatch.StartNew()
let tickets = File.ReadAllLines (__SOURCE_DIRECTORY__ + "/../Tickets1M.csv")
              |> Array.Parallel.map (fun line -> line.Split ',' |> Array.map uint8)
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

let inline combinations (xs: uint8[]) =
    let length = Array.length xs
    seq {
        for i in 0..(length - 3) do
        for j in (i + 1)..(length - 2) do
        for k in (j + 1)..(length - 1) do
            yield(uint32 xs[i] <<< 16) ||| (uint32 xs[j] <<< 8) ||| (uint32 xs[k])
    }

timer.Restart()
let combinationCounts =
    tickets
    |> Array.Parallel.collectn 20 combinations
    |> Array.Parallel.countById
timer.Stop()

combinationCounts
|> Array.filter (fun (_, count) -> count >= 700)
|> Array.sortByDescending snd
|> Array.iter (fun (combination, count) -> printfn $"%032B{combination} occurred %i{count} times")

printfn $"\nmost common combinations calculated in %i{timer.ElapsedMilliseconds} milliseconds"
