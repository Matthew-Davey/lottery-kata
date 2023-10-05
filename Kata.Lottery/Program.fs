open System
open System.Diagnostics
open System.IO
open System.Runtime.InteropServices
open System.Runtime.Intrinsics.X86
open FSharp.NativeInterop

let inline (|||) (a: uint64) (b: uint64) = (# "or" a b : uint64 #)
let inline (&&&) (a: uint64) (b: uint64) = (# "and" a b : uint64 #)
let inline (<<<) a b = (# "shl" a b : uint64 #)

let loadTickets (filePath: string) =
    // Load the contents of the file into native memory. Unfortunately there is too much data to use stack memory...
    let fileStream = new FileStream(path = filePath, mode = FileMode.Open, access = FileAccess.Read, share = FileShare.None, bufferSize = 4096, options = FileOptions.SequentialScan)
    let fileContent = Marshal.AllocHGlobal(int fileStream.Length)
    let fileSpan = Span(fileContent.ToPointer(), int fileStream.Length)
    fileStream.ReadExactly(fileSpan)
    
    let mutable index = 0

    // Allocate a region of memory large enough to hold all our tickets...
    let tickets = Marshal.AllocHGlobal(sizeof<uint64> * 1_000_000)
    let mutable ptr = NativePtr.ofNativeInt tickets
    
    for i in 0..999_999 do
        let mutable ticket = 0uL
        
        // Read the first 5 numbers of the ticket.
        // Read two bytes at a time. We know that the first byte will be a digit (we derive the digit value by
        // subtracting 48, which is the offset of the digits in the ASCII table). If the second byte is a comma, the
        // number is the value of the first digit. If the second byte is another digit, the number is the value of the
        // first digit * 10, plus the value of the second digit. In this case we also know that the following byte will
        // definitely be a comma, so we can skip over that.
        // For each number we deduce, we flip the corresponding bit in the ticket...
        for _ in 0..4 do
            let a, b = fileSpan[index], fileSpan[index + 1]
            index <- index + 2

            if b = 44uy then
                ticket <- ticket ||| (1uL <<< (a - 48uy))
            else
                ticket <- ticket ||| (1uL <<< (((a - 48uy) * 10uy) + (b - 48uy)))
                index <- index + 1
        
        // Read the 6th number of the ticket. The algorithm is the same except we check for a newline character instead
        // of a comma...        
        let a, b = fileSpan[index], fileSpan[index + 1]
        index <- index + 2
        
        if b = 10uy then
            ticket <- ticket ||| (1uL <<< (a - 48uy)) 
        else
            ticket <- ticket||| (1uL <<< (((a - 48uy) * 10uy) + (b - 48uy))) 
            index <- index + 1
        
        NativePtr.set ptr i ticket
            
    tickets
    
let countWins x0 x1 x2 x3 x4 x5 (tickets: nativeint) =
    // For this algorithm the draw must be expressed in the same way as the tickets. That is, for each number, the
    // corresponding bit must be flipped in a 64 bit integer...
    let draw = (1uL <<< x0) ||| (1uL <<< x1) ||| (1uL <<< x2) ||| (1uL <<< x3) ||| (1uL <<< x4) ||| (1uL <<< x5)
    
    let mutable match5 = 0
    let mutable match4 = 0
    let mutable match3 = 0
    
    let mutable ptr = NativePtr.ofNativeInt tickets
    
    for i in 0..999_999 do
        // Count the number of matching numbers by performing a bitwise AND between the draw and the ticket. The
        // resulting value will have positive bits only where there was a match between the draw and the ticket. We use
        // the SSE4.2 pop count instruction to count the number of populated bits. Keep a record of how many tickets
        // matched 3, 4 or 5 numbers...
        let ticket = NativePtr.get ptr i
        let matches = Popcnt.X64.PopCount (draw &&& ticket)
        if matches = 5uL then match5 <- match5 + 1
        if matches = 4uL then match4 <- match4 + 1
        if matches = 3uL then match3 <- match3 + 1
    
    [(5, match5); (4, match4); (3, match3)]

let timer = Stopwatch.StartNew()
let tickets = loadTickets "./Tickets1M.csv"
timer.Stop()

printfn $"1,000,000 rows parsed in %i{timer.ElapsedMilliseconds} milliseconds"
printfn "(17ms to beat)\n"
   
timer.Restart()
let wins = countWins 5 10 20 34 42 57 tickets
timer.Stop()

// expect 7, 425, 10615
wins |> List.iter (fun (matches, count) -> printfn $"tickets with %i{matches} winning number(s): %i{count}")

printfn $"1,000,000 tickets checked in %i{timer.ElapsedMilliseconds} milliseconds"
printfn "(0 milliseconds to beat)"

Marshal.FreeHGlobal(tickets)
