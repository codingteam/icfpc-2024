open System
open System.IO

let readLines (path: string) = File.ReadLines path

let toPoint (line: string) =
    let [| x; y |] = line.Split(" ")
    (int x, int y)

let clamp x = 
    if x > 1 then 1
    elif x < -1 then -1
    else x

let diffToCmd (diff: (int * int)) =
    match diff with
    | -1, -1 -> 1
    | 0, -1 -> 2
    | 1, -1 -> 3
    | -1, 0 -> 4
    | 0, 0 -> 5
    | 1, 0 -> 6
    | -1, 1 -> 7
    | 0, 1 -> 8
    | 1, 1 -> 9
    | _ -> failwith "impossible"

let solve (points: (int * int) seq): int array =
    let points = points |> Seq.toArray

    let mutable x = 0
    let mutable y = 0
    let mutable vx = 0
    let mutable vy = 0

    let cmds = ResizeArray()
    
    let mutable pointInd = 0
    while pointInd < points.Length do
        let (px, py) = points[pointInd]

        let vx' = clamp (px - x)
        let vy' = clamp (py - y)

        let dvx = clamp (vx' - vx)
        let dvy = clamp (vy' - vy)

        vx <- vx + dvx
        vy <- vy + dvy
        x <- x + vx
        y <- y + vy

        cmds.Add (diffToCmd (dvx, dvy))

        if x = px && y = py then
            pointInd <- pointInd + 1

    cmds.ToArray()

[<EntryPoint>]
let main args =
    match args with
    | [| inputPath; outputPath |] ->
        let points =
            readLines inputPath
            |> Seq.map toPoint

        let solution = 
            solve points
            |> Seq.map string
            |> String.concat ""

        File.WriteAllText(outputPath, solution)

    | _ -> 
        printfn "invalid args"

    0