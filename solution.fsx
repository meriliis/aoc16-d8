open System
open System.Text.RegularExpressions

let path = System.IO.Path.Combine(__SOURCE_DIRECTORY__,"input.txt")
let input = System.IO.File.ReadAllLines path

let screen = Array.init 6 (fun i -> Array.init 50 (fun j -> 0))

type Instruction =
    | Rect   of (int * int)
    | Rotate of (string * int * int)

let instructions = 
    input |> Array.map (fun s -> let cmd = s.Split()
                                 let nums = Regex.Matches(s, "\d+") 
                                            |> Seq.cast<Match> 
                                            |> Seq.map (fun m -> Int32.Parse(m.Value)) 
                                            |> List.ofSeq
                                 let (A, B) = (nums.Item(0), nums.Item(1))
                                 if cmd.[0] = "rect" then 
                                    Rect (A, B) 
                                 else 
                                    Rotate (cmd.[1], A, B))
          |> List.ofArray

let rotate (n : int) (arr : int array) = 
    arr |> Array.permute (fun i -> (i + n) % arr.Length)    

let rec applyInstructions (instructions : Instruction list) (screen : int array array) =
    match instructions with
    | []           -> screen
    | head :: tail -> match head with
                      | Rect (A, B)      -> screen |> Array.mapi (fun i row -> if i < B then Array.fill row 0 A 1)
                                            applyInstructions (instructions.[1..]) screen
                      | Rotate (s, A, B) -> if s = "row" then 
                                                let newScreen = screen 
                                                                |> Array.mapi (fun i row -> if i = A then rotate B row else row)
                                                applyInstructions (instructions.[1..]) newScreen
                                            else 
                                                let newCol = screen 
                                                             |> Array.map (fun row -> row.[A]) 
                                                             |> rotate B
                                                let newScreen = screen 
                                                                |> Array.mapi (fun i row -> row 
                                                                                            |> Array.mapi (fun j pix -> if j = A then newCol.[i] else pix))
                                                applyInstructions (instructions.[1..]) newScreen

let finalScreen = screen |> applyInstructions instructions
let noOfLitPixels = finalScreen |> Array.map (fun row -> Array.sum row) |> Array.sum
let finalScreen' = finalScreen 
                   |> Array.map (fun row -> row |> Array.map (fun pix -> if pix = 1 then "#" else ".") |> Array.fold (fun acc pix -> acc + pix) "")
                   |> Array.fold (fun acc row -> acc + row + "\n") ""

printfn "Number of lit pixels: %d" noOfLitPixels
printfn "Final screen: \n%s" finalScreen'