open Sudoku

[<EntryPoint>]
let main argv =
    let fileName = argv.[0] in
    let sudoku = System.Environment.CurrentDirectory + "/" + fileName |> Sudoku.readSudokuFromFile in
    let input = Sudoku.string sudoku in
    printfn "Input:\n%s\n" input;
    let stopWatch = System.Diagnostics.Stopwatch.StartNew();
    let ouput = match sudoku |> Sudoku.solve with
                | None -> "Sudoku infeasible; no solution."
                | Some sol -> sol |> Sudoku.string
    in
    stopWatch.Stop();
    printfn "Output:\n%s\n" ouput;
    printfn "Time to solve: %.3fs" (float stopWatch.ElapsedMilliseconds / 1000.0);
    0 // return an integer exit code
