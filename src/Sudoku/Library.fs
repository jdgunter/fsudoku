namespace Sudoku

open FSharpx.Collections

(** A cell in a sudoku puzzle. May be Empty, meaning that there are no possible
    values the cell can take on, Fixed, meaning that the is a single possible value
    the cell may contain, or Unfixed, meaning that there are multiple possibilites
    left.
*)
type Cell =
| Empty
| Fixed of value : int
| Unfixed of options : Set<int>

(** A sudoku grid. Modelled as a 2D array of cells. *)
type Grid = Cell [,]

(** A position in the grid. *)
type Position = {
    Row : int
    Col : int
}


(** Module containing functions for constructing and solving sudoku puzzles. *)
module Sudoku =

    (** A completely unconstrained cell. *)
    let mkUnConstrainedCell =
        Unfixed (Set(seq { 1..9 }))

    (** A completely unconstrained grid with no given information. *)
    let mkDefaultGrid =
        Array2D.init 9 9 (fun _ _ -> mkUnConstrainedCell)

    (** Get the cell at a position in the grid. *)
    let getCellAt pos (grid: Grid) =
        let i = pos.Row in
        let j = pos.Col in
        grid.[i,j]

    (** Fix a position to a given value in a sudoku grid. Mutates the grid. *)
    let mutateFixPosition pos value (grid: Grid) =
        grid.[pos.Row, pos.Col] <- Fixed value

    (** Fix a position to a given value in a sudoku grid without mutation. *)
    let fixPosition pos value (grid: Grid) =
        let fix i j =
            if i = pos.Row && j = pos.Col then
                Fixed value
            else
                grid.[i,j]
        Array2D.init 9 9 fix

    (** Make a sudoku grid with some of the cells filled in. *)
    let mkSudoku (givens: List<int * Position>) =
        let fix grid given = 
            let position = snd given in
            let value = fst given in
            fixPosition position value grid
        let currentGrid = Array2D.copy mkDefaultGrid in
        for given in givens do
            let value = fst given in
            let row = (snd given).Row in
            let col = (snd given).Col in 
            currentGrid.[row, col] <- Fixed value
        currentGrid

    (** Parse a sudoku puzzle from a string. *)
    let parseSudoku (sudokuString: string) =
        let arrayOfGivenChars = sudokuString.Split( [|','|] ) in
        let firstPos = { Row=0; Col=0 } in
        let nextPos pos = 
            if pos.Col = 8 then 
                { Row=pos.Row+1; Col=0 } 
            else 
                { Row=pos.Row; Col=pos.Col + 1 }
        let mkOptionalGiven charString (pos: Position) = 
            if charString = "" then
                None
            else
                Some (System.Int32.Parse(charString), pos) 
        let rec mkGivens arrayOfGivenChars i currentPos givensAcc =
            if i = (Array.length arrayOfGivenChars) then
                givensAcc
            else
                let nextGivenChar = Array.get arrayOfGivenChars i in
                match (mkOptionalGiven nextGivenChar currentPos) with
                | Some given -> mkGivens arrayOfGivenChars (i+1) (nextPos currentPos) (given :: givensAcc)
                | None -> mkGivens arrayOfGivenChars (i+1) (nextPos currentPos) givensAcc
        mkSudoku (mkGivens arrayOfGivenChars 0 firstPos [])

    (** Read a sudoku puzzle string from a file. *)
    let readSudokuFromFile filepath =
        let lines = System.IO.File.ReadAllLines(filepath) in
        let input = Seq.fold (+) "" lines in
        let strippedInput = 
            String.collect (fun c -> if Seq.exists((=) c) [' ';'\n';'\t'] then "" else string c) input
        parseSudoku strippedInput

    (** Convert a sudoku puzzle to a string. *)
    let string (sudoku: Grid) =
        let stringAcc = ref "" in
        let string cell =
            match cell with
            | Empty -> "#"
            | Fixed value -> string value 
            | Unfixed _ -> "."
        let nRows = Array2D.length1 sudoku in
        let nCols = Array2D.length2 sudoku in
        for row = 0 to nRows-1 do
            for col = 0 to nCols-1 do
                stringAcc := !stringAcc + " " +  string sudoku.[row,col]
            if row < nRows-1 then
                stringAcc := !stringAcc + System.Environment.NewLine
        !stringAcc

    (** Compute the number of options left in an unfixed cell. *)
    let numOptions cell =
        match cell with
        | Empty -> 0
        | Fixed _ -> 1
        | Unfixed options -> options |> Set.count

    (** Module handling logic related to constraints for sudoku puzzles. *)
    module private Constraints =

        (** Compute the neighborhood of positions in the same row as a given position. *)
        let rowNeighborhood pos =
            Set(Seq.map (fun i -> { Row=pos.Row; Col=i }) (seq { 0..8 })).Remove(pos)

        (** Compute the neighborhood of positions in the same column as a given position. *)
        let colNeighborhood pos =
            Set(Seq.map (fun i -> { Row=i; Col=pos.Col }) (seq { 0..8 })).Remove(pos)

        (** Compute the neighborhood of positions in the same box as a given position. *)
        let boxNeighborhood pos =
            let indicesInSameBox i =
                match i with
                | 0 | 1 | 2 -> seq { 0..2 }
                | 3 | 4 | 5 -> seq { 3..5 }
                | 6 | 7 | 8 -> seq { 6..8 }
                | _ -> invalidArg "pos" "Position is out of bounds."
            let rowBoxIndices = indicesInSameBox pos.Row in
            let colBoxIndices = indicesInSameBox pos.Col in
                Set(seq { for i in rowBoxIndices do
                            for j in colBoxIndices do
                                yield { Row=i; Col=j } }).Remove(pos)

        (** Compute the neighborhood of a given position for a standard sudoku puzzle. *)
        let standardNeighborhood pos =
            [rowNeighborhood; colNeighborhood; boxNeighborhood] 
                |> List.map (fun f -> f pos) 
                |> List.fold (+) (Set([]))

        (** Remove a value from all the positions given in a sudoku grid. Mutates the grid. *)
        let internal removeValueFromOptions value (sudoku: Grid) positions =
            for pos in positions do
                let i = pos.Row in
                let j = pos.Col in
                sudoku.[i,j] <-
                    match sudoku.[i, j] with
                    | Empty -> Empty
                    | Fixed value' -> if value' = value then Empty else value' |> Fixed
                    | Unfixed options -> options.Remove(value) |> Unfixed

        (** Given the position of a Fixed cell, prune the value in the fixed cell from
            all other cells in the Fixed cell's neighborhood. *)
        let pruneFromNeighbors fixedPos (sudoku: Grid) =
            match getCellAt fixedPos sudoku with
            | Empty -> invalidArg "fixedPos" "The cell at this position is not fixed and cannot be used to prune other cells."
            | Unfixed _ -> invalidArg "fixedPos" "The cell at this position is not fixed and cannot be used to prune other cells."
            | Fixed value -> standardNeighborhood fixedPos |> removeValueFromOptions value sudoku

        (** Compute all positions in the grid satisfying a predicate. *)
        let getPositionsWhere predicate (sudoku: Grid) =
            let maybeFixedPositions =
                let nRows = Array2D.length1 sudoku in
                let nCols = Array2D.length2 sudoku in
                seq { for i = 0 to nRows-1 do
                        for j = 0 to nCols-1 do
                            match predicate sudoku.[i,j] with
                            | true -> Some { Row=i; Col=j }
                            | false -> None } in
            let rec filterSomes ls =
                match ls with
                | (Some x) :: tail -> x :: filterSomes tail
                | None :: tail -> filterSomes tail
                | [] -> []
            maybeFixedPositions |> List.ofSeq |> filterSomes

        (** Get list of the positions of Fixed cells in the grid. *)
        let fixedPositions = 
            let isFixed cell = 
                match cell with
                | Fixed _ -> true
                | _ -> false
            in getPositionsWhere isFixed

        (** Get list of the positions of Unfixed cells in the grid. *)
        let unfixedPositions =
            let isUnfixed cell =
                match cell with
                | Unfixed _ -> true
                | _ -> false
            in getPositionsWhere isUnfixed

        (** Apply constraints to prune options in unfixed cells in the grid. *)
        let applyConstraints sudoku alreadyFixed =
            let outputSudoku = Array2D.copy sudoku in
            let newFixedPositions = fixedPositions sudoku |> List.except alreadyFixed in 
            for pos in newFixedPositions do
                pruneFromNeighbors pos outputSudoku
            outputSudoku, Set.ofList newFixedPositions

    // End of Constraints module.

    (** Check if the sudoku contains a contradiction. *)
    let containsContradiction (sudoku: Grid) =
        sudoku |> Seq.cast<Cell> 
               |> Seq.exists (fun cell -> match cell with 
                                          | Empty -> true 
                                          | _ -> false)

    (** Check if the sudoku is fully solved. *)
    let solved (sudoku: Grid) =
        sudoku |> Seq.cast<Cell>
               |> Seq.forall (fun cell -> match cell with
                                          | Fixed _ -> true
                                          | _ -> false)

    (** Solve a sudoku puzzle. *)
    let solve sudoku =
        (** Main solving loop. This iteratively prunes all fixed nodes in the sudoku until
            no further progress may be made through pruning, then applies bifurcation to
            continue the solving process. *)
        let rec solvingLoop sudoku alreadyFixed =
            if containsContradiction sudoku then
                None
            else if solved sudoku then
                Some sudoku
            else
                let constrainedSudoku, newFixed = Constraints.applyConstraints sudoku alreadyFixed in
                if sudoku = constrainedSudoku then
                    bifurcate constrainedSudoku newFixed
                else
                    solvingLoop constrainedSudoku newFixed
        (** Perform bifurcation. This selects a cell with a minimal option count and 
            makes a guess as to what the value should be, then explores that branch.
            If that branch ends in a contradiction, then the next possible value is
            selected and that branch is explored. If all branches end in contradiction,
            then the sudoku has no solution. *)
        and bifurcate sudoku alreadyFixed =
            (** Bifurcate on the given position. *)
            let bifurcateOn pos (sudoku: Grid) =
                let cell = sudoku.[pos.Row, pos.Col] in
                match cell with
                    | Empty -> invalidArg "pos" "Cannot bifurcate on an empty cell."
                    | Fixed _ -> invalidArg "pos" "Cannot bifurcate on an already fixed cell."
                    | Unfixed options -> Seq.map (fun value -> fixPosition pos value sudoku) options |> LazyList.ofSeq
            (** Compute a position with a minimal option count. *)
            let position =
                let _numOptions cell =
                    match cell with
                    | Empty -> System.Int32.MaxValue
                    | Fixed _ -> System.Int32.MaxValue
                    | Unfixed _ -> numOptions cell
                let minIndex = ref { Row = -1; Col = -1; } in
                let currentMin = ref System.Int32.MaxValue in
                let nRows = Array2D.length1 sudoku in
                let nCols = Array2D.length2 sudoku in
                for i = 0 to nRows-1 do
                    for j = 0 to nCols-1 do
                        let cell = sudoku.[i,j] in
                        if _numOptions cell < !currentMin then
                            currentMin := _numOptions cell
                            minIndex := { Row=i; Col=j }
                if !currentMin = System.Int32.MaxValue then
                    invalidArg "sudoku" "No valid cells to bifurcate on."
                else
                    !minIndex
            (** Process the list of possible branches. *)
            let rec processList bifurcatedList =
                match bifurcatedList with
                | LazyList.Nil -> None
                | LazyList.Cons (candidate,rest) ->
                    match solvingLoop candidate alreadyFixed with
                    | None -> processList rest
                    | Some solution -> Some solution
            processList (bifurcateOn position sudoku)
        solvingLoop sudoku (Set([]))
