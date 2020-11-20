
exception ListsNotEqual

type Size = int

type Bolt = Bolt of Size
type Nut = Nut of Size

type Comparison =
    Bigger
    | Fit
    | Smaller

let private tryFitN (nut: Nut) (bolt: Bolt) =
    match (nut, bolt) with
    | (Nut n, Bolt b) when n > b -> Bigger
    | (Nut n, Bolt b) when n < b -> Smaller
    | _ -> Fit

let private tryFitB b n =
    match (tryFitN n b) with
    | Smaller -> Bigger
    | Bigger -> Smaller
    | _ -> Fit
    
let private partition (f: 'a -> Comparison) =
    let rec loop left right current =
        function
            | [] -> current, left, right
            | head::tail ->
                match (f head) with
                    | Bigger -> loop left (head::right) current
                    | Smaller -> loop (head::left) right current
                    | Fit -> loop left right (Some head)
                |> fun lp -> lp tail
    loop [] [] None

let rec fit (nuts: Nut list) (bolts: Bolt list) =
    match nuts with
    | [] -> []
    | _  when nuts.Length <> bolts.Length -> raise (ListsNotEqual)
    | pivotN::_ -> 
        let (pivotB, leftB, rightB) = partition (tryFitN pivotN) bolts
        match pivotB with
        | Some v -> 
            let (_, leftN, rightN) = partition (tryFitB v) nuts
            (fit leftN leftB) @ [(pivotN, v)] @ (fit rightN rightB)
        | None -> raise (ListsNotEqual)