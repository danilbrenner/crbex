type Comparison =
    More | Equal | Less

let rec findKthSmallest (comparer: 'a -> 'a -> Comparison) k =
    function
        | [] -> None 
        | ls when ls.Length < k -> None
        | piv::tail ->
            let (left, right) = List.partition (fun r -> (comparer piv r) = More) tail
            match left with
            | _ when k <> left.Length && left.Length <> 0 && left.Length > k -> findKthSmallest comparer k left
            | _ when k <> left.Length && right.Length <> 0 && left.Length <= k -> findKthSmallest comparer (k - (left.Length + 1)) right
            | _ -> Some piv