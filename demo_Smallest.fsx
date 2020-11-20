#load "KSmallest.fsx"

open KSmallest

let ls = [ 260; 889; 342; 197; 350; 340; 529; 511; 594; 664; 854; 428; 25; 125; 648; 940; 767; 955; 101; 557; 824; 38; 645; 303; 985; 667; 533; 735; 697; 213; ]

let comparer l r =
    match l with
    | l when l > r -> More
    | l when l < r -> Less
    | _ -> Equal

findKthSmallest comparer 8 ls