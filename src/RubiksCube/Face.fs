module RubiksCube.Face

open RubiksCube.Color

type Face = List<List<Color>>
type FaceMask = List<List<Color Option>>

let mapFace (prec: (Color -> 'b)) (face: Face): 'b list =
    List.concat <| List.map (List.map prec) face

let getMask(face: Face, set) =
    List.map (fun i ->
        List.map (fun j ->
            if List.contains (i, j) set then Some(face.Item(i).Item(j))
            else None
        ) [0..2]
    ) [0..2]

let invertedMask(face: Face, mask: FaceMask) =
    List.map (fun i ->
        List.map (fun j ->
            match mask.Item(i).Item(j) with 
            | Some _ -> None
            | None   -> Some <| face.Item(i).Item(j)
        ) [0..2]
    ) [0..2]

let combineMasks(a: FaceMask, b: FaceMask) =
    List.map (fun i ->
        List.map (fun j ->
            match (a.Item(i).Item(j), b.Item(i).Item(j)) with
            | (Some ax, _) -> ax
            | (_, Some bx) -> bx
        ) [0..2]
    ) [0..2]

let rotClockwise face =
    let [ [ f00; f01; f02 ] 
        ; [ f10; f11; f12 ] 
        ; [ f20; f21; f22 ] 
        ] = face
    [ [ f20; f10; f00 ]
    ; [ f21; f11; f01 ]
    ; [ f22; f12; f02 ]
    ]

let rotCounterClockwise face =
    let [ [ f00; f01; f02 ] 
        ; [ f10; f11; f12 ] 
        ; [ f20; f21; f22 ] 
        ] = face
    [ [ f02; f12; f22 ]
    ; [ f01; f11; f21 ]
    ; [ f00; f10; f20 ]
    ]

let rotRowClockwise (rf: Face) (sf: Face) set =
    let rotating = rotClockwise <| getMask(rf, set)
    let still = invertedMask(sf, rotating)
    combineMasks(rotating, still)

let rotRowCounterClockwise (rf: Face) (sf: Face) set =
    let rotating = rotCounterClockwise <| getMask(rf, set)
    let still = invertedMask(sf, rotating)
    combineMasks(rotating, still)

let shiftRow (rf: Face) (sf: Face) set =
    let shifting = getMask(rf, set)
    let still = invertedMask(sf, shifting)
    combineMasks(shifting, still)

let newFace color = 
    [ [ color; color; color ]
    ; [ color; color; color ]
    ; [ color; color; color ]
    ]

let solved face color =
    mapFace (fun c -> c = color) face
    |> List.filter id
    |> List.length

let bottomRow = [(2, 0); (2, 1); (2, 2)]
let topRow = [(0, 0); (0, 1); (0, 2)]
let leftRow = [(0, 0); (1, 0); (2, 0)]
let rightRow = [(0, 2); (1, 2); (2, 2)]