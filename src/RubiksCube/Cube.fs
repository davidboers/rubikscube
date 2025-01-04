module RubiksCube.Cube

open RubiksCube.Face
open RubiksCube.Color

open BlackFox.ColoredPrintf

open System

type Cube =
    { f: Face
      b: Face
      u: Face
      d: Face
      l: Face
      r: Face
    }

let F c = 
    { c with 
        f = rotClockwise c.f
        r = rotRowClockwise c.u c.r bottomRow
        d = rotRowClockwise c.r c.d leftRow
        l = rotRowClockwise c.d c.l topRow
        u = rotRowClockwise c.l c.u rightRow
    }

let B c = 
    { c with  // Not Working
        b = rotClockwise c.b
        r = rotRowClockwise c.d c.r topRow
        d = rotRowClockwise c.l c.d rightRow
        l = rotRowClockwise c.u c.l bottomRow
        u = rotRowClockwise c.r c.u leftRow
    }

let U c =
    { c with 
        u = rotClockwise c.u
        r = shiftRow c.f c.r topRow
        b = shiftRow c.r c.b topRow
        l = shiftRow c.b c.l topRow
        f = shiftRow c.l c.f topRow
    }

let D c =
    { c with 
        d = rotClockwise c.d
        r = shiftRow c.f c.r bottomRow
        b = shiftRow c.r c.b bottomRow
        l = shiftRow c.b c.l bottomRow
        f = shiftRow c.l c.f bottomRow
    }

let L c =
    { c with 
        l = rotClockwise c.l
        u = shiftRow c.f c.u leftRow
        b = shiftRow c.u c.b leftRow
        d = shiftRow c.b c.d leftRow
        f = shiftRow c.d c.f leftRow
    }

let R c = 
    { c with 
        r = rotClockwise c.r
        u = shiftRow c.f c.u rightRow
        b = shiftRow c.u c.b rightRow
        d = shiftRow c.b c.d rightRow
        f = shiftRow c.d c.f rightRow
    }

let F2 = F << F
let B2 = B << B
let U2 = U << U
let D2 = D << D
let L2 = L << L
let R2 = R << R

let F' c =
    { c with 
        f = rotCounterClockwise c.f
        l = rotRowCounterClockwise c.u c.l bottomRow
        d = rotRowCounterClockwise c.l c.d rightRow
        r = rotRowCounterClockwise c.d c.r topRow
        u = rotRowCounterClockwise c.r c.u leftRow
    }

let B' c =
    { c with // Not Working
        b = rotCounterClockwise c.b
        l = rotRowCounterClockwise c.d c.l topRow
        d = rotRowCounterClockwise c.r c.d leftRow
        r = rotRowCounterClockwise c.u c.r bottomRow
        u = rotRowCounterClockwise c.l c.u rightRow
    }

let U' c =
    { c with 
        u = rotClockwise c.u
        f = shiftRow c.r c.f topRow
        r = shiftRow c.b c.r topRow
        b = shiftRow c.l c.b topRow
        l = shiftRow c.f c.l topRow
    }

let D' c = 
    { c with 
        d = rotClockwise c.d
        f = shiftRow c.r c.f bottomRow
        r = shiftRow c.b c.r bottomRow
        b = shiftRow c.l c.b bottomRow
        l = shiftRow c.f c.l bottomRow
    }

let L' c = 
    { c with 
        l = rotClockwise c.l
        f = shiftRow c.u c.f leftRow
        u = shiftRow c.b c.u leftRow
        b = shiftRow c.d c.b leftRow
        d = shiftRow c.f c.d leftRow
    }

let R' c = 
    { c with 
        r = rotClockwise c.r
        f = shiftRow c.u c.f rightRow
        u = shiftRow c.b c.u rightRow
        b = shiftRow c.d c.b rightRow
        d = shiftRow c.f c.d rightRow
    }

let randMove (rnd: Random) =
    match rnd.Next(0, 17) with
    | 0 -> F
    | 1 -> B 
    | 2 -> U
    | 3 -> D
    | 4 -> L
    | 5 -> R
    | 6 -> F2
    | 7 -> B2
    | 8 -> U2
    | 9 -> D2
    | 10 -> L2
    | 11 -> R2
    | 12 -> F'
    | 13 -> B'
    | 14 -> U'
    | 15 -> D'
    | 16 -> L'
    | 17 -> R'

let rec scramble n (rnd: Random) (cube: Cube) =
    match n with
    | 1 -> randMove rnd cube
    | _ -> scramble (n-1) rnd <| randMove rnd cube

let defaultCube: Cube = { f = newFace(Red)    // Front
                        ; b = newFace(Orange) // Back
                        ; u = newFace(White)  // Top
                        ; d = newFace(Yellow) // Bottom
                        ; l = newFace(Green)  // Left
                        ; r = newFace(Blue)   // Right
                        }

let solved (cube: Cube) =
    [ solved cube.f Red
    ; solved cube.b Orange
    ; solved cube.u White
    ; solved cube.d Yellow
    ; solved cube.l Green
    ; solved cube.r Blue
    ]
    |> List.sum

let presentCube(cube: Cube) =
    colorprintfn "       $%A[%s] $%A[%s] $%A[%s]" 
                                        (consoleColor(cube.u.Item(0).Item(0))) (colorChar(cube.u.Item(0).Item(0))) // White
                                        (consoleColor(cube.u.Item(0).Item(1))) (colorChar(cube.u.Item(0).Item(1)))
                                        (consoleColor(cube.u.Item(0).Item(2))) (colorChar(cube.u.Item(0).Item(2)))

    colorprintfn "       $%A[%s] $%A[%s] $%A[%s]" 
                                        (consoleColor(cube.u.Item(1).Item(0))) (colorChar(cube.u.Item(1).Item(0))) 
                                        (consoleColor(cube.u.Item(1).Item(1))) (colorChar(cube.u.Item(1).Item(1)))
                                        (consoleColor(cube.u.Item(1).Item(2))) (colorChar(cube.u.Item(1).Item(2)))

    colorprintfn "       $%A[%s] $%A[%s] $%A[%s]" 
                                        (consoleColor(cube.u.Item(2).Item(0))) (colorChar(cube.u.Item(2).Item(0))) 
                                        (consoleColor(cube.u.Item(2).Item(1))) (colorChar(cube.u.Item(2).Item(1)))
                                        (consoleColor(cube.u.Item(2).Item(2))) (colorChar(cube.u.Item(2).Item(2)))

    printfn ""

    colorprintfn "$%A[%s] $%A[%s] $%A[%s]  $%A[%s] $%A[%s] $%A[%s]  $%A[%s] $%A[%s] $%A[%s]  $%A[%s] $%A[%s] $%A[%s]" 
                                        (consoleColor(cube.l.Item(0).Item(0))) (colorChar(cube.l.Item(0).Item(0))) // Green
                                        (consoleColor(cube.l.Item(0).Item(1))) (colorChar(cube.l.Item(0).Item(1)))
                                        (consoleColor(cube.l.Item(0).Item(2))) (colorChar(cube.l.Item(0).Item(2)))

                                        (consoleColor(cube.f.Item(0).Item(0))) (colorChar(cube.f.Item(0).Item(0))) // Red
                                        (consoleColor(cube.f.Item(0).Item(1))) (colorChar(cube.f.Item(0).Item(1)))
                                        (consoleColor(cube.f.Item(0).Item(2))) (colorChar(cube.f.Item(0).Item(2)))

                                        (consoleColor(cube.r.Item(0).Item(0))) (colorChar(cube.r.Item(0).Item(0))) // Blue
                                        (consoleColor(cube.r.Item(0).Item(1))) (colorChar(cube.r.Item(0).Item(1)))
                                        (consoleColor(cube.r.Item(0).Item(2))) (colorChar(cube.r.Item(0).Item(2)))

                                        (consoleColor(cube.b.Item(0).Item(0))) (colorChar(cube.b.Item(0).Item(0))) // Orange
                                        (consoleColor(cube.b.Item(0).Item(1))) (colorChar(cube.b.Item(0).Item(1)))
                                        (consoleColor(cube.b.Item(0).Item(2))) (colorChar(cube.b.Item(0).Item(2)))

    colorprintfn "$%A[%s] $%A[%s] $%A[%s]  $%A[%s] $%A[%s] $%A[%s]  $%A[%s] $%A[%s] $%A[%s]  $%A[%s] $%A[%s] $%A[%s]" 
                                        (consoleColor(cube.l.Item(1).Item(0))) (colorChar(cube.l.Item(1).Item(0))) // Green
                                        (consoleColor(cube.l.Item(1).Item(1))) (colorChar(cube.l.Item(1).Item(1)))
                                        (consoleColor(cube.l.Item(1).Item(2))) (colorChar(cube.l.Item(1).Item(2)))

                                        (consoleColor(cube.f.Item(1).Item(0))) (colorChar(cube.f.Item(1).Item(0))) // Red
                                        (consoleColor(cube.f.Item(1).Item(1))) (colorChar(cube.f.Item(1).Item(1)))
                                        (consoleColor(cube.f.Item(1).Item(2))) (colorChar(cube.f.Item(1).Item(2)))

                                        (consoleColor(cube.r.Item(1).Item(0))) (colorChar(cube.r.Item(1).Item(0))) // Blue
                                        (consoleColor(cube.r.Item(1).Item(1))) (colorChar(cube.r.Item(1).Item(1)))
                                        (consoleColor(cube.r.Item(1).Item(2))) (colorChar(cube.r.Item(1).Item(2)))

                                        (consoleColor(cube.b.Item(1).Item(0))) (colorChar(cube.b.Item(1).Item(0))) // Orange
                                        (consoleColor(cube.b.Item(1).Item(1))) (colorChar(cube.b.Item(1).Item(1)))
                                        (consoleColor(cube.b.Item(1).Item(2))) (colorChar(cube.b.Item(1).Item(2)))

    colorprintfn "$%A[%s] $%A[%s] $%A[%s]  $%A[%s] $%A[%s] $%A[%s]  $%A[%s] $%A[%s] $%A[%s]  $%A[%s] $%A[%s] $%A[%s]" 
                                        (consoleColor(cube.l.Item(2).Item(0))) (colorChar(cube.l.Item(2).Item(0))) // Green
                                        (consoleColor(cube.l.Item(2).Item(1))) (colorChar(cube.l.Item(2).Item(1)))
                                        (consoleColor(cube.l.Item(2).Item(2))) (colorChar(cube.l.Item(2).Item(2)))

                                        (consoleColor(cube.f.Item(2).Item(0))) (colorChar(cube.f.Item(2).Item(0))) // Red
                                        (consoleColor(cube.f.Item(2).Item(1))) (colorChar(cube.f.Item(2).Item(1)))
                                        (consoleColor(cube.f.Item(2).Item(2))) (colorChar(cube.f.Item(2).Item(2)))

                                        (consoleColor(cube.r.Item(2).Item(0))) (colorChar(cube.r.Item(2).Item(0))) // Blue
                                        (consoleColor(cube.r.Item(2).Item(1))) (colorChar(cube.r.Item(2).Item(1)))
                                        (consoleColor(cube.r.Item(2).Item(2))) (colorChar(cube.r.Item(2).Item(2)))

                                        (consoleColor(cube.b.Item(2).Item(0))) (colorChar(cube.b.Item(2).Item(0))) // Orange
                                        (consoleColor(cube.b.Item(2).Item(1))) (colorChar(cube.b.Item(2).Item(1)))
                                        (consoleColor(cube.b.Item(2).Item(2))) (colorChar(cube.b.Item(2).Item(2)))

    printfn ""

    colorprintfn "       $%A[%s] $%A[%s] $%A[%s]" 
                                        (consoleColor(cube.d.Item(0).Item(0))) (colorChar(cube.d.Item(0).Item(0))) // Yellow
                                        (consoleColor(cube.d.Item(0).Item(1))) (colorChar(cube.d.Item(0).Item(1)))
                                        (consoleColor(cube.d.Item(0).Item(2))) (colorChar(cube.d.Item(0).Item(2)))

    colorprintfn "       $%A[%s] $%A[%s] $%A[%s]" 
                                        (consoleColor(cube.d.Item(1).Item(0))) (colorChar(cube.d.Item(1).Item(0))) 
                                        (consoleColor(cube.d.Item(1).Item(1))) (colorChar(cube.d.Item(1).Item(1)))
                                        (consoleColor(cube.d.Item(1).Item(2))) (colorChar(cube.d.Item(1).Item(2)))

    colorprintfn "       $%A[%s] $%A[%s] $%A[%s]" 
                                        (consoleColor(cube.d.Item(2).Item(0))) (colorChar(cube.d.Item(2).Item(0))) 
                                        (consoleColor(cube.d.Item(2).Item(1))) (colorChar(cube.d.Item(2).Item(1)))
                                        (consoleColor(cube.d.Item(2).Item(2))) (colorChar(cube.d.Item(2).Item(2)))

    printfn ""