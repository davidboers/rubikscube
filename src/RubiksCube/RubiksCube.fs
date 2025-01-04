open Avalonia
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Themes.Fluent
open Avalonia.FuncUI.Hosts
open Avalonia.Controls
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.Layout

open BlackFox.ColoredPrintf

open System
open System.Drawing

module Rubiks =
    type Color =
        | Red
        | Orange
        | White
        | Yellow
        | Green
        | Blue

    let consoleColor(color: Color) =
        match color with
        | Red    -> ConsoleColor.Red
        | Orange -> ConsoleColor.DarkYellow
        | White  -> ConsoleColor.White
        | Yellow -> ConsoleColor.Yellow
        | Green  -> ConsoleColor.Green
        | Blue   -> ConsoleColor.Blue

    let backgroundColor(color: Color) =
        match color with
        | Red    -> "red"
        | Orange -> "orange"
        | White  -> "white"
        | Yellow -> "yellow"
        | Green  -> "green"
        | Blue   -> "blue"

    let colorChar(color: Color) =
        match color with
        | Red    -> "R"
        | Orange -> "O"
        | White  -> "W"
        | Yellow -> "Y"
        | Green  -> "G"
        | Blue   -> "B"

    type Face = List<List<Color>>
    type FaceMask = List<List<Color Option>>

    let mapFace (prec: (Color -> 'b), face: Face): 'b list =
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

    let bottomRow = [(2, 0); (2, 1); (2, 2)]
    let topRow = [(0, 0); (0, 1); (0, 2)]
    let leftRow = [(0, 0); (1, 0); (2, 0)]
    let rightRow = [(0, 2); (1, 2); (2, 2)]

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

module Main =

    let makeButton (prec, cube: IWritable<Rubiks.Cube>, label: string, dock: Dock) =
        Button.create [
            Button.dock dock
            Button.onClick (fun _ -> cube.Set(prec cube.Current); Rubiks.presentCube cube.Current)
            Button.content label
            Button.horizontalAlignment HorizontalAlignment.Stretch
            Button.horizontalContentAlignment HorizontalAlignment.Center
        ]

    let faceletDim = 40.
    let faceletStrk = faceletDim * 0.05

    let makeFacelet color =
        Rectangle.create [
            Shapes.Rectangle.fill (Rubiks.backgroundColor color)
            Shapes.Rectangle.stroke "black"
            Shapes.Rectangle.strokeThickness faceletStrk
            Shapes.Rectangle.width faceletDim
            Shapes.Rectangle.height faceletDim
        ]

    let makeFaceBorder (face: Rubiks.Face, dock: Dock Option) =
        WrapPanel.create [
            WrapPanel.width (faceletDim * 3. + faceletStrk * 3.)
            WrapPanel.height (faceletDim * 3. + faceletStrk * 3.)
            WrapPanel.children [
                makeFacelet(face.Item(0).Item(0))
                makeFacelet(face.Item(0).Item(1))
                makeFacelet(face.Item(0).Item(2))
                makeFacelet(face.Item(1).Item(0))
                makeFacelet(face.Item(1).Item(1))
                makeFacelet(face.Item(1).Item(2))
                makeFacelet(face.Item(2).Item(0))
                makeFacelet(face.Item(2).Item(1))
                makeFacelet(face.Item(2).Item(2))
            ]
            match dock with
            | Some d -> WrapPanel.dock d
            | None   -> WrapPanel.name "Idk"
        ]

    let view () =
        Component(fun ctx ->
            let cube = ctx.useState(Rubiks.scramble 25 (new Random()) Rubiks.defaultCube)

            DockPanel.create [
                DockPanel.children [
                    DockPanel.create [
                        DockPanel.dock Dock.Right
                        DockPanel.children [
                            makeButton(Rubiks.F, cube, "F", Dock.Top)
                            makeButton(Rubiks.B, cube, "B", Dock.Top)
                            makeButton(Rubiks.U, cube, "U", Dock.Top)
                            makeButton(Rubiks.D, cube, "D", Dock.Top)
                            makeButton(Rubiks.L, cube, "L", Dock.Top)
                            makeButton(Rubiks.R, cube, "R", Dock.Top)
                        ]
                    ]
                     
                    DockPanel.create [
                        DockPanel.dock Dock.Right
                        DockPanel.children [
                            makeButton(Rubiks.F2, cube, "F2", Dock.Top)
                            makeButton(Rubiks.B2, cube, "B2", Dock.Top)
                            makeButton(Rubiks.U2, cube, "U2", Dock.Top)
                            makeButton(Rubiks.D2, cube, "D2", Dock.Top)
                            makeButton(Rubiks.L2, cube, "L2", Dock.Top)
                            makeButton(Rubiks.R2, cube, "R2", Dock.Top)
                        ]
                    ]

                    DockPanel.create [
                        DockPanel.children [
                            makeButton(Rubiks.F', cube, "F'", Dock.Top)
                            makeButton(Rubiks.B', cube, "B'", Dock.Top)
                            makeButton(Rubiks.U', cube, "U'", Dock.Top)
                            makeButton(Rubiks.D', cube, "D'", Dock.Top)
                            makeButton(Rubiks.L', cube, "L'", Dock.Top)
                            makeButton(Rubiks.R', cube, "R'", Dock.Top)
                        ]
                    ]

                    DockPanel.create [
                        DockPanel.children [
                            makeFaceBorder(cube.Current.u, Some(Dock.Top))
                            makeFaceBorder(cube.Current.d, Some(Dock.Bottom))
                            makeFaceBorder(cube.Current.l, Some(Dock.Left))
                            makeFaceBorder(cube.Current.r, Some(Dock.Right))
                            makeFaceBorder(cube.Current.f, None)
                        ]
                    ]
                    makeFaceBorder(cube.Current.b, None)
                ]
            ]
        )

type MainWindow() =
    inherit HostWindow()
    do
        base.Title <- "Rubics Cube"
        base.Content <- Main.view ()

module Program =
    type App() =
        inherit Application()

        override this.Initialize() =
            this.Styles.Add (FluentTheme())
            this.RequestedThemeVariant <- Styling.ThemeVariant.Dark

        override this.OnFrameworkInitializationCompleted() =
            match this.ApplicationLifetime with
            | :? IClassicDesktopStyleApplicationLifetime as desktopLifetime ->
                desktopLifetime.MainWindow <- MainWindow()
            | _ -> ()

    [<EntryPoint>]
    let main(args: string[]) =
        AppBuilder
            .Configure<App>()
            .UsePlatformDetect()
            .UseSkia()
            .StartWithClassicDesktopLifetime(args)