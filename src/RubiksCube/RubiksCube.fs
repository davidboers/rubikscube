module RubiksCube.Core

open RubiksCube.Cube
open RubiksCube.Color
open RubiksCube.Face

open Avalonia
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Themes.Fluent
open Avalonia.FuncUI.Hosts
open Avalonia.Controls
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.Layout

open System

module Main =

    let makeButton (prec, cube: IWritable<Cube>, label: string, dock: Dock) =
        Button.create [
            Button.dock dock
            Button.onClick (fun _ -> cube.Set(prec cube.Current); presentCube cube.Current)
            Button.content label
            Button.horizontalAlignment HorizontalAlignment.Stretch
            Button.horizontalContentAlignment HorizontalAlignment.Center
        ]

    let faceletDim = 40.
    let faceletStrk = faceletDim * 0.05
    let margin = 10.
    let stageWidth  = (faceletDim * 3. * 4.) + (margin * 3.) + (faceletStrk * 4. * 3.) + 50.
    let stageHeight = (faceletDim * 3. * 3.) + (margin * 2.) + (faceletStrk * 3. * 4.) + 50.

    let makeFacelet color =
        Rectangle.create [
            Shapes.Rectangle.fill (backgroundColor color)
            Shapes.Rectangle.stroke "black"
            Shapes.Rectangle.strokeThickness faceletStrk
            Shapes.Rectangle.width faceletDim
            Shapes.Rectangle.height faceletDim
        ]

    let makeFaceBorder (face: Face, dock: Dock Option) =
        WrapPanel.create [
            WrapPanel.width (faceletDim * 3. + faceletStrk * 3.)
            WrapPanel.height (faceletDim * 3. + faceletStrk * 3.)
            WrapPanel.margin margin
            WrapPanel.horizontalAlignment HorizontalAlignment.Stretch
            WrapPanel.verticalAlignment VerticalAlignment.Stretch
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

    let progress cube =
        (string <| Cube.solved cube) + " / " + (string <| 9. * 6.)

    let view () =
        Component(fun ctx ->
            let cube = ctx.useState(scramble 25 (new Random()) defaultCube)

            DockPanel.create [
                DockPanel.children [
                    TextBlock.create [
                        TextBlock.text <| progress cube.Current
                        TextBlock.dock Dock.Top
                        TextBlock.horizontalAlignment HorizontalAlignment.Center
                        TextBlock.fontSize 48
                    ]

                    Button.create [
                        Button.onClick (fun _ -> cube.Set(scramble 25 (new Random()) cube.Current))
                        Button.content "Scramble"
                        TextBlock.horizontalAlignment HorizontalAlignment.Center
                        Button.dock Dock.Top
                    ]

                    DockPanel.create [
                        DockPanel.dock Dock.Right
                        DockPanel.verticalAlignment VerticalAlignment.Center
                        DockPanel.children [
                            makeButton(F, cube, "F", Dock.Top)
                            makeButton(B, cube, "B", Dock.Top)
                            makeButton(U, cube, "U", Dock.Top)
                            makeButton(D, cube, "D", Dock.Top)
                            makeButton(L, cube, "L", Dock.Top)
                            makeButton(R, cube, "R", Dock.Top)
                        ]
                    ]
                     
                    DockPanel.create [
                        DockPanel.dock Dock.Right
                        DockPanel.verticalAlignment VerticalAlignment.Center
                        DockPanel.children [
                            makeButton(F2, cube, "F2", Dock.Top)
                            makeButton(B2, cube, "B2", Dock.Top)
                            makeButton(U2, cube, "U2", Dock.Top)
                            makeButton(D2, cube, "D2", Dock.Top)
                            makeButton(L2, cube, "L2", Dock.Top)
                            makeButton(R2, cube, "R2", Dock.Top)
                        ]
                    ]

                    DockPanel.create [
                        DockPanel.verticalAlignment VerticalAlignment.Center
                        DockPanel.children [
                            makeButton(F', cube, "F'", Dock.Top)
                            makeButton(B', cube, "B'", Dock.Top)
                            makeButton(U', cube, "U'", Dock.Top)
                            makeButton(D', cube, "D'", Dock.Top)
                            makeButton(L', cube, "L'", Dock.Top)
                            makeButton(R', cube, "R'", Dock.Top)
                        ]
                    ]

                    Border.create [
                        Border.borderBrush "white"
                        Border.borderThickness faceletStrk
                        Border.width stageWidth
                        Border.height stageHeight
                        Border.height 500
                        Border.child (
                            DockPanel.create [
                                DockPanel.children [ 
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
                    ]
                ]
            ]
        )

type MainWindow() =
    inherit HostWindow()
    do
        base.Title <- "Rubics Cube"
        base.Icon <- WindowIcon(System.IO.Path.Combine("Assets", "Icons", "icon.ico"))
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