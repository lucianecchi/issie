module Hlp23Tick3

// important file containing all the Draw block (Symbol, BusWire and Sheet) types
// types are arranged in 3 submodules for each of Symbol, BusWire and Sheet
open DrawModelType

// open the submodules for these types which are needed
// so they do not need to be referred to using module name `BusWireT.Wire`
open DrawModelType.SymbolT
open DrawModelType.BusWireT

// open modules possiblly needed for drawSymbol
open Fable.React
open Fable.React.Props
open Elmish

// the standard Issie types for components, connections, etc
open CommonTypes

// some helpers to draw lines, text etc
open DrawHelpers

// the file containing symbol subfunctions etc
open Symbol

/// submodule for constant definitions used in this module
module Constants =
    let houseW = 200.0
    let houseH = 300.0

/// Record containing BusWire helper functions that might be needed by updateWireHook
/// functions are fed in at the updatewireHook function call in BusWireUpdate.
/// This is needed because HLPTick3 is earlier in the F# compile order than Buswire so
/// the functions cannot be called directly.
/// Add functions as needed.
/// NB these helpers are not needed to do Tick3
type Tick3BusWireHelpers =
    { AutoRoute: BusWireT.Model -> Wire -> Wire
      ReverseWire: Wire -> Wire
      MoveSegment: Model -> Segment -> float -> Wire }


/// Return Some reactElement list to replace drawSymbol by your own code
/// Choose which symbols this function controls by
/// returning None for the default Issie drawSymbol in SymbolView.
/// Drawhelpers contains some helpers you can use to draw lines, circles, etc.
/// drawsymbol contains lots of example code showing how they can be used.
/// The returned value is a list of SVG objects (reactElement list) that will be
/// displayed on screen.
//  for Tick 3 see the Tick 3 Powerpoint for what you need to do.
//  the house picture, and its dependence on the two parameters, will be assessed via interview.

// this function takes in the distance between the centers of the windows
// and the number of windows (which will be the actual number divided
// by 2). It returns a list of offsets from the center (which are all the
// windows centers on one side. This will be flipped appropriately to make the
// complete set of windows.

// returns a grid of coordinate offsets for the centres of all the windows in the grid
// could return window objects (that is something that puts together the lines of a window already)
// ie a react element list
let windows gridH gridV h v =

    let makeCentres grid n : float list =
        ([ 0.0 ], [ 0 .. n - 2 ])
        ||> List.fold (fun centres _ -> [ List.head centres + grid ] @ centres)
        |> List.rev

    let makeFullLine nTot grid centres =
        let flipCentres lst =
            (List.rev >> List.map (fun x -> -x)) lst

        let offsetCentres (lst: float list) =
            List.map (fun x -> x + (grid / 2.0)) lst
        // if odd leave them as is and duplicate all of them apart from the first, reverse and put at beginning
        // if even add half the offset then reverse and append
        match nTot with
        | odd when odd % 2 = 1 -> (List.tail >> flipCentres) centres @ centres
        | even -> (offsetCentres >> flipCentres) centres @ offsetCentres centres

    let makeGrid isHzntl centres =
        let repeat el = List.map (fun _ -> el) [ 0 .. h - 1 ]

        match isHzntl with
        | true -> List.map (fun _ -> centres) [ 0 .. v - 1 ]
        | false -> List.map repeat centres

    let combine grid n isHzntl =
        makeCentres grid (int (ceil (float n / 2.0)))
        |> makeFullLine n grid
        |> makeGrid isHzntl

    let windowsX = combine gridH h true
    let windowsY = combine gridV v false
    List.zip windowsX windowsY |> List.map (fun (lst1, lst2) -> List.zip lst1 lst2)

// given a centre and the dimensions return a list of Lines
// this can be used for all the elements, the key is how the centre is found
// there is definitely a better way to do this
let makeElement sizeX sizeY line (centreEl: XYPos) =
    let offsetX = sizeX / 2.0
    let offsetY = sizeY / 2.0
    let left = centreEl.X - offsetX
    let right = centreEl.X + offsetX
    let top = centreEl.Y - offsetY
    let bottom = centreEl.Y + offsetY

    []
    |> List.append [ makeLine left top right top line ]
    |> List.append [ makeLine left top left bottom line ]
    |> List.append [ makeLine right top right bottom line ]
    |> List.append [ makeLine left bottom right bottom line ]

let makeAll h v =
    let getXYPos (x, y) = { X = x; Y = y }
    // door height and window height is the same, divide by 1 + v, the heigh is 8/10 of that with the remaining 1/10 at the top and 1/10 at the bottom
    let gridV = Constants.houseH / (1.0 + float v)
    let fixtureHeight = gridV * 0.8
    let gridH = Constants.houseW / float h
    let fixtureWidth = gridH * 0.7 

    let doorCentre =
        { X = 0.0
          Y = (Constants.houseH / 2.0 - (fixtureHeight / 2.0)) }

    let houseCentre = { X = 0.0; Y = 0.0 }

    let outlineWindows =
        windows gridH gridV h v
        |> List.map (List.map (fun (x, y) -> (x, y - fixtureHeight / 2.0)))
        |> List.map (List.map getXYPos)
        |> List.collect (List.collect (makeElement fixtureWidth fixtureHeight { defaultLine with StrokeWidth = "2px" })) // will need to change a bit (this is just skeleton)

    let outlineHouse =
        makeElement Constants.houseW Constants.houseH { defaultLine with StrokeWidth = "4px" } houseCentre

    let outlineDoor =
        makeElement (fixtureWidth / 2.0) fixtureHeight { defaultLine with StrokeWidth = "2px" } doorCentre

    outlineWindows @ outlineHouse @ outlineDoor

let drawSymbolHook (symbol: Symbol) (theme: ThemeType) : ReactElement list option =
    // replace the code below by your own code
    match symbol.Component.Type with
    | Constant1(width, constValue, _) ->
        printfn $"CONSTANT: width={width} ConstVale={constValue}"
        Some(makeAll width (int constValue))
    | _ ->
        printfn "Symbol Hook"
        None

/// Return Some newWire to replace updateWire by your own code defined here.
/// Choose which wires you control by returning None to use the
/// default updateWire function defined in BusWireUpdate.
/// The wire shape and position can be changed by changing wire.Segments and wire.StartPos.
/// See updateWire for the default autoroute wire update function.
/// The return value must be a (possibly modified) copy of wire.

// For tick 3 modify the updated wires (in some cases) somehow.
// e.g. if they have 3 visual segments and have a standard (you decide what) orientation change where the middle
// segment is on screen so it is 1/3 of the way between the two components instead of 1/2.
// do something more creative or useful if you like.
// This part of Tick will pass if you can demo one wire changing as you move a symbol in some way different from
// Issie: the change need not work on all quadrants (where it is not implemented the wire should default to
// Issie standard.
let updateWireHook (model: BusWireT.Model) (wire: Wire) (tick3Helpers: Tick3BusWireHelpers) : Wire option =
    let segmentInfo =
        wire.Segments |> List.map (fun (seg: Segment) -> seg.Length, seg.Mode)

    match wire.Segments with
    | [ _; _; l; _; r; _; _ ] when l.Length > 0.0 && r.Length > 0.0 ->
        let hzntlDistance = l.Length + r.Length

        let newSegments =
            wire.Segments
            |> List.updateAt
                2
                { l with
                    Length = (0.3 * hzntlDistance) }
            |> List.updateAt
                4
                { r with
                    Length = (0.7 * hzntlDistance) }

        let newWire = { wire with Segments = newSegments }
        printfn "%s" $"Wire: Initial Orientation={wire.InitialOrientation}\nSegments={segmentInfo}"
        Some(newWire)
    | _ -> None


//---------------------------------------------------------------------//
//-------included here because it will be used in project work---------//
//---------------------------------------------------------------------//

/// This function is called at the end of a symbol (or multi-symbol) move
/// when the mouse goes up.
/// at this time it would make sense to try for a better autoroute of
/// all the moved wires e.g. avoiding eachother, avoiding other wires,
/// etc, etc.
///
/// wireIds is the list of wire ids that have one end connected to a
/// moved symbol.
/// Any required change in wire positions or shapes should be returned by
/// changing the values of busWireModel.Wires which
/// is a Map<ConnectionId , Wire> and contains all wires
/// keyed by their wire Id (type ConnectionId)
/// No change required for Tick 3
let smartAutoRouteWires
    (wireIds: ConnectionId list)
    (tick3Helpers: Tick3BusWireHelpers)
    (model: SheetT.Model)
    : SheetT.Model =
    let busWireModel = model.Wire // contained as field of Sheet model
    let symbolModel = model.Wire.Symbol // contained as field of BusWire Model
    let wires = busWireModel.Wires // all wire info
    // NB to return updated wires here you would need nested record update
    // {model with Wire = {model.Wire with Wires = wires'}}
    // Better syntax to do that can be found using optics lenses
    // see DrawModelT for already defined lenses and Issie wiki
    // for how they work
    model // no smart autoroute for now, so return model with no chnage

//---------------------------------------------------------------------//
//------------------- Snap Functionality-------------------------------//
//---------------------------------------------------------------------//

(*

 Needed for one part of project work (not for Tick 3):
    Sheet.getNewSegmentSnapInfo
    Sheet.getNewSymbolSnapInfo

 These functions can be changed to alter which things symbols or segments snap to:
 They are called at the start of a segment or symbol drag operation.

 If you want to change these inside a drag operation - you may need to alter other code.
 The snap code is all in one place and well-structured - it should be easy to change.

 *)
