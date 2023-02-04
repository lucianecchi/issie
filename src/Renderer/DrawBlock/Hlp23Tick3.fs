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
    let houseH = 111.0
    let houseL = 111.0

/// Record containing BusWire helper functions that might be needed by updateWireHook
/// functions are fed in at the updatewireHook function call in BusWireUpdate.
/// This is needed because HLPTick3 is earlier in the F# compile order than Buswire so
/// the functions cannot be called directly.
/// Add functions as needed.
/// NB these helpers are not needed to do Tick3
type Tick3BusWireHelpers = {
    AutoRoute: BusWireT.Model -> Wire -> Wire
    ReverseWire: Wire -> Wire
    MoveSegment: Model -> Segment -> float -> Wire
    }


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
let rec zip (a,b) = 
    match (a, b) with
    | ha :: _, hb :: _ when a.Length = 1 && b.Length = 1 -> [(ha, hb)]
    | ha :: ta, hb :: tb -> [(ha, hb)] @ zip (ta, tb)
    | _ -> failwithf "should not happen, lists should have same size"

// returns a grid of coordinate offsets for the centres of all the windows in the grid 
// could return window objects (that is something that puts together the lines of a window already)
// ie a react element list 
let windows gridH gridV h v = 
    let makeCentres (graph:float) (n:int) : float list = 
        ([0.0], [0..n]) ||> List.fold (fun centres _ -> [List.head centres + graph] @ centres)
        |> List.rev

    let makeFullLine  (nTot: int) (grid: float) (centres: float list) : float list = 
        let flipCentres (lst: float list) = 
            lst
            |> List.map (fun x -> -x)
            |> List.rev
        let offsetCentres (lst: float list) = 
            List.map (fun x -> x + (grid/ 2.0)) lst
        match nTot with 
        // if odd leave them as is and duplicate all of them apart from the first, reverse and put at beginning
        | odd when nTot % 2 = 1 -> (List.tail >> flipCentres) centres @ centres
        // if even add half the offset then reverse and append 
        | even -> (offsetCentres >> flipCentres) centres @ offsetCentres centres

    let makeGrid isHzntl centres =   
        let repeat el = List.map (fun _ -> el) [0..h]
        match isHzntl with 
        | true -> List.map (fun _ -> centres) [0..v]
        | false -> List.map repeat centres

    let windowsX =
        makeCentres gridH (int (ceil (float h / 2.0)))
        |> makeFullLine h gridH
        |> makeGrid true 
    let windowsY =
        makeCentres gridV (int (ceil (float v / 2.0))) 
        |> makeFullLine v gridV
        |> makeGrid false
    zip (windowsX, windowsY)
    |> List.map zip

// given a centre and the dimensions return a list of Lines 
// this can be used for all the elements, the key is how the centre is found
let makeElement sizeX sizeY (centreEl: float * float) =
    [] 

let makeAll (centreHouse: float*float) h v  = 
    let house = makeElement Constants.houseL Constants.houseH centreHouse 
    // door height and window height is the same, divide by 1 + v, the heigh is 8/10 of that with the remaining 1/10 at the top and 1/10 at the bottom 
    let gridV = Constants.houseH / (1.0 + float v)
    let fixtureHeight = gridV* 0.8
    let gridH = Constants.houseH /  float h
    let fixtureWidth = gridH * 0.8 // set these constants by trial and error
    let windowsCentre = (fst(centreHouse), snd(centreHouse) + (1.5 * gridV))
    let doorCentre = (fst(centreHouse), snd(centreHouse)) // need to change!!!! the y is wrong 
    windows gridH gridV h v 
    |> List.map (List.map (makeElement fixtureHeight fixtureWidth)) // will need to change a bit (this is just skeleton)
    |> List.append (makeElement Constants.houseL Constants.houseH centreHouse )
    |> List.append (makeElement fixtureHeight (fixtureWidth / 2.0) doorCentre )
    

 
// TODO 
// calculate gridH, gridV, sizeX and sizeV from the set dimensions of the house 
// append to the final list the lines that make the house (given the center and the sizes) 
// append to the final list the lines that make the door
// actually make function that creates lines and react object 

    
let drawSymbolHook 
        (symbol:Symbol) 
        (theme:ThemeType) 
        : ReactElement list option =
    // replace the code below by your own code
    match symbol.Component.Type with
    | Constant1 (width,constValue, _) ->
        let leftCorner = symbol.Pos 
        let H = symbol.HScale
        let V = symbol.VScale
        // depending on whether H and V are defined the centre of the component is found (not sure whether H and V should be defined
        // because the description says they are only defined for custom components, does this classify as a custom component?)
        let centre = 
            match H, V with
            | Some h, Some v -> {X = leftCorner.X + (h/2.0); Y = leftCorner.Y + (v/2.0)}
            | None, None -> {X = leftCorner.X + (Constants.houseH /2.0); Y = (Constants.houseL /2.0)}
            | _ -> failwithf "should not happen: if one is defined the other one should be defined as well"
        
        
        printfn $"CONSTANT: width={width} ConstVale={constValue}"
    | _ -> printfn "Symbol Hook"
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
let updateWireHook 
        (model: BusWireT.Model) 
        (wire: Wire) 
        (tick3Helpers: Tick3BusWireHelpers)
        : Wire option =
    let segmentInfo =
        wire.Segments
        |> List.map (fun (seg:Segment) -> seg.Length,seg.Mode)
    printfn "%s" $"Wire: Initial Orientation={wire.InitialOrientation}\nSegments={segmentInfo}"
    None

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
