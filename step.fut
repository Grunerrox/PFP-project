include world
include alchemy
include pressure

-- Position in a Margolus neighborhood; ranges from 0-3.
type marg_pos = i32

-- A Margolus neighborhood.  We will just call these 'hood's, because
-- it is more gangsta.
type hood = (u8,u8,u8,u8)


type phood = ((i32,bool),(i32,bool),(i32,bool),(i32,bool))

type inthood = (i32,i32,i32,i32)

-- The following two functions should be used for all hood
-- interaction.  Never just pattern patch directly on the value!
-- Pretend it is an abstract type.
fun hoodQuadrants ((ul,ur,dl,dr): hood): (element, element, element, element) =
  (ul,ur,dl,dr)

fun PhoodQuadrants ((ul,ur,dl,dr): phood): ((i32,bool), (i32,bool), (i32,bool), (i32,bool)) =
  (ul,ur,dl,dr)

fun InthoodQuadrants ((ul,ur,dl,dr): inthood): (i32,i32,i32,int) =
  (ul,ur,dl,dr)

fun hoodFromQuadrants (ul: element) (ur: element) (dl: element) (dr: element): hood =
  (ul,ur,dl,dr)


fun wall_val(e :i32) : i32 =
  if isWallI32 e then 0 else e


fun hood_to_phood(hood : hood): phood =
  let (ul, ur, dl, dr ) = hood
  in ((wall_val (i32 ul), isWall ul), (wall_val (i32 ur), isWall ur), (wall_val (i32 dl), isWall dl),(wall_val (i32 dr), isWall dr))

fun phood_to_inthood(hood : phood): inthood =
  let ( (ul,_), (ur,_), (dl,_), (dr,_) ) = hood
  in (ul, ur, dl, dr)

fun hood_to_inthood(hood : hood): inthood =
  let (ul, ur, dl, dr ) = hood
  in ((i32 ul), (i32 ur), (i32 dl), (i32 dr))

-- Return the requested quadrant from the given hood.
fun hoodQuadrant (h: hood) (i: marg_pos): element =
  let (ul0, ur0, dl0, dr0) = hoodQuadrants h in
  if      i == 0 then ul0
  else if i == 1 then ur0
  else if i == 2 then dl0
  else                dr0

-- Return the requested quadrant from the given hood.
fun setHoodQuadrant (h: hood) (i: marg_pos) (x: element): hood =
  let (ul0, ur0, dl0, dr0) = hoodQuadrants h in
  if      i == 0 then hoodFromQuadrants x ur0 dl0 dr0
  else if i == 1 then hoodFromQuadrants ul0 x dl0 dr0
  else if i == 2 then hoodFromQuadrants ul0 ur0 x dr0
  else                hoodFromQuadrants ul0 ur0 dl0 x

-- Swap to quadrants in a hood.
fun swapHoodQuadrants (h: hood) (i: marg_pos) (j: marg_pos): hood =
  let x = hoodQuadrant h i
  let y = hoodQuadrant h j
  in setHoodQuadrant (setHoodQuadrant h i y) j x

-- Make sure the permutation has no duplicate entries.
fun permuteHoodQuadrants (h: hood) ((ul,ur,dl,dr): (marg_pos, marg_pos, marg_pos, marg_pos)): hood =
  hoodFromQuadrants (hoodQuadrant h ul) (hoodQuadrant h ur)
                    (hoodQuadrant h dl) (hoodQuadrant h dr)

fun indexToHood (offset: i32) (i: i32): (i32, i32) =
  if offset == 0 then (i / 2, i % 2)
  else ((i+1) / 2, (i+1) % 2)

-- Given a hood array at offset -1 or 0, return the element at index
-- (x,y).  Out-of-bounds returns 'nothing'.
fun hoodArrayIndex (offset: i32) (elems: [w][h]hood) ((x,y): (i32,i32)): element =
  -- First, figure out which hood (x,y) is in.
  let (hx,ix) = indexToHood offset x
  let (hy,iy) = indexToHood offset y

  -- Then read if we are in-bounds.
  in if hx < 0 || hx >= w || hy < 0 || hy >= h
     then nothing
     else hoodQuadrant (unsafe elems[hx,hy]) (ix+iy*2)

-- From http://stackoverflow.com/a/12996028
fun hash(x: i32): i32 =
  let x = ((x >> 16) ^ x) * 0x45d9f3b
  let x = ((x >> 16) ^ x) * 0x45d9f3b
  let x = ((x >> 16) ^ x) in
  x
val emptyHood : hood =
    (u8(0),u8(0),u8(0),u8(0))

val emptyPHood : phood =
    hood_to_phood emptyHood

fun press_func(e1: i32, isWall1 : bool) (e2:i32, isWall2 :bool) : (i32,bool) =
  if isWall1 then (e2, isWall2) else
   if isWall2 then (e1 + e2,true) else
      if e2 == 0 then (0,false) else (e1 + e2, false)

fun hoods_press(hood1 : phood) (hood2 : phood): phood =
  let (_, _, dl1 , dr1) = hood1
  let (ul2, ur2, dl2 , dr2) = hood2
  in (press_func dl1 ul2, press_func dr1 ur2, press_func dl1 dl2, press_func dr1 dr2)


fun hoodPress(hood1 : phood) : phood =
  let (ul, ur, dl , dr) = hood1
  in (ul, ur, press_func ul dl, press_func ur dr)


fun cpressure(hoodsc : [h]phood) : [h]phood =
  let chood = (map (fn x_r => hoodPress x_r ) hoodsc)
  in scan hoods_press emptyPHood chood

fun hood_pressure (hoods: [w][h]phood) : [w][h]inthood =
  let press_hood =  map (fn x => cpressure x)  hoods
  in phoods_to_inthoods press_hood

fun phoods_to_inthoods(phoods: [w][h]phood) : [w][h]inthood =
  (map (fn x_r => map phood_to_inthood  x_r ) phoods)

fun hoods_to_phoods(hoods: [w][h]hood) : [w][h]phood =
  (map (fn x_r => map hood_to_phood  x_r ) hoods)

-- fun hoods_to_inthoods(hoods: [w][h]hood) : [w][h]inthood =
--   (map (fn x_r => map hood_to_inthood  x_r ) hoods)

-- An array with a "random" number for every hood.
fun hoodRandoms ((w,h): (i32,i32)) ((lower,upper): (i32,i32)) (gen: i32): [w][h]i32 =
  reshape (w,h)
  (map (fn i => (hash (gen ^ i*4)) % (upper-lower+1) + lower) (iota (w*h)))


-- Apply thickness
fun thickness_func(e1: i32, isWall1 : bool) (e2: i32, isWall2 : bool) : (i32, bool) =
  if isWall1 e1 then (e1,true) else
   if isWall e2 then (e1 + e2,true) else (0,false)

fun wall_thick(hood1 : phood) (hood2 : phood): phood =
  let (ul1,ur1,dl1,dr1) = hood1
  let (ul2,ur2,dl2,dr2) = hood2
  -- since hoods arent flipped we need to call them opposite.
  in (thickness_func ul1 dl2, thickness_func ur1 dr2, thickness_func ul1 dl2, thickness ur1 ur2)

fun wallThickness (hood : phood) : phood =
  let (ul,ur,dl,dr) = hood
  in (ul, ur, thickness_func ul dl, thickness_func ur dr)

fun cthickness (hoods: [h]phood) : [h]phood =
  let chood = (map (fn x_r => wallThickness x_r ) hoods)
  let flipped_hood = chood[::-1]
  in scan wall_thick emptyPHood flipped_hood

fun wall_thickness (hoods: [w][h]phood) : [w][h]inthood =
  let thick_hood = (map (fn x => cthickness x ) hoods)
  in phoods_to_inthoods thick_hoods



-- Compute interactions and aging for every hood, returning a new
-- array of hoods.
fun step (gen: i32) (hoods: [w][h]hood) : [w][h]hood =
  let phoods = hoods_to_phoods hoods
  let hoodsPress = hood_pressure phoods
  let thickness = wall_thickness phoods
  let randomish = hoodRandoms (w,h) (0,100) gen
  let envs = map (fn randomish_r hoods_r hood_p => map interactions randomish_r hoods_r hood_p)
                 randomish hoods hoodsPress --thickness
  in map (fn r0 r1 => map ageHood r0 r1) randomish
     (map (fn r => map gravity r) envs)

-- Age every cell within a hood.  We use our (single) random number to
-- generate four new random numbers,which are then used for the aging.
fun ageHood (seed: i32) (h: hood): hood =
  let (ul, ur, dl, dr) = hoodQuadrants h in
  hoodFromQuadrants (age (hash (seed^0) % 100) ul)
                    (age (hash (seed^1) % 100) ur)
                    (age (hash (seed^2) % 100) dl)
                    (age (hash (seed^3) % 100) dr)




fun interactions (r: i32) (h: hood) (p: inthood): hood =
  let h' = pressure h p
  in alchemy r h'

fun pressure (h: hood) (p: inthood): hood =
  let (ul0, ur0, dl0, dr0) =  hoodQuadrants h
  let (pul, pur, pdl, pdr) =  InthoodQuadrants p
  let ul = apply_pressure ul0 pul
  let ur = apply_pressure ur0 pur
  let dr = apply_pressure dr0 pdr
  let dl = apply_pressure dl0 pdl
  in hoodFromQuadrants ul ur dl dr


-- Apply alchemy within a hood.
fun alchemy (r: i32) (h: hood): hood =
  let (ul0, ur0, dl0, dr0) = hoodQuadrants h  in
  if ul0 == ur0 && ur0 == dl0 && dl0 == dr0
  then h
  else -- Apply interaction among the components
       let (ul1, ur1) = applyAlchemy r ul0 ur0
       let (ur , dr2) = applyAlchemy r ur1 dr0
       let (dr , dl3) = applyAlchemy r dr2 dl0
       let (dl , ul ) = applyAlchemy r dl3 ul1
       in hoodFromQuadrants ul ur dl dr



-- Apply gravity within a hood.
fun gravity (h: hood): hood =
  let (ul, ur, dl, dr) = hoodQuadrants h

  let (ul, ur, dl, dr) =
    -- First check for fluid flow.
    if ((isFluid dl && dr == nothing) || (isFluid dr && dl == nothing)) &&
       isFluid ul && isFluid ur
    then (ul, ur, dr, dl)
    else if isFluid ul && weight ur < weight ul && dl != nothing && dr != nothing && ! isWall dl && ! isWall dr
    then (ur, ul, dl, dr)
    else if isFluid ur && weight ul < weight ur && dl != nothing && dr != nothing && ! isWall dl && ! isWall dr
    then (ur, ul, dr, dl)
    else if isFluid dl && weight ul < weight dl && weight ur < weight dl && weight dr < weight dl
    then (ul, ur, dr, dl)
    else if isFluid dr && weight ul < weight dr && weight ur < weight dr && weight dl < weight dr
    then (ul, ur, dr, dl)

    -- No fluid flow?  Let gravity do its work.
    else let (ul, dl) = checkIfDrop ul dl
         let (ur, dr) = checkIfDrop ur dr
         let (ul, dr) = checkIfDrop ul dr
         let (ur, dl) = checkIfDrop ur dl
         in (ul, ur, dl, dr)

  in hoodFromQuadrants ul ur dl dr

fun checkIfDrop (above: element) (below: element): (element, element) =
  if isWall above || isWall below || weight below >= weight above
  then (above, below)
  else (below, above)
