include world
include alchemy
include pressure

-- Position in a Margolus neighborhood; ranges from 0-3.
type marg_pos = int

-- A Margolus neighborhood.  We will just call these 'hood's, because
-- it is more gangsta.
type hood = (u8,u8,u8,u8)

type phood =(int,int,int,int)

-- The following two functions should be used for all hood
-- interaction.  Never just pattern patch directly on the value!
-- Pretend it is an abstract type.
fun hoodQuadrants ((ul,ur,dl,dr): hood): (element, element, element, element) =
  (ul,ur,dl,dr)

fun PhoodQuadrants ((ul,ur,dl,dr): phood): (int, int, int, int) =
  (ul,ur,dl,dr)

fun hoodFromQuadrants (ul: element) (ur: element) (dl: element) (dr: element): hood =
  (ul,ur,dl,dr)

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

fun indexToHood (offset: int) (i: int): (int, int) =
  if offset == 0 then (i / 2, i % 2)
  else ((i+1) / 2, (i+1) % 2)

-- Given a hood array at offset -1 or 0, return the element at index
-- (x,y).  Out-of-bounds returns 'nothing'.
fun hoodArrayIndex (offset: int) (elems: [w][h]hood) ((x,y): (int,int)): element =
  -- First, figure out which hood (x,y) is in.
  let (hx,ix) = indexToHood offset x
  let (hy,iy) = indexToHood offset y

  -- Then read if we are in-bounds.
  in if hx < 0 || hx >= w || hy < 0 || hy >= h
     then nothing
     else hoodQuadrant (unsafe elems[hx,hy]) (ix+iy*2)

-- From http://stackoverflow.com/a/12996028
fun hash(x: int): int =
  let x = ((x >> 16) ^ x) * 0x45d9f3b
  let x = ((x >> 16) ^ x) * 0x45d9f3b
  let x = ((x >> 16) ^ x) in
  x
val emptyHood : hood =
    (u8(0),u8(0),u8(0),u8(0))

val emptyPHood : phood =
    inthood emptyHood


fun inthood (hood:hood) : phood =
  let (ul,ur, dl , dr) = hood
  in
  (int(ul),int(ur),int(dl),int(dr))

fun hoodPress(hood1 : phood) (hood2 : phood) : phood =
  let (_, _, dl1 , dr1) = hood1
  let (ul2, ur2, dl2, dr2) = hood2
  let ul = if ul2 == 0 || (isWallInt dl1 && isWallInt ul2)  then 0 else
    if isWallInt dl1 then  dl1 + ul2 else if isWallInt ul2  then dl1 else dl1 + ul2

  let ur = if ul2 == 0 || (isWallInt dr1 && isWallInt ur2)  then 0 else
    if isWallInt dr1 then  dr1 + ur2 else if isWallInt ur2  then dr1 else dr1 + ur2

  let dl = if dl2 == 0 || (isWallInt ul2 && isWallInt dl2) then 0 else
    if isWallInt ul2 then  dl2 else if isWallInt dl2 then ul2 else dl1+ul2+dl2

  let dr = if dr2 == 0 || (isWallInt ur2 && isWallInt dr2) then 0 else
    if isWallInt ur2 then  dr2 else if isWallInt dr2 then ur2 else dr1+ur2+dr2

  in (ul, ur, dl, dr)


fun cpressure(hoodsc : [h]phood) : [h]phood =
  scan hoodPress emptyPHood hoodsc

fun hood_pressure (hoods: [w][h]phood) : [w][h]phood =
  map (fn x => cpressure x)  hoods

fun hoods_to_phoods(hoods: [w][h]hood) : [w][h]phood =
  (map (fn x_r => map inthood  x_r ) hoods)

-- An array with a "random" number for every hood.
fun hoodRandoms ((w,h): (int,int)) ((lower,upper): (int,int)) (gen: int): [w][h]int =
  reshape (w,h)
  (map (fn i => (hash (gen ^ i*4)) % (upper-lower+1) + lower) (iota (w*h)))

fun wall_thickness (hoods: [w][h]phood) : [w][h]phood =
  hoods
-- Compute interactions and aging for every hood, returning a new
-- array of hoods.
fun step (gen: int) (hoods: [w][h]hood) : [w][h]hood =
  let phoods = hoods_to_phoods hoods
  let hoodsPress = hood_pressure phoods
  let thickness = wall_thickness phoods
  let randomish = hoodRandoms (w,h) (0,100) gen
  let envs = map (fn randomish_r hoods_r hood_p => map interactions randomish_r hoods_r hood_p)
                 randomish hoods hoodsPress thickhness
  in map (fn r0 r1 => map ageHood r0 r1) randomish
     (map (fn r => map gravity r) envs)

-- Age every cell within a hood.  We use our (single) random number to
-- generate four new random numbers,which are then used for the aging.
fun ageHood (seed: int) (h: hood): hood =
  let (ul, ur, dl, dr) = hoodQuadrants h in
  hoodFromQuadrants (age (hash (seed^0) % 100) ul)
                    (age (hash (seed^1) % 100) ur)
                    (age (hash (seed^2) % 100) dl)
                    (age (hash (seed^3) % 100) dr)




fun interactions (r: int) (h: hood) (p: phood): hood =
  let h' = pressure h p
  in alchemy r h'

fun pressure (h: hood) (p: phood): hood =
  let (ul0, ur0, dl0, dr0) = hoodQuadrants h  in
  let (pul, pur, pdl, pdr) = PhoodQuadrants p  in
  let ul = apply_pressure ul0 pul
  let ur = apply_pressure ur0 pur
  let dr = apply_pressure dr0 pdr
  let dl = apply_pressure dl0 pdl
  in hoodFromQuadrants ul ur dl dr


-- Apply alchemy within a hood.
fun alchemy (r: int) (h: hood): hood =
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
