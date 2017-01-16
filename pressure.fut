include world

fun apply_pressure (x: element) (p:int) : element =
  if x == metal && p > 1000
  then sand
  else
  if x == stone && p > 1000
  then sand else x
