include world

fun apply_pressure (x: element) (p:int) : element =
  if x == metal && p > 200
  then sand
  else x
