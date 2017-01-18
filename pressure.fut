include world

fun apply_pressure (x: element) (p:int) (t:int) : element =
  if x == metal && p > (200*(t+1))
  then sand
  else
  x
