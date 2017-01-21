include world


fun apply_pressure (x: element) (p:int) (t:int) : element =
  if x == metal && p > t
  then sand
  else if x == wall && p > t
  then sand
  else x
