include world

fun apply_pressure (x: element) (p:int) : element =
  if x == metal && p > 100
  then sand else x
