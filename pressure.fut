include world

fun apply_pressure (x: element) (p:u8) : element =
  if x == metal && p > u8(100)
  then nothing
  else x
