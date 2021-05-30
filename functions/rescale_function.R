ReScale <- function(x, first, last){
  (last - first)/(max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) * (x-min(x, na.rm = TRUE)) + first
}