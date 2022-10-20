#function generator
defunct <- function(msg = "This function is depreciated") function(...) return(stop(msg))
