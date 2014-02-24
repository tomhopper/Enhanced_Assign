get_user <- function(){
  env <- if(.Platform$OS.type == "windows") "USERNAME" else "USER"
  unname(Sys.getenv(env))
}

assign_with_metadata <- function(x, value, ..., pos = parent.frame(), inherits = FALSE){
  attr(value, "creator") <- get_user()
  attr(value, "time_created") <- Sys.time()
  more_attr <- list(...)
  attr_names <- names(more_attr)
  for(i in seq_along(more_attr))  {
    attr(value, attr_names[i]) <- more_attr[[i]]
  }
  assign(x, value, pos = pos, inherits = inherits)
}

`%<-%` <- function(x, value){
  xname <- deparse(substitute(x))
  pos <- parent.frame()
  assign_with_metadata(xname, value, pos = pos)
}

`%<<-%` <- function(x, value) {
  xname <- deparse(substitute(x))
  pos <- globalenv()
  assign_with_metadata(xname, value, pos = pos)
} 