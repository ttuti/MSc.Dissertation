cat("\nLoading generic utility helper functions...\n")

# Formatting options for script generation (tabbing options):
#------------------------------------------------------------

add.tab <- function() {
  invisible(assign("m.tab", 
                   get("m.tab") + 1L,
                   envir = .GlobalEnv))
}
remove.tab <- function() {
  invisible(assign("m.tab", 
                   max(0L, get("m.tab") - 1L, na.rm = TRUE),
                   envir = .GlobalEnv))
}
reset.tab <- function() {
  invisible(assign("m.tab", 
                   0L,
                   envir = .GlobalEnv))
}
get.tab <- function() {
  return(paste0(rep("\t", 
                    get("m.tab"))
                , collapse = ""
  )
  )
}

# Trim string:
#-------------

trim.string <- function(x) {
  if(is.null(x))
  {
    return("")
  }
  if(is.na(x)) 
  {
    return("")
  }
  x <- as.character(x)
  if(nchar(x) > 0L) {
    left.end <- regexpr("[^[:space:]]", x)
    x <- substr(x, left.end, nchar(x))
    rm(left.end)
    reverse <- paste0(rev(strsplit(x, "")[[1L]]), collapse = "")
    right.end <- regexpr("[^[:space:]]", reverse)
    ret.val <- substr(reverse, right.end, nchar(reverse))
    ret.val<-paste0(rev(strsplit(ret.val, "")[[1L]]), collapse = "")
    return(ret.val)
  }
  return(x)
}

# Tab a character string:
#------------------------

replace.leading.space.with.tab <- function(x) {
  if (regexpr("[^[ \t]+", x) > 0L) {
    stop.val <- attr(regexpr("^[ \t]+", x), "match.length")
    substr(x, 1L, stop.val) <- paste0(rep("\t", nchar(substr(x, 1L, stop.val))) ,collapse = "")
  }
  return(x)
}