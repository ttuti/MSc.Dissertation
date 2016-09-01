#Generate header comments for code files
#---------------------------------------


generate.header.comment<-function(
  summary
  ,file.name
  ,author = "Timothy Tuti"
  ,email = "timothy.nganga@postgrad.manchester.ac.uk"
  ,company = "University of Manchester"
  ,website = "www.manchester.ac.uk/"
  ,disclaimer = c(
    "This source code is owned and managed by Timothy Tuti."
    ,"Permission must be sought from the above mentioned entity for use, distribution or modification."
    ,"If you do not have permission, any violation of the above can constitute malice and criminal intent."
    ,"All other rights reserved."
  )) {
  
  #Closure to get file name (extract from path)
  
  get.file.name <- function(path)
  {
    path.length <- nchar(path)
    pos <- max(gregexpr("[\\\\/]", path)[[1L]])
    ret.val <- substr(path,pos, path.length)
    ret.val <- gsub("[\\\\/]", "", ret.val)
    return(ret.val)
  }
  
  #Define content (dummmy XML format)
  
  year <- substr(as.character(Sys.Date()), 1L, 4L)
  final.header <- character(0L)
  final.header <-c(final.header, " ")
  final.header <-c(final.header, paste0("<copyright file=\"", get.file.name(file.name), "\" company=\"", company, "\">"))
  final.header <-c(final.header, " ")
  final.header <-c(final.header, paste0("\tCopyright (c) ", year, " All rights reserved, ", website))
  final.header <-c(final.header, " ")
  final.header <-c(final.header, paste0("\t", disclaimer))
  final.header <-c(final.header, " ")
  final.header <-c(final.header, paste0("</copyright>"))
  final.header <-c(final.header, " ")
  final.header <-c(final.header, paste0("<author>", author, "</author>"))
  final.header <-c(final.header, paste0("<email>", email, "</email>"))
  final.header <-c(final.header, paste0("<date>", as.character(Sys.Date(), format = "%d %B, %Y"), "</date>"))
  final.header <-c(final.header, paste0("<summary>"))
  final.header <-c(final.header, " ")
  final.header <-c(final.header, paste0("\t", summary))
  final.header <-c(final.header, " ")
  final.header <-c(final.header, paste0("</summary>"))
  final.header <-c(final.header, " ")
  final.header <-paste0("#\t", final.header)
  final.header <-c(final.header, " ")
  
  #Add header comment to file
  
  file.contents <- readLines(file.name, warn = FALSE)
  file.contents <- paste(c(final.header, file.contents), collapse = "\n")
  cat(file.contents, file = file.name, append = FALSE)
}

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL){
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  if (numPlots==1) {
    print(plots[[1]])
  }else{
    grid.newpage() # Set up the page
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}