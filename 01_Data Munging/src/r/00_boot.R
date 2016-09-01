
rm(list=ls(all=T))
cat('________________________________________________________________________________________________________________________\n\n')
cat('Welcome ', Sys.getenv('USERNAME'), '!\n\n', sep = '')
cat('The date today is', format(Sys.Date(), '%d %B %Y.'), '\n')
cat('Reporting session started at ', format(Sys.time(), '%I:%M %p'), '\n\n', sep = '')

# Set project path
setwd("E:/SCHOOL/Dissertation/DHS Dataset/R_Codes")
home.dir <- "E:/SCHOOL/Dissertation/DHS Dataset"
main.folder<-file.path(home.dir, "R_Codes")
m.project.path <- path.expand(file.path(home.dir,"R_Codes"))

m.project.path <- gsub("\\\\","/",m.project.path)
if(!file.exists(m.project.path)) {
  if(dir.create(m.project.path, recursive = TRUE))
    stop("The project directory \""
         ,m.project.path
         ,"\"  has been created!\nPlease fill it with the relevant files and folders!")
  else
    stop("The project directory \""
         ,m.project.path
         ,"\"  could not be created!")
}


# Run scripts

run.app <- function(files) {
  run.app.internal <- function(file) {
    if(!file.exists(file)) {
      stop("The file \"", file, "\" does not exist!\n")
    }
    source(file, max.deparse.length =Inf, echo=FALSE)
  }
  invisible(sapply(files, run.app.internal))
}
files <- sort(list.files(file.path(m.project.path, "src", "r")))
files <- files[-which(files %in% c("00_boot.R"))]
files <- files[order(files)]
files <- files[grepl("[[:digit:]{2}]", files)]
files <- file.path(m.project.path, "src", "r", files)
invisible(run.app(files))

#Clean up

rm(files)
rm(run.app)


# Display message

cat("\n\n", "Report generation ended at ", format(Sys.time(), "%I:%M %p"), sep = "")
cat("\n\nGoodbye ", Sys.getenv("USER"), "!\n\n", sep = "")
cat("----------------------------------------\n\n")
Sys.sleep(5)
