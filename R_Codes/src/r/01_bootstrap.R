#create file
create.file<-function(path,file.name,overwrite=F){
  if(!is.null(path))
    setwd(dir = path)
  for(i in file.name){
    exst=file.exists(i)
    if(!overwrite){
      if(!exst){
        cat("",file=i)
      }
    }else{
      cat("",file=i)
    }
  }
  setwd(dir=main.folder)
}

##chek empty generated files

check.empty.files=function(directory){
  
  directory=ifelse(grepl("[\\]",directory), 
                   gsub("[\\]","/" ,directory)
                   ,directory)
  setwd(directory)
  
  fls=list.files(directory, all.files=F)
  empty= fls[file.info(fls)[,1]==0]
  for(s in empty)
    message("\n\tThe script \"",s, "\" is empty! fill it with necessary before proceeding\n")
}


#pkg loader

pkgload=function(packages){
  inst <- packages %in% installed.packages()
  suppressPackageStartupMessages(
    suppressWarnings(
      lapply(packages, require, character.only=TRUE)))
}

libs_required <- c("foreign","gtools","RCurl","xlsx","stringr","Hmisc","limma","plyr","gdata","extrafont","scales","reshape2","ReporteRs","ggplot2","RColorBrewer","zoo","data.table","ineq")

lapply(libs_required, require, character.only = TRUE)

create.dir.if.missing<-function(path)
{
  if (!file.exists(path)) {
    return.value <- dir.create(path, showWarnings=FALSE, recursive = TRUE)
    if (!return.value) {
      stop("The directory \"", path.expand(path), "\" could not be created!")
    }
  }
  return(path)
}

today.date <- Sys.Date()-30
report.start.date <- format(today.date, format = "%d %b %Y")
m.dataset.name <- "dhs"
generate.lbl <- "auto.generated."
m.project.name <- "District Health Survey Analysis Output"

# Setting paths and file locations

m.rscripts.dir <- create.dir.if.missing(file.path(m.project.path, "src", "r"))
m.shell.scripts.dir <-create.dir.if.missing(file.path(m.project.path, "src", "shell"))
m.ds.doc <-create.dir.if.missing(file.path(m.project.path, "data"))
m.cache.doc <-create.dir.if.missing(file.path(m.project.path, "cache", "doc"))
m.cache.src <-create.dir.if.missing(file.path(m.project.path, "cache", "src"))
m.cache.ds <-create.dir.if.missing(file.path(m.project.path, "cache", "ds"))
m.cache.tmp <-create.dir.if.missing(file.path(m.project.path, "cache", "tmp"))
m.sys.asset <-create.dir.if.missing(file.path(m.project.path, "sys", "asset"))
m.sys.asset.css <-create.dir.if.missing(file.path(m.project.path, "sys", "asset", "css"))
m.sys.asset.img <-create.dir.if.missing(file.path(m.project.path,"sys","asset","img"))
m.sys.asset.doc <-create.dir.if.missing(file.path(m.project.path, "sys", "asset", "doc"))
m.sys.template <-create.dir.if.missing(file.path(m.project.path, "sys", "templ"))
m.report.location <-file.path(m.cache.doc)
m.external.data.loc <- "E:/SCHOOL/Dissertation/DHS Dataset/Data"
m.external.data.loc.hse <- "E:/SCHOOL/Dissertation/DHS Dataset/Household"
m.household.gini.data.loc <- "E:/SCHOOL/Dissertation/DHS Dataset/R_Codes/data"

dhs.analysis.cols <- c("V000","V001","V002","V003","V005","V012","V013","V024","V025","V106",
                       "V714","V717","V120","V121","V130","V131","V136","V137","V157","V158",
                       "V159","V190","V218","V201","V212","BORD","B0","B4","B5","HW1","HW8",
                       "HW13","HW71","M4","M34","M39","M39A","V414E","V414F","V414O","V411",
                       "V411A","V412","V414P","V414H","V414M","V414N","V414G","V414J","V414K","V414L","V414W")
# m.report.template.location <-file.path(m.sys.template, "Hospital_report_tmpl.docx")
# m.report.aop.location <- file.path(m.sys.asset.doc, "Hospital_Report_aop.csv")
# m.report.aop.script.location <- file.path(m.cache.src, "Report AOP Plugins.R")
# hash.freetext <- create.dir.if.missing(file.path(m.project.path, "data", "HashTable_V.csv"))
# hash.dropdown <- create.dir.if.missing(file.path(m.project.path, "data", "HashTable_Dropdown.csv"))
# venn.lbls <- create.dir.if.missing(file.path(m.project.path, "data", "venn.labels.csv"))

#seeting essentials for getting data from redcap

