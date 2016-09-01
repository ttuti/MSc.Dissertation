
if (!is.element("devtools", .packages(all.available = T))) install.packages("devtools")
library(devtools)

load.lib <- m.external.data.loc

if(length(dir(path=m.cache.ds,all.files =TRUE,pattern="*.RData")) ==0){
  
  cat("\tLoading data and metadata...\n")

  csv.files <- list.files(path= load.lib,recursive=T, pattern="*.csv", full.names = T)
  for (i in 1:length(csv.files)){
    
    file.loc.lbl <- file.path(m.cache.src,"label.R")
    file.loc.lvl <- file.path(m.cache.src,"level.R")
    labels.exist <- F
    
    last.idx.slash <- regexpr("\\/[^\\/]*$", csv.files[i])
    country.year <- gsub(paste0(load.lib,"/"),"",csv.files[i])
    first.idx.slash <- regexpr("[\\/].+", country.year)
    name.idx.slash <- regexpr("\\/[^\\/]*$", country.year)[[1]]-1
    idx <- last.idx.slash[[1]] + 1
    idx.first <- first.idx.slash[[1]]-1
    dataset.name <- substr(csv.files[i],idx,nchar(csv.files[i]))
    sub.data.name <- substr(country.year,first.idx.slash[[1]]+1,name.idx.slash)
    sub.data.name <- gsub(" ","",sub.data.name)
    country.year <- substr(country.year,1,idx.first)
    
    if(grepl("_DD",toupper(dataset.name))){
      tmp.data <- as.data.table(read.csv(csv.files[i]))
      tmp.data <- tmp.data[ï..variable %in% dhs.analysis.cols,copy(.SD)]
      tmp.data[,key:=.I]
      setkey(tmp.data,key)
      label.script.val.lbl <- tmp.data[value=="Variable Label",copy(.SD)]
      if(nrow(label.script.val.lbl) > 0){
        labels.exist <- T
        label.script.val.lbl[,create.labelling.script(.SD),by=key]
      }
      
      lvl.script.fc <- data.table::copy(tmp.data[value!="Variable Label",(.SD)])
      #setnames(lvl.script.fc,"`ï..variable`","variable")
      names(lvl.script.fc) <- c("variable", "value", "Label", "key")
      lvl.script.fc$key <- NULL
      lvl.script.fc <- as.data.frame(lvl.script.fc)
      lvl.script.fc.vars <- split(lvl.script.fc,lvl.script.fc$variable,drop=T)
      
      create.levels <- lapply(lvl.script.fc.vars,create.levelling.script)
      
      if(exists("dataset")){
        if("ï..CASEID" %in% names(dataset))
          setnames(dataset,"ï..CASEID","CASEID")
        if("ï..HHID" %in% names(dataset))
          setnames(dataset,"ï..HHID","HHID")
        if("ï..MCASEID" %in% names(dataset))
          setnames(dataset,"ï..MCASEID","MCASEID")
        if("ï..VAHHID" %in% names(dataset))
          setnames(dataset,"ï..VAHHID","VAHHID")
        
        source(file=file.loc.lvl)
        if(labels.exist){
          source(file=file.loc.lbl)
        }
      }
      country.year <- gsub(" ", "",country.year)
      country.year.file <- paste0(country.year,".",sub.data.name,".RData")
      file.data <- file.path(m.cache.ds,country.year.file)
      
      assign(country.year,dataset)
      save(list=country.year,file=file.data, envir = .GlobalEnv)
      #Clean up
      rm(tmp.data)
      rm(label.script.val.lbl)
      rm(lvl.script.fc)
      rm(lvl.script.fc.vars)
      rm(dataset)
      rm(create.levels)
      rm(list=country.year)
      rm(list=c("last.idx.slash","idx","idx.first","first.idx.slash","sub.data.name",
                "name.idx.slash","country.year.file"))
      
      if(file.exists(file.loc.lbl)){
        unlink(file.loc.lbl,force = T)
      }
      if(file.exists(file.loc.lvl)){
        unlink(file.loc.lvl,force=T)
      }
    }else{
      dataset <- as.data.table(read.csv(csv.files[i]))
      dataset <- dataset[,copy(.SD),.SDcols=dhs.analysis.cols]
      dataset[,key:=.I]
      setkey(dataset,key)
    }
  }#End of for loop
}
cat("Data columns labeled, leveled, and saved as R objects!\n")

