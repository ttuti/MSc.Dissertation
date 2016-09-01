
create.labelling.script <- function(row.data=NULL,loc=m.cache.src,script="label.R"){
  file.loc <- file.path(m.cache.src,script)
  if (!file.exists(file.loc)){
    file.create(file.loc)
  }
  script.line <- paste0('setattr(dataset$',gsub("$",".",row.data$ï..variable,fixed=T),',"label","',gsub("NA - ","",gsub('"','-',row.data$Label,fixed=T),fixed=T),'")','\n')
  cat(script.line, file = file.loc, append = TRUE)
  script.line <- paste0('setattr(dataset$',gsub("$",".",row.data$ï..variable,fixed=T),',"DHS.Code","',gsub("$",".",row.data$ï..variable,fixed=T),'")','\n')
  cat(script.line, file = file.loc, append = TRUE)
}

create.levelling.script <- function(var.data=NULL,loc=m.cache.src,script="level.R"){
  file.loc <- file.path(m.cache.src,script)
  if (!file.exists(file.loc)){
    file.create(file.loc)
  }
  
  var <- as.character(unique(var.data$variable)[1])
  
  if(!(var %in% c("M39"))){
  
      levels <- as.integer(as.character(var.data$value))
      
      exclude.na.vals <- c(994:999,9994:9999,99998:99999)
      
      if(!all(levels %in% exclude.na.vals)){
        
        levels <- paste0("c(",paste0(levels,collapse=","),")")
        
        labels <- as.character(var.data$Label)
        labels <- gsub('"',"'",labels)
        labels <- paste0('c("',paste0(labels,collapse='","'),'")')
        script.line <- paste0('dataset$',gsub("$",".",var,fixed=T),' <- factor(dataset$',gsub("$",".",var,fixed=T),',levels=',levels,',labels=make.unique(',labels,'), exclude=NULL)','\n')
        cat(script.line, file = file.loc, append = TRUE)
      }
  }
  
}

replicate.values.multiple.births <- function(col.vals=NA_character_){
  #browser()
  if(any(!is.na(col.vals))){
    val.to.replicate <- unique(col.vals[!is.na(col.vals)])
    if(length(val.to.replicate)==1)
      return(val.to.replicate)
  }else{
    return(col.vals)
  }
}


# cat("Reporting helper functions loaded!\n")