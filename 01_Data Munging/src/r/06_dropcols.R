###
#
# Drop columns you will not use
#
##

data.loc <- m.cache.ds

rdata.files <- list.files(path= data.loc,recursive=T, pattern="*.RData", full.names = T)

data.list <- vector("list", 3) 

for(i in 1:length(rdata.files)){
  rdata.loc <- rdata.files[i]
  load(rdata.loc)
  last.idx.slash <- regexpr("\\/[^\\/]*$", rdata.loc)
  last.idx.slash <- last.idx.slash[[1]] + 1
  dataset.name <- substr(rdata.loc,last.idx.slash,nchar(rdata.loc))
  dataset.name <- substr(dataset.name,1,(regexpr('.', dataset.name,fixed=T)[[1]])-1)
  dataset <- copy(get(dataset.name))
  rm(list = ls(pattern = dataset.name)) 
  dataset[,key:=.I]
  setkey(dataset,key)
  dataset[,V000:=dataset.name]
  setnames(dataset,c("V000","V001","V002","V003","V005","V012","V013","V024","V025"),
           c("Country","Cluster","Household","Respondent","Weight.Women","Age","Age.Group","Region","Residence.Type"))
  setnames(dataset,c("V106","V714","V717","V120","V121"),c("Highest.Edu.Lvl","Employed","Occupational.Class","Radio","TV"))
  setnames(dataset,c("V130","V131","V136","V137"),c("Religion","Ethnicity","Household.Size","Children.U5"))
  setnames(dataset,c("V157","V158","V159"),c("Newspaper.Freq","Radio.Freq","TV.Freq"))
  setnames(dataset,c("V190","V201","V212","V218"),c("Wealth.Idx","Births.N","Age.First.Birth","Children.Alive.N"))
  setnames(dataset,c("BORD","B0","B4","B5","HW1","HW8","HW13","HW71"),
           c("Birth.Order","N.Births","Child.Sex","Child.Alive","Child.Age","Child.WAM","Measured","Child.WAZ"))
  setnames(dataset,c("M4","M34","M39A","M39"),c("Duration.Breastfeed","Time.To.Breastfeed","Fed.Solid.Foods","Meal.Freq"))
  
  #Abit of recoding
  dataset[Child.Age < 24 ,Early.Init.Breastfed:=0L,by=key]
  dataset[Child.Age < 24 & 
            Time.To.Breastfeed %in% c("Immediately","Within first hour") &
            !(Duration.Breastfeed=="Never breastfed"),Early.Init.Breastfed:=1L,by=key]
  
  dataset[Child.Age < 6 ,Exclusive.Breastfeeding:=0L,by=key]
  dataset[Child.Age < 6 & Fed.Solid.Foods == "No",Exclusive.Breastfeeding:=1L,by=key]
  
  dataset[Child.Age >= 6 & Child.Age < 9, Intro.Complementary.Foods:=0L,by=key]
  dataset[Child.Age >= 6 & Child.Age < 9 & Fed.Solid.Foods == "Yes",Intro.Complementary.Foods:=1L,by=key]
  
  dataset[Child.Age >= 12 & Child.Age < 15, Continued.Breastfeeding:=0L,by=key]
  dataset[Child.Age >= 12 & Child.Age < 15 & Duration.Breastfeed == "Still breastfeeding",Continued.Breastfeeding:=1L,by=key]
  
  dataset[Child.Age >= 6 & Child.Age < 24 ,Fortified.Foods:=0L,by=key]
  dataset[Child.Age >= 6 & Child.Age < 24  & V414W == "No",Fortified.Foods:=1L,by=key]
  
  dataset[Child.Age >= 6 & Child.Age < 24,Recommended.Feeding.Freq:=0L,by=key]
  dataset[(Duration.Breastfeed == "Still breastfeeding" & Fed.Solid.Foods == "Yes") &
          ((Child.Age >= 6 & Child.Age < 9) & (!is.na(Meal.Freq) & Meal.Freq >= 2)| 
           (Child.Age >= 9 & Child.Age < 24) & (!is.na(Meal.Freq) & Meal.Freq >= 3)),Recommended.Feeding.Freq:=1L,by=key]
  
  dataset[(Duration.Breastfeed != "Still breastfeeding" & Fed.Solid.Foods == "Yes") &
          (Child.Age >= 6 & Child.Age < 24) & (!is.na(Meal.Freq) & Meal.Freq >= 4),
          Recommended.Feeding.Freq:=1L,by=key]
  
  dataset[,paste0(c("grains","tubers","legumes","dairy","flesh","eggs","fruits.veg")):=0L]
  dataset[V414E=="Yes",grains:=1L,by=key]
  dataset[V414F=="Yes",tubers:=1L,by=key]
  dataset[V414O=="Yes",legumes:=1L,by=key]
  dataset[V414P=="Yes"|V411=="Yes"|V411A=="Yes"|V412=="Yes",dairy:=1L,by=key]
  dataset[V414H=="Yes"|V414M=="Yes"|V414N=="Yes",flesh:=1L,by=key]
  dataset[V414J=="Yes"|V414K=="Yes"|V414L=="Yes",fruits.veg:=1L,by=key]
  dataset[V414G=="Yes",eggs:=1L,by=key]
  
  dataset[Child.Age >= 6 & Child.Age < 24,Dietary.Diversity:=0]
  dataset[Child.Age >= 6 & Child.Age < 24,Dietary.Diversity:=rowSums(.SD),
          .SDcols=c("grains","tubers","legumes","dairy","flesh","eggs","fruits.veg"),by=key]
  
  dataset[Child.Age >= 6 & Child.Age < 24,Acceptable.Diet:=0L]
  dataset[Child.Age < 6 & Duration.Breastfeed == "Still breastfeeding",Acceptable.Diet:=1L,by=key]
  dataset[Child.Age >= 6 & Child.Age < 24 & Dietary.Diversity >=4 & Recommended.Feeding.Freq==1,Acceptable.Diet:=1L,by=key]
  
  dataset[,paste0(c("grains","tubers","legumes","dairy","flesh","eggs","fruits.veg")):=NULL]
  dataset[,paste0(c("V414E","V414F","V414O","V411","V411A","V412","V414P","V414H","V414M","V414N","V414G","V414J","V414K","V414L","V414W")):=NULL]
  
  
  cols.to.clean <- c("Child.WAM","Child.WAZ")
  missing.vals <- c(c(9996:9999),c(99998:99999))
  
  for(i in cols.to.clean){
    str.cmd <- paste0("dataset[!is.na(",i,") & ",i," %in% missing.vals,",i,":=NA_integer_,by=key]")
    eval(parse(text=str.cmd))
    str.cmd <- paste0("dataset[,",i,":=as.numeric(",i,")]")
    eval(parse(text=str.cmd))
    str.cmd <- paste0("dataset[,",i,":=as.numeric(round(",i,"/100,2))]")
    eval(parse(text=str.cmd))
  }
  
  if(haskey(dataset)){
    setkey(dataset,NULL)
  }
  suppressWarnings(dataset[,key:=NULL])
  data.list[[dataset.name]] <- dataset
}

dataset.new <- rbindlist(data.list,use.names=T)
dataset.new$Country <- factor(dataset.new$Country)
rm(data.list)

#Copy attributes
for(i in names(dataset)){
  str.cmd <- paste0("setattr(dataset.new$`",i,"`,'label',attr(dataset$`",i,"`,'label'))")
  eval(parse(text=str.cmd))
  str.cmd <- paste0("setattr(dataset.new$`",i,"`,'DHS.Code',attr(dataset$`",i,"`,'DHS.Code'))")
  eval(parse(text=str.cmd))
}
dataset <- copy(dataset.new)
dataset[,key:=.I]
setkey(dataset,key)
save(dataset,file= paste0(m.ds.doc,"/dataset.RData"))
rm(dataset.new)
rm(list=c("last.idx.slash","dataset.name","rdata.loc","rdata.files","str.cmd"))

cat("Data columns renamed and irrelevant columns removed!\n")
