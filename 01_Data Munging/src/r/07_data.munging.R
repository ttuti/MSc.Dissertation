
cat("\nData munging... Create Indicatators\n")

U2.dataset <- dataset[!is.na(Child.WAZ) & Child.Age >= 6 & Child.Age < 24 & Measured=="Measured",copy(.SD)]
setattr(U2.dataset,"desc","U2 Anthroprometrics Living children")

U2.dataset[,Country.Cluster:=paste0(Country,".",Cluster),by=key]

U2.dataset[,Urban.Residence:=0]
U2.dataset[Residence.Type=="Urban",Urban.Residence:=1,by=key]
setattr(U2.dataset$Urban.Residence,'label',attr(dataset$Residence.Type,'label'))
setattr(U2.dataset$Urban.Residence,'DHS.Code',attr(dataset$Residence.Type,'DHS.Code'))
U2.dataset[,Residence.Type:=NULL]


U2.dataset[,Mother.Employed:=0]
U2.dataset[Employed=="Yes",Mother.Employed:=1,by=key]
setattr(U2.dataset$Mother.Employed,'label',attr(dataset$Employed,'label'))
setattr(U2.dataset$Mother.Employed,'DHS.Code',attr(dataset$Employed,'DHS.Code'))
U2.dataset[,Employed:=NULL]

#Response Variable
#Children's Nutrition Status

#Underweight: weight for age < -2 standard deviations (SD) of the WHO Child Growth Standards median
#Stunting: height for age < -2 SD of the WHO Child Growth Standards median
#Wasting: weight for height < -2 SD of the WHO Child Growth Standards median
#Overweight: weight for height > +2 SD of the WHO Child Growth Standards median 

#Used WAZ
U2.dataset[!is.na(Child.WAZ) & 
             Child.WAZ > as.numeric(2),Nutrition.Status:=1L,by=key]
U2.dataset[!is.na(Child.WAZ) & 
             Child.WAZ <= as.numeric(2) & 
             Child.WAZ >= as.numeric(-2),Nutrition.Status:=2L,by=key]
U2.dataset[!is.na(Child.WAZ) & 
             Child.WAZ < as.numeric(-2),Nutrition.Status:=3L,by=key]
U2.dataset[!is.na(Child.WAZ) & 
             Child.WAZ < as.numeric(-3),Nutrition.Status:=4L,by=key]

U2.dataset[,Nutrition.Status:=factor(Nutrition.Status,levels=c(1L:4L),
                                     labels=c("Overweight","Healthy","Moderately Underweight","Severely Underweight"))]

U2.dataset[,merge.key:=paste0(Cluster,"_",Household),by=key]
U2.dataset.country <- split(U2.dataset,U2.dataset$Country)

load.lib.house <- m.external.data.loc.hse
csv.files <- list.files(path= load.lib.house,recursive=T, pattern="*.csv", full.names = T)

i <- 1
library(foreign)

U2.household.country <- lapply(U2.dataset.country,function(set.country,country.names){
  i <- get("i",envir = .GlobalEnv)
  file.name <- country.names[i]
  use.csv <- csv.files[grep(file.name,csv.files)]
  household.data <- data.table(read.csv(file=use.csv))
  household.data[,key:=.I]
  setkey(household.data,key)
  household.data[,merge.key:=paste0(HV001,"_",HV002),by=key]
  setkey(household.data,NULL)
  setkey(set.country,NULL)
  setkey(set.country,merge.key)
  setkey(household.data,merge.key)
  household.gini <- merge(household.data,set.country,by="merge.key")
  
  #Get wealth group
  wealth.min <- min(household.data$HV271)
  wealth.max <- max(household.data$HV271)
  wealth.range <- wealth.max - wealth.min
  household.data$wealth.group <- as.integer((household.data$HV271 - wealth.min)/(wealth.range/99)) + 1
  
  household.data <- household.data[,copy(.SD),.SDcols=c("wealth.group","merge.key")]
  country.hse.wealth <- merge(household.data,set.country,by="merge.key")
  
  use.dta <- gsub(".csv",".dta",use.csv)
  write.dta(household.gini,file=use.dta)
  i = i+1
  assign("i",i,.GlobalEnv)
  
  return(country.hse.wealth)
},names(U2.dataset.country))

i <- 1
dta.files <- list.files(path= m.household.gini.data.loc,recursive=T, pattern="*.dta", full.names = T)
U2.household.country <- lapply(U2.household.country,function(set.country,country.names){
  i <- get("i",envir = .GlobalEnv)
  file.name <- country.names[i]
  use.dta <- dta.files[grep(file.name,dta.files)]
  hse.wealth.data <- data.table(read.dta(file=use.dta))
  hse.wealth.data <- hse.wealth.data[,copy(.SD),.SDcols=c("w_group","gini")]
  
  setkey(hse.wealth.data,NULL)
  setkey(set.country,NULL)
  setkey(set.country,wealth.group)
  setkey(hse.wealth.data,w_group)
  kr.hse.gini <- merge(set.country,hse.wealth.data,by.x="wealth.group",by.y="w_group",all.x = T)
  
  i = i+1
  assign("i",i,.GlobalEnv)
  
  return(kr.hse.gini)
},names(U2.household.country))

U2.household <- rbindlist(U2.household.country)

U2.household[,wealth.group:=NULL]
U2.household[,merge.key:=NULL]
U2.household[,key:=NULL]
U2.dataset[,merge.key:=NULL]
U2.dataset[,key:=NULL]

rm(U2.household.country)
rm(U2.dataset.country)

setnames(U2.household,"gini","GINI")

#Copy attributes
for(i in names(U2.dataset)){
  str.cmd <- paste0("setattr(U2.household$`",i,"`,'label',attr(U2.dataset$`",i,"`,'label'))")
  eval(parse(text=str.cmd))
  str.cmd <- paste0("setattr(U2.household$`",i,"`,'DHS.Code',attr(U2.dataset$`",i,"`,'DHS.Code'))")
  eval(parse(text=str.cmd))
}

U2.dataset <- copy(U2.household) 
rm(U2.household)
save(U2.dataset,file = paste0(m.ds.doc,"/dataset.household.RData"))

cat("Get supplement columns from household dataset!\n")

U2.dataset[,key:=.I]
U2.dataset[,cons:=1L]
setkey(U2.dataset,key)

#Create Media Indicators
U2.dataset[,Media.Exposure:=1L]
U2.dataset[((!is.na(Radio)&Radio=="Yes")&(!is.na(TV)&TV!="Yes"))|
           ((!is.na(Radio)&Radio!="Yes")&(!is.na(TV)&TV=="Yes")),Media.Exposure:=2L,by=key]
U2.dataset[(!is.na(Radio)&Radio=="Yes")&
           (!is.na(TV)&TV=="Yes"),Media.Exposure:=3L,by=key]
U2.dataset[,Media.Exposure:=factor(Media.Exposure,levels=c(1:3),labels=c("No Exposure to TV/Radio","Exposure to either TV/Radio","Exposure to both TV and Radio"))]

U2.dataset[,paste0(c("TV","Radio")):=NULL]


U2.dataset <- U2.dataset[Nutrition.Status!="Overweight",copy(.SD)]
U2.dataset[,Nutrition.Status:=factor(Nutrition.Status)]
U2.dataset[,key:=.I]
setkey(U2.dataset,key)

U2.dataset[,Malnourished:=0L]
U2.dataset[Nutrition.Status!= "Healthy",Malnourished:=1L,by=key]

U2.dataset[,Survived.Ratio:=NA_real_]
U2.dataset[!is.na(Children.Alive.N) & !is.na(Births.N),Survived.Ratio:=round(Children.Alive.N/Births.N,2),by=key]

U2.dataset$Occupational.Class[is.na(U2.dataset$Occupational.Class)] <- "Not working"

U2.dataset[,Occupation:=NA_integer_]
U2.dataset[Occupational.Class %in% c("Professional/technical/managerial"),Occupation:=4L,by=key]
U2.dataset[Occupational.Class %in% c("Clerical","Sales","Services"),Occupation:=3L,by=key]
U2.dataset[Occupational.Class %in% c("Skilled manual","Unskilled manual","Household and domestic","Other","Agricultural - self employed","Agricultural - employee"),Occupation:=2L,by=key]
U2.dataset[Occupational.Class %in% c("Not working","Don't know"),Occupation:=1L,by=key]
U2.dataset[,Occupation:=factor(Occupation,levels=c(1:4),labels=c("Housewife","Routine & Manual","Intermediate","Professional & Managerial"))]
U2.dataset[,Occupational.Class:=NULL]
setnames(U2.dataset,"Occupation","Occupational.Class")


check.missingness.bool <- U2.dataset[,lapply(.SD,is.na)]
count.missing <- colSums(check.missingness.bool)
count.missing <- count.missing[count.missing>0]

U2.dataset <- U2.dataset[!is.na(Acceptable.Diet),copy(.SD)]

setnames(U2.dataset,c("Highest.Edu.Lvl"),c("Education.Attainment"))

U2.dataset[,Health.Status:=factor(Malnourished,levels=c(0,1),labels=c("Healthy","Malnourished"))]
U2.dataset[,Residence:=factor(Urban.Residence,levels=c(0,1),labels=c("Rural","Urban"))]
U2.dataset[,Diet.Intake:=factor(Acceptable.Diet,levels=c(0,1),labels=c("Unhealthy","Healthy"))]
U2.dataset[,Employement.Status:=factor(Mother.Employed,levels=c(0,1),labels=c("Unemployed","Employed"))]

save(U2.dataset,file = file.path(m.cache.ds,"U2.dataset.RData"))

use.cols <- c("Country","Country.Cluster", "Household","Weight.Women", 
  "Age", "Age.Group", "Region", "Education.Attainment", "Household.Size",
  "Wealth.Idx","Age.First.Birth","Birth.Order", "Child.Sex", "Child.Age",
  "Nutrition.Status","Media.Exposure","Survived.Ratio", "Occupational.Class", 
  "Health.Status", "Residence", "Acceptable.Diet","Diet.Intake","Malnourished","Employement.Status", "key", "cons"  
  )

#Recalculate weights to cater for sub-sample and pooled sample
U2.dataset[,Weight.Women:=(Weight.Women/1000000)]
weight.adj <- U2.dataset[,sum(Weight.Women),by=Country]
setnames(weight.adj,names(weight.adj),c("Country","Wc"))


U2.dataset[Country=="Kenya",Weight:=Weight.Women/weight.adj$Wc[weight.adj$Country=="Kenya"]]
U2.dataset[Country=="Ghana",Weight:=Weight.Women/weight.adj$Wc[weight.adj$Country=="Ghana"]]
U2.dataset[Country=="Namibia",Weight:=Weight.Women/weight.adj$Wc[weight.adj$Country=="Namibia"]]
U2.dataset[Country=="Rwanda",Weight:=Weight.Women/weight.adj$Wc[weight.adj$Country=="Rwanda"]]
U2.dataset[Country=="Senegal",Weight:=Weight.Women/weight.adj$Wc[weight.adj$Country=="Senegal"]]

analysis.dataset <- U2.dataset[,copy(.SD),.SDcols=use.cols]
setnames(analysis.dataset,c("Age","Wealth.Idx"),c("Mothers.Age","Wealth"))
analysis.dataset[,Household.Wealth:=as.integer(Wealth)]
analysis.dataset[,Neighbourhood.Poverty:=round(mean(Household.Wealth,na.rm = T),2),by=Country.Cluster]
write.dta(analysis.dataset,file=paste0(m.stata.code.loc,"analysis.dataset.dta"))

cat("\nData munged!\n")
