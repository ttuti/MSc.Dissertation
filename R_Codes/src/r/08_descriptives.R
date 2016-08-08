
U2.dataset[,Cluster.House:=paste0(Cluster,".",Household)]
U2.dataset[,Cluster.House:=as.factor(Cluster.House)]

U2.dataset[,Cluster.House:=paste0(Cluster,".",Household)]
U2.dataset[,Cluster.House:=as.factor(Cluster.House)]

country.counts <- data.frame(prop.table(table(U2.dataset$Country)))

U2.dataset[,Household.Wealth:=as.integer(Wealth.Idx)-1]
U2.dataset[,Neighbourhood.Poverty:=round(weighted.mean(x=Household.Wealth,w=Weight.Women,na.rm = T),2),by=Country.Cluster]

U2.dataset[,Dietary.Diversity:=ifelse(Dietary.Diversity<4,0,1)]
U2.dataset[,Breastfed:=factor(Early.Init.Breastfed,levels = c(0,1),labels=c("No","Yes"))]
U2.dataset[,Recommended.Feeding:=factor(Recommended.Feeding.Freq,levels = c(0,1),labels=c("No","Yes"))]
U2.dataset[,Diet.Diversity:=factor(Dietary.Diversity,levels = c(0,1),labels=c("No","Yes"))]
U2.dataset[,Exposed:=ifelse(Media.Exposure!="No Exposure to TV/Radio","Yes","No")]
U2.dataset[,TV.1:=as.integer(TV.Freq)-1]
U2.dataset[,Radio.1:=as.integer(Radio.Freq)-1]
U2.dataset[,Dailies.1:=as.integer(Newspaper.Freq)-1]

U2.dataset <- U2.dataset[Child.Age >= 6 & Child.Age < 24,copy(.SD)]

U2.dataset.country <- split(U2.dataset,U2.dataset$Country)

generate.stats <- function(column.data,weights=NA){
  #browser()
  if(class(column.data) %in% c("integer","numeric")){
    #column.data <- column.data[!is.na(column.data)]
    mean.stat <- round(wtd.mean(x=column.data,weights=weights,na.rm=T),2)
    sd.stat <- round(sqrt(wtd.var(x=column.data,weights=weights)),2)
    range.stat <- paste0(range(column.data),collapse = "-")
    ret.str <- paste0("Mean=",mean.stat,", SD=",sd.stat,", Range(",range.stat,")")
    return(ret.str)
  }
  if(class(column.data) %in% c("factor","character")){
    if(length(levels(column.data))>10){
      ret.str <- paste0("",length(unique(as.character(column.data))),
                        ",Mean Children/House= ",
                        round(length(as.character(column.data))/length(unique(as.character(column.data))),2))
      return(ret.str)
    }
    lvl.stats <- table(column.data)
    lvl.stats.prop <- round(prop.table(lvl.stats),4)*100
    ret.str <- paste0("N= ",lvl.stats,"(",lvl.stats.prop,"%)")
    names(ret.str) <- levels(column.data)
    return(ret.str)
  }
}

desc.table <- lapply(U2.dataset.country,function(country.data){
  #browser()
  cols.to.use <- c("Cluster.House","Age","Education.Attainment","Occupational.Class","Employement.Status",
                   "Household.Wealth","Neighbourhood.Poverty","Residence","Child.Age","Child.Sex",
                   "Health.Status","Breastfed","Recommended.Feeding", "Diet.Diversity","Exposed",
                   "Dailies.1","Radio.1", "TV.1")
  desc.col <- country.data[,copy(.SD),.SDcols=cols.to.use]
  desc.stats <- lapply(desc.col,generate.stats,weights=country.data$Weight.Women)
  desc.stats <- unlist(desc.stats)
  return(desc.stats)
})


desc.table <- as.data.frame(do.call("cbind",desc.table))

desc.table$Indicators <- row.names(desc.table)
row.names(desc.table) <- NULL
desc.table <- as.data.table(desc.table)

setcolorder(desc.table,c("Indicators","Ghana","Kenya","Namibia","Rwanda","Senegal"))

library(xlsx)
write.xlsx(desc.table,file="E:/SCHOOL/Dissertation/DHS Dataset/descriptives.xlsx",row.names = F,sheetName = "1")

mplus.vars <- c("Country", "Cluster", "Household","Respondent", "Weight.Women", 
  "Age","Child.Age","Child.Sex","Birth.Order","Education.Attainment","Media.Exposure",
  "Newspaper.Freq","Radio.Freq", "TV.Freq", 
  "Early.Init.Breastfed","Recommended.Feeding.Freq", "Dietary.Diversity",
  "Urban.Residence","Mother.Employed", "Occupational.Class",
  "Malnourished","Household.Wealth", "Neighbourhood.Poverty")

health.behaviours <- U2.dataset[,copy(.SD),.SDcols=mplus.vars]
health.behaviours[,Newspaper.Freq:=as.integer(Newspaper.Freq)-1]
health.behaviours[,Radio.Freq:=as.integer(Radio.Freq)-1]
health.behaviours[,TV.Freq:=as.integer(TV.Freq)-1]
#health.behaviours[,Dietary.Diversity:=ifelse(Dietary.Diversity<4,0,1)]

setnames(health.behaviours,names(health.behaviours),c("Country", "Cluster", "Hsehold","Resp", "Weight", "M_Age", 
                                                      "C_Age","C_Sex","B_Order","Edu_Att","Med_Expo", "Dailies", 
                                                      "Radio", "TV","Brstfed","Eat_Freq", "Diet_Div",  
                                                      "Urban", "Employed", "Work_Cls", "Maln","Wealth", "NeighPov"))
health.behaviours[,Commty:=paste0(substr(Country,1,2),Cluster)]
health.behaviours[,Comm_Hse:=paste0(Commty,"_",Hsehold)]
health.behaviours[,Child_id:=paste0(Comm_Hse,"_",Resp,"_",B_Order)]


dhs.dataset <- copy(health.behaviours)
dhs.dataset[,key:=.I]
setkey(dhs.dataset,key)

write.dta(health.behaviours,file="E:/SCHOOL/Dissertation/DHS Dataset/MPLUS - Weighted/healthbehaviours.dta")

keys <- read.table(file="E:/SCHOOL/Dissertation/DHS Dataset/MPLUS - Weighted/key.txt")
names(keys) <- c("key","Comm_Hse")
keys$key <- gsub(":","",keys$key)
keys$key <- stringr::str_trim(side = "both",string = keys$key)
keys$key <- as.integer(keys$key)

media <- read.table(file = "E:/SCHOOL/Dissertation/DHS Dataset/MPLUS - Weighted/mlwin_fscore.txt")

names(media)<- c("Dailies","Radio","TV","Weight","Media_use","B_DAILIES","B_DAILIES_SE","B_RADIO","B_RADIO_SE","B_TV","B_TV_SE","Commu_Hse")
media <- media[,c("Media_use","Commu_Hse")]
media <- media[!duplicated(media), ]
media.new <- merge(media,keys,by.x="Commu_Hse",by.y = "key")
media.new$Commu_Hse <- NULL
dhs <- merge(dhs.dataset,media.new,by="Comm_Hse")

lca <- read.table(file = "E:/SCHOOL/Dissertation/DHS Dataset/MPLUS - Weighted/mlwin_cprob.txt")

names(lca) <- c("BRSTFED","EAT_FREQ","DIET_DIV","WT","Childu_id","HLTH_BHV#1","B_BRSTFED","B_EAT_FREQ","B_DIET_DIV",
                "C_HLTH_BHV","CB_BRSTFED","CB_EAT_FREQ","CB_DIET_DIV","CPROB1","CPROB2","HLTH_BHV","Commu_Hse")

keys.lca <- read.table(file="E:/SCHOOL/Dissertation/DHS Dataset/MPLUS - Weighted/keys_lca.txt")
names(keys.lca) <- c("key","Child_id")
keys.lca$key <- gsub(":","",keys.lca$key)
keys.lca$key <- stringr::str_trim(side = "both",string = keys.lca$key)
keys.lca$key <- as.integer(keys.lca$key)

lca <- lca[,c("HLTH_BHV","Childu_id")]
lca <- lca[!duplicated(lca), ]
lca.new <- merge(lca,keys.lca,by.x="Childu_id",by.y="key")
lca.new$Childu_id <- NULL
dhs <- merge(dhs,lca.new,by="Child_id")


setnames(dhs,c("Media_use","HLTH_BHV"),c("Media_Usage","Behaviour.Class"))

dhs[,Behaviour.Class:=factor(Behaviour.Class,levels=c(1:2),labels=c("Low","Moderate"))]

write.dta(dhs,file="E:/SCHOOL/Dissertation/DHS Dataset/MLwiN - Weighted/dataset.dta")


#lca.new <- merge(lca,keys,by.x ="COMM_HSE",by.y="key")

health.bhv <- as.data.table(read.table("E:/SCHOOL/Dissertation/DHS Dataset/MPLUS 2/healthbehaviours.dat",sep=","))
setnames(health.bhv,names(health.bhv),c("Country","Cluster","Hsehold","Resp","Weight","M_Age","C_Age","C_Sex",
                                        "B_Order","Edu_Att","Med_Expo","Dailies","Radio","TV","Brstfed","Eat_Freq",
                                        "Diet_Div","Urban","Employed","Work_Cls","Maln","Wealth","NeighPov","Comm_Hse","Child_id"))

health <- merge(health.bhv,media,by.x ="Comm_Hse",by.y="Commu_Hse")

setcolorder(health,c("Country", "Cluster", "Hsehold", "Resp", "Weight", 
                     "M_Age", "C_Age", "C_Sex", "B_Order", "Edu_Att", "Med_Expo", 
                     "Dailies", "Radio", "TV", "Brstfed", "Eat_Freq", "Diet_Div", 
                     "Urban", "Employed", "Work_Cls", "Maln", "Wealth", "NeighPov", 
                     "Comm_Hse","Child_id","Media_use"))

#health <- merge(health,lca,by.x ="Comm_Hse",by.y="COMM_HSE")


write.table(health,file="E:/SCHOOL/Dissertation/DHS Dataset/MPLUS 2/healthbehaviours2step.dat",sep = ",",col.names=F,row.names=F)

