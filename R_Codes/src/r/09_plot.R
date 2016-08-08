windowsFonts(Corbel=windowsFont("Corbel"))

attach(U2.dataset)
who.waz <- ggplot(data = U2.dataset,aes(Child.WAZ,fill = "red")) + geom_density(alpha = 0.3) 
who.waz <- who.waz + geom_density(aes(Child.WAM,fill = "blue"),alpha = 0.3)
who.waz <- who.waz + facet_wrap( ~ Country,scales = "free")
who.waz <- who.waz + xlab("Weight for Age (Z-Score vs Median)") + ylab("Proportional Density")
who.waz <- who.waz + scale_fill_manual(name="Weight-For-Age",values=c("red","blue"),labels=c("Z-Score","Median"))
who.waz <- who.waz + theme(legend.position = c(0.8, 0.2))
who.waz <- who.waz + theme(text=element_text(family="Corbel"))
ggsave(who.waz,file="Figure 1.tiff",dpi=600,width=7.09,height=4.14,units="in",compression = "lzw")

library(plyr)

U2.dataset.graph <- U2.dataset[!is.na(Wealth.Idx),copy(.SD)]
U2.nutrition.neg <- ddply(U2.dataset.graph,.(Country,Wealth.Idx),summarise,Ratio=sum(Malnourished))
U2.nutrition.denom <- ddply(U2.dataset.graph,.(Country),summarise,Ratio=sum(Malnourished))
U2.nut.graph <- merge(U2.nutrition.neg,U2.nutrition.denom,by="Country")
rm(U2.nutrition.denom)
rm(U2.nutrition.neg)
rm(U2.dataset.graph)
U2.nut.graph$Ratio <- round(U2.nut.graph$Ratio.x / U2.nut.graph$Ratio.y,2)
U2.nut.graph$Ratio.x <- NULL
U2.nut.graph$Ratio.y <- NULL
U2.nutrition.graph <- ddply(U2.nut.graph,.(Country),summarise,Ratio=cumsum(Ratio))
U2.nut.graph$Cumilative.Ratio <- U2.nutrition.graph$Ratio
U2.nut.graph$Wealth <- as.integer(U2.nut.graph$Wealth.Idx)*0.2
rm(U2.nutrition.graph)
U2.nut.graph$Wealth.Idx <- NULL
U2.nut.graph$Ratio <- NULL
Add.Country <- data.frame(Country=unique(U2.nut.graph$Country),
                          Cumilative.Ratio=rep(0,length(unique(U2.nut.graph$Country))),
                          Wealth=rep(0,length(unique(U2.nut.graph$Country))))

U2.nut.graph <- rbind(U2.nut.graph,Add.Country)
rm(Add.Country)

Conc.Plot <- ggplot(data=U2.nut.graph,aes(x=Wealth,y=Cumilative.Ratio,group=Country,color=Country))
Conc.Plot <- Conc.Plot + geom_line(size=1.1)+geom_point(size=2,aes(shape=Country))+ geom_segment(aes(x=0,xend= 1,y =0,yend=1),colour="dark red",size=1.1)
Conc.Plot <- Conc.Plot + xlab("Cumulative %  of children Ranked by Household Wealth")
Conc.Plot <- Conc.Plot + ylab("Cumulative % of children with Poor Nutrition Status")
Conc.Plot <- Conc.Plot + scale_x_continuous(limits =c(0,1))
Conc.Plot <- Conc.Plot + scale_y_continuous(limits = c(0,1))

Conc.Plot <- Conc.Plot + theme(text=element_text(family="Corbel"))

ggsave(Conc.Plot,file="Figure 2.tiff",dpi=600,width=7.09,height=4.14,units="in",compression = "lzw")

library(ineq)

U2.dataset.country <- split(U2.dataset,U2.dataset$Country)
i <- 1
lorenz.tables <- lapply(U2.dataset.country,function(country.set,country.names){
  i <- get("i",envir = .GlobalEnv)
  country <- country.names[i]
  country.lorenz <- Lc(country.set$GINI)
  lorenz.table <- data.frame(p = country.lorenz$p,L = country.lorenz$L,Country =country)
  i = i+1
  assign("i",i,.GlobalEnv)
  return(lorenz.table)
},names(U2.dataset.country))

lorenz.table <- rbindlist(lorenz.tables)
rm(lorenz.tables)

GINI.plot <-
  ggplot(data = lorenz.table) + geom_line(aes(
    x = p,y = L,group = Country,colour = Country
  )) + geom_abline()
GINI.plot <-
  GINI.plot + scale_x_continuous(name = "Cumulative share of children from lowest to highest household wealth", limits =
                                   c(0,1))
GINI.plot <-
  GINI.plot + scale_y_continuous(name = "Cumulative share of wealth", limits =
                                   c(0,1))
GINI.plot <- GINI.plot + theme(text=element_text(family="Corbel"))

ggsave(GINI.plot,file="Figure 3.tiff",dpi=600,width=7.09,height=4.14,units="in",compression = "lzw")


attach(U2.dataset)
dhs[,Wealth.Idx:=Wealth+1]
dhs.plot.denom <- ddply(dhs,.(Wealth.Idx,Edu_Att),summarise,Count=length(Behaviour.Class))
dhs.plot <- ddply(dhs,.(Wealth.Idx,Edu_Att,Behaviour.Class),summarise,
                  Count=length(Behaviour.Class))

dhs.plot$Prop[dhs.plot$Wealth.Idx==1 & dhs.plot$Edu_Att=="No education"] <- round(dhs.plot$Count[dhs.plot$Wealth.Idx==1 & dhs.plot$Edu_Att=="No education"]/dhs.plot.denom$Count[dhs.plot.denom$Wealth.Idx==1 & dhs.plot.denom$Edu_Att=="No education"],2)
dhs.plot$Prop[dhs.plot$Wealth.Idx==2 & dhs.plot$Edu_Att=="No education"] <- round(dhs.plot$Count[dhs.plot$Wealth.Idx==2 & dhs.plot$Edu_Att=="No education"]/dhs.plot.denom$Count[dhs.plot.denom$Wealth.Idx==2 & dhs.plot.denom$Edu_Att=="No education"],2)
dhs.plot$Prop[dhs.plot$Wealth.Idx==3 & dhs.plot$Edu_Att=="No education"] <- round(dhs.plot$Count[dhs.plot$Wealth.Idx==3 & dhs.plot$Edu_Att=="No education"]/dhs.plot.denom$Count[dhs.plot.denom$Wealth.Idx==3 & dhs.plot.denom$Edu_Att=="No education"],2)
dhs.plot$Prop[dhs.plot$Wealth.Idx==4 & dhs.plot$Edu_Att=="No education"] <- round(dhs.plot$Count[dhs.plot$Wealth.Idx==4 & dhs.plot$Edu_Att=="No education"]/dhs.plot.denom$Count[dhs.plot.denom$Wealth.Idx==4 & dhs.plot.denom$Edu_Att=="No education"],2)
dhs.plot$Prop[dhs.plot$Wealth.Idx==5 & dhs.plot$Edu_Att=="No education"] <- round(dhs.plot$Count[dhs.plot$Wealth.Idx==5 & dhs.plot$Edu_Att=="No education"]/dhs.plot.denom$Count[dhs.plot.denom$Wealth.Idx==5 & dhs.plot.denom$Edu_Att=="No education"],2)

dhs.plot$Prop[dhs.plot$Wealth.Idx==1 & dhs.plot$Edu_Att=="Primary"] <- round(dhs.plot$Count[dhs.plot$Wealth.Idx==1 & dhs.plot$Edu_Att=="Primary"]/dhs.plot.denom$Count[dhs.plot.denom$Wealth.Idx==1 & dhs.plot.denom$Edu_Att=="Primary"],2)
dhs.plot$Prop[dhs.plot$Wealth.Idx==2 & dhs.plot$Edu_Att=="Primary"] <- round(dhs.plot$Count[dhs.plot$Wealth.Idx==2 & dhs.plot$Edu_Att=="Primary"]/dhs.plot.denom$Count[dhs.plot.denom$Wealth.Idx==2 & dhs.plot.denom$Edu_Att=="Primary"],2)
dhs.plot$Prop[dhs.plot$Wealth.Idx==3 & dhs.plot$Edu_Att=="Primary"] <- round(dhs.plot$Count[dhs.plot$Wealth.Idx==3 & dhs.plot$Edu_Att=="Primary"]/dhs.plot.denom$Count[dhs.plot.denom$Wealth.Idx==3 & dhs.plot.denom$Edu_Att=="Primary"],2)
dhs.plot$Prop[dhs.plot$Wealth.Idx==4 & dhs.plot$Edu_Att=="Primary"] <- round(dhs.plot$Count[dhs.plot$Wealth.Idx==4 & dhs.plot$Edu_Att=="Primary"]/dhs.plot.denom$Count[dhs.plot.denom$Wealth.Idx==4 & dhs.plot.denom$Edu_Att=="Primary"],2)
dhs.plot$Prop[dhs.plot$Wealth.Idx==5 & dhs.plot$Edu_Att=="Primary"] <- round(dhs.plot$Count[dhs.plot$Wealth.Idx==5 & dhs.plot$Edu_Att=="Primary"]/dhs.plot.denom$Count[dhs.plot.denom$Wealth.Idx==5 & dhs.plot.denom$Edu_Att=="Primary"],2)

dhs.plot$Prop[dhs.plot$Wealth.Idx==1 & dhs.plot$Edu_Att=="Secondary"] <- round(dhs.plot$Count[dhs.plot$Wealth.Idx==1 & dhs.plot$Edu_Att=="Secondary"]/dhs.plot.denom$Count[dhs.plot.denom$Wealth.Idx==1 & dhs.plot.denom$Edu_Att=="Secondary"],2)
dhs.plot$Prop[dhs.plot$Wealth.Idx==2 & dhs.plot$Edu_Att=="Secondary"] <- round(dhs.plot$Count[dhs.plot$Wealth.Idx==2 & dhs.plot$Edu_Att=="Secondary"]/dhs.plot.denom$Count[dhs.plot.denom$Wealth.Idx==2 & dhs.plot.denom$Edu_Att=="Secondary"],2)
dhs.plot$Prop[dhs.plot$Wealth.Idx==3 & dhs.plot$Edu_Att=="Secondary"] <- round(dhs.plot$Count[dhs.plot$Wealth.Idx==3 & dhs.plot$Edu_Att=="Secondary"]/dhs.plot.denom$Count[dhs.plot.denom$Wealth.Idx==3 & dhs.plot.denom$Edu_Att=="Secondary"],2)
dhs.plot$Prop[dhs.plot$Wealth.Idx==4 & dhs.plot$Edu_Att=="Secondary"] <- round(dhs.plot$Count[dhs.plot$Wealth.Idx==4 & dhs.plot$Edu_Att=="Secondary"]/dhs.plot.denom$Count[dhs.plot.denom$Wealth.Idx==4 & dhs.plot.denom$Edu_Att=="Secondary"],2)
dhs.plot$Prop[dhs.plot$Wealth.Idx==5 & dhs.plot$Edu_Att=="Secondary"] <- round(dhs.plot$Count[dhs.plot$Wealth.Idx==5 & dhs.plot$Edu_Att=="Secondary"]/dhs.plot.denom$Count[dhs.plot.denom$Wealth.Idx==5 & dhs.plot.denom$Edu_Att=="Secondary"],2)

dhs.plot$Prop[dhs.plot$Wealth.Idx==1 & dhs.plot$Edu_Att=="Higher"] <- round(dhs.plot$Count[dhs.plot$Wealth.Idx==1 & dhs.plot$Edu_Att=="Higher"]/dhs.plot.denom$Count[dhs.plot.denom$Wealth.Idx==1 & dhs.plot.denom$Edu_Att=="Higher"],2)
dhs.plot$Prop[dhs.plot$Wealth.Idx==2 & dhs.plot$Edu_Att=="Higher"] <- round(dhs.plot$Count[dhs.plot$Wealth.Idx==2 & dhs.plot$Edu_Att=="Higher"]/dhs.plot.denom$Count[dhs.plot.denom$Wealth.Idx==2 & dhs.plot.denom$Edu_Att=="Higher"],2)
dhs.plot$Prop[dhs.plot$Wealth.Idx==3 & dhs.plot$Edu_Att=="Higher"] <- round(dhs.plot$Count[dhs.plot$Wealth.Idx==3 & dhs.plot$Edu_Att=="Higher"]/dhs.plot.denom$Count[dhs.plot.denom$Wealth.Idx==3 & dhs.plot.denom$Edu_Att=="Higher"],2)
dhs.plot$Prop[dhs.plot$Wealth.Idx==4 & dhs.plot$Edu_Att=="Higher"] <- round(dhs.plot$Count[dhs.plot$Wealth.Idx==4 & dhs.plot$Edu_Att=="Higher"]/dhs.plot.denom$Count[dhs.plot.denom$Wealth.Idx==4 & dhs.plot.denom$Edu_Att=="Higher"],2)
dhs.plot$Prop[dhs.plot$Wealth.Idx==5 & dhs.plot$Edu_Att=="Higher"] <- round(dhs.plot$Count[dhs.plot$Wealth.Idx==5 & dhs.plot$Edu_Att=="Higher"]/dhs.plot.denom$Count[dhs.plot.denom$Wealth.Idx==5 & dhs.plot.denom$Edu_Att=="Higher"],2)

dhs.plot$Wealth.Idx <- factor(dhs.plot$Wealth.Idx,levels=c(1:5),labels=c("Poorest", "Poorer", "Middle", "Richer", "Richest"))

windowsFonts(Corbel=windowsFont("Corbel"))
edu.bhv <- ggplot(data = dhs.plot,aes(x=Wealth.Idx,y=Prop,fill=Behaviour.Class)) 
edu.bhv <- edu.bhv + geom_bar(position='dodge',stat = 'identity',alpha = 0.6)+scale_y_continuous(labels = percent_format())
edu.bhv <- edu.bhv + facet_wrap( ~ Edu_Att,scales = "free")
edu.bhv <- edu.bhv + xlab("Wealth Quintile") + ylab("Proportion of Children")
edu.bhv <- edu.bhv + ggtitle("Distribution of dietary health behaviours by wealth quintiles \n classified by mother's education attainment level")
edu.bhv <- edu.bhv + theme(text=element_text(family="Corbel"))
ggsave(edu.bhv,file="Figure 4.tiff",dpi=600,width=7.09,height=4.14,units="in",compression = "lzw")

dhs$Nutrition.Status <- factor(dhs$Maln,levels=c(0:1),labels=c("Healthy","Malnourished"))

dhs.plot.denom <- ddply(dhs,.(Wealth.Idx,Work_Cls),summarise,Count=length(Nutrition.Status))
dhs.plot <- ddply(dhs,.(Wealth.Idx,Work_Cls,Nutrition.Status),summarise,
                  Count=length(Nutrition.Status))

dhs.plot$Prop[dhs.plot$Wealth.Idx==1 & dhs.plot$Work_Cls=="Housewife"] <- round(dhs.plot$Count[dhs.plot$Wealth.Idx==1 & dhs.plot$Work_Cls=="Housewife"]/dhs.plot.denom$Count[dhs.plot.denom$Wealth.Idx==1 & dhs.plot.denom$Work_Cls=="Housewife"],2)
dhs.plot$Prop[dhs.plot$Wealth.Idx==2 & dhs.plot$Work_Cls=="Housewife"] <- round(dhs.plot$Count[dhs.plot$Wealth.Idx==2 & dhs.plot$Work_Cls=="Housewife"]/dhs.plot.denom$Count[dhs.plot.denom$Wealth.Idx==2 & dhs.plot.denom$Work_Cls=="Housewife"],2)
dhs.plot$Prop[dhs.plot$Wealth.Idx==3 & dhs.plot$Work_Cls=="Housewife"] <- round(dhs.plot$Count[dhs.plot$Wealth.Idx==3 & dhs.plot$Work_Cls=="Housewife"]/dhs.plot.denom$Count[dhs.plot.denom$Wealth.Idx==3 & dhs.plot.denom$Work_Cls=="Housewife"],2)
dhs.plot$Prop[dhs.plot$Wealth.Idx==4 & dhs.plot$Work_Cls=="Housewife"] <- round(dhs.plot$Count[dhs.plot$Wealth.Idx==4 & dhs.plot$Work_Cls=="Housewife"]/dhs.plot.denom$Count[dhs.plot.denom$Wealth.Idx==4 & dhs.plot.denom$Work_Cls=="Housewife"],2)
dhs.plot$Prop[dhs.plot$Wealth.Idx==5 & dhs.plot$Work_Cls=="Housewife"] <- round(dhs.plot$Count[dhs.plot$Wealth.Idx==5 & dhs.plot$Work_Cls=="Housewife"]/dhs.plot.denom$Count[dhs.plot.denom$Wealth.Idx==5 & dhs.plot.denom$Work_Cls=="Housewife"],2)

dhs.plot$Prop[dhs.plot$Wealth.Idx==1 & dhs.plot$Work_Cls=="Routine & Manual"] <- round(dhs.plot$Count[dhs.plot$Wealth.Idx==1 & dhs.plot$Work_Cls=="Routine & Manual"]/dhs.plot.denom$Count[dhs.plot.denom$Wealth.Idx==1 & dhs.plot.denom$Work_Cls=="Routine & Manual"],2)
dhs.plot$Prop[dhs.plot$Wealth.Idx==2 & dhs.plot$Work_Cls=="Routine & Manual"] <- round(dhs.plot$Count[dhs.plot$Wealth.Idx==2 & dhs.plot$Work_Cls=="Routine & Manual"]/dhs.plot.denom$Count[dhs.plot.denom$Wealth.Idx==2 & dhs.plot.denom$Work_Cls=="Routine & Manual"],2)
dhs.plot$Prop[dhs.plot$Wealth.Idx==3 & dhs.plot$Work_Cls=="Routine & Manual"] <- round(dhs.plot$Count[dhs.plot$Wealth.Idx==3 & dhs.plot$Work_Cls=="Routine & Manual"]/dhs.plot.denom$Count[dhs.plot.denom$Wealth.Idx==3 & dhs.plot.denom$Work_Cls=="Routine & Manual"],2)
dhs.plot$Prop[dhs.plot$Wealth.Idx==4 & dhs.plot$Work_Cls=="Routine & Manual"] <- round(dhs.plot$Count[dhs.plot$Wealth.Idx==4 & dhs.plot$Work_Cls=="Routine & Manual"]/dhs.plot.denom$Count[dhs.plot.denom$Wealth.Idx==4 & dhs.plot.denom$Work_Cls=="Routine & Manual"],2)
dhs.plot$Prop[dhs.plot$Wealth.Idx==5 & dhs.plot$Work_Cls=="Routine & Manual"] <- round(dhs.plot$Count[dhs.plot$Wealth.Idx==5 & dhs.plot$Work_Cls=="Routine & Manual"]/dhs.plot.denom$Count[dhs.plot.denom$Wealth.Idx==5 & dhs.plot.denom$Work_Cls=="Routine & Manual"],2)

dhs.plot$Prop[dhs.plot$Wealth.Idx==1 & dhs.plot$Work_Cls=="Intermediate"] <- round(dhs.plot$Count[dhs.plot$Wealth.Idx==1 & dhs.plot$Work_Cls=="Intermediate"]/dhs.plot.denom$Count[dhs.plot.denom$Wealth.Idx==1 & dhs.plot.denom$Work_Cls=="Intermediate"],2)
dhs.plot$Prop[dhs.plot$Wealth.Idx==2 & dhs.plot$Work_Cls=="Intermediate"] <- round(dhs.plot$Count[dhs.plot$Wealth.Idx==2 & dhs.plot$Work_Cls=="Intermediate"]/dhs.plot.denom$Count[dhs.plot.denom$Wealth.Idx==2 & dhs.plot.denom$Work_Cls=="Intermediate"],2)
dhs.plot$Prop[dhs.plot$Wealth.Idx==3 & dhs.plot$Work_Cls=="Intermediate"] <- round(dhs.plot$Count[dhs.plot$Wealth.Idx==3 & dhs.plot$Work_Cls=="Intermediate"]/dhs.plot.denom$Count[dhs.plot.denom$Wealth.Idx==3 & dhs.plot.denom$Work_Cls=="Intermediate"],2)
dhs.plot$Prop[dhs.plot$Wealth.Idx==4 & dhs.plot$Work_Cls=="Intermediate"] <- round(dhs.plot$Count[dhs.plot$Wealth.Idx==4 & dhs.plot$Work_Cls=="Intermediate"]/dhs.plot.denom$Count[dhs.plot.denom$Wealth.Idx==4 & dhs.plot.denom$Work_Cls=="Intermediate"],2)
dhs.plot$Prop[dhs.plot$Wealth.Idx==5 & dhs.plot$Work_Cls=="Intermediate"] <- round(dhs.plot$Count[dhs.plot$Wealth.Idx==5 & dhs.plot$Work_Cls=="Intermediate"]/dhs.plot.denom$Count[dhs.plot.denom$Wealth.Idx==5 & dhs.plot.denom$Work_Cls=="Intermediate"],2)

dhs.plot$Prop[dhs.plot$Wealth.Idx==1 & dhs.plot$Work_Cls=="Professional & Managerial"] <- round(dhs.plot$Count[dhs.plot$Wealth.Idx==1 & dhs.plot$Work_Cls=="Professional & Managerial"]/dhs.plot.denom$Count[dhs.plot.denom$Wealth.Idx==1 & dhs.plot.denom$Work_Cls=="Professional & Managerial"],2)
dhs.plot$Prop[dhs.plot$Wealth.Idx==2 & dhs.plot$Work_Cls=="Professional & Managerial"] <- round(dhs.plot$Count[dhs.plot$Wealth.Idx==2 & dhs.plot$Work_Cls=="Professional & Managerial"]/dhs.plot.denom$Count[dhs.plot.denom$Wealth.Idx==2 & dhs.plot.denom$Work_Cls=="Professional & Managerial"],2)
dhs.plot$Prop[dhs.plot$Wealth.Idx==3 & dhs.plot$Work_Cls=="Professional & Managerial"] <- round(dhs.plot$Count[dhs.plot$Wealth.Idx==3 & dhs.plot$Work_Cls=="Professional & Managerial"]/dhs.plot.denom$Count[dhs.plot.denom$Wealth.Idx==3 & dhs.plot.denom$Work_Cls=="Professional & Managerial"],2)
dhs.plot$Prop[dhs.plot$Wealth.Idx==4 & dhs.plot$Work_Cls=="Professional & Managerial"] <- round(dhs.plot$Count[dhs.plot$Wealth.Idx==4 & dhs.plot$Work_Cls=="Professional & Managerial"]/dhs.plot.denom$Count[dhs.plot.denom$Wealth.Idx==4 & dhs.plot.denom$Work_Cls=="Professional & Managerial"],2)
dhs.plot$Prop[dhs.plot$Wealth.Idx==5 & dhs.plot$Work_Cls=="Professional & Managerial"] <- round(dhs.plot$Count[dhs.plot$Wealth.Idx==5 & dhs.plot$Work_Cls=="Professional & Managerial"]/dhs.plot.denom$Count[dhs.plot.denom$Wealth.Idx==5 & dhs.plot.denom$Work_Cls=="Professional & Managerial"],2)

dhs.plot$Wealth.Idx <- factor(dhs.plot$Wealth.Idx,levels=c(1:5),labels=c("Poorest", "Poorer", "Middle", "Richer", "Richest"))

work.class <- ggplot(data = dhs.plot,aes(x=Wealth.Idx,y=Prop,fill=Nutrition.Status)) 
work.class <- work.class + geom_bar(position='dodge',stat = 'identity',alpha = 0.6)+scale_y_continuous(labels = percent_format())
work.class <- work.class + facet_wrap( ~ Work_Cls,scales = "free")
work.class <- work.class + xlab("Wealth Quintile") + ylab("Proportion of Children")
work.class <- work.class + ggtitle("Nutrition status of children by wealth quintiles \n classified by mother's occupation class")
work.class <- work.class + theme(text=element_text(family="Corbel"))
ggsave(work.class,file="Figure 5.tiff",dpi=600,width=7.09,height=4.14,units="in",compression = "lzw")

