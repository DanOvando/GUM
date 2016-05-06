drv<-'/Users/Kristin/Desktop/GUM'
setwd(drv)
SRR2<-read.csv("Data/SAUP_Region_RECON_ISSCAAP.csv", header=T, stringsAsFactors=F)

#library(GUM)
library(ggplot2)
library(plyr)
#devtools::install_github('DanOvando/GUM', build_vignettes = T)

devtools::load_all(drv)
#vignette('Run_GUM')

###Set a unique id per "stock":
SRR2$IdOrig <- id(SRR2[c("scientific_name", "area_name", "RegionFAO")], drop=T)

###Modify/add necessary columns names/columns:
SRR2$BvBmsy <- NA
SRR2$phi <- 0.188
SRR2$Price<-SRR2$landed_value/SRR2$tonnes
colnames(SRR2)[colnames(SRR2) == "scientific_name"] <- "SciName"
colnames(SRR2)[colnames(SRR2) == "common_name"] <- "CommName"
colnames(SRR2)[colnames(SRR2) == "ISSCAAP"] <- "SpeciesCat"
colnames(SRR2)[colnames(SRR2) == "year"] <- "Year"
colnames(SRR2)[colnames(SRR2) == "tonnes"] <- "Catch"
colnames(SRR2)[colnames(SRR2) == "Definition"] <- "SpeciesCatName"

####Sum the catch over the various catch types:
test2<-SRR2 %>%
  group_by(IdOrig, area_name, RegionFAO, Year, SpeciesCat, SciName, CommName, BvBmsy) %>%
  summarize(Catch=sum(Catch, na.rm=T), Price=mean(Price, na.rm=T))
test3<-data.frame(test2)
#test3<-test2[which(test2$SpeciesCat!=47),]


less_dat = test3 %>% dplyr::select(IdOrig, area_name, RegionFAO, Year, SpeciesCat,SciName,CommName, BvBmsy,Catch,Price) %>% mutate(IdOrig = as.character(IdOrig))

stocks = unique(less_dat$IdOrig)
sub = sample(stocks, 100, replace = F)
small_dat = filter(less_dat, IdOrig %in% sub)

###Run the GUM for status:
results2 = run_gum_assessment(dat = small_dat)
results<-data.frame(results2)
name(results)

sapply(list.files(pattern="[.]R$", path="R", full.names=TRUE), source)
##Add in Open Access data:
OpenAccess<-FindOpenAccess(results, BaselineYear=2010,BOAtol=0.1, MaxOpenAccess=0.3)
OpenAccess_df<-data.frame(OpenAccess)
results<-results%>%
  left_join(OpenAccess_df)
results$CatchShare<-0
results$Dbase<-"SAUP_R"


##Running the GUM for the status can take a while depending on computer and how many stocks--suggest saving these results to then be able to read in later to do projections, etc.:
#write.csv(results, "Output/SAUP_RECON_GUM_RESULTS.csv", row.names=F)

ggplot(results,aes(MSY)) + 
  geom_histogram()


##################RUN PROJECTIONS:
projections<-RunProjection(Data= results, BaselineYear=2010, NumCPUs=1, StatusQuoPolicy, Policies = c('StatusQuoOpenAccess', 'Opt', 'CatchShare', 'StatusQuoFForever', 'StatusQuoBForever', 'Fmsy', 'CloseDown'), Discount = 0, CatchShareCost = 0.77, CatchSharePrice = 1.31, IdVar = 'IdOrig', bvec =  seq(0.00000001, 2.5, length.out = 30), tol = 0.1, beta = 1.3, ProjectionTime = 30)
