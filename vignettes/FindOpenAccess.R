##################################
## Find Open Access Equilibrium for each species category
##################################

# Data<-results
# BOAtol<-0.1
# MaxOpenAccess<-0.3

FindOpenAccess<-function(Data,BaselineYear,BOAtol, MaxOpenAccess)
{
  
  SpeciesCats<- unique(Data$SpeciesCatName)
  
  OpenAccess<-data.frame(matrix(NA,nrow=length(SpeciesCats),ncol=2))
  colnames(OpenAccess)<-c('SpeciesCatName','BvBmsyOpenAccess')
  
  # Calculate BOA given set BOAtol

  for (s in 1:length(SpeciesCats))
  {
    CategoryStocks<-Data[Data$SpeciesCatName==SpeciesCats[s] & Data$year==BaselineYear,]
 
    b<- CategoryStocks$BvBmsy
    
    phi<- CategoryStocks$phi
   
    CategoryStocks$FOA<- ((phi+1)/phi)*(1-b^phi/(phi+1))
    
    CategoryStocks$EQSum<- b+((phi+1)/phi)*(1-b^phi/(phi+1))
    
#     KobeSpace<-ddply(CategoryStocks,c('IdOrig'),summarize,KobeSpot=abs((EQSum-FvFmsy-BvBmsy))) 
    KobeSpace<-ddply(CategoryStocks,c('IdOrig'),summarize,KobeSpot=abs((FOA-FvFmsy))) 
    
    # Any stock on the equilibrium line has a value of 2 for the sum of BvBmsy and FvFmsy
    # This ddply calculates the distance from the equilibrium line for each stock
    # subset this dataset to only include stocks within a desired tolerance, make option on Master
    
    IdsBOA<-KobeSpace$IdOrig[KobeSpace$KobeSpot<=BOAtol] # identify stocks that are within tolerance of equilibrium
    
    OpenAccess$SpeciesCatName[s]<-SpeciesCats[s] # store species category
   
    OpenAccess$BvBmsyOpenAccess[s]<-quantile(CategoryStocks$BvBmsy[CategoryStocks$IdOrig %in% IdsBOA],c(0.1,0.25))[1] # store BvBmsy of 25 percentile 
  
  }
  
  # remove species categories without data (fisheries considered to be in equilibrium)
  OpenAccess<-OpenAccess[is.na(OpenAccess$BvBmsyOpenAccess)==F,]
  
  # save original estimate
  OpenAccess$OrigBOA<-OpenAccess$BvBmsyOpenAccess
  
  # cap BOA for species categories with higher than expected BOA
#   OpenAccess$BvBmsyOpenAccess[(OpenAccess$BvBmsyOpenAccess)>=0.3]<- 0.3 # limit BOA to 0.3
  
  OpenAccess$BvBmsyOpenAccess[(OpenAccess$BvBmsyOpenAccess)>=MaxOpenAccess]<- MaxOpenAccess # limit BOA to 0.3
  
  
  # plot original BOA estimates
#   PlotOpenAccess<-OpenAccess
#   
#   PlotOpenAccess<-PlotOpenAccess[with(PlotOpenAccess,order(OrigBOA)),]
#   
#   levels<-PlotOpenAccess$SpeciesCatName
#   
#   PlotOpenAccess$SpeciesCatName<-factor(PlotOpenAccess$SpeciesCatName,levels=c(levels))
#   
#   pdf(file=paste(FigureFolder,"Orig BOA Barplot.pdf",sep=''),width=16,height=10) 
#   
#   print(ggplot(PlotOpenAccess,aes(x=SpeciesCatName,y=OrigBOA)) +
#     geom_bar(stat='identity',fill='blue') +
#     coord_flip() +
#     geom_abline(intercept=1,slope=0) +
#     theme(text=element_text(size=20)) +
#     labs(y='Original B/Bmsy at Open Access',x="ISSCAAP Category"))
#   
#   dev.off()
  
#   ## run diagnostic on range of BOA values obtained from different BOAtol values
  
  # create BOA range to loop over
  RangeBOA<-seq(from=0.02, to=1,by=0.02)
  
  # create dataframe to fill with BOA tol sensitivity results
  SensitivityBOA<-data.frame(matrix(NA,nrow=length(SpeciesCats),ncol=length(RangeBOA)+1))
  colnames(SensitivityBOA)<-c('SpeciesCatName',RangeBOA)

  for (s in 1:length(SpeciesCats))
  {
    CategoryStocks<-Data[Data$SpeciesCatName==SpeciesCats[s] & Data$year==BaselineYear,]
    
    b<- CategoryStocks$BvBmsy
    
    phi<- CategoryStocks$phi
    
    CategoryStocks$EQSum<- b+((phi+1)/phi)*(1-b^phi/(phi+1))
    
    KobeSpace<-ddply(CategoryStocks,c('IdOrig'),summarize,KobeSpot=abs((EQSum-FvFmsy-BvBmsy))) 
    # Any stock on the equilibrium line has a value of 2 for the sum of BvBmsy and FvFmsy
    # This ddply calculates the distance from the equilibrium line for each stock
    # subset this dataset to only include stocks within a desired tolerance, make option on Master
    
    SensitivityBOA$SpeciesCatName[s]<-SpeciesCats[s] # store species category
    
    for(a in 1:length(RangeBOA))
    {
      # set temporary BOAtol
      tempBOAtol<-RangeBOA[a]

      # identify stocks that are within tolerance of equilibrium    
      tempIdsBOA<-KobeSpace$IdOrig[KobeSpace$KobeSpot<=tempBOAtol] 
      
      SensitivityBOA[s,a+1]<-quantile(CategoryStocks$BvBmsy[CategoryStocks$IdOrig %in% tempIdsBOA],c(0.1,0.25))[1] # store BvBmsy of 25 percentile 
    }
  }
  
#   SensPlot<-melt(SensitivityBOA,id.vars='SpeciesCatName',measure.vars=c(2:51),variable.name='BOAtolerance',value.name='BOA')
#   
#   SensPlot$BOAtolerance<-as.numeric(levels(SensPlot$BOAtolerance))[SensPlot$BOAtolerance]
# 
#   SensPlot<-SensPlot[is.na(SensPlot$BOA)==F,]
# 
#   pdf(file=paste(FigureFolder,'BOA Sensitivity to BOAtol.pdf',sep=''))
#   print(ggplot(SensPlot,aes(x=BOAtolerance,y=BOA,color=SpeciesCatName)) +
#     geom_line() +
#     facet_wrap(~SpeciesCatName))
#   dev.off()  

  return(BvBmsyOpenAccess=OpenAccess)
}
