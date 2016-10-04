# Produce Dataframe from sbfiles
sb2DF<-function(fileList,col_Names){
  Flow<-lapply(sbFiles$url[fileList],RCurl::getURL)
  # function to open all files as data frames
  All<-lapply(Flow,function(i){
    read.csv(text=unlist(i),row.names=1)
  })
  # Add column names 
  All<-mapply(colN,All,col_Names,SIMPLIFY=F)
  print(colnames(All))
  # bind the data frames together
  bData<-dplyr::bind_cols(All)
  # Add timestamps as row names
  row.names(bData)<-rownames(All[[1]])
  # remove columns for seg32 and 34 of the MM river
  #bData<-bData[,-grep(paste(c("seg.32_MM","seg.34_MM"),collapse="|"),colnames(bData))]
  bData<-bData[,-grep(paste(c("MM_seg.32","MM_seg.34"),collapse="|"),colnames(bData))]
  return(bData)
}

# function to add the column names for each site or the GCM names to the dataframe
colN<-function(DF,site){
  print(site)
  #colnames(DF)<-paste(colnames(DF),"_",site,sep="")
  colnames(DF)<-paste(site,"_",colnames(DF),sep="")
  print(colnames(DF))
  return (DF)
}

# function to average GCMS
avGCM<-function(DF,gcms,yr){
  gcmList<-lapply(gcms,function(x) DF[,grep(x,colnames(DF))])
  #gcmList<-lapply(gcms[1:2],function(x) futData2030[,grep(x,colnames(futData2030))])
  mean.dat <- data.frame(matrix(unlist(lapply(gcmList, function (x) lapply(x, mean, na.rm=TRUE))),nrow=185,byrow=F))
  colnames(mean.dat)<-paste(gcms,"ann",yr,sep="_")
  mean.dat<-cbind(mean.dat,rowMeans(mean.dat))
  colnames(mean.dat)<-paste(c(gcms,"Mean"),"ann",yr,sep="_")
  return(mean.dat)
}

# current function for seasonal means
# need to test out
seasonalMeans_Base<-function(DF){
  TS<-xts::as.xts(DF)
  #print(TS)
  win<-colMeans(DF[xts::.indexmon(TS) %in% c(0,1,2),])
  spr<-colMeans(DF[xts::.indexmon(TS) %in% c(3,4,5),])
  sum<-colMeans(DF[xts::.indexmon(TS) %in% c(6,7,8),])
  fall<-colMeans(DF[xts::.indexmon(TS) %in% c(9,10,11),])
  seas<-rbind(win,spr,sum,fall)
  rownames(seas)<-c("win","spr","sum","fall")
  return(seas)
}

seasonalMeans<-function(DF,DFCC,gcms,yr){
  TS<-xts::as.xts(DF)
  win<-colMeans(DF[xts::.indexmon(TS) %in% c(0,1,2),])
  spr<-colMeans(DF[xts::.indexmon(TS) %in% c(3,4,5),])
  sum<-colMeans(DF[xts::.indexmon(TS) %in% c(6,7,8),])
  fall<-colMeans(DF[xts::.indexmon(TS) %in% c(9,10,11),])
  
  seasDF<-rbind(win,spr,sum,fall)
  rownames(seasDF)<-c("win","spr","sum","fall")
  seas<-c("win","spr","sum","fall")
  gcmList<-lapply(gcms,function(x) seasDF[,grep(x,colnames(seasDF))])
  
  mean.dat<-dplyr::bind_cols(lapply(gcmList,function(x) data.frame(matrix(unlist(x),nrow=185,byrow=T))))
  colnames(mean.dat)<-unlist(lapply(gcms,paste,seas,yr,sep="_"))
  baseSeas2<-do.call(cbind,replicate((dim(mean.dat)[2]/dim(baseSeas)[2]),baseSeas,simplify=F))
  DepSeas<-((mean.dat-baseSeas)/baseSeas)*100
  gcmMeans<-data.frame(matrix(unlist(lapply(seas,function(x) rowMeans(DepSeas[,grepl(x,colnames(DepSeas))]))),nrow=185,byrow=T))
  colnames(gcmMeans)<-paste("Mean",seas,yr,sep="_")
  
  DepSeas2<-cbind(DepSeas,gcmMeans)
  return(DepSeas2)
}

# New Mean Monthly calcualtion, disregard following two
MeanMonthly<-function(DF,DFCC,gcms,yr){
  TS2<-xts::as.xts(DF)
  mos<-list(9,10,11,0,1,2,3,4,5,6,7,8)
  DF_MM1<-lapply(mos,function(x) colMeans(DF[xts::.indexmon(TS2) %in% c(x),]))
  DF_MM<-do.call(rbind,DF_MM1)
  
  mths<-c("oct","nov","dec","jan","feb","mar","apr","may","jun","jul","aug","sep")
  gcmList<-lapply(gcms,function(x) DF_MM[,grep(x,colnames(DF_MM))])
  #mean.dat <- data.frame(matrix(unlist(lapply(gcmList, function (x) lapply(x, mean, na.rm=TRUE))),nrow=185,byrow=T))
  mean.dat<-dplyr::bind_cols(lapply(gcmList,function(x) data.frame(matrix(unlist(x),nrow=185,byrow=T))))
  colnames(mean.dat)<-unlist(lapply(gcms,paste,mths,yr,sep="_"))
  gcmMeans<-data.frame(matrix(unlist(lapply(mths,function(x) rowMeans(mean.dat[,grepl(x,colnames(mean.dat))]))),nrow=185,byrow=T))
  mean.dat<-cbind(mean.dat,gcmMeans)
  colnames(mean.dat)<-unlist(lapply(c(gcms,"Mean"),paste,mths,yr,sep="_"))
  Base_MM<-do.call(cbind,replicate((dim(mean.dat)[2]/dim(DFCC)[2]),DFCC,simplify=F))
  Dep_MM<-((mean.dat-Base_MM)/Base_MM)*100
  return(Dep_MM)
}


# MeanMonthly<-function(DF,DFCC){
#   TS2<-xts::as.xts(DF)
#   mos<-list(9,10,11,0,1,2,3,4,5,6,7,8)
#   DF_MM1<-lapply(mos,function(x) colMeans(DF[xts::.indexmon(TS2) %in% c(x),]))
#   DF_MM<-t(do.call(rbind,DF_MM1))
#   #Base_MM<-do.call(rbind,replicate((dim(DF_MM)[1]/dim(DFCC)[1]),DFCC,simplify=F))
#   #print(head(Base_MM))
#   #Dep_MM<-((DF_MM-Base_MM)/Base_MM)*100
#   #return(Dep_MM)
#   return (DF_MM)
# }
# 
# # function to average GCMS
# avGCM_MM<-function(DF,DFCC,gcms,yr){
#   mths<-c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
#   gcmList<-lapply(gcms,function(x) DF[,grep(x,colnames(DF))])
#   #mean.dat <- data.frame(matrix(unlist(lapply(gcmList, function (x) lapply(x, mean, na.rm=TRUE))),nrow=185,byrow=T))
#   mean.dat<-dplyr::bind_cols(lapply(gcmList,function(x) data.frame(matrix(unlist(x),nrow=185,byrow=T))))
#   colnames(mean.dat)<-unlist(lapply(gcms,paste,mths,yr,sep="_"))
#   gcmMeans<-data.frame(matrix(unlist(lapply(mths,function(x) rowMeans(mean.dat[,grepl(x,colnames(mean.dat))]))),nrow=185,byrow=T))
#   mean.dat<-cbind(mean.dat,gcmMeans)
#   colnames(mean.dat)<-unlist(lapply(c(gcms,"Mean"),paste,mths,yr,sep="_"))
#   Base_MM<-do.call(cbind,replicate((dim(mean.dat)[2]/dim(baseData_MM)[2]),baseData_MM,simplify=F))
#   Dep_MM<-((mean.dat-Base_MM)/Base_MM)*100
#   return(Dep_MM)
# }
# current function for seasonal means
# need to test out
# seasonalMeans<-function(DF,DFCC){
#   TS<-xts::as.xts(DF)
#   #print(TS)
#   win<-colMeans(DF[xts::.indexmon(TS) %in% c(0,1,2),])
#   winBase<-rep(DFCC[,"win"],length(win)/length(DFCC[,"win"]))
#   winDep<-((win-winBase)/winBase)*100
#   
#   
#   spr<-colMeans(DF[xts::.indexmon(TS) %in% c(3,4,5),])
#   sprBase<-rep(DFCC[,"spr"],length(win)/length(DFCC[,"spr"]))
#   sprDep<-((spr-sprBase)/sprBase)*100
#   
#   sum<-colMeans(DF[xts::.indexmon(TS) %in% c(6,7,8),])
#   sumBase<-rep(DFCC[,"sum"],length(win)/length(DFCC[,"sum"]))
#   sumDep<-((sum-sumBase)/sumBase)*100
#   
#   fall<-colMeans(DF[xts::.indexmon(TS) %in% c(9,10,11),])
#   fallBase<-rep(DFCC[,"fall"],length(win)/length(DFCC[,"fall"]))
#   fallDep<-((fall-fallBase)/fallBase)*100
#   
#   seas<-rbind(winDep,sprDep,sumDep,fallDep)
#   rownames(seas)<-c("win","spr","sum","fall")
#   return(seas)
# }
# 
# # function to average GCMS
# avGCM_Seas<-function(DF,gcms,yr){
#   seas<-c("win","spr","sum","fall")
#   gcmList<-lapply(gcms,function(x) DF[,grep(x,colnames(DF))])
#   #gcmList<-lapply(gcms[1:2],function(x) futData2030[,grep(x,colnames(futData2030))])
#   #mean.dat <- data.frame(matrix(unlist(lapply(gcmList, function (x) lapply(x, mean, na.rm=TRUE))),nrow=185,byrow=T))
#   mean.dat<-dplyr::bind_cols(lapply(gcmList,function(x) data.frame(matrix(unlist(x),nrow=185,byrow=T))))
#   colnames(mean.dat)<-unlist(lapply(gcms,paste,seas,yr,sep="_"))
#   gcmMeans<-data.frame(matrix(unlist(lapply(seas,function(x) rowMeans(mean.dat[,grepl(x,colnames(mean.dat))]))),nrow=185,byrow=T))
#   colnames(gcmMeans)<-paste("Mean",seas,yr,sep="_")
#   mean.dat<-cbind(mean.dat,gcmMeans)
#   return(mean.dat)
# }


