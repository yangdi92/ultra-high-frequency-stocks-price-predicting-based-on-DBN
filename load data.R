######this program used to load data and merge and process data
library(lubridate)
# library the package to transform time 
# load data from  your computer disk
# path0: the file path where you deposit your data of ShangHai Stock Exchange monthly
# stocksymb: the  stock symbol which you are interested
path0<-c('F:/SH201409-TXT')
stocksymb<-c('600000')
dirs<-dir('F:/SH201409-TXT')
days<-substr(dirs,7,8)
files<-paste(dirs,'txt',sep='.')
path1<-paste(path0,dirs,sep='/')
path2<-paste(path1,stocksymb,sep='/')
path<-paste(path2,files,sep='_')
###########delete the useless variables#######
rm(days,path1,path2)
##############################################

##############################################
for(i in 1:length(path)){
  m<-read.table(path[i],header=T,sep=',')
  dates<-as.character(m[[1]])
  dates<-ymd(dates)
  times<-as.character((m[[2]]))
  times<-hms(times)
  price<-as.numeric(m[[3]])
  volume<-as.numeric(m[[4]])
  PB1<-as.numeric(m$B1价)
  VB1<-as.numeric(m$B1量)
  PS1<-as.numeric(m$S1价)
  VS1<-as.numeric(m$S1量)
  m<-data.frame(dates,times,price,volume,PB1,VB1,PS1,VS1)
  # First, remove trades outside of the normal trading time
  # istar:the time ShangHai Stock Exchange begin working every trading day
  # ibreak0:the time to break at moring
  # ibreak1:time to work again at afternoon
  # iend:time which ShangHai Stock Exchange end work every trading day
  # iadj:adjusted time to remove market-break effect
  istar=hms('9:30:00')
  ibreak0=hms('11:30:00')
  ibreak1=hms('13:00:00')
  iend=hms('15:00:00')
  iadj<-hms('11:00:00')
  b<-m$times
  indexm<-c(1:length(b))[b<ibreak0&b>istar]
  indexa<-c(1:length(b))[b<iend&b>ibreak1]
  index<-c(indexm,indexa)
  m<-m[index,]
  mT<-as.numeric(as.duration(b[indexm]-istar))
  aT<-as.numeric(as.duration(b[indexa]-iadj))
  # the time in seconds to midnight
  Times<-c(mT,aT)+9.5*3600
  m$times<-Times
  assign(paste('day',i,sep='.'),m)
}