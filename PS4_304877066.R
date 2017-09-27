library(data.table)
library(lubridate)
library(dplyr)
library(zoo)
library(moments)
library(xlsx)
library(reshape2)
#library(sqldf)

#funda
funda=read.csv("D:/UCLA/4_size and market/funda.csv",header=T)
funda=as.data.table(funda)
funda=funda[indfmt=="INDL",]
funda[,tic:=NULL]; funda=funda[,indfmt:=NULL]
str(funda)

#PRBA
prba=read.csv("D:/UCLA/4_size and market/PRBA.csv",header=T)
prba=as.data.table(prba)
prba=prba[,c("gvkey","datadate","prba")]

#merge prba
setkey(funda,gvkey,datadate)
setkey(prba,gvkey,datadate)
funda=merge(funda,prba,all.x = T)
funda[!is.na(prba),"prba_new"]=funda[!is.na(prba),-prba]
funda$datadate=ymd(funda$datadate)
funda[,year:=year(datadate)]

#calculate book value
#she= seq>ceq+pstk > at-lt-mib > at-lt
funda[,she:=seq]
funda[is.na(she),"she"]=funda[is.na(she),ceq]+funda[is.na(she),pstk]
funda[which(is.na(she)),"she"]=funda[which(is.na(she)),at]-funda[which(is.na(she)),lt]-funda[which(is.na(she)),mib]
funda[which(is.na(she)),"she"]=funda[which(is.na(she)),at]-funda[which(is.na(she)),lt]
#DT
funda[,dt:=txditc]
funda[which(is.na(dt)),"dt"]=funda[which(is.na(dt)),"itcb"]+funda[which(is.na(dt)),"txdb"]
INDX=which(is.na(funda$dt))
INDX2=which((!is.na(funda$itcb)) | (!is.na(funda$txdb)))
INDX3=INDX2[which(INDX2 %in% INDX)]
funda[INDX3,"dt"]=rowSums(funda[INDX3,c("itcb","txdb")],na.rm = T)   #####here
rm(INDX,INDX2,INDX3)
#PS
funda[,ps:=-pstkrv]
funda[which(is.na(ps)),"ps"]=funda[which(is.na(ps)),-pstkl]
funda[which(is.na(ps)),"ps"]=funda[which(is.na(ps)),-pstk]
#BE=SHE-PS+DT-PRBA
INDX=which(!is.na(funda$she))
funda[,be:=apply(funda[,c('she','ps','dt','prba_new')], 1, sum, na.rm = T)]  ####here
funda[is.na(she),"be"]=NA
#sum(is.na(funda$be))  
funda=funda[!is.na(funda$be),]   ######if book value is NA, then remove observation
funda=funda[,c("gvkey","datadate","year","fyear","be")]

#in funda, one gvkey could have multiple book value under one year because of fisical year report
#i use the more late datadate in calulating book value for the year
setorderv(funda, c("gvkey","datadate"),order=c(1,-1))
funda=unique(funda,by=c("gvkey","year"))
setorderv(funda,c("gvkey","datadate"),order=c(1,1))

#######################################################################
#linktable
load("D:/UCLA/4_size and market/linktable.Rdata")
linktable$gvkey=as.numeric(linktable$gvkey)
linktable$lpermno=as.numeric(linktable$lpermno)
linktable$linkdt=ymd(linktable$linkdt)
linktable$linkenddt=ymd(linktable$linkenddt)
linktable=linktable[!is.na(linktable$lpermno),]

str(linktable)
linktable$gvkey=as.integer(linktable$gvkey)
linktable$linkdt=ymd(linktable$linkdt)
linktable$linkenddt=ymd(linktable$linkenddt)
##clean
linktable[!linktable$linkprim %in% c('P','C'),"lpermno"]=NA
linktable[!linktable$linktype %in% c('LU','LC'),"lpermno"]=NA
#linktable[linktable$liid=="00X" | liid=="99X","lpermno"]=NA
linktable=linktable[!is.na(linktable$lpermno),]

funda2=left_join(funda,linktable,by=c("gvkey"="gvkey"))
funda2=as.data.table(funda2)
funda2=funda2[((funda2$linkdt<=funda2$datadate)|is.na(funda2$linkdt)) & ((funda2$datadate<=funda2$linkenddt)|is.na(funda2$linkenddt)),]
funda2=funda2[!is.na(funda2$lpermno),] #if lpermno is NA, then omit
###because there exists the situation where one lpermno match with multiple gvkey (of different datadate)
###choose the gvkey with latest datadate
setorderv(funda2,c('lpermno','datadate'),c(1,-1))
funda2=unique(funda2, by=c('lpermno','year'))
setorderv(funda2,c("lpermno","datadate"))

####################################################
#CRSP data
#mret
mret=read.csv("D:/UCLA/4_size and market/mret.csv",header=T)
mret=as.data.table(mret)
mret$date=ymd(mret$date)
mret=mret[SHRCD %in% c(10,11),]
mret=mret[EXCHCD %in% c(1,2,3),]
mret$RET<-as.numeric(levels(mret$RET))[mret$RET]
mret$DLRET<-as.numeric(levels(mret$DLRET))[mret$DLRET]
mret$PRC=abs(mret$PRC)

#fill in time
setkey(mret,PERMNO)
mret[,time:=year(date)*12+month(date)]
mret[,time_diff:=as.integer(time-shift(time)),by=PERMNO]
incomplete_permno=unique(mret[time_diff !=1, .(PERMNO)])
setkey(incomplete_permno,PERMNO)                         
Time=seq(from=min(mret$time,na.rm=T),to=max(mret$time, na.rm=T))
filled=incomplete_permno[CJ(unique(incomplete_permno$PERMNO),Time),allow.cartesian=T,roll=-Inf]
mret=merge(mret,filled,by.x=c('PERMNO','time'),by.y=c('PERMNO','V2'),all=T)
setkey(mret)
setorder(mret,PERMNO,time)

#there are at least 24 months, to include such company(permno)
setorder(mret,PERMNO,date)
mret[,lag24prc:=shift(PRC,24),by=PERMNO]
mret[,noob:=sum(!is.na(lag24prc)), by=PERMNO]
mret=mret[noob!=0,]
mret[,c('lag24prc','time_diff','noob'):=NULL]

#size : june of t
setorder(mret,PERMNO,time)
mret[,lag1prc:=shift(PRC*SHROUT,1),by=PERMNO]
# dec of t-1
mret[,lag7prc:=shift(PRC*SHROUT,7),by=PERMNO]

#merge CRSP(mret) with compustat(funda)
mret[,matchyear:= ifelse(month(date)<=6, year(date)-2,  year(date)-1)]
funda2$lpermno=as.integer(funda2$lpermno)
comb=left_join(mret,unique(funda2[,c("be","year","lpermno","lpermco","fyear")]),by=c("PERMNO"="lpermno", "matchyear"="year"))  ###match with fyear
comb=as.data.table(comb)

#repeat July lag1prc, lag7prc for Aug t-June t+1 
JULY=comb[month(comb$date)==7,c("PERMNO","matchyear","lag1prc","lag7prc")]
comb[,lag1prc:=NULL]
comb[,lag7prc:=NULL]
comb2=left_join(comb,JULY,by=c("PERMNO"="PERMNO","matchyear"="matchyear"))
comb2=as.data.table(comb2)
#
#Sas RESTORE RET, DERET missing value as A,B,C  R read as factor data type
#comb2$PRC=abs(comb2$PRC)
comb2$lag1prc=abs(comb2$lag1prc)
comb2$lag7prc=abs(comb2$lag7prc)
comb2$be=comb2$be*1000
setorder(comb2,PERMNO,date)
comb2[,lag:=shift(PRC*SHROUT),by="PERMNO"]

#
setorder(comb2,lpermco,date)
comb2[,lag7prcsum:=sum(lag7prc),by=c("lpermco","date")]
comb2$b2m=comb2$be/comb2$lag7prcsum
#remove observations if lag1prc lag7prc b2m is NA
comb3=comb2[(!is.na(comb2$lag1prc)) & (!is.na(comb2$lag7prc)) &(!is.na(comb2$b2m)),]
comb3=as.data.table(comb3)


#size_decile
setorder(comb3,date,lag1prc)
comb3[,size_decile:=cut(lag1prc,breaks = c(min(lag1prc),quantile(lag1prc[EXCHCD==1],probs = seq(0.1,0.9,by=0.1)),max(lag1prc)), na.rm=T,include.lowest=T,labels=1:10),by="date"]

#b2m_decile
setorder(comb3,date,b2m)
comb3[,b2m_decile:=cut(b2m,breaks =c(min(b2m),quantile(b2m[EXCHCD==1],probs = seq(0.1,0.9,by=0.1)),max(b2m)), na.rm=T,include.lowest=T,labels=1:10),by="date"]

######################################################################################################################

# calculate ret as RET+DLRET
comb3$RET_new=ifelse(!is.na(comb3$DLRET)|!is.na(comb3$RET), rowSums(comb3[,c("RET","DLRET")],na.rm=T), "NA")
comb3$RET_new=as.numeric(comb3$RET_new)
#calculated weighted average return
setorder(comb3,date,size_decile)
comb3[,wgt_size:=lag/sum(lag,na.rm = T),by=c("date","size_decile")]
size_mret=comb3[,list(wgt_ret=sum(wgt_size*RET_new,na.rm = T)),by=c("date","size_decile")]
size_mret=size_mret[year(size_mret$date)>=1973,]

setorder(comb3,date,b2m_decile)
comb3[,wgt_b2m:=lag/sum(lag,na.rm = T),by=c("date","b2m_decile")]
b2m_mret=comb3[,list(wgt_ret=sum(wgt_b2m*RET_new,na.rm = T)),by=c("date","b2m_decile")]
b2m_mret=b2m_mret[year(b2m_mret$date)>=1973,]

######################################################
FF_mkt=read.csv("C:/Users/echom/OneDrive/UCLA/3rd quarter/237H quantitative asset mgm/3. momentum/FF_mkt.csv")
FF_mkt=as.data.table(FF_mkt)
FF_mkt[,X:=NULL]
FF_mkt[,3:6]=FF_mkt[,3:6]/100

##################################################################################
size_mret$Year=year(size_mret$date)
size_mret$Month=month(size_mret$date)
size_mret=merge(size_mret,FF_mkt[,c("Year","Month","Rf")],by.x = c("Year","Month"),by.y = c("Year","Month"),all.x=T)
size_mret$exc_ret=size_mret$wgt_ret-size_mret$Rf
size_stat=summarise(group_by(size_mret,size_decile),aret=12*mean(exc_ret),asd=sqrt(12)*sd(exc_ret), aSR=aret/asd,skewness=skewness(exc_ret))
size_mret2=size_mret[,c("Year","Month","size_decile","exc_ret")]
size_mret2=dcast(size_mret2,Year+Month~size_decile,value.var = "exc_ret")
size_mret2=as.data.table(size_mret2)

#compare with FF b2m portfolio, 10 decile
FF_size=read.csv("D:/UCLA/4_size and market/Portfolios_Formed_on_ME.csv",header=T,skip=12,nrows=1090)
FF_size=as.data.table(FF_size)
FF_size$X=as.numeric(levels(FF_size$X))[FF_size$X]
FF_size$Year=as.integer(FF_size$X%/%100)
FF_size$Month=as.integer(FF_size$X-FF_size$Year*100)
FF_size=FF_size[,-c(1:10)]
colnames(FF_size)=c(paste("decile",1:10,sep=""),"Year","Month")
FF_size=FF_size[Year>=1973 &Year<=2015,]
FF_size[,1:10]=FF_size[,1:10]/100
setcolorder(FF_size,c("Year","Month",paste("decile",1:10,sep = "")))
#correlation of 10 deciles ---size
corr_size=as.data.table(diag(cor(size_mret2[,-c(1,2)],FF_size[,-c(1,2)])))
rownames(corr_size)=paste("decile",1:10)
colnames(corr_size)="correlation"
size_stat=cbind(size_stat,corr_size)

#longshort_size
ls_size=size_mret[size_decile==1,"exc_ret"]-size_mret[size_decile==10,"exc_ret"]
ls_size=cbind(unique(size_mret$date),ls_size)
ls_size=ls_size[,list(Year=year(V1),Month=month(V1),exc_ret)]
ls_size=cbind(ls_size, FF_size[,decile1]-FF_size[,decile10])
ls_size=as.data.table(ls_size)
colnames(ls_size)=c("Year","Month","ls","FF_ls")
cor(ls_size[,"ls"],ls_size[,"FF_ls"])
ls_size_stat=summarise(ls_size,annualret=12*mean(ls),annualsd=sqrt(12)*sd(ls),annualSharpRatio=annualret/annualsd, skewness=skewness(ls),correlation=cor(ls,FF_ls))
rownames(ls_size_stat)="longshort_size"
#############################################################################################################################
b2m_mret[,Year:=year(b2m_mret$date)]
b2m_mret$Month=month(b2m_mret$date)
b2m_mret=merge(b2m_mret,FF_mkt[,c("Year","Month","Rf")],by.x = c("Year","Month"),by.y = c("Year","Month"),all.x=T)
b2m_mret$exc_ret=b2m_mret$wgt_ret-b2m_mret$Rf
#stat summary
b2m_stat=summarise(group_by(b2m_mret,b2m_decile),aret=12*mean(exc_ret),asd=sqrt(12)*sd(exc_ret), aSR=aret/asd,skewness=skewness(exc_ret))
b2m_mret2=b2m_mret[,c("Year","Month","b2m_decile","exc_ret")]
b2m_mret2=dcast(b2m_mret2,Year+Month~b2m_decile,value.var = "exc_ret")
b2m_mret2=as.data.table(b2m_mret2)

#compare with FF b2m portfolio, 10 decile
FF_b2m=read.csv("D:/UCLA/4_size and market/Portfolios_Formed_on_BE-ME.csv",header=T,skip=23,nrows=1090)
FF_b2m=as.data.table(FF_b2m)
FF_b2m$Year=as.integer(FF_b2m$X%/%100)
FF_b2m$Month=as.integer(FF_b2m$X-FF_b2m$Year*100)
FF_b2m=FF_b2m[,-c(1:10)]
colnames(FF_b2m)=c(paste("decile",1:10,sep=""),"Year","Month")
FF_b2m=FF_b2m[Year>=1973 &Year<=2015,]
FF_b2m[,1:10]=FF_b2m[,1:10]/100
setcolorder(FF_b2m,c("Year","Month",paste("decile",1:10,sep = "")))
#correlation of 10 deciles ---b2m
corr_b2m=as.data.table(diag(cor(b2m_mret2[,-c(1,2)],FF_b2m[,-c(1,2)])))   ##########
rownames(corr_b2m)=paste("decile",1:10)
colnames(corr_b2m)="correlation"
b2m_stat=cbind(b2m_stat,corr_b2m)

#longshort_b2m
ls_b2m=b2m_mret[b2m_decile==10,"exc_ret"]-b2m_mret[b2m_decile==1,"exc_ret"]
ls_b2m=cbind(unique(size_mret$date),ls_b2m)
ls_b2m=ls_b2m[,list(Year=year(V1),Month=month(V1),exc_ret)]
ls_b2m=cbind(ls_b2m, FF_b2m[,decile10]-FF_b2m[,decile1])
ls_b2m=as.data.table(ls_b2m)
colnames(ls_b2m)=c("Year","Month","ls","FF_ls")
ls_b2m_stat=summarise(ls_b2m,annualret=12*mean(ls),annualsd=sqrt(12)*sd(ls),annualSharpRatio=annualret/annualsd, skewness=skewness(ls),correlation=cor(ls,FF_ls))
rownames(ls_b2m_stat)="longshort_size"
#######################################################################################

size_mret3=size_mret2[Year>=2010,]
plot(unique(size_mret[year(date)>=2010,date]),size_mret3$`1`-size_mret3$`10`,type="l",main = "long short portfolio-size",xlab = "date",ylab = "return")
abline(h=0)

b2m_mret3=b2m_mret2[Year>=2010,]
plot(unique(b2m_mret[year(date)>=2010,date]),b2m_mret3$`10`-b2m_mret3$`1`,type="l",main = "long short portfolio-book to market",xlab = "date",ylab = "return")
abline(h=0)

###########################################################################################
#construct six portfolios
#size_decile
setorder(comb3,date,lag1prc)
comb3[,size_frac:=cut(lag1prc,breaks = c(min(lag1prc),quantile(lag1prc[EXCHCD==1],probs = 0.5),max(lag1prc)), na.rm=T,include.lowest=T,labels=1:2),by="date"]
comb3[,b2m_frac:=cut(b2m,breaks = c(min(b2m),quantile(b2m[EXCHCD==1],probs = c(0.3,0.7)),max(b2m)),na.rm=T,include.lowest=T, labels=1:3),by="date"]
comb3$size_frac=as.integer(comb3$size_frac)
comb3$b2m_frac=as.integer(comb3$b2m_frac)
comb3$frac=paste(comb3$size_frac,comb3$b2m_frac,sep = "") 
setorder(comb3,date,frac)
comb4=comb3[,list(frac_ret=sum(RET_new*lag/sum(lag,na.rm = T),na.rm = T)),by=c("date","frac")]
comb5=dcast(comb4,date~frac, value.var="frac_ret")
comb5=as.data.table(comb5)
colnames(comb5)=c("date",paste("frac",c(11,12,13,21,22,23),sep = ""))
comb5[,smb:=(frac11+frac12+frac13)/3-(frac21+frac22+frac23)/3]
comb5[,hml:=(frac13+frac23)/2-(frac11+frac21)/2]
comb5[,Year:=year(date)]
comb5[,Month:=month(date)]
comb5=left_join(comb5,FF_mkt[,c("Year","Month","SMB","HML","Rf")],by=c("Year"="Year","Month"="Month"))
comb5=as.data.table(comb5)
comb5[,2:7]=comb5[,2:7]-comb5[,Rf]
comb5=comb5[Year>=1973,]
smb_stat=summarise(comb5,annual_exc_ret=12*mean(smb),annual_sd=sqrt(12)*sd(smb),annual_SR=annual_exc_ret/annual_sd,skewness=skewness(smb),correlation=cor(comb5$smb,comb5$SMB))
rownames(smb_stat)="SMB"
hml_stat=summarise(comb5,annual_exc_ret=12*mean(hml),annual_sd=sqrt(12)*sd(hml),annual_SR=annual_exc_ret/annual_sd,skewness=skewness(hml),correlation=cor(comb5$hml,comb5$HML))
rownames(hml_stat)="HML"
smbhml=rbind(smb_stat,hml_stat)

plot(comb5$date,FF_mkt[Year>=1973 & Year<=2015,SMB],type = "l",main = "SMB",ylab = "excess ret",xlab = "time")
plot(comb5$date,FF_mkt[Year>=1973 & Year<=2015,HML],type = "l",main = "HML",ylab = "excess ret",xlab = "time")

#####################################################################################
#6
plot(comb5$date,FF_size$decile1-FF_size$decile10,type = "l",main = "characteristic and factor portfolio for size",ylab="returen",xlab = "time")
lines(comb5$date,FF_mkt[Year>=1973 & Year<=2015,SMB],col="red")
legend("bottomright",c("characteristic","factor"),col = c("black","red"),pch=16)

plot(comb5$date,FF_b2m$decile10-FF_b2m$decile1,type = "l",main = "characteristic and factor portfolio for book to market",ylab="returen",xlab = "time")
lines(comb5$date,FF_mkt[Year>=1973 & Year<=2015,HML],col="red")
legend("bottomright",c("characteristic","factor"),col = c("black","red"),pch=16)




#check
# x=unique(funda2[,c("be","year","lpermno")])
# x=as.data.table(x)
# setorder(x,"lpermno","year")
# x[,noob:=sum(!is.na(be)),by=c("lpermno","year")]
# str(x)
# x=x[!is.na(lpermno),]
# str(x)
# x[noob>1,]
# 
# setorder(funda,"gvkey","datadate")
# sum(is.na(funda$be))
# funda[,noob:=sum(!is.na(be)),by=c("gvkey","year")]
# funda[noob>1,]
# funda[,noob:=NULL]
# 
# tally(comb3,is.na(PRC))
# comb3[DLRET==-99,]
# 
# setorder(linktable,"lpermno")
# linktable[,noob:=sum(!is.na("gvkey")),by=c("gvkey","year")]
# funda[noob>1,]
# funda[,noob:=NULL]