
##Customer ID length (6 8 10)
len <- 8
inputpath <- "account_analysis/inputs"

#install.packages("plotrix");

library(plotrix);

width=500;
height=400;

options(scipen=5);
#############read csv name##############
File_name <-list.files(path="C:/Users/ChenSiQi/Desktop/account_analysis/inputs",pattern=".csv$",all.files=TRUE,full.name=FALSE)
File_D<-File_name[grep("。.csv$",File_name)]
File_P<-File_name[-grep("。.csv$",File_name)]

if ( length(File_D)!=length(File_P) ){
  cat("reading File Error，Stop right now！！！！！ ")         
}

for (loop in 1:length(File_D)) {
  ###########extract file name####################################
  name_D <-as.character(File_D[loop])
  name_P <-as.character(File_P[loop])
  P<-substr(name_P,1,len);
  D<-substr(name_D,1,len);
  #########if the name does not match, check!########
  if (D!=P ){
    cat("The File name are not the same,stop it right now！！！！！ ") 
    break;
  }
  ###############create working directory###################################
  setwd("C:/Users/ChenSiQi/Desktop/account_analysis/")
  dir.create(D)
  setwd(paste("C:/Users/ChenSiQi/Desktop/account_analysis/",D,sep=""))
  
  ###############read#########################
  Duizhang <-read.csv(paste("C:/Users/ChenSiQi/Desktop/account_analysis/inputs/",
                            name_D,sep=""),header=FALSE,skip=2)
  Pingcang <-read.csv(paste("C:/Users/ChenSiQi/Desktop/account_analysis/inputs/",
                            name_P,sep=""),header=FALSE,skip=2)
  #############Build the base data sequence###################
  #Assets Today
  zichan <- Duizhang[,31]+Duizhang[,9];
  #share and net worth
  fene <- matrix(0,nrow=(dim(Duizhang)[1]+1),ncol=1);
  net_value <- matrix(0,nrow=(dim(Duizhang)[1]+1),ncol=1);
  fene[1,1] <-Duizhang[1,7];
  net_value[1,1] <-1;
  fene[2,1] <-fene[1,1]+Duizhang[1,8]/net_value[1,1];
  net_value[2,1] <-zichan[1]/fene[2,1];
  for (i in 2:dim(Duizhang)[1]+1){
    fene[i,1] <-fene[i-1,1]+Duizhang[i-1,8]/net_value[i-1,1]-Duizhang[i-2,9]/net_value[i-1,1];
    net_value[i,1] <-zichan[i-1]/fene[i,1];
  }
  
  #net in
  #      net_in <-Duizhang[,8]-Duizhang[,9];
  #      #base value
  #      base_value <- matrix(0,nrow=dim(Duizhang)[1],ncol=1);
  #      for (i in 1: (dim(Duizhang)[1]) ) {
  #          base_value[i,1] <- (Duizhang[i,31]-net_in[i])/Duizhang[i,7]
  #Cumulative net value
  #      net_value <- cumprod(base_value)
  #time(as abscissa)
  Filter <-seq(1,dim(Duizhang)[1],by=2);
  time <-as.character(Duizhang[,1])[Filter];
  y_axis <- c(0:20)/10;
  y_point <- c(0:20)/10;
  
  #      y_axis <- round(seq(min(net_value)-0.1,max(net_value)+0.1,
  #                          length.out=length(net_value)/2),digits=1);
  #      y_point <-seq(min(net_value)-0.1,max(net_value)+0.1,length.out=length(net_value)/2)
  ################Cumulative net worth trend################
  jpeg(file="1.jpeg", quality=100,width=500,height=400)
  plot(net_value,main="累计单位净值走势图",type="l",axes=FALSE,col=4,
       lwd=2,las=1);
  legend("topleft","累计净值",pch=15,col=4);
  axis(1,Filter,time);
  axis(2,at=y_point,y_axis,las=1);
  box();
  dev.off()
  
  
  
  ############Analysis of transaction methods############
  
  #Filter_sameday 1 - within a day，0 - overnight
  #Filter_winlose  1 - profit  0 - loss
  pcrq<-as.vector(Pingcang[,1])# Change the close date format
  kcrq<-as.vector(Pingcang[,17])# Change the open date format
  Filter_sameday <- ifelse(pcrq==kcrq,1,0)
  #Filter_sameday<-ifelse(Pingcang[,1]==Pingcang[,17],1,0)
  Filter_winlose <- ifelse(Pingcang[,16]>0,1,0)
  
  ##############Total closing profit and loss#################
  sum_sameday <-matrix(0,nrow=3,ncol=1);
  sum_diffday <-matrix(0,nrow=3,ncol=1);
  
  for (i in 1:dim(Pingcang)[1] ){
    if (Filter_sameday[i]==1){
      if(Filter_winlose[i]==1){
        sum_sameday[2] <-sum_sameday[2]+Pingcang[i,16]
      }
      if(Filter_winlose[i]==0){
        sum_sameday[3] <-sum_sameday[3]+Pingcang[i,16]
      }
    }
    if (Filter_sameday[i]==0){
      if(Filter_winlose[i]==1){
        sum_diffday[2] <-sum_diffday[2]+Pingcang[i,16]
      }
      if(Filter_winlose[i]==0){
        sum_diffday[3] <-sum_diffday[3]+Pingcang[i,16]
      }
    }    
  }
  
  
  
  
  
  ###Overview of the account situation during the analysis period##
  BeginDate <-as.vector(Duizhang[,1][1])
  EndDate <- as.vector(Duizhang[,1][dim(Duizhang)[1]])
  Begin_earn <- paste(Duizhang[1,7],"(元)") 
  End_earn <- paste(Duizhang[dim(Duizhang)[1],31],"(元)") 
  net_earn <- paste(sum(Duizhang[,8]-Duizhang[,9]),"(元)")  
  sum_earn<- sum(Duizhang[,26])
  #earn_rate <-paste(round(((sum_sameday[3]+sum_diffday[3])/(sum_sameday[2]+sum_diffday[2]-sum_sameday[3]-sum_diffday[3]))*100,2),"%")  #盈亏率
  #earn_rate<-paste(round(((Duizhang[dim(Duizhang)[1],31]-Duizhang[1,31])/Duizhang[1,31])*100,2),"%")
  earn_rate<-paste(round(((sum(Duizhang[,26]))/(Duizhang[1,7]+(sum(Duizhang[,8]-Duizhang[,9]))))*100,2),"%")
  sum_earn <-  paste(sum_earn,"(元)")
  good <- paste(as.character(unique(Pingcang[,8])),collapse=",")
  #accound
  NO. <- paste(Duizhang[1,4]);
  #name
  ClientName <- paste(Duizhang[1,5]);
  
  ###Max draw down
  ###Maxdrawdown <- paste(round((1-net_value/max(net_value))*100,2),"%")
  
  ##
  DataFrame <- matrix(0,nrow=10,ncol=2)
  DataFrame[,2]<- c(NO.,ClientName,BeginDate,EndDate,Begin_earn,End_earn,
                    net_earn,sum_earn,earn_rate,good)
  DataFrame[,1]<-c("资金账号","客户姓名","开始日期","结束日期","期初权益","期末权益",
                   "净入金","总盈亏","盈亏率","主要交易品种")     
  DataFrame <- as.data.frame(DataFrame)
  write.table(DataFrame,"1.csv",sep = ",",append=TRUE,row.names = FALSE,
              col.names =FALSE)                   
  
  
  sum_sameday[1]<- sum_sameday[2]+sum_sameday[3]
  sum_diffday[1]<- sum_diffday[2]+sum_diffday[3]
  
  Pingcang_day_sum <- data.frame(sameday=sum_sameday,diffday=sum_diffday)
  rownames(Pingcang_day_sum)<- c("总平仓盈亏（万元）","总平仓盈利（万元）","总平仓亏损（万元）")
  colnames(Pingcang_day_sum)<- c("日内交易","隔夜交易")
  jpeg(file="2.jpeg", quality=100,width=width,height=height)
  barplot(t(Pingcang_day_sum/10000),beside=TRUE,main="总平仓盈亏",col=c(1,2),
          las=1,width=0.5,ylim=c(min(Pingcang_day_sum)*1.5/10000,
                                 max(Pingcang_day_sum)*1.5/10000));
  legend.txt <-c("日内交易","隔夜交易");
  legend("topright",legend=legend.txt,col=c(1,2),
         pch=15,bty="o");
  box();
  dev.off()
  
  
  
  
  ######################win rate######################
  winning_rate <- matrix(0,ncol=2,nrow=4);
  for (i in 1:length(Filter_sameday) ){
    if (Filter_sameday[i]==1){
      winning_rate[3,1] <-winning_rate[3,1]+1
      if(Filter_winlose[i]==1){
        winning_rate[1,1]<-winning_rate[1,1]+1  
      }
    }
    else{
      winning_rate[3,2]<- winning_rate[3,2]+1
      if(Filter_winlose[i]==1){
        winning_rate[1,2]<- winning_rate[1,2]+1 
      }
    }
  }

  winning_rate[2,]<-winning_rate[3,]-winning_rate[1,];
  winning_rate[4,]<-winning_rate[1,]/winning_rate[3,];
  winning_rate[4, is.na(winning_rate[4,])] =0;
  winning_rate[4,]<-paste(round(winning_rate[4,]*100,2),"%")
  colnames(winning_rate) <-c("日内交易","隔夜交易")
  rownames(winning_rate) <-c("胜(次)","败(次)","交易总量(次)","胜率(%)")
  write.table(winning_rate[c(1,2,4),],"1.csv",sep=",",append=TRUE,row.names = TRUE,
              col.names = TRUE )
  
  ####################average profit and loss#############
  
  avg_sameday <-matrix(0,nrow=3,ncol=1);
  avg_diffday <-matrix(0,nrow=3,ncol=1);
  avg_sameday[1:2,1]<-sum_sameday[2:3,1]/as.numeric(winning_rate[1:2,1]);
  avg_diffday[1:2,1]<-sum_diffday[2:3,1]/as.numeric(winning_rate[1:2,2]);
  avg_sameday[3,1]<-as.numeric(winning_rate[3,1]);
  avg_diffday[3,1]<-as.numeric(winning_rate[3,2]);
  avg_diffday[is.na(avg_diffday)]=0;
  avg_sameday[is.na(avg_sameday)]=0;
  
  Pingcang_day_avg <- data.frame(sameday=avg_sameday,diffday=avg_diffday)
  rownames(Pingcang_day_avg)<- c("平均每笔盈利(元)","平均每笔亏损(元)","交易量(单边)(手)")
  colnames(Pingcang_day_avg)<- c("日内交易","隔夜交易")
  jpeg(file="3.jpeg", quality=100,width=width,height=height)
  
  barplot(t(Pingcang_day_avg),beside=TRUE,main="平均每笔盈亏",col=c(1,2),
          ylim=c(min(Pingcang_day_avg)*2,max(Pingcang_day_avg)*2),las=1,
          width=0.5);
  legend.txt <-c("日内交易","隔夜交易");
  legend("topright",legend=legend.txt,col=c(1,2),
         pch=15,bty="o");
  box();
  dev.off();
  
  #######################Trading direction analysis###################
  ##Filter_longshort 1- long, 0 - short
  #Filter_winlose  1- profit , 0 - loss
  
  Filter_longshort <-ifelse(Pingcang[,11]=="买",1,0);
  
  
  #############Analysis of total closing profit and loss###################
  
  sum_long <-matrix(0,nrow=3,ncol=1);
  sum_short <-matrix(0,nrow=3,ncol=1);
  
  for (i in 1:dim(Pingcang)[1] ){
    if (Filter_longshort[i]==1){
      if(Filter_winlose[i]==1){
        sum_long[2] <-sum_long[2]+Pingcang[i,16]
      }
      if(Filter_winlose[i]==0){
        sum_long[3] <-sum_long[3]+Pingcang[i,16]
      }
    }
    if (Filter_longshort[i]==0){
      if(Filter_winlose[i]==1){
        sum_short[2] <-sum_short[2]+Pingcang[i,16]
      }
      if(Filter_winlose[i]==0){
        sum_short[3] <-sum_short[3]+Pingcang[i,16]
      }
    }    
  }
  sum_long[1]<- sum_long[2]+sum_long[3];
  sum_short[1]<- sum_short[2]+sum_short[3];
  
  Pingcang_ls_sum <- data.frame(long=sum_long,short=sum_short)
  rownames(Pingcang_ls_sum)<- c("总平仓盈亏(万元)","总平仓盈利(万元)","总平仓亏损(万元)")
  colnames(Pingcang_ls_sum)<- c("做多交易","做空交易")
  jpeg(file="4.jpeg", quality=100,width=width,height=height)
  barplot(t(Pingcang_ls_sum/10000),beside=TRUE,main="总平仓盈亏",col=c(1,2)
          ,width=0.5,las=1,ylim=c(min(Pingcang_ls_sum)*1.5/10000,
                                  max(Pingcang_ls_sum)*1.5/10000));
  legend.txt <-c("做多交易","做空交易");
  legend("topright",legend=legend.txt,col=c(1,2),
         pch=15,bty="o",);
  box();
  dev.off();
  
  ########################win rate#########################
  winning_rate_ls <- matrix(0,ncol=2,nrow=4);
  for (i in 1:length(Filter_longshort) ){
    if (Filter_longshort[i]==1){
      winning_rate_ls[3,1] <-winning_rate_ls[3,1]+1
      if(Filter_winlose[i]==1){
        winning_rate_ls[1,1]<-winning_rate_ls[1,1]+1  
      }
    }
    else{
      winning_rate_ls[3,2]<- winning_rate_ls[3,2]+1
      if(Filter_winlose[i]==1){
        winning_rate_ls[1,2]<- winning_rate_ls[1,2]+1 
      }
    }
  }
  winning_rate_ls[2,]<-winning_rate_ls[3,]-winning_rate_ls[1,];
  winning_rate_ls[4,]<-winning_rate_ls[1,]/winning_rate_ls[3,];
  winning_rate_ls[4, is.na(winning_rate_ls[4,]) ] =0;
  winning_rate_ls[4,]<- paste(round(winning_rate_ls[4,]*100,2),"%")
  
  colnames(winning_rate_ls) <-c("做多交易","做空交易")
  rownames(winning_rate_ls) <-c("胜(次)","败(次)","交易总量(次)","胜率(%)")
  write.table(winning_rate_ls[c(1,2,4),],"1.csv",sep=",",append=TRUE,row.names = TRUE,
              col.names = TRUE )
  
  
  ########################avg loss or profit#####################
  avg_long <-matrix(0,nrow=3,ncol=1);
  avg_short <-matrix(0,nrow=3,ncol=1);
  avg_long[1:2,1]<-sum_long[2:3,1]/as.numeric(winning_rate_ls[1:2,1]);
  avg_short[1:2,1]<-sum_short[2:3,1]/as.numeric(winning_rate_ls[1:2,2]);
  avg_long[3,1]<-as.numeric(winning_rate_ls[3,1]);
  avg_short[3,1]<-as.numeric(winning_rate_ls[3,2]);
  avg_long[is.na(avg_long)]=0;
  avg_short[is.na(avg_short)]=0;
  
  Pingcang_ls_avg <- data.frame(long=avg_long,short=avg_short)
  rownames(Pingcang_ls_avg)<- c("平均每笔盈利(元)","平均每笔亏损(元)","交易量(单边)(手)")
  colnames(Pingcang_ls_avg)<- c("做多交易","做空交易")
  jpeg(file="5.jpeg", quality=100,width=width,height=height)
  
  barplot(t(Pingcang_ls_avg),beside=TRUE,main="平均每笔盈亏",col=c(1,2),
          ylim=c(min(Pingcang_ls_avg)*2,max(Pingcang_ls_avg)*2),
          width=0.5,las=1);
  legend.txt <-c("做多交易","做空交易");
  legend("topright",legend=legend.txt,col=c(1,2),
         pch=15,bty="o");
  box();
  dev.off()
  ########################product analysis###########################
  #Filter_winlose  1 - profit , 0 - loss
  good <-unique(Pingcang[,8])#product types
  num <- length(good);  
  
  Filter_good <- matrix(0,nrow=dim(Pingcang)[1],ncol=1);
  for (i in 1:dim(Pingcang)[1]){
    for(j in 1:num){
      if (Pingcang[i,8]==good[j]){
        Filter_good[i]=j;
      }   
    }
    
  }
  
  volume_good<- matrix(0,nrow=num,ncol=1);
  winning_good <-matrix(0, nrow=num,ncol=1);
  gain_good<-matrix(0, nrow=num,ncol=1);
  lose_good<-matrix(0, nrow=num,ncol=1);
  count_good<-matrix(0, nrow=num,ncol=1);
  for (i in 1:dim(Pingcang)[1]){
    type=Filter_good[i];
    volume_good[type]<- volume_good[type]+Pingcang[i,12];
    count_good[type]<-count_good[type]+1;
    
    if(Filter_winlose[i]==1){
      gain_good[type]<-gain_good[type]+Pingcang[i,16];  
      winning_good[type]<- winning_good[type]+1;
    }
    else{
      lose_good[type]<-lose_good[type]+Pingcang[i,16]; 
    }
  }
  volume_good <- volume_good/sum(volume_good);
  winning_good<-winning_good/count_good;  
  sum_good<-gain_good+lose_good;         
  ratio_good<-gain_good/abs(lose_good);  
  ratio_good[is.na(ratio_good)|is.infinite(ratio_good)] <-0
  
  ##调整成百分比格式
  volume_good<-paste(round(volume_good*100,2),"%");
  winning_good<-paste(round(winning_good*100,2),"%");
  dataframe_good<- data.frame(good=good,volume=volume_good,
                              win=winning_good,sum=sum_good,
                              ratio=ratio_good);
  colnames(dataframe_good)<-c("品种","成交量占比","胜率","总平仓盈亏",
                              "平均盈亏比");
  write.table(dataframe_good,"1.csv",sep=",",append=TRUE,row.names = FALSE,
              col.names = TRUE )
  
  #################### Transaction risk analysis###########
  ##previous coordinates
  #Filter <-seq(1,dim(Duizhang)[1],by=2);
  #time <-Duizhang[Filter,1];
  
  #########Equity and Risk Change Chart###########3###
  End_earn <-Duizhang[,31]/10000;
  Risk_1 <-as.numeric(Duizhang[,33]);
  x<-1:length(End_earn);
  jpeg(file="6.jpeg", quality=100,width=width,height=height)
  twoord.plot(lx=x,ly=End_earn,rx=x,ry=Risk_1,lcol=1,rcol=2,
              type=c("bar","l"),lwd=2,rpch=15,halfwidth=0.4,
              main="权益及风险度变化图",ylab="(万)",
              xticklab=time,xtickpos=Filter)
  legend.txt <-c("期末权益","风险度1");
  legend("topleft",legend=legend.txt,col=c(1,2),
         pch=15,bty="o");
  dev.off();
  
  ############Cumulative Profit and Loss Chart####################
  Cul_return <- matrix(0,ncol=3,nrow=dim(Duizhang)[1]);
  Cul_return[,1]<- Duizhang[,26]/1000;
  Cul_return[,2]<- Duizhang[,27]/1000;
  Cul_return[,3]<- Duizhang[,28]/1000;
  
  jpeg(file="7.jpeg", quality=100,width=width,height=height)
  plot <-plot(Cul_return[,1],main="累计盈亏变化图",ylim=c(min(Cul_return)-10,max(Cul_return)+20),
              type="l",col=2,lwd=2,ylab="（万）",las=2,xlab="");
  lines(Cul_return[,2],type="l",col=3,lwd=2);
  lines(Cul_return[,3],type="l",col=4,lwd=2);
  legend.txt=c("总盈亏","平均盈亏(盯)","浮动盈亏(盯)");
  legend("topright",legend=legend.txt,col=c(2,3,4),
         pch=15,bty="o");
  dev.off();
  
  ##############drawdown distribution#############
  ##net_value
  net_value1 <-net_value[-1,];
  Drawdown <-matrix(0,ncol=1,nrow=length(net_value1)-1);
  for (i in 2: length(net_value1)){
    Drawdown[i] <- 1-net_value1[i]/max(net_value1[1:i]);
    if(is.na(Drawdown[i])==1){
      Drawdown[i]=0;
    }
  }
  max_drawdown <-max(Drawdown);
  avg_drawdown <-mean(Drawdown);
  ## Calculate the drawdown rate
  freq <-c(0.02,0.05,0.1,0.3,"0.3以上");
  count <-matrix(0,ncol=1,nrow=5);

  for (j in 1:length(Drawdown)){
    for (i in 1:4){
      if (Drawdown[j] <= freq[i]){
        count[i]=count[i]+1
      }
    }
  }
  
  count[5]<-length(Drawdown)-count[4];
  count[4]<-count[4]-count[3];
  count[3]<-count[3]-count[2];
  count[2]<- count[2]-count[1];
  cul_count <-matrix(0,nrow=5,ncol=1);
  cul_count[1]<-count[1];
  for (i in 2:5){
    cul_count[i]<-cul_count[i-1]+count[i];
  }
  cul_count <-cul_count/length(Drawdown);
  x<-1:5;
  jpeg(file="8.jpeg", quality=100,width=width,height=height)
  
  twoord.plot(lx=x,ly=count,rx=x,ry=cul_count,main="回撤率分布图",
              lylim=c(0,max(count)*1.1),xlim=c(0.5,5.5),
              rylim=c(0,1),lcol=4,rcol=3,
              type=c("bar","o"),lwd=3,lpch=15,xticklab=freq); 
  legend.txt <-c("频率","累积%");
  legend("right",legend=legend.txt,col=c(4,3),
         pch=15,bty="o");
  dev.off();
  
  #####################Total closing profit and loss data##############################
  write.table(Pingcang_day_sum,"1.csv",sep=",",append=TRUE,
              row.names = TRUE,col.names = TRUE )
  
  #Determine day/night profit and loss
  if (Pingcang_day_sum[1,1]>=0){
    ryyk11 <- "盈利";
  }else{
    ryyk11 <- "亏损";
  }
  
  if (Pingcang_day_sum[2,1]>=0){
    ryyk12 <- "盈利";
  }else{
    ryyk12 <- "亏损";
  }
  
  if (Pingcang_day_sum[3,1]>0){
    ryyk13 <- "盈利";
  }else{
    ryyk13 <- "亏损";
  }
  huashu1 <- paste("日内交易的总平仓盈亏为",ryyk11,
                   round((Pingcang_day_sum[1,1]/10000),2),"万元",
                   "，其中",ryyk12,round((Pingcang_day_sum[2,1]/10000),2),"万元",
                   "，",ryyk13,round((Pingcang_day_sum[3,1]/10000),2),"万元","。",sep="",collapse=NULL);
  write.table(huashu1,"1.csv",sep="",append=TRUE,
              row.names = FALSE,col.names = FALSE)
  
  if (Pingcang_day_sum[1,2]>=0){
    ryyk21 <- "盈利";
  }else{
    ryyk21 <- "亏损";
  }
  
  if (Pingcang_day_sum[2,2]>=0){
    ryyk22 <- "盈利";
  }else{
    ryyk22 <- "亏损";
  }
  
  if (Pingcang_day_sum[3,2]>0){
    ryyk23 <- "盈利";
  }else{
    ryyk23 <- "亏损";
  }
  huashu2 <- paste("隔夜交易的总平仓盈亏为",ryyk21,
                   round((Pingcang_day_sum[1,2]/10000),2),"万元",
                   "，其中",ryyk22,round((Pingcang_day_sum[2,2]/10000),2),"万元",
                   "，",ryyk23,round((Pingcang_day_sum[3,2]/10000),2),"万元","。",sep="",collapse=NULL);
  write.table(huashu2,"1.csv",sep="",append=TRUE,
              row.names = FALSE,col.names = FALSE )
  
  ####################
  write.table(Pingcang_ls_sum,"1.csv",sep=",",append=TRUE,
              row.names = TRUE,col.names = TRUE )
  
  if (Pingcang_ls_sum[1,1]>=0){
    lsyk11 <- "盈利";
  }else{
    lsyk11 <- "亏损";
  }
  
  if (Pingcang_ls_sum[2,1]>=0){
    lsyk12 <- "盈利";
  }else{
    lsyk12 <- "亏损";
  }
  
  if (Pingcang_ls_sum[3,1]>0){
    lsyk13 <- "盈利";
  }else{
    lsyk13 <- "亏损";
  }
  huashu3 <- paste("做多交易的总平仓盈亏为",lsyk11,
                   round((Pingcang_ls_sum[1,1]/10000),2),"万元",
                   "，其中",lsyk12,round((Pingcang_ls_sum[2,1]/10000),2),"万元",
                   "，",lsyk13,round((Pingcang_ls_sum[3,1]/10000),2),"万元","。",sep="",collapse=NULL);
  write.table(huashu3,"1.csv",sep="",append=TRUE,
              row.names = FALSE,col.names = FALSE)
  
  if (Pingcang_ls_sum[1,2]>=0){
    lsyk21 <- "盈利";
  }else{
    lsyk21 <- "亏损";
  }
  
  if (Pingcang_ls_sum[2,2]>=0){
    lsyk22 <- "盈利";
  }else{
    lsyk22 <- "亏损";
  }
  
  if (Pingcang_ls_sum[3,2]>0){
    lsyk23 <- "盈利";
  }else{
    lsyk23 <- "亏损";
  }
  huashu4 <- paste("做空交易的总平仓盈亏为",lsyk21,
                   round((Pingcang_ls_sum[1,2]/10000),2),"万元",
                   "，其中",lsyk22,round((Pingcang_ls_sum[2,2]/10000),2),"万元",
                   "，",lsyk23,round((Pingcang_ls_sum[3,2]/10000),2),"万元","。",sep="",collapse=NULL);
  write.table(huashu4,"1.csv",sep="",append=TRUE,
              row.names = FALSE,col.names = FALSE )
  
  #####################Drawdown and Risk#############################
  avg_risk <-mean(Duizhang[,33]);
  max_risk <-max(Duizhang[,33]);
  avg_risk <- paste(round(avg_risk*100,2),"%",sep="",collapse=NULL)
  max_risk <- paste(round(max_risk*100,2),"%",sep="",collapse=NULL)
  avg_drawdown <- paste(round(avg_drawdown*100,2),"%",sep="",collapse=NULL)
  max_drawdown <- paste(round(max_drawdown*100,2),"%",sep="",collapse=NULL)
  
  DataFrame <- matrix(0,nrow=4,ncol=2)
  DataFrame[,2]<- c(avg_risk,max_risk,avg_drawdown,max_drawdown);
  DataFrame[,1]<-c("平均风险度","最大风险度","平均回撤","最大回撤") ;
  
  DataFrame <- as.data.frame(DataFrame)
  write.table(DataFrame,"1.csv",sep = ",",append=TRUE,row.names = FALSE,
              col.names =FALSE)
  
  #####################Profit and loss days###############################
  #profit rate
  shouyilv <- matrix(0,nrow=(dim(Duizhang)[1]),ncol=1);
  for (i in 2:dim(Duizhang)[1]){
    shouyilv[i,1] <- (Duizhang[i,26]-Duizhang[i,14])/(Duizhang[i,7]+Duizhang[i,8]);
    if(is.na(shouyilv[i,1])==1){
      shouyilv[i,1]=0
    }
  }
  
  #days
  yinglitianshu <- matrix(0,nrow=(dim(Duizhang)[1]),ncol=1)
  kuisuntianshu <- matrix(0,nrow=(dim(Duizhang)[1]),ncol=1)
  lianxuyinglitianshu <- matrix(0,nrow=(dim(Duizhang)[1]),ncol=1)
  lianxukuisuntianshu <- matrix(0,nrow=(dim(Duizhang)[1]),ncol=1)
  
  
  #win days
  for (i in 2:dim(Duizhang)[1]){
    if (shouyilv[i] > 0) {
      yinglitianshu[i,1] <- yinglitianshu[i-1,1] + 1
    }
    else{
      yinglitianshu[i,1] <- yinglitianshu[i-1,1] + 0
    }
  }
  ylts<-0
  if (Duizhang[,25][1]>0){ylts<-1}
  ylts <- ylts+yinglitianshu[dim(Duizhang)[1]]
  
  
  #loss days
  for (i in 2:dim(Duizhang)[1]){
    if (shouyilv[i] < 0){
      kuisuntianshu[i,1] <- kuisuntianshu[i-1,1] + 1
    }
    else{
      kuisuntianshu[i,1] <- kuisuntianshu[i-1,1] + 0
    }
  }
  ksts<-0
  if (Duizhang[,25][1]<0){ksts<-1}
  ksts <- ksts+kuisuntianshu[dim(Duizhang)[1]]
  
  #consecutive profitable days
  for (i in 2:dim(Duizhang)[1]){
    if (shouyilv[i] > 0) {
      lianxuyinglitianshu[i,1] <- lianxuyinglitianshu[i-1,1] + 1
    }
    else{
      lianxuyinglitianshu[i,1] <- 0
    }
  }
  zdlxylts <- max(lianxuyinglitianshu[2:dim(Duizhang)[1]])
  
  #Consecutive days of loss
  for (i in 2:dim(Duizhang)[1]){
    if (shouyilv[i] < 0){
      lianxukuisuntianshu[i,1] <- lianxukuisuntianshu[i-1,1] + 1
    }
    else{
      lianxukuisuntianshu[i,1] <- 0
    }
  }
  zdlxksts <- max(lianxukuisuntianshu[2:dim(Duizhang)[1]])
  
  
  ######################The longest number of days without a new high##########################
  ##Drawdown
  buchuangxingaotianshu <- matrix(0,nrow=(dim(Duizhang)[1]),ncol=1);
  
  for (i in 2:length(net_value1)){
    if (Drawdown[i]!= 0){
      buchuangxingaotianshu[i,1] <- buchuangxingaotianshu[i-1,1] + 1
    }
    else{
      buchuangxingaotianshu[i,1] <- 0
    }
  }
  zcbcxgts <- max(buchuangxingaotianshu[2:dim(Duizhang)[1]])
  
  ######################Shape ratio##########################
  #Average daily profit
  pjdryl <- mean(shouyilv[2:dim(Duizhang)[1]])
  #Standard deviation of daily profit
  drylbzc <- sd(shouyilv[2:dim(Duizhang)[1]])
  #Shape ratio
  SharpeRatio <- pjdryl*sqrt(250)/drylbzc
  
  #####################Sortino Ratio########################
  #The square of the current loss
  dcks <- matrix(0,nrow=(dim(Duizhang)[1]),ncol=1);
  
  for (i in 2:length(net_value1)){
    if (shouyilv[i] < 0) {
      dcks[i] <- (shouyilv[i])^2
    }
  }
  
  #Downward standard deviation of one-day earnings
  drylxxbzc <- sqrt(sum(dcks[2:dim(Duizhang)[1]])/ksts)
  
  #Sortino Ratio
  SortinoRatio <- pjdryl*sqrt(250)/drylxxbzc
  
  
  ########################write table##########################
  SharpeRatio <- paste(round(SharpeRatio,2));
  SortinoRatio <- paste(round(SortinoRatio,2));
  
  DataFrame <- matrix(0,nrow=7,ncol=2)
  DataFrame[,2]<- c(ylts,ksts,zdlxylts,zdlxksts,zcbcxgts,SharpeRatio,
                    SortinoRatio);
  DataFrame[,1]<-c("盈利天数","亏损天数","最大连续盈利天数","最大连续亏损天数",
                   "最长不创新高天数","夏普比率","索提诺比率");
  DataFrame <- as.data.frame(DataFrame)
  write.table(DataFrame,"1.csv",sep = ",",append=TRUE,row.names = FALSE,
              col.names =FALSE)
  
  huashu_final1 <- paste("持仓的平均风险度为",avg_risk,
                        "，最大风险度为",max_risk,
                        "。",sep="", collapse=NULL);
  huashu_final2 <- paste("资金平均回撤幅度为",avg_drawdown,
                        "，最大回撤幅度为",max_drawdown,
                        "。",sep="", collapse=NULL);
  write.table(huashu_final1,"1.csv",sep="",append=TRUE,
              row.names = FALSE,col.names = FALSE)
  write.table(huashu_final2,"1.csv",sep="",append=TRUE,
              row.names = FALSE,col.names = FALSE)
  
}

