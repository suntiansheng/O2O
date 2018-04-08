rm(list=ls())
gc()
setwd("G:/2017/ali0311")
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
ex_user_view<-read_csv('extra_user_view.txt',col_names = c('uid','sid','time'))
user_view<-read_csv('user_view.txt',col_names = c('uid','sid','time'))
user_pay<-read_csv('user_pay.txt',col_names = c('uid','sid','time'))
shop_info<-read_csv('shop_info.txt',col_names = c('sid','city','loc','pay','score','cnt','level','c1','c2','c3'))
# U1<-inner_join(user_view,user_pay,by=c('uid','sid'))
# user_pay%>%group_by(uid,sid)->U1g
# U1g%>%summarise(n())->t1
# names(t1)<-c('uid','sid','n')
# t1<-arrange(t1,desc(n))
# t1_pay<-filter(user_pay,uid==t1$uid[1],sid==t1$sid[1])
# t1_view<-filter(user_view,uid==t1$uid[1],sid==t1$sid[1])
# t1_view_ex<-filter(ex_user_view,uid==t1$uid[1],sid==t1$sid[1])
# View(t2_view)
# View(tt2)
# 
# 
# user_view%>%group_by(uid,sid)->U2g
# U2g%>%summarise(n())->t2
# names(t2)<-c('uid','sid','n')
# t2<-arrange(t2,desc(n))
# 
# ids<-3
# t2_pay<-filter(user_pay,uid==t2$uid[ids],sid==t2$sid[ids])
# t2_view<-filter(user_view,uid==t2$uid[ids],sid==t2$sid[ids])
# View(t2_view)
# View(t2_pay)




#切换思路，生成sidXday结构
strptime(user_pay$time,'%Y-%m-%dT%H:%M:%SZ') 
mutate(user_pay,day=as.Date(strptime(user_pay$time,'%Y-%m-%dT%H:%M:%SZ')))->user_pay_d
mutate(user_view,day=as.Date(time))->user_view_d
mutate(ex_user_view,day=as.Date(time))->ex_user_view_d


days<-seq.Date(from=as.Date('2015-07-01'),to=as.Date('2016-10-31'),by=1)
sids<-unique(shop_info$sid)

#产生框架
crossing(sid=sids,day=days)->main_frame


#groupby sid and day
group_by(user_pay_d,sid,day)->Ug_pay
summarise(Ug_pay,pay_total=n())->pays

rbind(ex_user_view_d,user_view_d)->user_view_all
group_by(user_view_all,sid,day)->Ug_view
summarise(Ug_view,view_total=n())->views

tmp<-left_join(main_frame,views,by=c('sid','day'))
pays$sid <- as.numeric(pays$sid)
tmp<-left_join(tmp,pays,by=c('sid','day'))
main_data<-left_join(tmp,shop_info,by=c('sid'))


main_data_by_city<-group_by(main_data,city)

#visualbe model
library(ggplot2)
p1<- ggplot(main_data,aes(x=c1,y=)) +geom_bar()

p2<- ggplot(pays,aes(x=sid,y=pay_total)) +geom_boxplot()

#产生所有预测集的框架
days_total<-seq.Date(from=as.Date('2016-11-01'),to=as.Date('2016-11-14'),by=1)
sids_total<-unique(shop_info$sid)
crossing(sid=sids_total,day=days_total)->main_frame_pred
main_frame_pred$pred<-NA


citys<-unique(main_data$city)




i0<-1
if(file.exists('out.csv')) file.remove('out.csv')
for(i in i0:length(citys)){
  cat(i,'\n')
  selcity<-i
  
  #对某一城市的pays预测
  t1<-filter(main_data,city==citys[selcity])
  
  
  # t1%>%group_by(day)%>%summarise(views=sum(view_total,na.rm=TRUE),
  #                                pays=sum(pay_total,na.rm=TRUE))->t2
  # 
  # par(mfrow=c(2,1))
  # plot.ts(t2$views)
  # plot.ts(t2$pays)
  
  # library(forecast)
  # m1<-auto.arima(t2$pays)
  # plot.ts(m1$residuals/t2$pays*100)
  # quantile(abs((m1$residuals/t2$pays*100)))
  # 
  # #预测城市未来14天的流量
  # fm1<-forecast(m1,h = 14)
  # fm1
  # 
  #同一城市不同店铺的流量预测
  # selsid<-2
  # c_sids<-unique(t1$sid)
  # t_cs<-filter(main_data,city==citys[selcity],sid==c_sids[selsid])
  # t_cs<-left_join(t_cs,t2,by='day')
  
  
  
  
  
  
  #直接对一个城市做回归预测
  t1%>%filter(day>=as.Date('2015-07-01'))->t1
  mutate(t1,dnum=day-as.Date('2015-07-01'),wday=wday(day),week=week(day))->t1
  
  
  t1%>%select(sid,day,dnum,wday,week,pay_total,loc,pay,score,cnt,level,c1,c2,c3)->t3
  
  t3<-filter(t3,!is.na(pay_total))
  
  t3[is.na(t3$score),'score']<-0
  t3[is.na(t3$cnt),'cnt']<-0
  t3[is.na(t3$c3),'c3']<-0
  
  t3$dnum<-as.integer(t3$dnum)
  t3$wday<-as.integer(t3$wday)
  t3$week<-as.integer(t3$week)
  t3$loc<-as.factor(t3$loc)
  
  t3$score<-as.factor(t3$score)
  t3$pay<-as.factor(t3$pay)
  t3$cnt<-as.factor(t3$cnt)
  t3$level<-as.factor(t3$level)
  t3$c1<-as.factor(t3$c1)
  t3$c2<-as.factor(t3$c2)
  t3$c3<-as.factor(t3$c3)
  
  # library(party)
  # m4<-cforest(pay_total~dnum+wday+week+loc+pay+score+cnt+c1+c2+c3,data=t3)
  
  
  
  # library(randomForest)
  # m4<-randomForest(pay_total~dnum+wday+week+loc1+loc2+pay+score+cnt+c1+c2+c3,
  #                  data=t3,
  #                  ntree=1000,
  #                  type='regression')
  # m4<-randomForest(pay_total~.,data=t3)
  library(nnet)
  t3tr<-select(t3,dnum=dnum,wday=wday,week=week,pay_total=pay_total,
               loc=loc,pay=pay,score=score,cnt=cnt,level=level,c1=c1,c2=c2,c3=c3)
  t3tr_n<-ifelse(sapply(t3tr,function(x)length(unique(x)))==1,FALSE,TRUE)
  t3tr<-t3tr[,t3tr_n]
  m4 <- nnet(pay_total~.,
               data=t3tr,
               size=10, linout=TRUE, skip=TRUE, MaxNWts=10000, trace=FALSE, maxit=100)
    
  
  
  # library(e1071)
  # m5<-svm(pay_total~dnum+wday+week+loc+pay+score+cnt+c1+c2+c3,data=t3)
  # quantile(abs(m5$fitted-t3$pay_total)/t3$pay_total*100)
  
  # m4<-glm(pay_total~dnum+wday+week+loc+pay+score+cnt+c1+c2+c3,
  #                          data=t3)
  
  # library(rpart)
  # m4<-rpart(pay_total~dnum+wday+week+loc+pay+score+cnt+c1+c2+c3,
  #         data=t3)
  # quantile(abs(m4$fitted.values-t3$pay_total)/t3$pay_total*100)
  # quantile(abs(predict(m4,t3)-t3$pay_total)/t3$pay_total*100)
  
  #构建预测数据集合
  #产生预测集框架
  days_t<-seq.Date(from=as.Date('2016-11-01'),to=as.Date('2016-11-14'),by=1)
  sids_t<-unique(t1$sid)
  crossing(sid=sids_t,day=days_t)->main_frame_t
  mutate(main_frame_t,dnum=day-as.Date('2015-07-01'),wday=wday(day),week=week(day))->main_frame_t
  left_join(main_frame_t,shop_info,by='sid')->main_frame_t
  t3t<-main_frame_t
  t3t[is.na(t3t$score),'score']<-0
  t3t[is.na(t3t$cnt),'cnt']<-0
  t3t[is.na(t3t$c3),'c3']<-0
  
  t3t$dnum<-as.integer(t3t$dnum)
  t3t$wday<-as.integer(t3t$wday)
  t3t$week<-as.integer(t3t$week)
  t3t$loc<-as.factor(t3t$loc)
  
  t3t$score<-as.factor(t3t$score)
  t3t$pay<-as.factor(t3t$pay)
  t3t$cnt<-as.factor(t3t$cnt)
  t3t$level<-as.factor(t3t$level)
  t3t$c1<-as.factor(t3t$c1)
  t3t$c2<-as.factor(t3t$c2)
  t3t$c3<-as.factor(t3t$c3)
  t3t$pred<-predict(m4,newdata = t3t)
  # t3t$loc%in% t3$loc
  
  t3t<-select(t3t,sid=sid,pred=pred)
  mf1<-function(x){ paste(round(x),collapse = ',')}
  mf2<-function(x){ paste(x,collapse = ',')}
  t3to<-t3t%>%group_by(sid)%>%summarise(pred=mf1(pred))
  write.table(t3to,file = 'out.csv',append = T,sep = ',',
              col.names = F,row.names = F,quote = F)
}