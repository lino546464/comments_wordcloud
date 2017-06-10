library(dplyr)
library(ggplot2)
library(stringr)
shops = read.csv('H:\\Python\\dianpingshop.csv',header=F, sep=',')
head(shops)
names(shops) = c('taste','avgprice','comment_counts','title','district','address','id','name','environment','service')
tbl_df(shops)
#shop_a = transform(shops,id=id,name=name,title=title,avgprice=avgprice,district,address)
shop_s = shops[,c(7,8,4,2,5,6,1,9,10,3)]
tbl_df(shop_s)
shop_s$avgprice = str_replace_all(shop_s$avgprice,'[人均：元]','')
shop_s$taste = str_replace_all(shop_s$taste,'[口味：]','')
shop_s$environment = str_replace_all(shop_s$environment,'[环境：]','')
shop_s$service = str_replace_all(shop_s$service,'[服务：]','')
head(shop_s)
ggplot(shop_s,aes(x=title,y=as.numeric(avgprice) ,colour=service))+geom_point()+ylim(0,700)+ylab('avgprice')
ggplot(shop_s,aes(x=factor(title),y=as.numeric(avgprice)))+geom_boxplot()+ylim(0,500)+ylab('avgprice')
ggplot(shop_s,aes(x=title,fill=title))+geom_bar()
ggplot(shop_s,aes(x=district))+geom_bar()+theme(axis.text.x = element_text(angle = 30,size = 8))
dis = shop_s$district[(table(shop_s$district)>5)]
table(dis)
ggthemr()
install.packages('jiebaR')
library(jiebaR)
cc = worker()
cc['H:\\Python\\dianpingshopcomments.txt']
library(readr)
####
###词云
####
library(wordcloud2)
library(dplyr)
shop_comment = read.csv('H:\\Python\\dianping\\commentseg.txt',header = F,sep = ' ')
head(shop_comment)
names(shop_comment) = c('words','Feq')
shop_comment = arrange(shop_comment,desc(Feq))
shop_comment = shop_comment[-1,]
comment_s = shop_comment[1:1000,]
wordcloud2(comment_s,color='random-dark',backgroundColor='gray')
install.packages('ggthemr')
library(devtools)
install_github('cttobin/ggthemr')
httr::set_config( httr::config( ssl_verifypeer = 0L ))
library(ggthemr)
ggthemr('copper')
?set_swatch
library(RColorBrewer)
display.brewer.all()
set_swatch(c(brewer.pal(12,'Set3'),brewer.pal(8,'Set2'),brewer.pal(18,'Set1')))
ggplot(shop_s,aes(x=title,y=as.numeric(avgprice) ,colour=service))+geom_point()+ylim(0,700)+ylab('avgprice')
ggplot(shop_s,aes(x=factor(title),y=as.numeric(avgprice)))+geom_boxplot()+ylim(0,500)+ylab('avgprice')
ggplot(shop_s,aes(x=title,fill=title))+geom_bar()
ggplot(shop_s,aes(x=district))+geom_bar()+theme(axis.text.x = element_text(angle = 30,size = 8))
set_swatch()

###
###评论分析
###
library(dplyr)
library(plyr)
library(stringr)
setwd('H:\\Python\\dianping')
conmment_1 = read.csv('seg_1星.csv',header=F,sep=',',stringsAsFactors=F)
conmment_2 = read.csv('seg_2星.csv',header=F,sep=',',stringsAsFactors=F)
conmment_3 = read.csv('seg_3星.csv',header=F,sep=',',stringsAsFactors=F)
conmment_4 = read.csv('seg_4星.csv',header=F,sep=',',stringsAsFactors=F)
conmment_5 = read.csv('seg_5星.csv',header=F,sep=',',stringsAsFactors=F)
index_4 = sample(1:1460,600)
c_4 = conmment_4[index_4,]
index_5 = sample(1:5285,600)
c_5 = conmment_5[index_5,]
c_c = bind_rows(conmment_1,conmment_2,conmment_3)
c_c$V1 = '差评'
c_h = bind_rows(c_4,c_5)
c_h$V1 = '好评'
coos = bind_rows(c_c,c_h)
coos[coos=='']=NA
index = sample(1:2394,600)
coos_train = coos[-index,]
coos_test = coos[index,]
install.packages('klaR')
library(klaR)
install.packages('combinat')
coos_train$V1 = as.factor(coos_train$V1)
bayes = NaiveBayes(V1~.,coos_train)
names(bayes)
coos_test$V1=as.factor(coos_test$V1)
pre_bayes = predict(bayes,coos_test)
bayes$tables[1]
library(e1071)
bayes1 = naiveBayes(coos_train,coos_train$V1)
bayes1
pre_bayes1 = predict(bayes1,coos_test)
