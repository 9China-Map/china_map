
install.packages('tidyverse')
install.packages('scatterpie')
install.packages('ggrepel')
library(tidyverse)
library(sp)
library(scatterpie)
library(ggplot2)
#分列
library(tidyr)
library(dplyr)

library(tidyverse)
#添加列
library(stringr)
library(ggrepel)

#方法一:画地图
#map1 <-readOGR(dsn = "china.geojason", stringsAsFactors = FALSE)
#Encoding(map1@data$name) <- "UTF-8"

#方法二：画地图
china_map<-readOGR("E:/桌面内容/bilibiliRlearning-master/2021_r21_china_map/bou2_4p.shp")
china_map2 <- fortify(china_map)
#地图
p<-ggplot(china_map2) + 
  geom_polygon(aes(x=long, y=lat, group = group), fill = '#669933', colour = '#FFFF99',alpha=0.9,
               show.legend = 'F')+
  
  theme_bw() + 
  theme(panel.grid.minor = element_blank(), legend.background = element_blank()) +
  scale_x_continuous(breaks = c(80, 90, 100, 110,120,130), expand = c(0,0), 
                     labels = c('80°E', '90°E', '100°E', '110°E', '120°E','130°E')) +
  scale_y_continuous(breaks = c(10,20,30,40,50,60), expand = c(0,0), 
                     labels = c('10°N','20°N','30°N','40°N','50°N', '60°N'))+
  coord_map('polyconic')+labs(x=NULL,y=NULL)
p
pdf(file = "china_map.pdf",width=30,height=30)
dev.off()

onexlsx<-readxl::read_xlsx('shiyan.xlsx')#将实验种子数据读进去
onexlsx%>%data.frame(onexlsx)#转换为数据框
onexlsx$ID<-str_sub(onexlsx$ID,1,7)#将ID列提取前7个字符串
onexlsx
fenlie<-separate(onexlsx,ID,c("A","B"),sep="-")#将ID列分割成A,B两列
fenlie

new<- table(onexlsx$ID)#计算各品种的频数
new<-data.frame(new)
new 
#给计算出频数的Var1用‘-’分列
newxin<-separate(new,Var1,c("A","B"),sep="-")
newxin
#给种子种类分组
zong<-split(newxin,newxin$A)
zong
#给分的组命列名
baicao<-zong$稗草
baicao  
names(baicao) <- c("种类1", "B", "baicao")
gouwei<-zong$狗尾
gouwei
names(gouwei) <- c("种类2", "B", "gouwei")
heimai<-zong$黑麦
names(heimai) <- c("种类3", "B", "heimai")
huwei<-zong$虎尾
names(huwei) <- c("种类4", "B", "huwei")
matang<-zong$马唐
names(matang) <- c("种类5", "B", "matang")
niujin<-zong$牛筋
names(niujin) <- c("种类6", "B", "niujin")
x1<-zong$x1
names(x1) <- c("种类7", "B", "x1")
X1<-zong$X1
names(X1) <- c("种类8", "B", "X1")
x2<-zong$x2
names(x2) <- c("种类9", "B", "x2")
X2<-zong$X2
names(X2) <- c("种类10", "B", "X2")

#从fenlie里面筛选'B','经度’，‘纬度’三列,并去除重复值（行）
norep<-fenlie %>%
  select(3,4,5) %>% 
  distinct(B, .keep_all = TRUE)
norep
colnames(norep)[2]<-c('long')
colnames(norep)[3]<-c('lat')
norep
#通过相同的列B合并所有种子数据
baicao2<-full_join(norep,baicao,by='B') 
gouwei2<-full_join(baicao2,gouwei,by='B')
heimai2<-full_join(gouwei2,heimai,by='B') 
huwei2<-full_join(heimai2,huwei,by='B')
matang2<-full_join(huwei2,matang,by='B')
niujin2<-full_join(matang2,niujin,by='B')
x12<-full_join(niujin2,x1,by='B')
X12<-full_join(x12,X1,by='B')
x22<-full_join(X12,x2,by='B')
X22<-full_join(x22,X2,by='B')
#去除所有种类名1-10，多余重复值
X22
X22 %>% 
  select(1,2,3,5,7,9,11,13,15,17,19,21,23)->heb  #命名为heb
heb

#将所有的NA替换为0
heb[is.na(heb)]=0
#去除没有经纬度的行
heb<-head(heb,-3)
heb

#地图p
#饼图半径为0.1
heb$radius <-0.05

#合并地图p和饼图数据heb
p+geom_scatterpie(data=heb,
                  aes(x=long,y=lat,group=B,r=radius),
                  cols=c('baicao','gouwei','heimai','huwei','matang','niujin','x1','X1','x2','X2'),color=NA,alpha=0.8)+
   geom_scatterpie_legend(heb$radius, x=80, y=10)
 # scale_fill_brewer(palette = 8)
# print to pdf
pdf(file = "china_map.pdf",width=30,height=30)
dev.off()
