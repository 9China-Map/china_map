install.packages('tidyverse')
install.packages('scatterpie')
install.packages('ggrepel')
install.packages('sp')
install.packages('ggplot2')
install.packages('tidyr')
install.packages('dplyr')
install.packages('tidyverse')
install.packages('stringr')
install.packages('ggrepel')
library(tidyverse)
library(sp)
library(scatterpie)
library(ggplot2)
#Disaggregate
library(tidyr)
library(dplyr)
library(tidyverse)
#Add Column
library(stringr)
library(ggrepel)
#Method 1: Draw a map
china_map <-readOGR(dsn = "china.geojason", stringsAsFactors = FALSE)
Encoding(china_map@data$name) <- "UTF-8"
#Method 2: Draw a map
#china_map<-readOGR("/home/bs674/china_map(1)/bou2_4p.shp")
china_map2 <- fortify(china_map)
#Map
p<-ggplot(china_map2) + 
  geom_polygon(aes(x=long, y=lat, group = group), fill = 'white', colour = 'black',alpha=0.9,lwd=0.1,
               show.legend = 'F')+
  
  theme_bw() + 
  theme(panel.grid.minor = element_blank(), legend.background = element_blank()) +
  scale_x_continuous(breaks = c(80, 90, 100, 110,120,130), expand = c(0,0), 
                     labels = c('80°E', '90°E', '100°E', '110°E', '120°E','130°E')) +
  scale_y_continuous(breaks = c(10,20,30,40,50,60), expand = c(0,0), 
                     labels = c('10°N','20°N','30°N','40°N','50°N', '60°N'))+
  coord_map('polyconic')+labs(x=NULL,y=NULL)
p

onexlsx<-readxl::read_xlsx('shiyan.xlsx')  #Read the experimental seed data
onexlsx%>%data.frame(onexlsx)     #Convert to data frame
onexlsx$ID<-str_sub(onexlsx$ID,1,7)    #Extract the first 7 strings from the ID column
onexlsx
fenlie<-separate(onexlsx,ID,c("A","B"),sep="-")   #Split the ID column into A and B columns
fenlie

new<- table(onexlsx$ID)   #Calculate the frequency of each variety

new<-data.frame(new)
new 
#Var1 of calculated frequency is divided by '-'
newxin<-separate(new,Var1,c("A","B"),sep="-")
newxin
#Group seeds
zong<-split(newxin,newxin$A)
zong
#Name the group
baicao<-zong$稗草
baicao  
names(baicao) <- c("kind1", "B", "baicao")
gouwei<-zong$狗尾
gouwei
names(gouwei) <- c("kind2", "B", "gouwei")
heimai<-zong$黑麦
names(heimai) <- c("kind3", "B", "heimai")
huwei<-zong$虎尾
names(huwei) <- c("kind4", "B", "huwei")
matang<-zong$马唐
names(matang) <- c("kind5", "B", "matang")
niujin<-zong$牛筋
names(niujin) <- c("kind6", "B", "niujin")
x1<-zong$x1
names(x1) <- c("kind7", "B", "x1")
X1<-zong$X1
names(X1) <- c("kind8", "B", "X1")
x2<-zong$x2
names(x2) <- c("kind9", "B", "x2")
X2<-zong$X2
names(X2) <- c("kind10", "B", "X2")
#Extract the 3, 4 and 5 columns in the "fenlie" file and remove the duplicate values (rows)
norep<-fenlie %>%
  select(3,4,5) %>% 
  distinct(B, .keep_all = TRUE)
norep
colnames(norep)[2]<-c("long")
colnames(norep)[3]<-c("lat")
#Merge all seed data through the same column B
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
#Remove redundant data
X22
X22 %>% 
  select(1,2,3,5,7,9,11,13,15,17,19,21,23)->heb  #Named "heb" 
heb
#Replace all NA with 0
heb[is.na(heb)]=0
#Remove lines without longitude and latitude
heb<-head(heb,-3)
heb

#Map file name is P
#The pie radius is 0.5
heb$radius <-0.05
#Merge maps and pie charts
scpdf<-p+geom_scatterpie(data=heb,
                  aes(x=long,y=lat,group=B,r=radius),
                  cols=c('baicao','gouwei','heimai','huwei','matang','niujin','x1','X1','x2','X2'),
                  #color=NA,
                  alpha=0.6,lwd=0.005)+
   geom_scatterpie_legend(heb$radius, x=80, y=10)
scpdf
 # scale_fill_brewer(palette = 5)
#attach(mtcars)

pdf("scpdf.pdf",width=100,height=100)
scpdf
dev.off()

