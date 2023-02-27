install.packages('rgdal')
install.packages('mapproj')
install.packages('readxl')
install.packages('ggpubr')
install.packages('ggplot2')
library(rgdal)
library(ggplot2)
library(readxl)
library(cowplot)
library(ggpubr)
#The modified file is called "jie.xlsx"
xlsxdat<-read_xlsx('jie.xlsx',sheet=1,col_names=TRUE,na='NA')
map1 <- readOGR(dsn = "china.geojason", stringsAsFactors = FALSE)
Encoding(map1@data$name) <- "UTF-8"
map2 <- fortify(map1)
p1<-data.frame(xlsxdat)

#Remove NA
na.omit(p1)
p2<-na.omit(p1)
p3<-split(p2,p2$region)
#Bar chart
x1<-ggplot(p2,aes(year))+geom_bar(aes(fill=region))+
  coord_polar('x')+ylim(-10,20)+theme_void()+
  geom_text(data = data.frame(year=unique(p2$year),y=20),aes(x=year,y=y,label=year))+
  #theme_bw() + 
  #theme(panel.grid.minor = element_blank(), legend.background = element_blank(),axis.text.x=element_text(angle = 45,hjust = 1))
x1
#Frequency histogram of all wheat
x1<-ggplot(p2,aes(year))+ geom_histogram(position="identity", aes(fill=region),alpha=0.4,binwidth = 10)+
  coord_polar('x')+ylim(-10,20)+theme_void()+
  geom_text(data = data.frame(year=unique(p2$year),y=20),aes(x=year,y=y,label=year))+
  #theme_bw() + 
  #theme(panel.grid.minor = element_blank(), legend.background = element_blank(),axis.text.x=element_text(angle = 45,hjust = 1))
x1

x3<-ggplot(p3$`low and middle Yangtze Rivervalley winter wheat region`,aes(year,))+geom_histogram(position="identity", aes(fill=region),alpha=0.4,binwidth = 10)+
  theme_bw() + 
  theme(panel.grid.minor = element_blank(), legend.background = element_blank(),panel.grid=element_blank(),axis.text.x=element_text(angle = 45,hjust = 1))
x3
x4<-ggplot(p3$`Yellow and HuaiRiver valley winter wheat region`,aes(year))+ geom_histogram(position="identity", aes(fill=region),alpha=0.4,binwidth = 10)+
  theme_bw() + 
  theme(panel.grid.minor = element_blank(), legend.background = element_blank(),panel.grid=element_blank(),axis.text.x=element_text(angle = 45,hjust = 1))

pdf("x4.pdf",width=40,height=55)
x4
plot(p3$`Yellow and HuaiRiver valley winter wheat region`, mpg)
dev.off()

x4
#A map without borders
p<-ggplot(map2) + 
  geom_polygon(aes(x=long, y=lat, group = group), fill = '#66CC33', colour = '#FFFF99',
               show.legend = F)+
  
  theme_bw() + 
  theme(panel.grid.minor = element_blank(), legend.background = element_blank(),axis.text.x=element_text(angle = 45,hjust = 1)) +  
  scale_x_continuous(
    breaks = c(80, 90, 100, 110,120,130), expand = c(0,0), 
    labels = c('80°E', '90°E', '100°E', '110°E', '120°E','130°E')) +
  scale_y_continuous(breaks = c(10,20,30,40,50,60), expand = c(0,0), 
                     labels = c('10°N','20°N','30°N','40°N','50°N', '60°N'))+
  coord_map('polyconic')+labs(x=NULL,y=NULL)

pdf("p.pdf",width=40,height=55)
p
plot(p1, mpg)
dev.off()

