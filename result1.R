library (tidyverse)
library(scales)
require(scales)
point <- format_format(big.mark = " ", decimal.mark = ",", scientific = FALSE)

rozmir=data.frame(
  first=c(8,9.23),
  A5=c(5.83, 8.27))
dpi.a<-900
graph.k<-1
graphw<-graph.k*rozmir$A5[2]
graphh<-graph.k*rozmir$A5[1]

result_m$ArtAxis<-result_m$Article%/%10
result_m$ArtAxis<-as.character(result_m$ArtAxis)
for (i in 1:nrow(result_m))
{
  if (result_m$Article[i]%%10>0) result_m$ArtAxis[i]<-paste(result_m$ArtAxis[i],"-",result_m$Article[i]%%10,sep = "")
  result_m$ArtAxis[i]<-paste(result_m$ArtAxis[i]," ч.",result_m$Part[i],sep = "")
  }

mysave<-function (name,year=1)
{ggsave (paste(year,"visinc-",name,".png",sep=""), width = graphw, height = graphh, path="GRAPH/",dpi=dpi.a)}

vis<-1
chapter_ord<-c("I","II","III","IV","V","VI","VII","VIII","IX","X",
               "XI","XII","XIII","XIV","XV","XVI","XVII","XVIII","XIX","XX")   

reda_cc<-c(2001,2019,2021)
for (reda in 1:3)
{
vis<-1
    result<-subset(result_m,result_m$RED==reda_cc[reda])
result$Chapter<-as.roman(result$Chapter)
trust<-result
trust<-arrange(trust,Part)
trust<-arrange(trust,Article)
trust$Order<-1:nrow(trust)

gr1<-summarise(group_by(trust,Chapter),
               Average1=mean (INDEX))
gr1$Chapter<-as.roman(gr1$Chapter)
gr1<-arrange(gr1,Chapter)

graph1<-ggplot(gr1,aes(x=factor(Chapter,levels=chapter_ord),y=Average1,label=round(Average1,1)))
graph1+geom_bar(stat = "identity")+labs(title = "Середній рівень інтенсивності санкцій 
за Розділами Особливої частини КК", subtitle=paste("редакція ",reda_cc[reda]," року"),x="Розділ Особливої частини КК", 
y = "Інтенсивність санкцій")+geom_label()

mysave (vis,reda_cc[reda])
vis<-vis+1



gr2<-summarise(group_by(trust,Chapter,Article,Part),
               Order=Order,
               Statya=ArtAxis,
               INDEX=INDEX)
gr2$Chapter<-as.roman(gr2$Chapter)
for (chapt in 1:5)
{
graph2<-ggplot(subset(gr2,Chapter<as.roman(chapt*4+1)&Chapter>=as.roman(chapt*4-3)), aes(x=factor(Order,labels = Statya), y=INDEX))
graph2+geom_point()+geom_line()+
  facet_wrap(ncol=1,
             .~factor(as.character(Chapter),
                      levels=chapter_ord),
             scales="free_x")+
  labs(y="інтенсивність санкцій",
       title = paste("Інтенсивність санкцій за розділами Особливої частини КК. Розділи ",as.roman(chapt*4-3),"-",as.roman(chapt*4),sep=""),
       subtitle=paste("редакція ",reda_cc[reda],"року"))+
  theme (axis.text.x = element_text(angle=90, hjust=1, size = 6),axis.title.x=element_blank())    

mysave (vis,reda_cc[reda])
vis<-vis+1
}


gr3<-summarise(group_by(trust,Chapter,Article),
                              INDEX=mean(INDEX))
gr3$Counter<-0
for (i in 1:20)
{
gr3$Counter[gr3$Chapter==as.roman(i)]<-1:nrow(subset(gr3,Chapter==as.roman(i)))
}
  

graph4<-ggplot(gr3,aes(x=Counter, y=INDEX))
graph4+geom_point()+geom_line()+geom_smooth(method = "lm")+
  facet_wrap(ncol=4,
             .~factor(as.character(Chapter),
                      levels=chapter_ord),
             scales="free_x")+scale_y_continuous(limits = c(0, 140))+
  labs(y="інтенсивність санкцій",
       title = "Середні значення санкцій статей за розділами Особливої частини КК", 
       subtitle=paste("редакція ",reda_cc[reda],"року"))+
  theme (axis.text.x=element_blank(),axis.title.x=element_blank())

mysave (vis,reda_cc[reda])
vis<-vis+1

}



result<-subset(result_m,RED==2001|RED==2021)
result$Chapter<-as.roman(result$Chapter)
trust<-result
trust<-arrange(trust,Part)
trust<-arrange(trust,Article)

gr1<-summarise(group_by(trust,RED,Chapter),
               Average1=mean (INDEX))
gr1$Chapter<-as.roman(gr1$Chapter)
gr1<-arrange(gr1,Chapter)


delta=data.frame(chapter=1:20)
delta$chapter<-as.roman(delta$chapter)
delta$delta1<-0

delta$delta1<-gr1$Average1[gr1$RED==2021]-gr1$Average1[gr1$RED==2001]  

graph1<-ggplot(delta,aes(x=factor(chapter,levels=chapter_ord),y=delta1,label=round(delta1,1)))
graph1+geom_bar(stat = "identity",fill=ifelse(delta$delta1>0,"red","green"))+
labs(title = "Динаміка середнього рівня інтенсивності санкцій статей Особливої частини КК",
     subtitle = "порівняння редакцій КК 2001 та 2021 років",
     x="Розділ Особливої частини КК",y = "зміна інтенсивності санкцій")+geom_text()


mysave (vis,"01-19-21")
vis<-vis+1

gr1<-summarise(group_by(result_m,RED),
               Average1=mean (INDEX))
graph1<-ggplot(gr1,aes(x=factor(RED),y=Average1,label=round(Average1,1)))
graph1+geom_bar(stat = "identity")+
  labs(title = "Cередній рівень інтенсивності санкцій статей Особливої частини КК",
       subtitle = "порівняння редакцій КК 2001, 2019 та 2021 років",
       x="редакція КК",y = "середній рівень інтенсивності санкцій")+geom_label(y=30)

mysave (vis,"01-19-21")
vis<-vis+1

compare<-summarise(group_by(result,ArtAxis,Chapter),
                   deltaindex=INDEX[RED==2021]-INDEX[RED==2001]
                   )

for (chapt in 1:5)
{
  graph2<-ggplot(subset(compare,Chapter<as.roman(chapt*4+1)&Chapter>=as.roman(chapt*4-3)), aes(x=factor(ArtAxis), y=deltaindex))
  graph2+geom_bar(stat = "identity")+geom_hline(yintercept = 0, color="red")+
    facet_wrap(ncol=1,
               .~factor(as.character(Chapter),
                        levels=chapter_ord),
               scales="free_x")+
    labs(y="інтенсивність санкцій",
         title = paste("Динаміка інтенсивності санкцій за розділами Особливої частини КК. Розділи ",as.roman(chapt*4-3),"-",as.roman(chapt*4),sep=""),
         subtitle=paste("порівняння редакцій 2001 та 2021 років"))+
    theme (axis.text.x = element_text(angle=90, hjust=1, size = 6),axis.title.x=element_blank())    
  
  mysave (vis,"compare")
  vis<-vis+1
}
newart<-subset(result,RED==2021)
#newart$new[newart$ArtAxis in compare$ArtAxis]<-1




