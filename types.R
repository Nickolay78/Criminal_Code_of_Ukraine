

reda_cc<-c(2001,2019,2021)
for (reda in 1:3)
{
  vis<-10
  trust<-subset(result_m,result_m$RED==reda_cc[reda])


      gr5<-summarise(group_by(trust,Chapter),
               PROST=sum(GRADEAUTO==1),
               NETYAJ=sum(GRADEAUTO==2),
               TYAJ=sum(GRADEAUTO==3),
               OSTYAJ=sum(GRADEAUTO==4))



      gr5<-as.data.frame (gr5)
      gr5<-reshape(gr5,
             timevar = "Comment", 
             times=c("PROST","NETYAJ","TYAJ","OSTYAJ"), 
             v.names = "QNT", 
             varying = c("PROST","NETYAJ","TYAJ","OSTYAJ"), 
             direction = "long")

      bylo<-c("PROST","NETYAJ","TYAJ","OSTYAJ")
    if (reda>1) stalo<-c("проступки","нетяжкі злочини","тяжкі злочини","особливо тяжкі злочини") else
        stalo<-c("злочини невеликої тяжкості","злочини середньої тяжкості","тяжкі злочини","особливо тяжкі злочини")
        for (i in 1:length(bylo))
          {gr5$Comment[gr5$Comment==bylo[i]]<-stalo[i]}

      gr5$QNT[gr5$QNT==0]<-NA

    graph5<-ggplot(gr5, aes(label=QNT,x=factor(Comment,levels = stalo), y=QNT))
      graph5+geom_bar(aes(fill=factor(Comment,levels = stalo)),stat="identity")+
        geom_text(aes(y=0,hjust=0))+
          facet_wrap(ncol=4,
             .~factor(as.character(Chapter),
                      levels=chapter_ord))+
  labs(title = "Кількість правопорушень за ступенем тяжкості
за розділами Особливої частини КК", subtitle=paste("редакція",reda_cc[reda], "року"))+
  theme (legend.title = element_blank(),legend.position = "bottom",
         axis.text=element_blank(),axis.ticks=element_blank(),
         axis.title=element_blank())+coord_flip()    

  mysave (vis,reda_cc[reda])
  vis<-vis+1

      gr5$QNT[is.na(gr5$QN)]<-0
        gr6<-summarise(group_by(gr5,Comment),
               QNT2=sum(QNT))

        graph6<-ggplot(gr6, aes(label=QNT2,x=factor(Comment,levels = stalo), y=QNT2))
        graph6+geom_bar(aes(fill=factor(Comment,levels = stalo)),stat="identity")+
          geom_text(position = position_stack(vjust = 0.5))+
            labs(title = "Розподіл правопорушень, передбачених Особливою частиною КК за ступенем тяжкості",
       subtitle=paste("редакція",reda_cc[reda], "року"))+
          theme (legend.title = element_blank(),legend.position = "bottom",
         axis.text=element_blank(),axis.ticks=element_blank(),
         axis.title=element_blank())+coord_flip()    

  mysave (vis,reda_cc[reda])
  vis<-vis+1
}


trust<-subset(result_m,result_m$RED==2001|result_m$RED==2021)


gr5<-summarise(group_by(trust,RED, Chapter),
               PROST=sum(GRADEAUTO==1),
               NETYAJ=sum(GRADEAUTO==2),
               TYAJ=sum(GRADEAUTO==3),
               OSTYAJ=sum(GRADEAUTO==4))



gr5<-as.data.frame (gr5)
gr5<-reshape(gr5,
             timevar = "Comment", 
             times=c("PROST","NETYAJ","TYAJ","OSTYAJ"), 
             v.names = "QNT", 
             varying = c("PROST","NETYAJ","TYAJ","OSTYAJ"), 
             direction = "long")

bylo<-c("PROST","NETYAJ","TYAJ","OSTYAJ")
stalo<-c("злочини невеликої тяжкості/проступки","злочини середньої тяжкості/нетяжкі","тяжкі злочини","особливо тяжкі злочини")
for (i in 1:length(bylo))
{gr5$Comment[gr5$Comment==bylo[i]]<-stalo[i]}




gr5$QNT[is.na(gr5$QNT)]<-0
gr6<-summarise(group_by(gr5,RED, Comment),
               QNT2=sum(QNT))

graph6<-ggplot(gr6, aes(label=QNT2,x=factor(Comment,levels = stalo), y=QNT2))
graph6+geom_bar(aes(fill=factor(Comment,levels = stalo)),stat="identity")+
  facet_wrap(.~RED)+
  geom_text(position = position_stack(vjust = 0.5))+
  labs(title = "Розподіл правопорушень, передбачених Особливою частиною КК за ступенем тяжкості",
       subtitle=paste("порівняння редакцій 2001 та 2021 років"))+
  theme (legend.title = element_blank(),legend.position = "bottom",
         axis.text=element_blank(),axis.ticks=element_blank(),
         axis.title=element_blank())

mysave (vis,"compare01-21_by_art12")
vis<-vis+1

gr7<-summarise(group_by(gr5,RED),QNTT=sum(QNT))







