vis<-50
court_proc<-read.csv("court_proc.csv",header = TRUE, sep = ";")

nulls<-summarise(group_by(court_proc,Year),
                 nullq=sum(CRTOT==0),
                 varq=sum(CRTOT>=0))

nulls$procnull<-nulls$nullq/nulls$varq*100

nullgr<-ggplot(nulls,aes(label=round(procnull,1),x=factor(Year),y=procnull))
nullgr+geom_bar(stat="identity")+
  geom_text(position = position_stack(vjust = 0.5))+
  labs (title="Частка заборон, які не використовувалися жодного разу (2013-2020 роки)")+
  theme (axis.title.y = element_blank(),axis.text.y = element_blank(),
         axis.ticks.y=element_blank(), axis.title.x = element_blank())

mysave (vis,"compnull")
vis<-vis+1



types_year<-read.csv("types08-20.csv",header = TRUE, sep = ";")
names(types_year)<-c("COMENT","QNT","Year")
bylo1<-c("total","prost","seredn", "tyaj","osob tyaj")
stalo1<-c("усього засуджено","невеликої тяжкості/проступки", "середньої тяжкості/нетяжкі", "тяжкі", "особливо тяжкі")
stalo<-c("невеликої тяжкості/проступки", "середньої тяжкості/нетяжкі", "тяжкі", "особливо тяжкі")


for (i in 1:length(bylo1))
{types_year$COMENT[types_year$COMENT==bylo1[i]]<-stalo1[i]}


types_all<-types_year
types_year<-subset(types_year,types_year$COMENT!="усього засуджено")
graph_t<-ggplot(types_year, aes(label=QNT,x=factor(COMENT,levels = stalo), y=QNT))
graph_t+geom_bar(aes(fill=factor(COMENT,levels = stalo)),stat="identity")+
  geom_text(aes(y=0,hjust=0))+
  facet_wrap(ncol=5,
             .~Year)+
  labs(title = "Кількість засуджених осіб за ступенем тяжкості правопорушень", 
       subtitle=paste("2008-2020 роки"))+
  theme (legend.title = element_blank(),legend.position = "bottom",
         axis.text=element_blank(),axis.ticks=element_blank(),
         axis.title=element_blank())+coord_flip()   


mysave (vis,"types08-20-1")
vis<-vis+1


graph_t<-ggplot(types_all, aes(label=QNT,x=factor(COMENT,levels = stalo), y=QNT))
graph_t+geom_bar(aes(fill=factor(COMENT,levels = stalo1)),stat="identity")+
  geom_text(aes(y=0,hjust=0))+
  facet_wrap(ncol=5,
             .~Year)+
  labs(title = "Кількість засуджених осіб за ступенем тяжкості правопорушень", 
       subtitle=paste("2008-2020 роки"))+
  theme (legend.title = element_blank(),legend.position = "bottom",
         axis.text=element_blank(),axis.ticks=element_blank(),
         axis.title=element_blank())+coord_flip()   


mysave (vis,"types08-20-2")
vis<-vis+1


