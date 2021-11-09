#LIMP	IMPMAX	DISBATMAX	RESTOLMAX	ARMAX	LIMMAX	CORWMAX	CIVMAX	DEPRMAX	FINEMAX CONF

Punish<-data.frame()

numbrs<-c(6,7,9,11,13,15,17,19,21,23,25)
type_punish<-c("довічне позбавлення
волі",
"позбавлення волі
на певний строк",
"тримання в дисциплінарному
батальйоні",
"обмеження волі",
"арешт",
"службові обмеження
для військовослужбовців",
"виправні роботи",
"громадські роботи",
"позбавлення права
обіймати певні посади...",
"штраф",
"конфіскація майна")

reda_cc<-c(2001,2019,2021)
for (reda in 1:3)
{
  vis<-20
  result<-subset(result_m,result_m$RED==reda_cc[reda])
  
quant<-c(1:11)
for (i in 1:11)
{
  quant[i]<-sum(result[,numbrs[i]]>0)
}
P_type<-data.frame(QNT=quant,
                 Type=type_punish)
Punish<-rbind(Punish,P_type)
grt<-ggplot(P_type, aes(x=factor(Type,levels = type_punish),y=QNT,label = QNT))
grt+geom_bar(aes(fill=factor(Type,levels = type_punish)),stat = "identity")+
theme(legend.title=element_blank(),
      axis.text = element_blank(),axis.title = element_blank(),
      axis.ticks=element_blank())+
  labs (title = "Розподіл покарань, передбачених статтями Особливої частини КК",
        subtitle=paste("редакція ",reda_cc[reda]," року"))+
  geom_text(aes(y=25))

mysave (vis,reda_cc[reda])
vis<-vis+1
}
Punish$Year<-2001
Punish$Year[12:22]<-rep(2019,11)
Punish$Year[23:33]<-rep(2021,11)

Punish<-subset(Punish,Year==2001|Year==2021)
grt<-ggplot(Punish, aes(x=factor(Type,levels = type_punish),y=QNT,label = QNT))
grt+geom_bar(aes(fill=factor(Type,levels = type_punish)),stat = "identity")+
  facet_wrap(.~Year)+theme(legend.title=element_blank(),
        axis.text = element_blank(),axis.title = element_blank(),
        axis.ticks=element_blank(),legend.position = "bottom")+
  labs (title = "Розподіл покарань, передбачених статтями Особливої частини КК",
        subtitle="редакції 2001,2019 та 2021 років")+
  geom_text()

mysave (vis,"punish-representation-01-21")
vis<-vis+1
