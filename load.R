library(readr)
library (tidyverse)
library (compare)
library (gganimate)
library (gifski)

result_m<-data.frame()
input_flile<-c("red2001.csv","red2019.csv","red2021.csv")

for (redact in 1:3)

{
red2021 <- read_delim(input_flile[redact], ";",
escape_double = FALSE, col_types = cols(RED = col_number(),
Article = col_number(), Part = col_number(),
LIMP = col_number(), IMPMAX = col_number(),
IMPMIN = col_number(), DISBATMAX = col_number(),
DISBATMIN = col_number(), RESTOLMAX = col_number(),
RESTOLMIN = col_number(), ARMAX = col_number(),
ARMIN = col_number(), LIMMAX = col_number(),
LIMMIN = col_number(), CORWMAX = col_number(),
CORWMIN = col_number(), CIVMAX = col_number(),
CIVMIN = col_number(), DEPRMAX = col_number(),
DEPRMIN = col_number(), FINEMAX = col_number(),
FINEMIN = col_number(), CONF = col_number(),
DEPRDOP = col_number(), DEPRDOPMAX = col_number(),
DEPRDOPMIN = col_number(), FINEDOP = col_number(),
FINEDOPMAX = col_number(), FINEDOPMIN = col_number(),
GRADEMAN = col_number(), GRADEAUTO = col_number()),
trim_ws = TRUE)


frame1<-as.data.frame(red2021)

if (redact>1) 
  {frame1$GRADEAUTO[frame1$IMPMAX>10]<-4
frame1$GRADEAUTO[frame1$IMPMAX>5&is.na(frame1$GRADEAUTO)]<-3
frame1$GRADEAUTO[frame1$IMPMAX>0&is.na(frame1$GRADEAUTO)]<-2
frame1$GRADEAUTO[frame1$FINEMAX>25000&is.na(frame1$GRADEAUTO)]<-4
frame1$GRADEAUTO[frame1$FINEMAX>10000&is.na(frame1$GRADEAUTO)]<-3
frame1$GRADEAUTO[frame1$FINEMAX>3000&is.na(frame1$GRADEAUTO)]<-2
frame1$GRADEAUTO[is.na(frame1$GRADEAUTO)]<-1} else
{
  frame1$GRADEAUTO[frame1$IMPMAX>10]<-4
  frame1$GRADEAUTO[frame1$IMPMAX>5&is.na(frame1$GRADEAUTO)]<-3
  frame1$GRADEAUTO[frame1$IMPMAX>2&is.na(frame1$GRADEAUTO)]<-2
  frame1$GRADEAUTO[is.na(frame1$GRADEAUTO)]<-1
}

result<-data.frame()
for (grade in 4:1)
{
  type.change<-subset(frame1,frame1$GRADEAUTO==grade)




for (i in 1:nrow(type.change))
{
  need.change<-FALSE
  for (j in 24:9)
  {
    if (need.change&is.na(type.change[i,j])) type.change[i,j]<- -1
    else if (!need.change&is.na(type.change[i,j])) type.change[i,j]<-9999999
    if (!is.na (type.change [i,j])&type.change [i,j]!=9999999&type.change[i,j]!=-1) need.change<-TRUE
  }
}

#arrange

arrange.type<-type.change
arrange.type$RANK<-0
arrange.type<-arrange.type%>%
  arrange (desc(FINEDOPMIN))%>%
    arrange (desc(FINEDOPMAX))%>%
      arrange (desc(FINEDOP))  %>%
        arrange (desc(DEPRDOPMIN))%>%
          arrange (desc(DEPRDOPMAX))%>%
            arrange (desc(DEPRDOP))%>%
              arrange (desc(CONF))%>%
                arrange (desc(FINEMIN))%>%
                  arrange (desc(FINEMAX))%>%
                    arrange (desc(DEPRMIN))%>%
                      arrange (desc(DEPRMAX))%>%
                        arrange (desc(CIVMIN))%>%
                      arrange (desc(CIVMAX))%>%
                    arrange (desc(CORWMIN))%>%
                  arrange (desc(CORWMAX))%>%
                arrange (desc(LIMMIN))%>%
              arrange (desc(LIMMAX))%>%
            arrange (desc(ARMIN))%>%
          arrange (desc(ARMAX))%>%
        arrange (desc(RESTOLMIN))%>%
      arrange (desc(RESTOLMAX))%>%
    arrange (desc(DISBATMIN))%>%
  arrange (desc(DISBATMAX))%>%
arrange (desc(IMPMIN))%>%
arrange (desc(IMPMAX))%>%
arrange (desc(LIMP))


result<-rbind(result,arrange.type)

}
result$RANK[1]<-1
for (i in 2:nrow(result)) 
{if (all.equal(result[i,6:31],result[i-1,6:31],check.attributes=FALSE)==TRUE) 
  {result$RANK[i]<-result$RANK[i-1]} else
  {result$RANK[i]<-result$RANK[i-1]+1}
}
result$INDEX<-0

for (i in 1:nrow(result))
{
result$INDEX[i]<-((result$RANK[nrow(result)]-result$RANK[i]+1)/result$RANK[nrow(result)])*100  
}
for (i in 1:nrow(result))
  for (j in 6:31)
    {if (is.na(result[i,j])) result [i,j]<-0
    if (result[i,j] == -1) result[i,j]<-0
      if (result[i,j] == 9999999) result[i,j]<-0}



result_m<-rbind(result_m,result)
}  
write.csv(result_m,"result.csv")
result<-result_m
