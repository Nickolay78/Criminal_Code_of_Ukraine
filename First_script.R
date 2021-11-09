# Скрипт визначає послідовність виконання всіх скриптів проєкту

library (beepr)


exlist<-c("load.R",
          "result1.R",
          "types.R",
          "punish.R",
          "court.R")                    
start<-Sys.time()

for (pow in 1:length(exlist))
{source(exlist[pow],encoding = "UTF-8",echo = TRUE)}

beep(5)
print (c(paste("Роботу розпочато: ",start," Роботу закінчено ",Sys.time())))
