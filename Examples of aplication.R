setwd("C:/Users/asus/Desktop/PEN/_LA_6/40_survrec/survrec")

library(survidm)

library(survrec)

data("bladder4state")

bladder4state


b3state <- multidf(time1=bladder4state$y1, event1=bladder4state$d1, 
                   time=bladder4state$y1+bladder4state$y2,status=bladder4state$d2)


KMWdf(b3state,x=13,y=20)
LDMdf(b3state,x=13,y=20)
LINdf(b3state,x=13,y=20)
WCHdf(b3state,x=13,y=20)


plot.multidf(x=b3state, t1=3, method="KMW", type = "s")
plot.multidf(x=b3state, t1=3, method="LIN", type = "s")
plot.multidf(x=b3state, t1=3, method="WCH", type = "s")
plot.multidf(x=b3state, t1=3, method="LANDMARK", type = "s")

data("bladder3")

b3 <- multidf(time1=bladder3$t1, event1=bladder3$d1, 
              time=bladder3$t2, status=bladder3$d2)

head(b3[[1]])
KMWdf(b3,x=13,y=20)
LDMdf(b3,x=13,y=20)
LINdf(b3,x=13,y=20)
WCHdf(b3,x=13,y=20)



data("bladder5state")

b4state <- multidf(time1=bladder5state$y1, event1=bladder5state$d1,
                   time2= bladder5state$y1+bladder5state$y2, event2=bladder5state$d2,
                   time=bladder5state$y1+bladder5state$y2+bladder5state$y3,
                   status=bladder5state$d3)


head(b4state[[1]])
KMW3df(b4state,x=13,y=20,z=40)
LDM3df(b4state,x=13,y=20,z=40)
LIN3df(b4state,x=13,y=20,z=40)
WCH3df(b4state,x=13,y=20,z=40)




b4 <- multidf(time1=bladder4$t1, event1=bladder4$d1,
              time2= bladder4$t2, event2=bladder4$d2,
              time=bladder4$t3, status=bladder4$d3)

head(b4)

KMW3df(b4,x=13,y=20,z=40)
LDM3df(b4,x=13,y=20,z=40)
LIN3df(b4,x=13,y=20,z=40)
WCH3df(b4,x=13,y=20,z=40)



b3state2 <- multidf(time1=bladder4state$y1, event1=bladder4state$d1, 
                    time=bladder4state$y1+bladder4state$y2,status=bladder4state$d2, 
                    size=bladder4state$size)

b3size <- multidf(time1=bladder3$t1, event1=bladder3$d1, 
                  time=bladder3$t2, status=bladder3$d2, size=bladder4state$size)

head(b3state2[[1]])
library(KernSmooth)
IPCWdf(object=b3state2, x=13, y=15, covariate="size", cov.value=3, window = "gaussian")
IPCWdf(object=b3state2, x=13, y=15, covariate="size", bw=2, cov.value=3, window = "gaussian")

IPCWdf(object=b3size, x=13, y=15, covariate="size", cov.value=3, window = "gaussian")
IPCWdf(object=b3size, x=13, y=15, covariate="size", bw=2, cov.value=3, window = "gaussian")


