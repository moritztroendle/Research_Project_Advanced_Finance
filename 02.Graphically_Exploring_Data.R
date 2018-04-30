###Graphically Exploring the Data
library("zoo")
##Time series and scatter plots

plot.zoo(xts_df) #Normal Data in xts Format

# Plotting First Differences for the different Data Objects has to be the same
#Xts Object
par(mfrow = c(1,1))
plot.zoo(diff_xts_df)

#Scatterplot XTS Variables
pairs(as.zoo(xts_df), gap = 0, cex = 0.1)

par(mfrow = c(2,2))
######### ACF and PCF of the XTS Object and its Differences
detach("package:TSA", unload=TRUE)
detach("package:vars", unload=TRUE)
detach("package:ggplot2", unload=TRUE)
detach("package:tidyverse", unload=TRUE)
detach("package:readxl", unload=TRUE)
library("stats")

### W_v1
acf(na.omit(xts_df$W_v1), lag=800)
pacf(na.omit(xts_df$W_v1), lag=800)
acf(na.omit(diff_xts_df$W_v1), lag=800)
pacf(na.omit(diff_xts_df$W_v1), lag=800)

#W_v2
acf(na.omit(xts_df$W_v2), lag=500)
pacf(na.omit(xts_df$W_v2), lag=500)
acf(na.omit(diff_xts_df$W_v2), lag=500)
pacf(na.omit(diff_xts_df$W_v2), lag=500)

#W_v3
acf(na.omit(xts_df$W_v3), lag=500)
pacf(na.omit(xts_df$W_v3), lag=500)
acf(na.omit(diff_xts_df$W_v3), lag=500)
pacf(na.omit(diff_xts_df$W_v3), lag=500)

#S
acf(na.omit(xts_df$S), lag=500)
pacf(na.omit(xts_df$S), lag=500)
acf(na.omit(diff_xts_df$S), lag=500)
pacf(na.omit(diff_xts_df$S), lag=500)

#U
acf(na.omit(xts_df$U), lag=500)
pacf(na.omit(xts_df$U), lag=500)
acf(na.omit(diff_xts_df$U), lag=500)
pacf(na.omit(diff_xts_df$U), lag=500)

#P
acf(na.omit(xts_df$P), lag=800)
pacf(na.omit(xts_df$P), lag=500)
acf(na.omit(diff_xts_df$P), lag=500)
pacf(na.omit(diff_xts_df$P), lag=500)



## Skewness, Kurtosis, 
skewness(xts_df)
skewness(xts_df.X)
kurtosis(xts_df)
kurtosis(xts_df.X)

###Jarque-Bera XTS Object
Ind.num<-coredata(xts_df.X)
jarque.test(Ind.num[,1]) #W_v1
jarque.test(Ind.num[,2]) #W_v2
jarque.test(Ind.num[,3]) #W_v3
jarque.test(Ind.num[,4]) #S
jarque.test(Ind.num[,5]) #U
jarque.test(Ind.num[,6]) #P

###Jarque-Bera First DIfferences XTS Object
jarque.bera.test(na.omit(diff_xts_df$W_v1))
jarque.bera.test(na.omit(diff_xts_df$W_v2))
jarque.bera.test(na.omit(diff_xts_df$W_v3))
jarque.bera.test(diff_xts_df$S)
jarque.bera.test(na.omit(diff_xts_df$U))
jarque.bera.test(diff_xts_df$P)


INDEXES.X.w <- apply.weekly(INDEXES.X, FUN = colSums) # compute weekly log-returns by summing within weeks

INDEXES.X.ws<-scale(INDEXES.X.w, center=TRUE, scale=TRUE)





Decomposing die ts Variablen
#tsW_v1dec<-decompose(tsW_v1)
#tsW_v2dec<-decompose(tsW_v2)
#tsW_v3dec<-decompose(tsW_v3)
#tsS_v1dec<-decompose(tsS)
#tsU_v1dec<-decompose(tsU)
#tsP_v1dec<-decompose(tsP)

#Plotting Decompositions der additiven ts Variablen
#plot(tsW_v1dec)
#plot(tsW_v2dec)
#plot(tsW_v3dec)
#plot(tsSdec)
#plot(tsUdec)
#plot(tsPdec)
