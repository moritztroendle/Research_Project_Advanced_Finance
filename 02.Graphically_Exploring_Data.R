---------------------------------------------------------------------------------------------------------
                                  #|| Graphically Exploring the Data ||#
---------------------------------------------------------------------------------------------------------
  
#Install Packages
install.packages("zoo")
install.packages("moments")
install.packages("tseries")

#Load Library
library("zoo")
library("moments")
library("tseries")

---------------------------------------------------------------------------------------------------------
#0. Test for Stationarity of Variable

  #Platzhalter

#0.1 For Original Data 
  
#0.2 For First Differences
  
#1.Time Series and Scatter Plots

#1.1 Time Series Plots: Original Data in xts Object  
plot.zoo(xts_df)
plot(xts_df)

  
#1.2 Scatter Plots: Original Data in xts/ts Object - Visual Inspection of Correlation between Variables
pairs(as.zoo(xts_df), gap = 0, cex = 0.1)
    plot(ts_df$W_v3, ts_df$S)
    plot(ts_df$W_v3, ts_df$P)
    plot(ts_df$W_v3, ts_df$S)
    plot(ts_df$W_v3, ts_df$U)
    plot(ts_df$P, ts_df$S)
    plot(ts_df$P, ts_df$U)
    plot(ts_df$S, ts_df$U)

#1.3 Time Series Plots: First Differences in xts Object
plot.zoo(diff_xts_df)

#1.4 Scatter Plots: First Differences in xts Object - Visual Inspection of Correlation between Variables
pairs(as.zoo(diff_xts_df), gap = 0, cex = 0.1)
    plot(diff_ts_df$W_v3,diff_ts_df$P)
    plot(diff_ts_df$W_v3,diff_ts_df$S)
    plot(diff_ts_df$W_v3,diff_ts_df$U)
    plot(diff_ts_df$P,diff_ts_df$S)
    plot(diff_ts_df$P,diff_ts_df$U)
    plot(diff_ts_df$S,diff_ts_df$U)


#2. Density/qq-Plots, Skewness/Kurtosis & Jarque-Bera-Test for Original Data and First Differences

par(mfrow = c(2,2))

#2.1 Word of Mout - Variabel: W_v1

    #2.1.1 Original Data
    summary(xts_df$W_v1)
    plot(density(na.omit(xts_df$W_v1)), main="Density Plot W_v1")
        lines(density(rnorm(1000000, 
              mean = mean(na.omit(xts_df$W_v1)), 
              sd = sd(na.omit(xts_df$W_v1)))), col="red")
    qqnorm((scale(ts_df$W_v1, center=TRUE, scale=TRUE)), ylim=c(-5,10))
        qqline(ndv)
    skewness(na.omit(xts_df$W_v1))
    kurtosis(na.omit(xts_df$W_v1))   
    jarque.bera.test(na.omit(xts_df$W_v1))
    
    # 2.1.2 First Differences
    summary(diff_xts_df$W_v1)
    plot(density(na.omit(diff_xts_df$W_v1)), main="Density Plot diff_W_v1")
        lines(density(rnorm(1000000, 
              mean = mean(na.omit(diff_xts_df$W_v1)), 
              sd = sd(na.omit(diff_xts_df$W_v1)))), col="red")
    qqnorm((scale(diff_ts_df$W_v1, center=TRUE, scale=TRUE)), ylim=c(-5,10))
        qqline(ndv)
    skewness(na.omit(diff_xts_df$W_v1))
    kurtosis(na.omit(diff_xts_df$W_v1))
    jarque.bera.test(na.omit(diff_xts_df$W_v1))
    

    #2.2 Word of Mout - Variabel: W_v2
    
    #2.2.1 Original Data
    summary(xts_df$W_v2)
    plot(density(na.omit(xts_df$W_v2)), main="Density Plot W_v2")
    lines(density(rnorm(1000000, 
                        mean = mean(na.omit(xts_df$W_v2)), 
                        sd = sd(na.omit(xts_df$W_v2)))), col="red")
    qqnorm((scale(ts_df$W_v2, center=TRUE, scale=TRUE)), ylim=c(-5,10))
    qqline(ndv)
    skewness(na.omit(xts_df$W_v2))
    kurtosis(na.omit(xts_df$W_v2))   
    jarque.bera.test(na.omit(xts_df$W_v2))
    
    # 2.2.2 First Differences
    summary(diff_xts_df$W_v2)
    plot(density(na.omit(diff_xts_df$W_v2)), main="Density Plot diff_W_v2")
    lines(density(rnorm(1000000, 
                        mean = mean(na.omit(diff_xts_df$W_v2)), 
                        sd = sd(na.omit(diff_xts_df$W_v2)))), col="red")
    qqnorm((scale(diff_ts_df$W_v2, center=TRUE, scale=TRUE)), ylim=c(-5,10))
    qqline(ndv)
    skewness(na.omit(diff_xts_df$W_v2))
    kurtosis(na.omit(diff_xts_df$W_v2))
    jarque.bera.test(na.omit(diff_xts_df$W_v2))
    
   
    
    #2.3 Word of Mout - Variabel: W_v3

    #2.3.1 Original Data
    summary(xts_df$W_v3)
    plot(density(na.omit(xts_df$W_v3)), main="Density Plot W_v3")
    lines(density(rnorm(1000000, 
                        mean = mean(na.omit(xts_df$W_v3)), 
                        sd = sd(na.omit(xts_df$W_v3)))), col="red")
    qqnorm((scale(ts_df$W_v3, center=TRUE, scale=TRUE)), ylim=c(-5,10))
    qqline(ndv)
    skewness(na.omit(xts_df$W_v3))
    kurtosis(na.omit(xts_df$W_v3))   
    jarque.bera.test(na.omit(xts_df$W_v3))
    
    # 2.3.2 First Differences
    summary(diff_xts_df$W_v3)
    plot(density(na.omit(diff_xts_df$W_v3)), main="Density Plot diff_W_v3")
    lines(density(rnorm(1000000, 
                        mean = mean(na.omit(diff_xts_df$W_v3)), 
                        sd = sd(na.omit(diff_xts_df$W_v3)))), col="red")
    qqnorm((scale(diff_ts_df$W_v3, center=TRUE, scale=TRUE)), ylim=c(-5,10))
    qqline(ndv)
    skewness(na.omit(diff_xts_df$W_v3))
    kurtosis(na.omit(diff_xts_df$W_v3))
    jarque.bera.test(na.omit(diff_xts_df$W_v3))
    
    
    
    #2.4 Word of Mout - Variabel: S
    
    #2.4.1 Original Data
    summary(xts_df$S)
    plot(density(na.omit(xts_df$S)), main="Density Plot S")
    lines(density(rnorm(1000000, 
                        mean = mean(na.omit(xts_df$S)), 
                        sd = sd(na.omit(xts_df$S)))), col="red")
    qqnorm((scale(ts_df$S, center=TRUE, scale=TRUE)), ylim=c(-5,10))
    qqline(ndv)
    skewness(na.omit(xts_df$S))
    kurtosis(na.omit(xts_df$S))   
    jarque.bera.test(na.omit(xts_df$S))
    
    # 2.4.2 First Differences
    summary(diff_xts_df$S)
    plot(density(na.omit(diff_xts_df$S)), main="Density Plot diff_S")
    lines(density(rnorm(1000000, 
                        mean = mean(na.omit(diff_xts_df$S)), 
                        sd = sd(na.omit(diff_xts_df$S)))), col="red")
    qqnorm((scale(diff_ts_df$S, center=TRUE, scale=TRUE)), ylim=c(-5,10))
    qqline(ndv)
    skewness(na.omit(diff_xts_df$S))
    kurtosis(na.omit(diff_xts_df$S))
    jarque.bera.test(na.omit(diff_xts_df$S))  
    
    
    #2.5 Word of Mout - Variabel: U

    #2.5.1 Original Data
    summary(xts_df$U)
    plot(density(na.omit(xts_df$U)), main="Density Plot U")
    lines(density(rnorm(1000000, 
                        mean = mean(na.omit(xts_df$U)), 
                        sd = sd(na.omit(xts_df$U)))), col="red")
    qqnorm((scale(ts_df$U, center=TRUE, scale=TRUE)), ylim=c(-5,10))
    qqline(ndv)
    skewness(na.omit(xts_df$U))
    kurtosis(na.omit(xts_df$U))   
    jarque.bera.test(na.omit(xts_df$U))
    
    # 2.5.2 First Differences
    summary(diff_xts_df$U)
    plot(density(na.omit(diff_xts_df$U)), main="Density Plot diff_U")
    lines(density(rnorm(1000000, 
                        mean = mean(na.omit(diff_xts_df$U)), 
                        sd = sd(na.omit(diff_xts_df$U)))), col="red")
    qqnorm((scale(diff_ts_df$U, center=TRUE, scale=TRUE)), ylim=c(-5,10))
    qqline(ndv)
    skewness(na.omit(diff_xts_df$U))
    kurtosis(na.omit(diff_xts_df$U))
    jarque.bera.test(na.omit(diff_xts_df$U))  
    
    #2.6 Word of Mout - Variabel: P
  
    #2.6.1 Original Data
    summary(xts_df$P)
    plot(density(na.omit(xts_df$P)), main="Density Plot P")
    lines(density(rnorm(1000000, 
                        mean = mean(na.omit(xts_df$P)), 
                        sd = sd(na.omit(xts_df$P)))), col="red")
    qqnorm((scale(ts_df$P, center=TRUE, scale=TRUE)), ylim=c(-5,10))
    qqline(ndv)
    skewness(na.omit(xts_df$P))
    kurtosis(na.omit(xts_df$P))   
    jarque.bera.test(na.omit(xts_df$P))
    
    # 2.6.2 First Differences
    summary(diff_xts_df$P)
    plot(density(na.omit(diff_xts_df$P)), main="Density Plot diff_P")
    lines(density(rnorm(1000000, 
                        mean = mean(na.omit(diff_xts_df$P)), 
                        sd = sd(na.omit(diff_xts_df$P)))), col="red")
    qqnorm((scale(diff_ts_df$P, center=TRUE, scale=TRUE)), ylim=c(-5,10))
    qqline(ndv)
    skewness(na.omit(diff_xts_df$P))
    kurtosis(na.omit(diff_xts_df$P))
    jarque.bera.test(na.omit(diff_xts_df$P))  
    
 
      
#3. Visual Inspection fo ACF and PACF for Original Data and First Differences

#3.1 Word of Mout - Variabel: W_v1
acf(na.omit(xts_df$W_v1), lag=800, main="ACF W_v1")
pacf(na.omit(xts_df$W_v1), lag=800, main="PACF W_v1")
acf(na.omit(diff_xts_df$W_v1), lag=800, main="ACF diff_W_v1")
pacf(na.omit(diff_xts_df$W_v1), lag=800, main="PACF diff_W_v1")
    
#3.2 Word of Mout - Variabel: W_v2
acf(na.omit(xts_df$W_v2), lag=800, main="ACF W_v2")
pacf(na.omit(xts_df$W_v2), lag=800, main="PACF W_v2")
acf(na.omit(diff_xts_df$W_v2), lag=800, main="ACF diff_W_v2")
pacf(na.omit(diff_xts_df$W_v2), lag=800, main="PACF diff_W_v2")

#3.3 Word of Mout - Variabel: W_v3
acf(na.omit(xts_df$W_v3), lag=800, main="ACF W_v3")
pacf(na.omit(xts_df$W_v3), lag=800, main="PACF W_v3")
acf(na.omit(diff_xts_df$W_v3), lag=800, main="ACF diff_W_v2")
pacf(na.omit(diff_xts_df$W_v3), lag=800, main="PACF diff_W_v2")

#3.4 Word of Mout - Variabel: S
acf(na.omit(xts_df$S), lag=800, main="ACF S")
pacf(na.omit(xts_df$S), lag=800, main="PACF S")
acf(na.omit(diff_xts_df$S), lag=800, main="ACF diff_S")
pacf(na.omit(diff_xts_df$S), lag=800, main="PACF diff_S")

#3.5 Word of Mout - Variabel: U
acf(na.omit(xts_df$U), lag=800, main="ACF U")
pacf(na.omit(xts_df$U), lag=800, main="PACF U")
acf(na.omit(diff_xts_df$U), lag=800, main="ACF diff_U")
pacf(na.omit(diff_xts_df$U), lag=800, main="PACF diff_U")

#3.6 Word of Mout - Variabel: P
acf(na.omit(xts_df$P), lag=800, main="ACF P")
pacf(na.omit(xts_df$P), lag=800, main="PACF P")
acf(na.omit(diff_xts_df$P), lag=800, main="ACF diff_P")
pacf(na.omit(diff_xts_df$P), lag=800, main="PACF diff_P")



#4. Decomposing Time Series Data

#4

decompose(xts_df$S)

class(xts_df$S)



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
