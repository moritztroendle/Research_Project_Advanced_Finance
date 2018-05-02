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


#2. Density Plots, Skewness & Kurtosis and Jarque-Bera-Test for Normallity for Original Data and First Differences

par(mfrow = c(2,1))

#2.1 Word of Mout - Variabel: W_v1

    #2.1.1 Original Data
    summary(xts_df$W_v1)
    plot(density(na.omit(xts_df$W_v1)))
        lines(density(rnorm(1000000, 
                      mean = mean(na.omit(xts_df$W_v1)), 
                      sd = mean(na.omit(xts_df$W_v1)))), 
              col="red")
    skewness(na.omit(xts_df$W_v1))
    kurtosis(na.omit(xts_df$W_v1))   
    jarque.bera.test(na.omit(xts_df$W_v1))
    
    # 2.1.2 First Differences
    summary(diff_xts_df$W_v1)
    plot(density(na.omit(diff_xts_df$W_v1)))
    lines(density(rnorm(1000000, 
                        mean = mean(na.omit(diff_xts_df$W_v1)), 
                        sd = mean(na.omit(diff_xts_df$W_v1)))), 
          col="red")
    skewness(na.omit(diff_xts_df$W_v1))
    kurtosis(na.omit(diff_xts_df$W_v1))
    jarque.bera.test(na.omit(diff_xts_df$W_v1))
    

#2.2 Word of Mout - Variabel: W_v2
    #2.2.1 Original Data
    summary(xts_df$W_v2)
    plot(density(na.omit(xts_df$W_v2)))
    lines(density(rnorm(1000000, 
                        mean = mean(na.omit(xts_df$W_v2)), 
                        sd = mean(na.omit(xts_df$W_v2)))), 
          col="red")
    skewness(na.omit(xts_df$W_v2))
    kurtosis(na.omit(xts_df$W_v2)) 
    jarque.bera.test(na.omit(xts_df$W_v2))
    
    #2.2.2 First Differences (hier klapp was noch nicht)
    summary(diff_xts_df$W_v2)
    plot(density(na.omit(diff_xts_df$W_v2)))
    lines(density(rnorm(1000000, 
                        mean = mean(na.omit(diff_xts_df$W_v2)), 
                        sd = mean(na.omit(diff_xts_df$W_v2)))), 
          col="red")
    skewness(na.omit(diff_xts_df$W_v2))
    kurtosis(na.omit(diff_xts_df$W_v2))
    jarque.bera.test(na.omit(diff_xts_df$W_v2))

bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
  

    summary(diff_xts_df$P)
    summary(na.omit(diff_xts_df$P))
    plot(density(na.omit(diff_xts_df$P)))
    lines(density(rnorm(1000000, 
                        mean = mean(na.omit(diff_xts_df$P)), 
                        sd = mean(na.omit(diff_xts_df$P)))), 
          col="red")
    
    

    
    
bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
#2.3 Word of Mout - Variabel: W_v3
    #2.3.1 Original Data
    summary(xts_df$W_v3)
    plot(density(na.omit(xts_df$W_v3)))
    lines(density(rnorm(1000000, 
                        mean = mean(na.omit(xts_df$W_v3)), 
                        sd = mean(na.omit(xts_df$W_v3)))), 
          col="red")
    skewness(na.omit(xts_df$W_v3))
    kurtosis(na.omit(xts_df$W_v3)) 
    jarque.bera.test(na.omit(xts_df$W_v3))
    
    #2.3.2 First Differences (hier klapp was noch nicht)
    summary(diff_xts_df$W_v3)
    plot(density(na.omit(diff_xts_df$W_v3)))
    lines(density(rnorm(1000000, 
                        mean = mean(na.omit(diff_xts_df$W_v3)), 
                        sd = mean(na.omit(diff_xts_df$W_v3)))), 
          col="red")
    skewness(na.omit(diff_xts_df$W_v3))
    kurtosis(na.omit(diff_xts_df$W_v3))
    jarque.bera.test(na.omit(diff_xts_df$W_v3))

    
#2.4 Word of Mout - Variabel: S
    #2.4.1 Original Data
    summary(xts_df$S)
    plot(density(na.omit(xts_df$S)))
    lines(density(rnorm(1000000, 
                        mean = mean(na.omit(xts_df$S)), 
                        sd = mean(na.omit(xts_df$S)))), 
          col="red")
    skewness(na.omit(xts_df$S))
    kurtosis(na.omit(xts_df$S)) 
    jarque.bera.test(na.omit(xts_df$S))
    
    #2.4.2 First Differences (hier klapp was noch nicht)
    summary(diff_xts_df$S)
    plot(density(na.omit(diff_xts_df$S)))
    lines(density(rnorm(1000000, 
                        mean = mean(na.omit(diff_xts_df$S)), 
                        sd = mean(na.omit(diff_xts_df$S)))), 
          col="red")
    skewness(na.omit(diff_xts_df$S))
    kurtosis(na.omit(diff_xts_df$S))
    jarque.bera.test(na.omit(diff_xts_df$S))
    
    
    
    yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy
    
    
    
    
    #2.2.5 Word of Mout - Variabel: U
    jarque.bera.test(na.omit(xts_df$U))
    jarque.bera.test(na.omit(diff_xts_df$U))
    
    #2.2.6 Word of Mout - Variabel: P
    jarque.bera.test(na.omit(xts_df$P))
    jarque.bera.test(na.omit(diff_xts_df$P))
    
 #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#   
    
    ######  Word-of-Mout  #####
    ## Plot the Variable
    summary(ts_df$W_v3)
    plot(density(ts_df$W_v3))
    lines(seq(0, 100, by=5), dnorm(seq(0, 100, by=5),
                                   mean(ts_df$W_v3), sd(ts_df$W_v3)), col="blue")
    # Plot First Difference
    W_v3df<-diff(ts_df$W_v3,1) 
    W_v3f.X <- W_v3df[W_v3df!= 0] #remove all 0
    plot(density(W_v3f.X))
    lines(seq(-100, 100, by=5), dnorm(seq(-100, 100, by=5),
                                      mean(W_v3f.X), sd(W_v3f.X)), col="blue")
    
    
    ########### Normal QQ Plot for Word od Mouth
    qqnorm(ts_df$W_v3, ylim=c(-5,100), xlim=c(-5,100))
    qqline(x)
    #Only Scaling
    W_v3s<-scale(df$W_v3, center=TRUE, scale=TRUE)
    qqnorm(W_v3s, ylim=c(-10,10))
    qqline(x)
    #Standardize and Scale Word of Mouth to create another QQ Plot
    W_v3z <- (W_v3f.X - mean(W_v3f.X))/sd(W_v3f.X) 
    tsW_v3scale<-scale(W_v3z, center=TRUE, scale=TRUE)
    qqnorm(tsW_v3scale, ylim=c(-10,10))
    qqline(x)    
    
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#   
    
 
    
      
#2. Visual Inspection fo ACF and PACF for Original Data and First Differences

par(mfrow = c(2,2))
    
#2.1 Word of Mout - Variabel: W_v1
acf(na.omit(xts_df$W_v1), lag=800)
pacf(na.omit(xts_df$W_v1), lag=800)
acf(na.omit(diff_xts_df$W_v1), lag=800)
pacf(na.omit(diff_xts_df$W_v1), lag=800)

#2.2 Word of Mout - Variabel: W_v2
acf(na.omit(xts_df$W_v2), lag=500)
pacf(na.omit(xts_df$W_v2), lag=500)
acf(na.omit(diff_xts_df$W_v2), lag=500)
pacf(na.omit(diff_xts_df$W_v2), lag=500)

#2.3 Word of Mout - Variabel: W_v3
acf(na.omit(xts_df$W_v3), lag=500)
pacf(na.omit(xts_df$W_v3), lag=500)
acf(na.omit(diff_xts_df$W_v3), lag=500)
pacf(na.omit(diff_xts_df$W_v3), lag=500)

#2.4 Word of Mout - Variabel: S
acf(na.omit(xts_df$S), lag=500)
pacf(na.omit(xts_df$S), lag=500)
acf(na.omit(diff_xts_df$S), lag=500)
pacf(na.omit(diff_xts_df$S), lag=500)

#2.5 Word of Mout - Variabel: U
acf(na.omit(xts_df$U), lag=500)
pacf(na.omit(xts_df$U), lag=500)
acf(na.omit(diff_xts_df$U), lag=500)
pacf(na.omit(diff_xts_df$U), lag=500)

#2.6 Word of Mout - Variabel: P
acf(na.omit(xts_df$P), lag=800)
pacf(na.omit(xts_df$P), lag=500)
acf(na.omit(diff_xts_df$P), lag=500)
pacf(na.omit(diff_xts_df$P), lag=500)





#+++++++++++++++++++++++++++++++++++++++++++++++#



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
