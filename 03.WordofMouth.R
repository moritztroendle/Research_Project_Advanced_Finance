#######Data Transformation ####### 

## Differences, 
ts.plot(diff(df$W_v3, lag=1))
ts.plot(df$W_v3)

##Log
W_v3_log <- log(xts_df$W_v3)
plot(xts_df$W_v3)
tm <-cbind(W_v3_log, xts_df$W_v3)
plot.ts(tm)

#Box-Cox Transformation
library(forecast)
plot.ts(BoxCox(df$W_v3, lambda = 0.5))
lambda <- BoxCox.lambda(df$W_v3) # try to calculate optimal lambda 
print(lambda)
plot.ts(BoxCox(df$W_v3, lambda = lambda))
plot.ts(diff(BoxCox(df$W_v3, lambda = 0.5))) # first difference of Box-Cox

#Percentage Change
W_v3_pch <- df$W_v3 / lag(df$W_v3) - 1
head(cbind(df$W_v3, W_v3_pch))

#First DIfferece of a logarithm
tsW_v3.X <- xts_df$W_v3[xts_df$W_v3!= 0] #omitt the 0 from the xtsdf Data Set
ln_tsW_v3.X<-log(tsW_v3.X) # Logarirthmieren der Variable ohne 0
plot(ln_tsW_v3.X) # Anschauen Log
dfln_tsW_v3.X<-diff(ln_tsW_v3.X)[-1] #Differenzeiren der Logaritmierten Variable #Frau Peter hat es so gemacht
plot(dfln_tsW_v3.X, main="Diff(Log(W_v3.X))") # Anschauen Diff(Log(W_v3.X))

#Moving Average
W_v3_ma <- ma(df$W_v3, order = 14)
ts.plot(W_v3_ma)


plot(density(na.omit(xts_df$S)))
lines(seq(0, 100, by=5), dnorm(seq(0, 100, by=5),
                               mean(xts_df$S), sd(xts_df$S)), col="blue")
