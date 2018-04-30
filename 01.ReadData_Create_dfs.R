#|| Daten in R laden und Objekttransformation ||#

#Install Packages
install.packages("TSA")
install.packages("vars")
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("readxl")

#Load Library
library(TSA)
library(vars)
library(ggplot2)
library(tidyverse)
library(readxl)

-----------------------------------------------------------------------------------------------------
#Datensatz einlesen aus Excel
df <- read_excel("final_v11.xlsx", 
                 sheet = "Final Dataframe", col_types = c("date", 
                                                          "numeric", "numeric", "numeric", 
                                                          "numeric", "numeric", "numeric"))


#Show first 6 rows and variable names of data set 
head(df)
names(df)

#Create a vector with normally distributed Values (ndv = normally distributed vector)
set.seed(20160420)
ndv <- rnorm(1000000)
plot(density(ndv))

#Data Frame df in Time Series Data Frame xts/zoo umwandeln (anders als ts)
library(xts)
df$Date_2 <- as.Date(df$Date)
names(df)
df<- df%>%select(-Date)
names(df)
xts_df <- xts(df[,-7], order.by=df$Date_2)
names(xts_df)

### Creating an XTS Object without NA's -> shortens Time-Period
xts_df.X <- na.omit(xts_df)

library("broom")
#Überführung der df Variablen in ts Variablen
###Problem er löscht die Daten von 02.01.2018-31.03.2018
ts_df<-data.frame(
  as.ts(df$W_v1, start=c(2012,3,31), end=c(2018,3,31), frequency=365),
  as.ts(df$W_v2, start=c(2012,3,31), end=c(2018,3,31), frequency=365),
  as.ts(df$W_v3, start=c(2012,3,31), end=c(2018,3,31), frequency=365),
  as.ts(df$S, start=c(2012,3,31), end=c(2018,3,31), frequency=365),
  as.ts(df$U, start=c(2012,3,31), end=c(2018,3,31), frequency=365),
  as.ts(df$P, start=c(2012,3,31), end=c(2018,3,31), frequency=365))
ts_df<-data.frame(ts_df, df$Date_2)
names(ts_df)
class(ts_df)

#Rename Variables
ts_df_names <- c("W_v1","W_v2","W_v3", "S", "U", "P", "Date")
names(ts_df) <- ts_df_names
names(ts_df)


####Creating First Differences Data Frames
##First Differences of the xts_df
diff_xts_df<-diff(xts_df)
class(diff_xts_df)
diff_xts_df<-diff_xts_df[-1,]

##First Differences of the ts_df
diff_ts_df<-data.frame(
  diff(ts_df$W_v1),
  diff(ts_df$W_v2),
  diff(ts_df$W_v3),
  diff(ts_df$S),
  diff(ts_df$U),
  diff(ts_df$P),
  ts_df$Date[-1])
class(diff_ts_df)
names(diff_ts_df)

ts_df_names <- c("W_v1","W_v2","W_v3", "S", "U", "P", "Date")
names(diff_ts_df) <- ts_df_names
names(diff_ts_df)

