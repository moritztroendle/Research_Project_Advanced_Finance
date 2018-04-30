#|| Read and Create Data ||#

#Install Packages
install.packages("TSA")
install.packages("vars")
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("readxl")
install.packages("MASS")
install.packages("dplyr")

#Load Library
library(TSA)
library(vars)
library(ggplot2)
library(tidyverse)
library(readxl)
library(dplyr)
library(xts)
library(broom)

-----------------------------------------------------------------------------------------------------
#1. Datensatz einlesen aus Excel
df <- read_excel("final_v11.xlsx", 
                 sheet = "Final Dataframe", col_types = c("date", 
                                                          "numeric", "numeric", "numeric", 
                                                          "numeric", "numeric", "numeric"))


#1.1 Show first 6 rows and variable names of data set 
  head(df)
  names(df)

#2. Create a vector with normally distributed Values (ndv = normally distributed vector)
set.seed(20160420)
ndv <- rnorm(1000000)
  plot(density(ndv))

#3. Transfrom Data Frame [df] into Time Series Data Frame with xts/zoo [xts_df] (diffrent form ts.object)
  #Notize an Nora: hier habe ich select in ind dplyr::select geändert und einfach das Datum, dass wir
  #schon hatten als Bezug für das xts object genommen, so brauchen wir Date_2 gar nicht
df$Date <- as.Date(df$Date)
  names(df)
  
xts_df <- xts(df[,-1], order.by=df$Date)
  names(df)
  names(xts_df)

#4. Transfrom Data Frame [df] into Time Series Objects [ts_df] 
    #ATTENTION: Data from 02/01/2018 to 31/03/2018 is deleted -> adjustment needed
ts_df <- data.frame(
      as.ts(df$W_v1, start=c(2012,3,31), end=c(2018,3,31), frequency=365),
      as.ts(df$W_v2, start=c(2012,3,31), end=c(2018,3,31), frequency=365),
      as.ts(df$W_v3, start=c(2012,3,31), end=c(2018,3,31), frequency=365),
      as.ts(df$S, start=c(2012,3,31), end=c(2018,3,31), frequency=365),
      as.ts(df$U, start=c(2012,3,31), end=c(2018,3,31), frequency=365),
      as.ts(df$P, start=c(2012,3,31), end=c(2018,3,31), frequency=365))

ts_df <- data.frame(ts_df, df$Date)
  names(ts_df)
  class(ts_df)

#4.1 Rename Variables
ts_df_names <- c("W_v1","W_v2","W_v3", "S", "U", "P", "Date")
names(ts_df) <- ts_df_names
  names(ts_df)


#5. Creating First Differences
#5.1 For xts Object [xts_df]
diff_xts_df<-diff(xts_df)
  class(diff_xts_df)
diff_xts_df<-diff_xts_df[-1,]

#5.2 For ts Object [ts_df]
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

#6. Viewing all Data Frames and Objects for visual Inspection
View(df)
View(xts_df)
View(ts_df)
View(diff_xts_df)
View(diff_ts_df)