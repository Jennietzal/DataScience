
library(dplyr)
library(mechkar)
#source("../R/mechkar.R")

df <- read.csv("C:/Users/Xnes/DataScience/data/bank-full.csv",sep = ";")
head(df)


df$marital <- factor(df$marital)
df$education <- factor(df$education)
df$housing <- factor(df$housing)
df$loan <- factor(df$loan)
df$contact <- factor(df$contact)
df$poutcome <- factor(df$poutcome)
df$job <- factor(df$job)
df$category<-(df$y)
df$y<-NULL
View(df)

library(Hmisc)




####################
###   Functions  ###
####################

outlierMatrix <- function(data,threshold=1.5) {
    vn <- names(data)
    outdata <- data.frame(row1=1:nrow(data))
    for(v in vn) {
        if(is.numeric(data[[v]])) {
            outlow <- quantile(data[[v]],probs = 0.25,na.rm = T) 
            outhigh <- quantile(data[[v]],probs = 0.75, na.rm = T)
            irq_level <- (outhigh - outlow) * threshold
            outlow <- outlow - irq_level
            outhigh <- outhigh +  irq_level
            mv <- ifelse(data[[v]] < outlow | data[[v]] > outhigh, 1, 0)
            outdata[v] <- mv
        } else {
            mv <- rep(0,nrow(data))
        }
    }
    outdata$row1 <- NULL
    return(outdata)
}


missingMatrix <- function(data) {
    vn <- names(data)
    missdata <- data.frame(row1=1:nrow(data))
    for(v in vn) {
        mv <- ifelse(is.na(data[[v]]),1,0)
        missdata[v] <- mv
    }
    missdata$row1 <- NULL
    return(missdata)
}


############
## 1) Summarize the dataset
############
summary(df)

############
## 2) Analyze the data using statistical analysis (Table1)
############

Table1(data=df,x=names(df),y=df$category)

############
## 3) Explore the data using graphics (exploreData / Sweetviz)
############
exploreData(data=df,dir = "C:/Users/Xnes/DataScience/stats/report")

############
## 4) Create a correlation matrix 
############
rcorr(as.matrix(df[,c(1,6,10,12:15)]),type = "spearman")

############
## 5) Plot the correlation using a correlation plot
############
library(corrgram)
corrgram(df[,c(1,6,10,12:15)])

############
## 6) Which variables have outliers?
############


############
## 7) Create a missing matrix. Which variables have more missing values? Are there rows with many missings?
############

missingMatrix <- function(data) {
    vn <- names(data)
    missdata <- data.frame(row1=1:nrow(data))
    for(v in vn) {
        mv <- ifelse(is.na(data[[v]]),1,0)
        missdata[v] <- mv
    }
    missdata$row1 <- NULL
    return(missdata)
}

#############
## 8) Use logistic regression to check if the missingness is related to any other variable
#############


