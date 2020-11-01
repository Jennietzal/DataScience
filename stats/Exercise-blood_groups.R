#########################
### ABO Blood Groups
#########################

# In this notebook we will analyze the world distribution of the different blood groups.
# 
# On the surface of the red blood cells (also known as erythrocytes) are expressed molecules 
# that are determined on the genetic code of individuals. Among those molecules are two 
# important ones, that determinate the blood type. The implication of those types is that 
# individuals needing a blood transfussion can not ever receive blood from any other 
# individual. Many people have to receive only the same type they have. Also, pregnant 
# women that their babies have an incompatible blood type with their mothers are at risk of 
# abortion or can generate a dangerous reaction on the mother, that may even cause the risk 
# of death on the mother. Thus, the importance of testing the blood type on all the 
# population.
# 
# There are four groups that are responsible of the blood compatibility:
# 
# -Group O: Is the most common blood group. People with this blood group can recieve blood 
# only from the same type.
# -Group A: Is the second most frequently found group. People with this group can recieve 
# from A and O groups.
# -Group B: Is the third type in frequency. People with this group can recieve from B and O 
# groups.
# -Group AB: Is the least type in frequency. People with this group can recieve blood from
# any type (A,B, and O).
# 
# Another molecule present in the red blood cells and which determines the compatibility of 
# the blood is known as the Rhesus D factor . This factor was discovered in monkies (from 
# here the name). When this factor is present we say that the blood type is Rh-positive (Rh+). \
# When it is absent is considered Rh-negative (Rh-).
# 
# In summary, the combination of the ABO types and the Rhesus D factor results in Eight 
# different possible blood types: O+, A+, B+, AB+, O-, A-, B-, AB-.
# 
# In the present dataset we have the distribution of the eight blood types by country. 
# We will analyze this data using the statistical knowledge we have learned.


if(!require(ggfortify)) install.packages("ggfortify")
library(dplyr)

path <- "C:/Users/Xnes/DataScience/data/blood_groups_world_distribution.csv"
abo <- read.csv(paste(path,sep=""))
str(abo)
names(abo) <- c("Country","Population","O+","A+","B+","AB+","O-","A-","B-","AB-")
View(abo)
summary(abo)
for(v in names(abo)) {
  if(is.character(abo[[v]])==TRUE) {
    abo[[v]] <- factor(abo[[v]])
  }
}

library(mechkar)

##############################################
# Please Answer to the following questions:
##############################################

#  1. Which is the most common blood type: O+ or O-?

exploreData(data = abo, dir = "C:/Users/Xnes/DataScience/stats/report-bloodgroups")
#O+ most common
#  2. On how many countries the most common blood type is A?
#79
abo$A<-abo$`A+`+abo$`A-`

length(which(abo$A>0.26))
abo%>% filter(abo$A>0.26)%>% select(Country,A)
#  3. Show the five countries with the higher percentage of AB types.

abo$AB<-abo$`AB+`+abo$`AB-`
plot(abo$AB~abo$Country)
abo%>%filter(AB>0.09)%>% select(Country,AB)

#  4. How many people (and which percent of the total world population) has a negative Rh?
# 397,137,349 which is 397M aprox and 5.98% almost 6% of the world
abo$Rhneg<-abo$`O-`+abo$`A-`+abo$`B-`+abo$`AB-`
TotalPop<-abo$Population %>% sum()
abo$Rhnegpop<-abo$Population * abo$Rhneg

sum(abo$Rhnegpop)/TotalPop

#  5. Is there a correlation between the distribution of Israel and the US?
#I deleted the population row to see the correlation better
#Yes p-value is less then 0.05 and cor almost 1
df2 <- data.frame(t(abo[-1:-2]))
colnames(df2) <- abo[,1]
rownames(df2)<-c("O+","A+","B+","AB+","O-","A-","B-","AB-")

for(v in names(abo)) {
  if(is.character(abo[[v]])==TRUE) {
    abo[[v]] <- factor(abo[[v]])
  }
}
View(df2)
plot(df2$Israel~ df2$`United States`)
cor.test(df2$Israel,df2$`United States`)


#  6. Is there a statistical significant difference between the distribution of Israel and Russia?
cor.test(df2$Israel,df2$Russia)
plot(df2$Israel~df2$Russia)

#ttest checking if there difference in mean
t.test(df2$Israel,df2$Russia,paired = TRUE)
mean(df2$Russia)
#No difference

#Ftest checking if there difference in Var
var.test(df2$Israel,df2$Russia,alternative  = "two.sided")
#pvalue is greater then 0.05 it means there is no difference between 2 countries

#  7. Use different clustering techniques to classify the distribution of the eight blood cell types.
#Which cluster give a more logical segmentation?


#### Note:
#  Use the NbClust package to calculate the optimal number of clusters. k=3
#Also use plots coloring the groups with the resulting clusters.
#You can use the autoplot function (ggfortify package) to see the cluster distribution.
#Pass the Country values as rownames so you can see each country on the graphs.
library(NbClust)
df1 <- data.frame(abo[,-1])
colnames(df1)<- c("Population","O+","A+","B+","AB+","O-","A-","B-","AB-")
rownames(df1)<-abo[,1]
View(df1)

nb<- NbClust(df1[,2:9],distance = "euclidean",min.nc = 2,max.nc = 8,method = "complete")

kmod <- kmeans(as.matrix(df1[,2:9]),centers=4)
table(kmod$cluster)
kmod$centers
ggfortify::ggfreqplot(df1)

hcmod <- hclust(d = dist(df1[,2:9],method ="canberra" ))
cluster_hc <- cutree(hcmod, 3)
plot(hcmod)
table(cluster_hc)


#  8. Analyze the dataset using Principal Component Analysis (PCA).











