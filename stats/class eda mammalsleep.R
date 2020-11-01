library(readr)
summary(mammalsleep)

mammalsleep$species<- factor(mammalsleep$species)
mammalsleep$odi<- factor(mammalsleep$odi)
mammalsleep$sei<- factor(mammalsleep$sei)
mammalsleep$pi<- factor(mammalsleep$pi)

# correlation pearson and spearman and cor.test gives as p-value
cor(mammalsleep$bw,mammalsleep$brw)
cor(mammalsleep$bw,mammalsleep$brw, method = "spearman")
cor.test(mammalsleep$bw,mammalsleep$brw,method = "spearman",na.rm=T)
cor.test(mammalsleep$bw,mammalsleep$sws,method = "spearman",na.rm=T)

# corelation mattrix
library(Hmisc)
rcorr(as.matrix(mammalsleep[,3:9]),type = "spearman")

#difference between 2 variables Norm Distribution
t.test(mammalsleep$ts, mammalsleep$ps,paired = TRUE)

#?????????? ???????????? 
mammalsleep$highrisk <- ifelse(mammalsleep$odi==4 | mammalsleep$odi==5,1,0)
table(mammalsleep$highrisk)
 #Difference between 2 groups not equal 43 vs.19
library(dplyr)
low <- mammalsleep %>% filter(highrisk==0) %>% select(mls)
high <- mammalsleep %>% filter(highrisk==1)  %>% select(mls)

t.test(low$mls,high$mls,paired = FALSE)
#For Categorical Variables

chisq.test(mammalsleep$odi,mammalsleep$sei)
table(odi=mammalsleep$odi,sei=mammalsleep$sei)
#Anova for 3 or more Groups of Variables. Relationship Significant or not


#Visualization
#Table when you have numeric Var (0 or 1) counts
pie(table(mammalsleep$highrisk),labels = c("Low","High"), main = "Pie Chart Of Highrisk",radius = 1)

barplot(table(mammalsleep$highrisk))

library(ggplot2)
ggplot(data= mammalsleep)+ geom_bar(aes(x=highrisk))
ggplot(data= mammalsleep)+ 
  geom_bar(aes(x=odi,group=highrisk,colour=factor(highrisk)))
#when you have index you can see Out layer
plot(mammalsleep$bw)
plot(mammalsleep$bw, ylim = c(0,2000))
plot(mammalsleep$gt)
lines(mammalsleep$gt)

plot(mammalsleep$bw ~ mammalsleep$brw, xlim= c(0,800), ylim=c(0,800))
plot(log(mammalsleep$bw+1) ~ log(mammalsleep$brw+1))

#Looks like Normal Distribution
hist(mammalsleep$ts)
boxplot(mammalsleep$ts)

hist(mammalsleep$bw, breaks = 40)
hist(log(mammalsleep$bw)) #no+1 because no 0 values in bw
boxplot(mammalsleep$bw)
boxplot(log(mammalsleep$bw)~ mammalsleep$odi)

#to see out layers
a1<- boxplot(mammalsleep$bw)
a1$out

#Distribution the same Variable by grouping
ggplot(data = mammalsleep)+ geom_density(aes(x=log(bw), group=odi, colour=odi))

#Correlation       
pairs(mammalsleep[,3:13])

library(corrgram)
corrgram(mammalsleep[,3:13],upper.panel = panel.pie)

mcor<- rcorr(as.matrix(mammalsleep[,3:9],type = "spearman"))
mcor$r
heatmap(mcor$r)
heatmap(mcor$P) #we want the white squares

#there is a site to show us different plot options Quik-R

