happy <- read.csv("C:/Users/Xnes/DataScience/data/SomervilleHappinessSurvey2015.csv")
summary(happy)
mod1<- glm(D~.,data=happy,family = "binomial")
summary(mod1)
exp(mod1$coefficients)

mod1b<- step(mod1,direction = "forward")
summary(mod1b)

mod1c<- step(mod1,direction = "backward")
summary(mod1c)

mod1a<- step(mod1,direction = "both")
summary(mod1a)
