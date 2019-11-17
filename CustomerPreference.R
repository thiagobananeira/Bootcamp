

##################################
# Predicting Customer Preference #
##################################


###########################
# Complete Responses Data #
###########################

setwd("C:/Users/thiag/Documents/datasets/")      # Sets work directory
Dataset <- read.csv("CompleteResponses.csv")     # Reads dataset
str(Dataset)                                     # Displays the structure of the data set

######################################
# Descriptive Analysis - First Steps #
######################################

summary(Dataset)                                             # Prints summary statistics

par(mfrow=c(3,3),mar=c(5.1,4.1,2.1,2.1),oma=c(0,0,4,0))      # Sets the plotting area  
lapply(names(Dataset), function(x) hist(                     # 
  Dataset[,x], main="", xlab=x))                             # Plots histograms
mtext("Histograms", line=0, side=3, outer=TRUE, cex=2)       #


par(mfrow=c(3,3),mar=c(5.1,4.1,2.1,2.1),oma=c(0,0,4,0))      # Sets the plotting area
lapply(names(Dataset), function(x) boxplot(                  #
  Dataset[,x], main="", xlab=x))                             # Plots boxplots
mtext("Boxplots", line=0, side=3, outer=TRUE, cex=2)         #

DatasetBrand0 <- Dataset[Dataset$brand==0,]                  # Creates data subsset
DatasetBrand1 <- Dataset[Dataset$brand==1,]                  # Creates data subset

par(mfrow=c(3,3))                                            # Sets the plotting area
lapply(names(DatasetBrand0), function(x) hist(               # 
  DatasetBrand0[,x], main="",xlab=x))                        # Plots histograms
mtext("Histograms - Brand Acer", line=0, side=3,             #
      outer=TRUE, cex=2)                                     # 


par(mfrow=c(3,3))                                            # Sets the plotting area
lapply(names(DatasetBrand1), function(x) hist(               # 
  DatasetBrand1[,x], main="", xlab=x))                       # Plots histograms
mtext("Histograms - Brand Sony", line=0, side=3,             #
      outer=TRUE, cex=2)                                     #

Dataset$brandf <- as.factor(Dataset$brand) 
levels(Dataset$brand) <- c("Acer", "Sony")

library(ggplot2)

ggplot(Dataset, aes(x=salary,fill=brandf))+
  geom_histogram(alpha=0.5,position="identity")

ggplot(Dataset, aes(x=age, y=salary, color=brandf))  + 
  geom_point() + ggtitle("") 

##########################################################
# Descriptive Analysis - Correlation matrix as a heatmap #
##########################################################

nums <- unlist(lapply(Dataset, is.numeric))                 # Creates logical statement vector for numerical variables
Dataset2 <- Dataset[,nums]                                  # Creates new dataset with numerical variables only

cormat <- round(cor(Dataset2),2)                            # Creates correlation matrix

reorder_cormat <- function(cormat){                         #
  dd <- as.dist((1-cormat)/2)                               #
  hc <- hclust(dd)                                          # Creates function to reorder the correlation matrix
  cormat <-cormat[hc$order, hc$order]                       #
}                                                           #

cormat <- reorder_cormat(cormat)                            # Reorders correlation matrix

get_upper_tri <- function(cormat){                          #      
  cormat[lower.tri(cormat)]<- NA                            # Creates function to retrieve upper triangle of the correlation matrix
  return(cormat)                                            #
}                                                           #

upper_tri <- get_upper_tri(cormat)                          # Retrieves upper triangle of the correlation matrix

library(reshape2)
melted_cormat <- melt(upper_tri, na.rm = TRUE)              # Melts the correlation matrix       

library(ggplot2)
ggheatmap <-ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+   #    
  geom_tile(color = "white")+                                              #
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",          #
                       midpoint = 0, limit = c(-1,1), space = "Lab",       #
                       name="Pearson\nCorrelation") +                      # Creates a ggheatmap
  theme_minimal()+                                                         #
  theme(axis.text.x = element_text(angle = 45, vjust = 1,                  #
                                   size = 12, hjust = 1))+                 #
  coord_fixed()                                                            #

ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +   # Adds the correlation coefficients on the graph
  theme(                                                                   #
    axis.title.x = element_blank(),                                        #
    axis.title.y = element_blank(),                                        #
    panel.grid.major = element_blank(),                                    # Removes axis labels, panel grids and background, and axis ticks
    panel.border = element_blank(),                                        #
    panel.background = element_blank(),                                    #
    axis.ticks = element_blank(),                                          #
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,                # Changes the position of the legend title
                               title.position = "top", title.hjust = 0.5))


#####################################
# Descriptive Analysis - Extensions #
#####################################

lmts <- range(DatasetBrand0$salary,DatasetBrand1$salary)      #
par(mfrow=c(1,2))                                             # 
boxplot(DatasetBrand0$salary,ylim=lmts, main="Acer")          # Plots boxplot for salary given brands
boxplot(DatasetBrand1$salary,ylim=lmts, main="Sony")          #
mtext("Boxplot of salary", line=0, side=3, outer=TRUE, cex=2) #


t.test(DatasetBrand0$salary,DatasetBrand1$salary)             # Runs t test for differences in means. 
                                                              # Result: significantly different means for salary

###############################
# Logistic regression results #
###############################

Dataset$brand <- as.numeric(Dataset$brand) 

mylogit1 <- glm(brand ~ salary + age, data = Dataset, family = "binomial")
summary(mylogit1)

salary_age<-Dataset$salary * Dataset$age                        # Creates salary x age
Dataset<-data.frame(Dataset,salary_age)                         # Adds salary x age to data set

mylogit2 <- glm(brand ~ salary + age + salary_age, data = Dataset, family = "binomial")
summary(mylogit2)

salary2<-Dataset$salary^2                                       # Creates salary squared
age2<-Dataset$age^2                                             # Creates age squared
Dataset<-data.frame(Dataset,salary2,age2)                       # Adds salary2 and age2 to data set

mylogit3 <- glm(brand ~ salary2 + age2 + salary + age + salary_age, data = Dataset, family = "binomial")
summary(mylogit3)

prediction_logit1<-predict(mylogit1,Dataset)
prediction_logit2<-predict(mylogit2,Dataset)
prediction_logit3<-predict(mylogit3,Dataset)

prob_logit1 <- exp(prediction_logit1)/(1+exp(prediction_logit1))     # 
prob_logit2 <- exp(prediction_logit2)/(1+exp(prediction_logit2))     # Converts log-odds to probability
prob_logit3 <- exp(prediction_logit3)/(1+exp(prediction_logit3))     #

prob_pred_logit1 <- prob_logit1  
prob_pred_logit2 <- prob_logit2
prob_pred_logit3 <- prob_logit3

for (i in 1:length(prob_logit1)){                                    # Creates brand predictions
  if (prob_logit1[[i]] <=.5 ) {
    prob_pred_logit1[[i]]<-0
  } else {
    prob_pred_logit1[[i]]<-1
  }
}

for (i in 1:length(prob_logit2)){                                    # Creates brand predictions
  if (prob_logit2[[i]] <=.5 ) {
    prob_pred_logit2[[i]]<-0
  } else {
    prob_pred_logit2[[i]]<-1
  }
}

for (i in 1:length(prob_logit3)){                                    # creating brand predictions
  if (prob_logit3[[i]] <=.5 ) {
    prob_pred_logit3[[i]]<-0
  } else {
    prob_pred_logit3[[i]]<-1
  }
}

resid_logit1 <- (Dataset$brand-prob_pred_logit1)^2
resid_logit2 <- (Dataset$brand-prob_pred_logit2)^2
resid_logit3 <- (Dataset$brand-prob_pred_logit3)^2

summary(resid_logit1)                                                # 53% of correctly labeled observations
summary(resid_logit2)                                                # 64% of correctly labeled observations
summary(resid_logit3)                                                # 75% of correctly labeled observations

#roc(Dataset$brand,prob_pred_logit3)

#############################
# Further residual analysis #
#############################

###############
# Using Caret #
###############

#install.packages("caret")
#install.packages("mlbench")
#install.packages("e1071")

library(caret)
library(mlbench)
library(e1071)

setwd("C:/Users/thiag/Documents/datasets/") 
Dataset <- read.csv("CompleteResponses.csv") 
str(Dataset) 
set.seed(123)

Dataset$brand <- as.factor(Dataset$brand)
levels(Dataset$brand) <- c("Acer", "Sony")

inTrain <- createDataPartition(y = Dataset$brand, p = .75,list = FALSE)
training <- Dataset[ inTrain,]
testing <- Dataset[-inTrain,]

# Reduced Dataset

Datasetred <- data.frame(Dataset$salary,Dataset$age,Dataset$brand)
names(Datasetred)<-c("salary","age","brand")

inTrainred <- createDataPartition(y = Datasetred$brand, p = .75,list = FALSE)
trainingred <- Datasetred[ inTrainred,]
testingred <- Datasetred[-inTrainred,]

##################
# Model 1 - C5.0 #
##################

ctrl1 <- trainControl(method = "repeatedcv",repeats = 3,                              # Defines controls to train()
                     classProbs = TRUE, summaryFunction = twoClassSummary)            #

model1Fit <- train(brand ~ ., data = training, method = "C5.0", tuneLength = 2,       #
                   trControl = ctrl1, metric = "ROC",                                 # Trains C5.0 model 
                   preProc = c("center", "scale"),verboseIter = TRUE)                 #         
model1Fit

######################
# Model 2 - C5.0 red #
######################

model2Fit <- train(brand ~ ., data = trainingred, method = "C5.0", tuneLength = 2,    #
                   trControl = ctrl1, metric = "ROC", preProc = c("center", "scale")  # Trains C5.0red model
                   , verboseIter = TRUE)                                               #
model2Fit

###########################
# Model 3 - Random Forest #
###########################

mtry <- 2
tunegrid <- expand.grid(.mtry=mtry)

model3Fit <- train(brand ~ ., data = training, method = "rf",
                   trControl = ctrl1, metric = "ROC", preProc = c("center", "scale")
                   , verboseIter = TRUE, tuneGrid=tunegrid)
model3Fit

###########################
# Model 4 - Random Forest #
###########################

mtry <- 4
tunegrid <- expand.grid(.mtry=mtry)

model4Fit <- train(brand ~ ., data = training, method = "rf",
                   trControl = ctrl1, metric = "ROC", preProc = c("center", "scale")
                   , verboseIter = TRUE, tuneGrid=tunegrid)
model4Fit

###########################
# Model 5 - Random Forest #
###########################

mtry <- 6
tunegrid <- expand.grid(.mtry=mtry)

model5Fit <- train(brand ~ ., data = training, method = "rf",
                   trControl = ctrl1, metric = "ROC", preProc = c("center", "scale")
                   , verboseIter = TRUE, tuneGrid=tunegrid)
model5Fit

###########################
# Model 6 - Random Forest #
###########################

mtry <- 8
tunegrid <- expand.grid(.mtry=mtry)

model6Fit <- train(brand ~ ., data = training, method = "rf",
                   trControl = ctrl1, metric = "ROC", preProc = c("center", "scale")
                   , verboseIter = TRUE, tuneGrid=tunegrid)
model6Fit

####################
# Model comparison #
####################

plsClasses1 <- predict(model1Fit, newdata = testing)           
plsClasses2 <- predict(model2Fit, newdata = testingred)
plsClasses3 <- predict(model3Fit, newdata = testing)
plsClasses4 <- predict(model4Fit, newdata = testing) 
plsClasses5 <- predict(model5Fit, newdata = testing) 
plsClasses6 <- predict(model6Fit, newdata = testing) 


confusionMatrix(data = plsClasses1, testing$brand) 
confusionMatrix(data = plsClasses2, testingred$brand)
confusionMatrix(data = plsClasses3, testing$brand) 
confusionMatrix(data = plsClasses4, testing$brand) 
confusionMatrix(data = plsClasses5, testing$brand) 
confusionMatrix(data = plsClasses6, testing$brand) 
                     
resamps <- resamples(list(C5.0 = model1Fit, C5.0red = model2Fit, RF2 = model3Fit, 
                          RF4 = model4Fit, RF6 = model5Fit, RF8 = model6Fit))
diffs <- diff(resamps)
summary(diffs)  

plot(model1Fit)
plot(model2Fit)

#####################
# Residual analysis #
#####################

modelClasses1 <- predict(model1Fit, newdata = testing)

testing1 <- data.frame(testing,modelClasses1)


rightwrong1 <- ifelse(testing1$brand =="Acer"& testing1$modelClasses1=="Acer","Acer-Acer",
                      ifelse(testing1$brand =="Acer"& testing1$modelClasses1=="Sony","Acer-Sony",
                             ifelse(testing1$brand =="Sony"& testing1$modelClasses1=="Sony","Sony-Sony", "Sony-Acer")))


testing1 <- data.frame(testing1,rightwrong1)

ggplot(testing1, aes(x=age, y=salary, color=ifelse(rightwrong1=="Acer-Acer" | 
                                                      rightwrong1=="Sony-Sony","right","")))  + 
  geom_point() + ggtitle("") + scale_color_discrete(name="Predictions")


################
# New dataset  #
################

Datasetinc <- read.csv("SurveyIncomplete.csv")  
Datasetinc$brand<-predict(model1Fit, newdata = Datasetinc)                      #Creates predictions for train set   

summary(Datasetinc)
str(Datasetinc)

ggplot(Datasetinc, aes(x=age, y=salary, color=brand)) + 
  geom_point() + ggtitle("")

ggplot(Dataset, aes(x=age, y=salary, color=brand)) + 
  geom_point() + ggtitle("")

# testing for differencs between results for complete and incomplete


Dataset <- read.csv("CompleteResponses.csv")  

fulldata <- rbind(Dataset,Datasetinc)
summary(fulldata)
str(fulldata)


