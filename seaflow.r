install.packages("caret")
install.packages("rpart")
install.packages("tree")
install.packages("randomForest")
install.packages("e1071")
install.packages("ggplot2")
install.packages("lattice")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)})

##Step 1: Read and summarize the data###
##1. How many particles labeled "synecho" are in the file provided? 18146 ###
##2.What is the 3rd Quantile of the field fsc_small? 39184
####(the summary function computes this on your behalf)

setwd("/Users/apple/Desktop/coursera/data science at scale specialization/practical predictive analytics")
mydata = read.csv("seaflow_21min.csv")
#mydata
summary(mydata)


###Step 2: Split the data into test and training sets###
###3. What is the mean of the variable "time" for your training set? 341.6
library(caret)
set.seed(3456)
inTrain<-createDataPartition(mydata$pop,p=0.5,list=FALSE)
training<-mydata[inTrain,]
testing<-mydata[-inTrain,]
summary(training)
testing

###Step 3: Plot the data###
##4. In the plot of pe vs. chl_small, the particles labeled ultra should appear to be
###somewhat "mixed" with two other populations of particles. Which two populations?pico and nano
library(ggplot2)
#ggplot(mydata, aes(x=chl_small,y=pe))+geom_line(aes(color=pop))
ggplot(mydata, aes(x=chl_small,y=pe,color=pop))+geom_line()
ggplot(mydata, aes(x=chl_small,y=pe,color=pop))+geom_jitter()  #pop is the category


###Step 4: Train a decision tree.###
###5. Use print(model) to inspect your tree. Which populations, 
###if any, is your tree incapable of recognizing? (Which populations do not appear on any branch?) crypto
##6. What is the value of the threshold on the pe field learned in your model? 5006.5
###7. Based on your decision tree, which variables appear to 
###be most important in predicting the class population? pe, chl_small

library(rpart)
fol <- formula(pop ~ fsc_small + fsc_perp + fsc_big + pe + chl_big + chl_small)
model <- rpart(fol, method="class", data=training)
print(model)

library("partykit")
rgraph <- as.party(model)
plot(rgraph)

library("rpart.plot")
rpart.plot(model,branch=0,branch.type=2,type=1,extra=102,shadow.col="pink",box.col="gray",split.col="magenta")


###Step 5: Evaluate the decision tree on the test data.####
###8.How accurate was your decision tree on the test data? 0.8563.
predict_model=predict(model, newdata=testing)
predict_model
str(predict_model)

#nrow(predict_model)  #36171
pop_test=c()
pop_names=c("crypto","nano","pico","synecho","ultra")
for (i in 1: nrow(predict_model)){
  pop_test=c(pop_test,pop_names[which.max(predict_model[i,])])
}
result=as.vector(testing$pop)==pop_test
result
table(result)
##FALSE  TRUE 
##5196 30975 
##length(result)
accuracy=sum(result)/length(result)
accuracy  #0.856349
testing$pop

###another way##
predict_model2=predict(model, newdata=testing, type="class")
predict_model2
table(predict_model2, testing$pop)
confusionMatrix(predict_model2,testing$pop)
##Accuracy : 0.8563 


###Step 6: Build and evaluate a random forest.####
###9. What was the accuracy of your random forest model on the test data?   0.9197  
##10.After calling importance(model), you should be able to determine which two variables appear to be 
###most important in terms of the gini impurity measure. Which two are they?pe, chl_small
library(randomForest)
model2<-randomForest(fol, data=training,method="class")
predict_model2=predict(model2,newdata=testing,type="class")
table(predict_model2,testing$pop)
confusionMatrix(predict_model2,testing$pop)  #  Accuracy : 0.9197  



##A random forest can obtain another estimate of variable importance 
#based on the Gini impurity that we discussed in the lecture. 
#The function importance(model) prints the mean decrease in gini importance 
#for each variable. The higher the number, the more the gini impurity score 
#decreases by branching on this variable, indicating that the variable is more important.

importance(model2)
##pe, chl_small




####Step 7: Train a support vector machine model and compare results.#####
###11.What was the accuracy of your support vector machine model on the test data?  0.9192  
library(e1071)
model3<-svm(fol,data=training)
predict_model3=predict(model3,newdata=testing,type="class")
table(predict_model3,testing$pop)
confusionMatrix(predict_model3, testing$pop)



###Step 8: Construct confusion matrices#####
###12. Construct a confusion matrix for each of the three methods using the table function. 
###What appears to be the most common error the models make? ultra is mistaken for pico




####Step 9: Sanity check the data###
####13. The variables in the dataset were assumed to be continuous, 
####but one of them takes on only a few discrete values, suggesting a problem. 
####Which variable exhibits this problem? fsc_big
####14.After removing data associated with file_id 208, 
####what was the effect on the accuracy of your svm model? 0.9724-0.9192=0.0532

mydata
##The measurements in this dataset are all supposed to be continuous
#(fsc_small, fsc_perp, fsc_big, pe, chl_small, chl_big), but one is not.
#Using plots or R code, figure out which field is corrupted.

library(ggplot2)
ggplot(mydata, aes(x = time , y = fsc_big,color=pop ) )+ geom_jitter()
#dev.off()
ggplot(mydata, aes(x = time , y = fsc_small,color=pop ) )+ geom_jitter()

ggplot(mydata, aes(x = time , y = fsc_perp,color=pop ) )+ geom_jitter()

ggplot(mydata, aes(x = time , y = pe,color=pop ) )+ geom_jitter()

ggplot(mydata, aes(x = time , y = chl_small,color=pop ) )+ geom_jitter()

ggplot(mydata, aes(x = time , y = chl_big,color=pop ) )+ geom_jitter()

library(e1071)
mydata_subset<-subset(mydata,file_id!=208)
set.seed(3456)
inTrain<-createDataPartition(mydata_subset$pop, p=0.5, list=FALSE)
training<-mydata_subset[inTrain,]
testing<-mydata_subset[-inTrain,]
summary(training)

model_subset_svm<-svm(fol,data=training)
predict_subset=predict(model_subset_svm,newdata=testing,type= "class")
table(predict_subset, testing$pop)

confusionMatrix(predict_subset, testing$pop)


#10,12 not right
