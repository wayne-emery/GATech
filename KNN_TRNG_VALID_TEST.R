



#Install packages and load library
#install.packages("kknn")
#install.packages("caret")
#install.packages("caTools")
install.packages("ggplot2")

#Load data
CCdata <- data.frame(read.delim("credit_card_data-headers.txt"))

#Load library
suppressWarnings(suppressMessages(library(kknn)))
suppressWarnings(suppressMessages(library(caret)))
library(caTools)
library(ggplot2)

#SplitData
sample = sample.split(CCdata$A15, SplitRatio = .60)
train = subset(CCdata, sample == TRUE)

#Split leftover data into validation and test
leftover  = subset(CCdata, sample == FALSE)
sample2 = sample.split(leftover, SplitRatio = .50)
validation = subset(leftover, sample2 = TRUE)
test = subset(leftover, sample2 = FALSE)
dataTypes <- list(train,validation,test)


#Create function
get_Accuracy = function(K, dataType){
  rowCount <- nrow(dataType)
  
  predicted <- rep(0,rowCount) #create vector of zeros
  
  #loop through all the rows
  
  for (iter in 1:rowCount){
    
    model_knn = kknn(R1~A1+A2+A3+A8+A9+A10+A11+A12+A14+A15,
                     dataType[-iter,], #the data we use to predict 
                     dataType[iter,],  #the row we are predicting
                     k=K,         #the number of points we're using
                     distance = 2,
                     kernel = "optimal",
                     scale = TRUE)
    
    #Compare prediction with true value
    #print(fitted(model_knn))
    #print(as.integer(fitted(model_knn) + .5))
    predicted[iter] <- as.integer(fitted(model_knn) + .5) #put rounded value into vector
  }
  #calculate and return accuracy
  
  accuracy <- sum(predicted == dataType[,11]) / rowCount
  return(accuracy)
}

numKtoCheck <- 20  #change this to how many k's to check
for (dataType in dataTypes){
  
  accuracy <- rep(0,numKtoCheck)
  for (K in 1:numKtoCheck){
    accuracy[K] <- get_Accuracy(K, dataType)
  }
  
  
#plot(accuracy)
  
  
  k.values <- 1:numKtoCheck
  acc.df <- data.frame(accuracy,k.values)
  print(acc.df)
  
  ggplot(acc.df,aes(x=k.values,y=accuracy)) + geom_point()+ geom_line(lty="dotted",color='red')

}
