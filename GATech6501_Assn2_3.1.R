library(kknn)
library(caret)
library(caTools)

CCdata <- data.frame(read.delim("credit_card_data-headers.txt"))
CCdata$R1 <- as.factor(CCdata$R1)

set.seed(101)

sample <- sample.split(CCdata$R1, SplitRatio = 4/5)
training <- subset(CCdata, sample == TRUE)
test <- subset(CCdata, sample == FALSE)

# print(test)
# print("Should have printed")

ctrl <- trainControl(method="repeatedcv",repeats = 3)  
knnFit <- train(R1 ~ ., data = training, method = "kknn", 
                trControl = ctrl, preProcess = c("center","scale"))
knnFit

knnFit$finalModel

test_pred <- predict(knnFit, newdata = test)
#test_pred

confusionMatrix(test_pred, test$R1 )   