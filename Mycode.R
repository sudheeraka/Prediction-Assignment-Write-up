setwd("D:/accademic/code")
#install.packages("doParallel")
#install.packages("randomForest")
#install.packages("e1071")
suppressWarnings(suppressMessages(library(caret)))
suppressWarnings(suppressMessages(library(randomForest)))
suppressWarnings(suppressMessages(library(e1071)))
set.seed(1603)

trainingFilename   <- 'pml-training.csv'
quizFilename       <- 'pml-testing.csv'
trainingUrl        <- 'https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv'
quizUrl            <- 'https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv'

# download.file(trainingUrl, trainingFilename)
# download.file(quizUrl,quizFilename)

training.df     <-read.csv(trainingFilename, na.strings=c("NA","","#DIV/0!"))
training.df     <-training.df[,colSums(is.na(training.df)) == 0]
dim(training.df) #;head(training.df,3)

quiz.df         <-read.csv(quizFilename , na.strings=c("NA", "", "#DIV/0!"))
quiz.df         <-quiz.df[,colSums(is.na(quiz.df)) == 0]
dim(quiz.df) #;head(quiz.df,3)

Training.df   <-training.df[,-c(1:7)]
Quiz.df <-quiz.df[,-c(1:7)]
dim(Training.df)

Training.nzv<-nzv(Training.df[,-ncol(Training.df)],saveMetrics=TRUE)
rownames(Training.nzv)


dim(Training.nzv)[1]

inTrain     <- createDataPartition(Training.df$classe, p = 0.6, list = FALSE)
inTraining  <- Training.df[inTrain,]
inTest      <- Training.df[-inTrain,]
dim(inTraining);dim(inTest)


myModelFilename <- "myModel.RData"
if (!file.exists(myModelFilename)) {

    # Parallel cores  
    #require(parallel)
    library(doParallel)
    ncores <- makeCluster(detectCores() - 1)
    registerDoParallel(cores=ncores)
    getDoParWorkers() # 3    
    
    # use Random Forest method with Cross Validation, 4 folds
    myModel <- train(classe ~ .
                , data = inTraining
                , method = "rf"
                , metric = "Accuracy"  # categorical outcome variable so choose accuracy
                , preProcess=c("center", "scale") # attempt to improve accuracy by normalising
                , trControl=trainControl(method = "cv"
                                        , number = 4 # folds of the training data
                                        , p= 0.60
                                        , allowParallel = TRUE 
#                                       , seeds=NA # don't let workers set seed 
                                        )
                )

    save(myModel, file = "myModel.RData")
    # 3:42 .. 3:49 without preProcess
    # 3:51 .. 3:58 with preProcess
    stopCluster(ncores)
} else {
    # Use cached model  
    load(file = myModelFilename, verbose = TRUE)
}

print(myModel, digits=4)

predTest <- predict(myModel, newdata=inTest)


predTest <- predict(myModel, newdata=inTest)
myModel$finalModel
varImp(myModel)

#Quiz_answers
print(predict(myModel, newdata=Quiz.df))


