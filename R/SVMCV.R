#' Title
#'
#' @param dataset
#'
#' @return
#' @export
#'
#' @examples
SVMCV = function(dataset){
  set.seed(323)
  library(caTools)
  library(RWeka)
  library(e1071)
  library(caret)


  folds = createFolds(dataset$prediction, k = 10)
  # in cv we are going to applying a created function to our 'folds'
  cv = lapply(folds, function(x) { # start of function
    # in the next two lines we will separate the Training set into it's 10 pieces
    training_fold = dataset[-x, ] # training fold =  training set minus (-) it's sub test fold
    test_fold = dataset[x, ] # here we describe the test fold individually

    # now apply (train) the classifer on the training_fold

    classifier = svm(formula = prediction ~ .,
                     data = training_fold,
                     type = 'C-classification',
                     kernel = 'radial')
    # next step in the loop, we calculate the predictions and cm and we equate the accuracy
    # note we are training on training_fold and testing its accuracy on the test_fold
    y_pred = predict(classifier, newdata = test_fold[-length(training_fold)])
    cm = table(test_fold[, length(training_fold)], y_pred)
    accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
    return(accuracy)
  })
  return(unlist(cv))
}
