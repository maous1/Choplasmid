#' Title
#'
#' @param dataset
#' @param cost
#'
#' @return
#' @export
#'
#' @examples
SVM_Overfitting_cost= function(dataset, cost){
  set.seed(323)
  library(caTools)
  library(RWeka)
  library(e1071)
  library(dplyr)
  i=1
  acc = rep(0,length(cost))
  sen = rep(0,length(cost))
  spe = rep(0,length(cost))
  for (overfit in cost) {




    library(caret)
    folds = createFolds(dataset$prediction, k = 10)
    # in cv we are going to applying a created function to our 'folds'
    cv = lapply(folds, function(x) { # start of function
      # in the next two lines we will separate the Training set into it's 10 pieces
      training_fold = dataset[-x, ] # training fold =  training set minus (-) it's sub test fold
      test_fold = dataset[x, ] # here we describe the test fold individually
      training_fold[-length(dataset)] = scale(training_fold[-length(dataset)])

      test_fold[-length(training_fold)] = scale(test_fold[-length(training_fold)])
      # now apply (train) the classifer on the training_fold

      classifier = svm(formula = prediction ~ .,
                       data = training_fold,
                       type = 'C-classification',
                       kernel = 'radial',cost = overfit)
      # next step in the loop, we calculate the predictions and cm and we equate the accuracy
      # note we are training on training_fold and testing its accuracy on the test_fold
      y_pred = predict(classifier, newdata = test_fold[-length(training_fold)])
      cm = table(test_fold[, length(training_fold)], y_pred)
      accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
      sensibility = cm[1,1]/(cm[1,1]+cm[2,1])
      specificity = cm[2,2]/(cm[2,2]+cm[1,2])
      retour = c(accuracy,sensibility,specificity)
      return(retour)
    })
    cv = unlist(cv)
    for (j in 1:length(cv)) {
      if (j%%3 == 1) {
        accuracy[((j+2)/3)] = cv[i]
      }else if (i%%3 == 2){sensibility[((j+1)/3)] = cv[j]}
      else{specificity[(j/3)] = cv[j]}

    }
    if (i==1) {
      acc[i] = mean(accuracy)
      sen[i] = mean(sensibility)
      spe[i] = mean(specificity)
      retour = data.frame(acc ,sen,spe)
    }else{retour = retour%>% add_row(acc = mean(accuracy),mean(sensibility),mean(specificity))}

    print(overfit)
    i=i+1
  }
  return(retour)
}
