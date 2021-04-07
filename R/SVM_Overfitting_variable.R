
#' Title
#'
#' @param dataset
#' @param L
#'
#' @return
#' @export
#'
#' @examples
SVM_Overfitting_variable = function(dataset, L = 5){

  ###########calcul des variables
  set.seed(323)
  library(caTools)
  library(RWeka)
  library(e1071)
  nvariable =seq(1, length(dataset), by = L)
  moyenne = rep(0,length(nvariable))
  for (overfit in nvariable) {




    library(caret)
    folds = createFolds(dataset$prediction, k = 10)
    # in cv we are going to applying a created function to our 'folds'
    cv = lapply(folds, function(x) { # start of function
      # in the next two lines we will separate the Training set into it's 10 pieces
      training_fold = dataset[-x, ] # training fold =  training set minus (-) it's sub test fold
      test_fold = dataset[x, ] # here we describe the test fold individually
      training_fold[-length(dataset)] = scale(training_fold[-length(dataset)])
      pred = training_fold[length(dataset)]

      pvaleur= rep(0,(length(dataset)))
      for (j in 1:(dim(training_fold)[2]-1)) {
        pvaleur[j]=pairwise.t.test(training_fold[,j], training_fold$prediction)$p.value
      }
      names(pvaleur)= colnames(training_fold)[1:(length(dataset)-1)]

      pvaleur= head(sort(pvaleur), overfit)
      names(pvaleur)

      training_fold=training_fold[names(pvaleur)]
      training_fold[length(training_fold)+1]=pred
      test_fold=test_fold[colnames(training_fold)]

      test_fold[-length(training_fold)] = scale(test_fold[-length(training_fold)])
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
    moyenne[(overfit+10)/20] = mean(unlist(cv))
    print(overfit)
  }
  return(moyenne)
}



