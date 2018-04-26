#  use mlr to train on the replay memory
Surro.mlr = R6Class("Surro.mlr",
  inherit = Surrogate,
  public = list(
    initialize = function(actCnt, stateCnt) {
      self$actCnt = actCnt
      self$stateCnt = stateCnt
    },

    train = function(X_train, Y_train) {
      library(mlr)
      if (nrow(X_train) < 50) 
        return(NULL)
      lrn = mlr::makeLearner("regr.ranger")
      df = as.data.frame(cbind(Y_train, X_train))
      colnames(df)[1] = c("rlr.target")
      df[2: self$actCnt] = NULL
      colnames(df)[2:(1+self$stateCnt)] = paste("v", as.character(1:self$stateCnt), sep = "")
      task = mlr::makeRegrTask(data = df, target = "rlr.target")
      self$model = mlr::train(lrn, task)
    },

    pred = function(X) {
      n = nrow(X)
      h = rnorm(n, mean = 0.5, sd = 0.1)
      if (is.null(self$model)) return(matrix(c(h, 1-h), nrow = n))
      df = as.data.frame(X)
      colnames(df) = paste("v", as.character(1:self$stateCnt), sep = "")
      preds = predict(self$model, newdata =  df)
      preds = as.data.frame(preds)
      df = cbind(preds, 1.0 - preds)
      data.matrix(df)
    }
    ),
  private = list(),
  active = list()
  )

