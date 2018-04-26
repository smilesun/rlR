#  use mlr to train on the replay memory
Surro.mlr = R6Class("Surro.mlr",
  inherit = Surrogate,
  public = list(
    initialize = function(actCnt, stateCnt) {
      self$actCnt = actCnt
      self$stateCnt = stateCnt
    },

    train = function(X_train, Y_train) {
      if(nrow(X_train) < 100) 
        return(NULL)
      lrn = mlr::makeLearner("regr.ranger")
      df = as.data.frame(cbind(Y_train, X_train))
      colnames(df)[1] = "rlr.target"
      task = mlr::makeRegrTask(data = df, target = "rlr.target")
      self$model = mlr::train(lrn, task)
    },

    pred = function(X) {
      n = nrow(X)
      if (is.null(self$model)) return(matrix(c(rep(1,n), rep(0,n)), nrow = n))
      preds = predict(self$model, newdata = X)
      cbind(preds, 1 - preds)
    }
    ),
  private = list(),
  active = list()
  )

