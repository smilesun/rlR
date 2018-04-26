#  use mlr to train on the replay memory
Surro.mlr = R6Class("Surro.mlr",
  inherit = Surrogate,
  public = list(
    initialize = function(actCnt, stateCnt) {
      self$actCnt = actCnt
      self$stateCnt = stateCnt
    },

    train = function(X_train, Y_train) {
      if (nrow(X_train) < 50) 
        return(NULL)
      lrn = mlr::makeLearner("regr.ranger")
      df = as.data.frame(cbind(Y_train, X_train))
      colnames(df)[1] = c("rlr.target")
      df[2: self$actCnt] = NULL
      task = mlr::makeRegrTask(data = df, target = "rlr.target")
      browser()
      self$model = mlr::train(lrn, task)
    },

    pred = function(X) {
      n = nrow(X)
      h = rnorm(n, mean = 0.5, sd = 0.1)
      if (is.null(self$model)) return(matrix(c(h, 1-h), nrow = n))
      preds = predict(self$model, newdata = X)
      cbind(preds, 1 - preds)
    }
    ),
  private = list(),
  active = list()
  )

