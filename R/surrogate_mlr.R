#  use mlr to train on the replay memory
Surro.mlr = R6Class("Surro.mlr",
  inherit = Brain,
  public = list(
    initialize = function(actCnt, stateCnt, fun, ...) {
      self$actCnt = actCnt
      self$stateCnt = stateCnt
      self$createModel.fun = fun
    },

    train = function(X_train, Y_train) {
      lrn = makeLearner("regr.ranger")
      df = cbind(Y_train, X_train)
      colnames(df)[1] = "rlr.target"
      mlr::makeRegrTask(data = df, target = "rlr.target")
      self$model = train(lrn, task)
    },

    pred = function(X) {
      preds = predict(self$model, newdata = X)
      preds
    }
    ),
  private = list(),
  active = list()
  )

