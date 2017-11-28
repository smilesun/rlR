#  use mlr to train on the replay memory
Surro.mlr= R6Class("Surro.mlr",
  inherit = Brain,
  public = list(
    train = function(X, tname) {
      train = makeClassifTask(data = do.call(rbind, data[1:200]), target = tname)
      test = do.call(rbind, data[201:1179])
## evaluation to predict optimal step ###
      lrn = makeLearner("classif.ranger")
      self$model = train(lrn, train)
    },

    pred = function(X) {
      preds = predict(self$model, newdata = X)
      preds
    }
    ),
  private = list(),
  active = list()
  )

