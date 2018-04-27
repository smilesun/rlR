#  use mlr to train on the replay memory
Surro.mlr = R6Class("Surro.mlr",
  inherit = Surrogate,
  public = list(
    initialize = function(actCnt, stateCnt) {
      self$actCnt = actCnt
      self$stateCnt = stateCnt
    },

    train = function(X_train, Y_train, acts) {
      library(mlr)
      acts = unlist(acts) + 1L
      res = lapply(1:nrow(Y_train), function(i) Y_train[i, acts[i]])
      targets = unlist(res)
      lrn = mlr::makeLearner("regr.ranger")
      df = as.data.frame(cbind(targets, acts, X_train))
      colnames(df)[1] = c("rlr.target")
      colnames(df)[2] = c("rlr.act")
      colnames(df)[3:(2 + self$stateCnt)] = paste("v", as.character(1:self$stateCnt), sep = "")
      task = mlr::makeRegrTask(data = df, target = "rlr.target")
      self$model = mlr::train(lrn, task)
    },

    insertAct = function(X,i) {
      df = as.data.frame(X)
      colnames(df) = paste("v", as.character(1:self$stateCnt), sep = "")
      df["rlr.act"] = rep(i, nrow(df))
      df
    },

    pred = function(X) {
      n = nrow(X)
      if (is.null(self$model)) return(matrix(rnorm(n*self$actCnt), nrow = n))
      preds = lapply(1:self$actCnt, function(i) predict(self$model, newdata = self$insertAct(X,i)))
      preds = as.data.frame(preds)
      df = cbind(preds)
      data.matrix(df)
    }
    ),
  private = list(),
  active = list()
  )

