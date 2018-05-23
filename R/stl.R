STList = R6::R6Class("STList",
  public = list(
    member = list(),
    add = function(value) {
      len = length(self$member)
      self$member[[len+1]] = value
     },
    save = function(filename) {
      temp = self$clone()
      save(temp, file = filename)
    }
    ),
  private = list(
    )
  )


STList$test = function() {
  st = STList$new()
  st$add(1)
  st$add(2)
  st$member
  st$save("hi.RData")
}
