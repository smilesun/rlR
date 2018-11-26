#' @importFrom magrittr %>% %<>%

PerformancePlots = R6::R6Class(
  "PerformancePlots",
  inherit = Performance,
  public = list(

    plot_2d_weights = function(layer = 1L, bias = FALSE) {

      #' @description (interactive) 2d plot of the neural network weights
      #' @param layer Integer - Index of the layer to visualize (starting at "1", default "1")
      #' @param bias Bool - Should the bias weights of this layer be visualized instead? (default "FALSE")
      #' @return Plotly object
      #' @export

      # some data manipulation for successful plotting
      weight_index = layer * 2L + bias - 1L
      s_weights    = self$agent$brain$getWeights()
      dim_weights  = dim(s_weights[[weight_index]])

      weights_data = {
        if (length(dim_weights) == 2)
          cbind(
            expand.grid(1:dim_weights[1], 1:dim_weights[2]),
            round(c(s_weights[[weight_index]]), 4)
          )
        else
          cbind(
            1:dim_weights,
            1,
            round(c(s_weights[[weight_index]]), 4)
          )
      } %>%
        as.data.frame()

      names(weights_data) = c("col_index", "row_index", "value")

      # define the interactive plot
      weights_data %>%
        plotly::plot_ly(
          x     = ~ col_index,
          y     = ~ row_index,
          color = ~ value,
          mode  = "markers",
          size  = I(10),
          type  = "scatter"
        ) %>%
        plotly::layout(
          showlegend = FALSE,
          xaxis = list(title = "Column Index", titlefont = list(size = 18)),
          yaxis = list(title = "Row Index",    titlefont = list(size = 18))
        ) %>%
        plotly::add_trace(
          text = ~ value
        )
    },


    plot_3d_weights = function(layer = 1L, bias = FALSE) {

      #' @description Interactive 3d plot of the neural network weights
      #' @param layer Integer - Index of the layer to visualize (starting at "1", default "1")
      #' @param bias Bool - Should the bias weights of this layer be visualized instead? (default "FALSE")
      #' @return Plotly object
      #' @export

      # some data manipulation for successful plotting
      weight_index = layer * 2L + bias - 1L
      s_weights    = self$agent$brain$getWeights()
      dim_weights  = dim(s_weights[[weight_index]])

      weights_data = {
        if (length(dim_weights) == 2)
          cbind(
            expand.grid(1:dim_weights[1], 1:dim_weights[2]),
            round(c(s_weights[[weight_index]]), 4)
          )
        else
          cbind(
            1:dim_weights,
            1,
            round(c(s_weights[[weight_index]]), 4)
          )
      } %>%
        as.data.frame()
      names(weights_data) = c("col_index", "row_index", "value")

      # define the interactive plot
      weights_data %>%
        plotly::plot_ly(
          x = ~ col_index,
          y = ~ row_index,
          z = ~ value,
          mode = "markers",
          color = ~ value,
          size  = I(5),
          type  = "scatter3d"
        ) %>%
        plotly::layout(
          showlegend = TRUE,
          xaxis = list(title = "Column Index", titlefont = list(size = 25)),
          yaxis = list(title = "Row Index",    titlefont = list(size = 25)),
          zaxis = list(title = "Weight Value", titlefont = list(size = 25))
        )
    },


    plot_2d_action_value = function(x_axis = 1L, y_axis = 1L, input = NULL, iteration = NULL) {

      #' @description Interactive 2d plot of the action value function. The points are actual values of the last batch,
      #'   the line is the prediction based on the trained model.
      #' @param x_axis Integer - State dimension to use as x axis in the plot (default "1")
      #' @param y_axis Integer - Action value dimension to use as y axis in the plot (default "1")
      #' @param input Numeric vector - Fixed values of all state dimensions to calculate the predictions for.
      #'   The entry corresponding to the chosen "x_axis" is ignored. If "NULL" then a vector with only 0s is used. (default "NULL")
      #' @param iteration Integer - Training iteration of which its model should be used for the predictions.
      #'   This requires the models to be stored during the training. If "NULL" then the most recent model will be used. (default "NULL")
      #' @return Plotly object
      #' @export

      # TODO: instead of only zeros, use mean of each state dimension
      if (is.null(input))
        input = rep(0, ncol(self$agent$replay.x))

      if (is.null(iteration))
        iteration = length(self$list_models)

      # set up data point generation
      state_space_l = as.list(input)

      min_s = min(self$agent$replay.x[, x_axis])
      max_s = max(self$agent$replay.x[, x_axis])
      # create a sequence from min to max (of the chosen state dimension) with 10 steps
      state_space_l[[x_axis]] = seq(
        from = min_s,
        to   = max_s,
        by   = (max_s - min_s) / 9
      )
      # create data matrix holding artificial data for the predictions
      state_space = expand.grid(state_space_l) %>% as.matrix

      # calculate predictions based on the created data points
      predictions = array(state_space, dim = c(nrow(state_space), ncol(state_space))) %>%
        self$list_models[[iteration]]$pred()

      state_space = cbind(state_space, predictions) %>% as.data.frame()
      names(state_space) = c(
        paste0("State_Dim_",     1:ncol(self$agent$replay.x)),
        paste0("ActionVal_Dim_", 1:ncol(self$agent$replay.y))
      )

      minibatch = cbind(self$agent$replay.x, self$agent$replay.y) %>%
        data.frame()

      names(minibatch) = c(
        paste0("State_Dim_",     1:ncol(self$agent$replay.x)),
        paste0("ActionVal_Dim_", 1:ncol(self$agent$replay.y))
      )

      # TODO: reimplement color and size argument
      minibatch %>%
        plotly::plot_ly(
          x = ~ get(paste0("State_Dim_",     x_axis)),
          y = ~ get(paste0("ActionVal_Dim_", y_axis)),
          mode = "markers",
          color = I("black"), #if (input$color_2d == "-") I("black") else ~ get(input$color_2d),
          size  = I(8),       #if (input$size_2d  == "-") I(8)       else ~ get(input$size_2d),
          name  = "Unfiltered",
          type  = "scatter"
        ) %>%
        plotly::layout(
          showlegend = TRUE,
          xaxis = list(title = paste0("State_Dim_",     x_axis), titlefont = list(size = 18)),
          yaxis = list(title = paste0("ActionVal_Dim_", y_axis), titlefont = list(size = 18))
        ) %>%
        plotly::add_trace(
          data = state_space,
          x    = ~ get(paste0("State_Dim_",     x_axis)),
          y    = ~ get(paste0("ActionVal_Dim_", y_axis)),
          size = I(5),
          name = 'trace 1',
          mode = 'lines',
          color = "#e6550d"
        )
    },


    plot_3d_action_value = function(
      x_axis    = 1L,
      y_axis    = 2L,
      z_axis    = 1L,
      input     = NULL,
      iteration = NULL,
      showscale = TRUE
    ) {

      #' @description Interactive 3d plot of the action value function. The points are actual values of the last batch,
      #'   the line is the prediction based on the trained model.
      #' @param x_axis Integer - State dimension to use as x axis in the plot (default "1")
      #' @param y_axis Integer - State dimension to use as y axis in the plot (default "2")
      #' @param z_axis Integer - Action value dimension to use as z axis in the plot (default "1")
      #' @param input Numeric vector - Fixed values of all state dimensions to calculate the predictions for.
      #'   The entry corresponding to the chosen "x_axis" and "y_axis" is ignored. If "NULL" then a vector with only 0s is used. (default "NULL")
      #' @param iteration Integer - Training iteration of which its model should be used for the predictions.
      #'   This requires the models to be stored during the training. If "NULL" then the most recent model will be used. (default "NULL")
      #' @return Plotly object
      #' @export

      # TODO: instead of only zeros, use mean of each state dimension
      if (is.null(input))
        input = rep(0, ncol(self$agent$replay.x))

      if (is.null(iteration))
        iteration = length(self$list_models)

      # set up data point generation
      state_space_l = as.list(input)

      min_sx = min(self$agent$replay.x[, x_axis])
      max_sx = max(self$agent$replay.x[, x_axis])
      min_sy = min(self$agent$replay.x[, y_axis])
      max_sy = max(self$agent$replay.x[, y_axis])

      # create a sequence from min to max (of the chosen state dimension) with 10 steps
      state_space_l[[x_axis]] = seq(
        from = min_sx,
        to   = max_sx,
        by   = (max_sx - min_sx) / 9
      )
      state_space_l[[y_axis]] = seq(
        from = min_sy,
        to   = max_sy,
        by   = (max_sy - min_sy) / 9
      )

      # create data matrix holding artificial data for the predictions
      state_space = expand.grid(state_space_l) %>% as.matrix

      # calculate predictions based on the created data points
      predictions = array(state_space, dim = c(nrow(state_space), ncol(state_space))) %>%
        self$list_models[[iteration]]$pred()

      state_space = cbind(state_space, predictions) %>% as.data.frame()
      names(state_space) = c(
        paste0("State_Dim_",     1:ncol(self$agent$replay.x)),
        paste0("ActionVal_Dim_", 1:ncol(self$agent$replay.y))
      )

      # create helper function for formatting the data points plus their predictions
      helper = function(df) {
        result = list(
          x = unique(df[[x_axis]]),
          y = unique(df[[y_axis]])
        )

        for (i in 1:ncol(self$agent$replay.y))
          result[[paste0("ActionVal_Dim_", i)]] =
            matrix(df[[length(df)-ncol(self$agent$replay.y)+i]], nrow = 100, ncol = 100, byrow = TRUE)

        return(result)
      }
      state_space %<>% helper

      minibatch = cbind(self$agent$replay.x, self$agent$replay.y) %>%
        data.frame()

      names(minibatch) = c(
        paste0("State_Dim_",     1:ncol(self$agent$replay.x)),
        paste0("ActionVal_Dim_", 1:ncol(self$agent$replay.y))
      )

      # TODO: reimplement color and size argument
      minibatch %>%
        plotly::plot_ly(
          x = ~ get(paste0("State_Dim_",     x_axis)),
          y = ~ get(paste0("State_Dim_",     y_axis)),
          z = ~ get(paste0("ActionVal_Dim_", z_axis)),
          mode = "markers",
          color = I("black"), #if (input$color == "-") I("black") else ~ get(input$color),
          size  = I(3), #if (input$size  == "-") I(3)       else ~ get(input$size),
          showlegend = FALSE,
          type  = "scatter3d"
        ) %>%
        plotly::layout(
          dragmode = "turntable",
          scene = list(
            xaxis  = list(title = paste0("State_Dim_",     x_axis), titlefont = list(size = 15)),
            yaxis  = list(title = paste0("State_Dim_",     y_axis), titlefont = list(size = 15)),
            zaxis  = list(title = paste0("ActionVal_Dim_", z_axis), titlefont = list(size = 15))#, tickangle = 90)
          )
        ) %>%
        plotly::add_surface(
          data = state_space,
          x = ~ x,
          y = ~ y,
          z = ~ get(paste0("ActionVal_Dim_", z_axis)),
          colorscale = "Viridis",
          showscale = showscale,
          opacity = 0.95,
          autocolorscale = FALSE
        )
    }
  ),
  private = list(),
  active = list()
)
