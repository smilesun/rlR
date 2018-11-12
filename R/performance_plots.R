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

      # TODO: instead of only zeros, use mean of each state dimension
      if (is.null(input))
        input = rep(0, ncol(self$agent$replay.x))

      # set up data point generation
      state_space_l = as.list(input)

      min_s = min(self$agent$replay.x[[x_axis]])
      max_s = max(self$agent$replay.x[[x_axis]])
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

      # TODO: reimplement color and size argument
      # create the interactive plot for the ui
      s_data %>%
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

    ############## CONTINUE WORK HERE ################

    # this is the main function for the app - everything the app does is defined here
    startApp = function() {
      # give useful error messages
      if (is.null(self$list_models)) stop("Models during training not saved - set 'agent.store.model = TRUE'")
      if ((self$agent$replay.x %>% dim %>% length) > 1) stop("App is not designed for image data (yet)")

      # catching the observed data points plus their predictions of the last replay memory batch
      data = cbind( self$agent$replay.x, self$agent$replay.y ) %>%
        data.frame()

      names(data) = c(
        paste0("State_Dim_", 1:ncol(self$agent$replay.x)),
        paste0("ActionVal_Dim_", 1:ncol(self$agent$replay.y))
      )

      # calculate space borders
      space_mins = sapply(data.frame(self$agent$replay.x), FUN = min)
      names(space_mins) = paste0("State_Dim_", 1:ncol(self$agent$replay.x))
      space_maxs = sapply(data.frame(self$agent$replay.x), FUN = max)
      names(space_maxs) = paste0("State_Dim_", 1:ncol(self$agent$replay.x))

      # get the weights for plotting
      weights = self$agent$brain$getWeights()
      # get the amount of layers for the user interface
      layer_index = as.list(1:(length(weights)/2))
      # list of state dimension
      variable_selection = variable_selection_null =
        data %>% names %>% .[1:ncol(self$agent$replay.x)] %>% as.list
      # append empty option
      variable_selection_null[[length(variable_selection) + 1]] = "-"
      # list of action value dimension
      value_selection =
        data %>% names %>% .[ncol(self$agent$replay.x) + 1:ncol(self$agent$replay.y)] %>% as.list

      # this is the main ui - some dynamic parts of it are defined further down
      ui = tagList(
        # shinythemes::themeSelector(), # uncomment this to check out different themes
        navbarPage(
          theme = shinythemes::shinytheme("cerulean"),
          "RL Visualization",

          # first panel: 2D weights
          tabPanel(
            "2D Weights",
            sidebarLayout(
              # Sidebar with selection inputs
              sidebarPanel(
                selectInput("layer", "Layer", layer_index, selected = 1),
                selectInput("weights", "Weights/Bias", list("Weights" = -1, "Bias" = 0), selected = "Weights"), # the numbers are important for later on
                width = 3
              ),

              # Show Interactive Graphic
              mainPanel(
                plotly::plotlyOutput("plot_weights_2d", width = "100%", height = "900px")
              )
            ),
            br(),
            br()
          ),

          # second panel: 3D weights
          tabPanel(
            "3D Weights",
            sidebarLayout(
              # Sidebar with selection inputs
              sidebarPanel(
                selectInput("layer_3d", "Layer", layer_index, selected = 1),
                selectInput("weights_3d", "Weights/Bias", list("Weights" = -1, "Bias" = 0), selected = "Weights"), # the numbers are important for later on
                width = 3
              ),

              # Show Interactive Graphic
              mainPanel(
                plotly::plotlyOutput("plot_weights_3d", width = "100%", height = "900px")
              )
            ),
            br(),
            br()
          ),

          # third panel: 2D actien value
          tabPanel(
            "2D Action Value",
            sidebarLayout(
              # Sidebar with a slider and selection inputs
              sidebarPanel(
                numericInput("iter_2d",  "Training Iteration", value = 1, min = 1, max = length(self$list_models)),
                selectInput("x_axis_2d", "X axis",    variable_selection,      selected = variable_selection[[1]]),
                selectInput("color_2d",  "Dot Color", variable_selection_null, selected = "-"),
                selectInput("size_2d",   "Dot Size",  variable_selection_null, selected = "-"),
                lapply(1:ncol(self$agent$replay.x), function(i) {
                  uiOutput(paste0('b', i))
                }),
                tags$hr(),
                selectInput("y_axis_2d", "Y axis", value_selection, selected = value_selection[[1]]),
                width = 3
              ),

              # Show Interactive Graphic
              mainPanel(
                plotly::plotlyOutput("plot_2d", width = "100%", height = "900px")
              )
            ),
            br(),
            br()
          ),

          # fourth panel: 3D action value
          tabPanel(
            "3D Action Value",
            sidebarLayout(
              # Sidebar with a slider and selection inputs
              sidebarPanel(
                selectInput("x_axis_3d", "X axis",    variable_selection,      selected = variable_selection[[1]]),
                selectInput("y_axis_3d", "Y axis",    variable_selection,      selected = variable_selection[[2]]),
                selectInput("z_axis",    "Z axis",    value_selection,         selected = value_selection[[1]]),
                selectInput("color",     "Dot Color", variable_selection_null, selected = "-"),
                selectInput("size",      "Dot Size",  variable_selection_null, selected = "-"),
                tags$hr(),
                h3("Prediction Surface"),
                numericInput("iter_3d", "Training Iteration", value = 1, min = 1, max = length(self$list_models)),
                lapply(1:ncol(self$agent$replay.x), function(i) {
                  uiOutput(paste0('c', i))
                }),
                tags$hr(),
                h3("Camera Position"),
                sliderInput("x_angle", "X Axis", min = -5, max = 5, value = 1.25, step = 0.25),
                sliderInput("y_angle", "Y Axis", min = -5, max = 5, value = 1.25, step = 0.25),
                sliderInput("z_angle", "Z Axis", min = -5, max = 5, value = 0.5, step = 0.25),
                width = 3
              ),

              # Show Interactive Graphic
              mainPanel(
                plotly::plotlyOutput("x2", width = "100%", height = "900px")
              )
            )
          )
        )
      )

      # define the server function for all the background calculations required by the app
      server = function(input, output) {
        # define usable objects for the reactive parts and dynamic ui elements
        s_space_maxs = crosstalk::SharedData$new(space_maxs)
        s_space_mins = crosstalk::SharedData$new(space_mins)
        s_data       = crosstalk::SharedData$new(data)
        s_weights    = self$agent$brain$getWeights()

        # ui element with dynamic amount of sliders for 2D action value panel
        lapply(1:ncol(self$agent$replay.x), function(i) {
          output[[paste0('b', i)]] <- renderUI({
            req(input$x_axis_2d)
            # only use sliders unequal to x-axis selection
            if (input$x_axis_2d != paste0("State_Dim_", i))
              sliderInput(
                paste0("slider_2d_", i), paste0("State_Dim_", i),
                min   = s_space_mins$data()[i] %>% floor,
                max   = s_space_maxs$data()[i] %>% ceiling,
                value = round(s_space_mins$data()[i] + (s_space_maxs$data()[i] - s_space_mins$data()[i])/2), # take the middle
                step  = (ceiling(s_space_maxs$data()[i]) - floor(s_space_mins$data()[i]))/20
              )
          })
        })

        # ui element with dynamic amount of sliders for 3D action value panel
        lapply(1:ncol(self$agent$replay.x), function(i) {
          output[[paste0('c', i)]] <- renderUI({
            req(input$x_axis_3d)
            # only use sliders unequal to x-axis selection
            if (input$x_axis_3d != paste0("State_Dim_", i) && input$y_axis_3d != paste0("State_Dim_", i))
              sliderInput(
                paste0("slider_3d_", i), paste0("State_Dim_", i),
                min   = s_space_mins$data()[i] %>% floor,
                max   = s_space_maxs$data()[i] %>% ceiling,
                value = round(s_space_mins$data()[i] + (s_space_maxs$data()[i] - s_space_mins$data()[i])/2), # take the middle
                step  = (ceiling(s_space_maxs$data()[i]) - floor(s_space_mins$data()[i]))/20
              )
          })
        })

        # interactive plot embedded into the ui for the 2d action value panel
        output$x2 <- plotly::renderPlotly({
          # wait until the parameters are loaded
          req(input$x_axis_3d)
          req(input$y_axis_3d)

          s <- input$x1_rows_selected
          # set up data points generation
          state_space = data.frame(State_Dim_1 = 0:9999)
          # using a loop, because the order of the columns is important
          for (i in 1:ncol(self$agent$replay.x)) {
            if (paste0("State_Dim_", i) == input$x_axis_3d)
              # generate evenly distributed points between space borders
              state_space[input$x_axis_3d] =
                rep(
                  sapply(
                    s_space_maxs$data()[input$x_axis_3d] - s_space_mins$data()[input$x_axis_3d],
                    function(x) 0:99/99 * x,
                    USE.NAMES = FALSE
                  ) + s_space_mins$data()[input$x_axis_3d],
                  100
                )
            else if (paste0("State_Dim_", i) == input$y_axis_3d)
              # generate evenly distributed points between space borders
              state_space[input$y_axis_3d] =
                rep(
                  sapply(
                    (s_space_maxs$data()[input$y_axis_3d] - s_space_mins$data()[input$y_axis_3d]),
                    function(x) 0:99/99 * x, USE.NAMES = FALSE
                  ) + s_space_mins$data()[input$y_axis_3d],
                  each = 100
                )
            else
              state_space[paste0("State_Dim_", i)] = input[[paste0("slider_3d_", i)]]
          }
          state_space %<>% as.matrix()
          # calculate the predictions for the data points
          predictions = array(state_space, dim = c(nrow(state_space), ncol(state_space))) %>%
            self$list_models[[input$iter_3d]]$pred()

          state_space = cbind(state_space, predictions) %>% as.data.frame()
          names(state_space) = c(
            paste0("State_Dim_",     1:ncol(self$agent$replay.x)),
            paste0("ActionVal_Dim_", 1:ncol(self$agent$replay.y))
          )

          # create helper function for formatting the data points plus their predictions
          helper = function(df) {
            result = list(
              x = unique(df[[input$x_axis_3d]]),
              y = unique(df[[input$y_axis_3d]])
            )

            for (i in 1:ncol(self$agent$replay.y))
              result[[paste0("ActionVal_Dim_", i)]] =
                matrix(df[[length(df)-ncol(self$agent$replay.y)+i]], nrow = 100, ncol = 100, byrow = TRUE)

            return(result)
          }
          state_space %<>% helper

          # create interactive plot for the ui
          s_data %>%
            plotly::plot_ly(
              x = ~ get(input$x_axis_3d),
              y = ~ get(input$y_axis_3d),
              z = ~ get(input$z_axis),
              mode = "markers",
              color = if (input$color == "-") I("black") else ~ get(input$color),
              size  = if (input$size  == "-") I(3)       else ~ get(input$size),
              type  = "scatter3d"
            ) %>%
            plotly::layout(
              dragmode = "turntable",
              showlegend = TRUE,
              scene = list(
                camera = list(eye   = list(x = input$x_angle, y = input$y_angle, z = input$z_angle)),
                xaxis  = list(title = input$x_axis_3d, titlefont = list(size = 25)),
                yaxis  = list(title = input$y_axis_3d, titlefont = list(size = 25)),
                zaxis  = list(title = input$z_axis,    titlefont = list(size = 25), tickangle = 90)
              )
            ) %>%
            plotly::add_surface(
              data = state_space,
              x = ~ x,
              y = ~ y,
              z = ~ get(input$z_axis),
              colorscale = "Viridis",
              opacity = 0.95,
              autocolorscale = FALSE
            )
        })
      }

      # start the app with the defined ui and server functionality
      shinyApp(ui, server)
    }
    # nothing else going on
  ),
  private = list(),
  active = list()
)
