#' @importFrom magrittr %>% %<>%
#' @import shiny

PerformanceShiny = R6::R6Class(
  "PerformanceShiny",
  inherit = Performance,
  public = list(
    startApp = function() {
      # maybe put the code of this function into an extra source file

      #library(DT)
      #library(plotly)
      #library(crosstalk)
      #library(shiny)

      data = cbind( perf$agent$replay.x, perf$agent$replay.y ) %>%
        data.frame()

      names(data) = c(
        paste0("X_", 1:ncol(perf$agent$replay.x)),
        paste0("Y_", 1:ncol(perf$agent$replay.y))
      )

      # calculate space borders
      space_mins = sapply(data.frame(perf$agent$replay.x), FUN = min)
      names(space_mins) = paste0("X_", 1:ncol(perf$agent$replay.x))
      space_maxs = sapply(data.frame(perf$agent$replay.x), FUN = max)
      names(space_maxs) = paste0("X_", 1:ncol(perf$agent$replay.x))

      # get the weights for plotting
      weights = perf$agent$brain$getWeights()
      # get the amount of layers for the user interface
      layer_index = as.list(1:(length(weights)/2))
      # list of state dimension
      variable_selection = variable_selection_null =
        data %>% names %>% .[1:ncol(perf$agent$replay.x)] %>% as.list
      # append empty option
      variable_selection_null[[length(variable_selection) + 1]] = "-"
      # list of action value dimension
      value_selection =
        data %>% names %>% .[ncol(perf$agent$replay.x) + 1:ncol(perf$agent$replay.y)] %>% as.list

      ui = tagList(
        # shinythemes::themeSelector(), # uncomment this to check out different themes
        navbarPage(
          theme = shinythemes::shinytheme("cerulean"),
          "RL Visualization",

          tabPanel(
            "2D Weights",
            sidebarLayout(
              # Sidebar with selection inputs
              sidebarPanel(
                selectInput("layer", "Layer", layer_index, selected = 1),
                selectInput("weights", "Weights/Bias", list("Weights" = 0, "Bias" = 1), selected = "Weights"), # the numbers are important for later on
                width = 3
              ),

              # Show Interactive Graphic
              mainPanel(
                plotlyOutput("plot_weights_2d", width = "100%", height = "900px")
              )
            ),
            br(),
            br()
          ),

          tabPanel(
            "3D Weights",
            sidebarLayout(
              # Sidebar with selection inputs
              sidebarPanel(
                selectInput("layer_3d", "Layer", layer_index, selected = 1),
                selectInput("weights_3d", "Weights/Bias", list("Weights" = 0, "Bias" = 1), selected = "Weights"), # the numbers are important for later on
                width = 3
              ),

              # Show Interactive Graphic
              mainPanel(
                plotlyOutput("plot_weights_3d", width = "100%", height = "900px")
              )
            ),
            br(),
            br()
          ),

          tabPanel(
            "2D Action Value",
            sidebarLayout(
              # Sidebar with a slider and selection inputs
              sidebarPanel(
                selectInput("x_axis_2d", "X axis", variable_selection, selected = variable_selection[[1]]),
                selectInput("color_2d", "Dot Color", variable_selection_null, selected = "-"),
                selectInput("size_2d", "Dot Size", variable_selection_null, selected = "-"),
                lapply(1:ncol(perf$agent$replay.x), function(i) {
                  uiOutput(paste0('b', i))
                }),
                tags$hr(),
                selectInput("y_axis_2d", "Y axis", value_selection, selected = value_selection[[1]]),
                width = 3
              ),

              # Show Interactive Graphic
              mainPanel(
                plotlyOutput("plot_2d", width = "100%", height = "900px")
              )
            ),
            br(),
            br()
          ),


          tabPanel(
            "3D Action Value",
            sidebarLayout(
              # Sidebar with a slider and selection inputs
              sidebarPanel(
                selectInput("x_axis_3d", "X axis", variable_selection, selected = variable_selection[[1]]),
                selectInput("y_axis_3d", "Y axis", variable_selection, selected = variable_selection[[2]]),
                selectInput("color", "Dot Color", variable_selection_null, selected = "-"),
                selectInput("size", "Dot Size", variable_selection_null, selected = "-"),
                lapply(1:ncol(perf$agent$replay.x), function(i) {
                  uiOutput(paste0('c', i))
                }),
                tags$hr(),
                selectInput("z_axis", "Z axis", value_selection, selected = value_selection[[1]]),
                width = 3
              ),

              # Show Interactive Graphic
              mainPanel(
                plotlyOutput("x2", width = "100%", height = "900px")
              )
            )
          )
        )
      )

      server = function(input, output) {
        s_space_maxs = SharedData$new(space_maxs)
        s_space_mins = SharedData$new(space_mins)
        s_data = SharedData$new(data)
        s_weights = perf$agent$brain$getWeights()


        lapply(1:ncol(perf$agent$replay.x), function(i) {
          output[[paste0('b', i)]] <- renderUI({
            req(input$x_axis_2d)
            # only use sliders unequal to x-axis selection
            if (input$x_axis_2d != paste0("X_", i))
              sliderInput(
                paste0("slider_2d_", i), paste0("X_", i),
                min = s_space_mins$data()[i] %>% floor,
                max = s_space_maxs$data()[i] %>% ceiling,
                value = round(s_space_mins$data()[i] + (s_space_maxs$data()[i] - s_space_mins$data()[i])/2), # take the middle
                step = (ceiling(s_space_maxs$data()[i]) - floor(s_space_mins$data()[i]))/20
              )
          })
        })


        lapply(1:ncol(perf$agent$replay.x), function(i) {
          output[[paste0('c', i)]] <- renderUI({
            req(input$x_axis_3d)
            # only use sliders unequal to x-axis selection
            if (input$x_axis_3d != paste0("X_", i) && input$y_axis_3d != paste0("X_", i))
              sliderInput(
                paste0("slider_3d_", i), paste0("X_", i),
                min = s_space_mins$data()[i] %>% floor,
                max = s_space_maxs$data()[i] %>% ceiling,
                value = round(s_space_mins$data()[i] + (s_space_maxs$data()[i] - s_space_mins$data()[i])/2), # take the middle
                step = (ceiling(s_space_maxs$data()[i]) - floor(s_space_mins$data()[i]))/20
              )
          })
        })


        output$plot_weights_2d = renderPlotly({

          weights_data = reactiveVal()
          weights_data({
            weight_index = as.integer(input$layer) + as.integer(input$weights)
            dim_weights  = dim(s_weights[[weight_index]])
            weights_data = cbind( expand.grid(1:dim_weights[1], 1:dim_weights[2]), round(c(s_weights[[weight_index]]), 4))
            names(weights_data) = c("col_index", "row_index", "value")
          })

          weights_data %>%
            plotly::plot_ly(
              x = ~ col_index,
              y = ~ row_index,
              color = ~ value,
              mode = "markers",
              size  = I(10),
              type  = "scatter"
            ) %>%
            plotly::layout(
              showlegend = FALSE,
              xaxis = list(title = "Column Index", titlefont = list(size = 18)),
              yaxis = list(title = "Row Index", titlefont = list(size = 18))
            ) %>%
            plotly::add_trace(
              text = ~ value
            )
        })

        output$plot_weights_3d <- renderPlotly({

          weights_data = reactiveVal()
          weights_data({
            weight_index = as.integer(input$layer_3d) + as.integer(input$weights_3d)
            dim_weights  = dim(s_weights[[weight_index]])
            weights_data = cbind( expand.grid(1:dim_weights[1], 1:dim_weights[2]), round(c(s_weights[[weight_index]]), 4))
            names(weights_data) = c("col_index", "row_index", "value")
          })

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
              yaxis = list(title = "Row Index", titlefont = list(size = 25)),
              zaxis = list(title = "Weight Value", titlefont = list(size = 25))
            )
        })


        # highlight selected rows in the scatterplot
        output$plot_2d <- renderPlotly({
          # wait until the parameters are loaded
          req(input$x_axis_2d)

          s = input$x1_rows_selected

          state_space = data.frame(X_1 = 0:99)
          # using a loop, because the order of the columns is important
          for (i in 1:ncol(perf$agent$replay.x)) {
            if (paste0("X_", i) == input$x_axis_2d)
              # generate evenly distributed points between space borders
              state_space[input$x_axis_2d] = sapply(
                (s_space_maxs$data()[input$x_axis_2d] - s_space_mins$data()[input$x_axis_2d]),
                function(x) 0:99/99 * x, USE.NAMES = FALSE
                ) + s_space_mins$data()[input$x_axis_2d]
            else
              state_space[paste0("X_", i)] = input[[paste0("slider_2d_", i)]]
          }
          state_space %<>% as.matrix
          predictions = array(state_space, dim = c(nrow(state_space), ncol(state_space))) %>% perf$agent$brain$pred()
          state_space = cbind(state_space, predictions) %>% as.data.frame()
          names(state_space) = c(
            paste0("X_", 1:ncol(perf$agent$replay.x)),
            paste0("Y_", 1:ncol(perf$agent$replay.y))
          )

          s_data %>%
            plotly::plot_ly(
              x = ~ get(input$x_axis_2d),
              y = ~ get(input$y_axis_2d),
              mode = "markers",
              color = if (input$color_2d == "-") I("black") else ~ get(input$color_2d),
              size  = if (input$size_2d  == "-") I(8)       else ~ get(input$size_2d),
              name  = "Unfiltered",
              type  = "scatter"
            ) %>%
            plotly::layout(
              showlegend = TRUE,
              xaxis = list(title = input$x_axis_2d, titlefont = list(size = 18)),
              yaxis = list(title = input$y_axis_2d, titlefont = list(size = 18))
            ) %>%
            plotly::add_trace(
              data = state_space,
              x    = ~ get(input$x_axis_2d),
              y    = ~ get(input$y_axis_2d),
              size = I(5),
              name = 'trace 1',
              mode = 'lines',
              color = "#e6550d"
            )
        })


        # highlight selected rows in the scatterplot
        output$x2 <- renderPlotly({
          # wait until the parameters are loaded
          req(input$x_axis_3d)
          req(input$y_axis_3d)

          s <- input$x1_rows_selected

          state_space = data.frame(X_1 = 0:9999)
          # using a loop, because the order of the columns is important
          for (i in 1:ncol(perf$agent$replay.x)) {
            if (paste0("X_", i) == input$x_axis_3d)
              # generate evenly distributed points between space borders
              state_space[input$x_axis_3d] = rep(sapply((s_space_maxs$data()[input$x_axis_3d] - s_space_mins$data()[input$x_axis_3d]), function(x) 0:99/99 * x, USE.NAMES = FALSE) + s_space_mins$data()[input$x_axis_3d], 100)
            else if (paste0("X_", i) == input$y_axis_3d)
              # generate evenly distributed points between space borders
              state_space[input$y_axis_3d] = rep(sapply((s_space_maxs$data()[input$y_axis_3d] - s_space_mins$data()[input$y_axis_3d]), function(x) 0:99/99 * x, USE.NAMES = FALSE) + s_space_mins$data()[input$y_axis_3d], each = 100)
            else
              state_space[paste0("X_", i)] = input[[paste0("slider_3d_", i)]]
          }
          state_space %<>% as.matrix()
          predictions = array(state_space, dim = c(nrow(state_space), ncol(state_space))) %>% perf$agent$brain$pred()
          state_space = cbind(state_space, predictions) %>% as.data.frame()
          names(state_space) = c(
            paste0("X_", 1:ncol(perf$agent$replay.x)),
            paste0("Y_", 1:ncol(perf$agent$replay.y))
          )

          helper = function(df) {
            list(
              x = unique(df[[input$x_axis_3d]]),
              y = unique(df[[input$y_axis_3d]]),
              Y_1 = matrix(df[[length(df)-1]], nrow = 100, ncol = 100, byrow = TRUE),
              Y_2 = matrix(df[[length(df)]], nrow = 100, ncol = 100, byrow = TRUE)
            )
          }
          state_space %<>% helper

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
                xaxis = list(title = input$x_axis_3d, titlefont = list(size = 25)),
                yaxis = list(title = input$y_axis_3d, titlefont = list(size = 25)),
                zaxis = list(title = input$z_axis, titlefont = list(size = 30))
              )
            ) %>%
            plotly::add_surface(
              data = state_space,
              x = ~ x,
              y = ~ y,
              z = ~ get(input$z_axis)
            )
        })
      }

      shinyApp(ui, server)
    }
  ),
  private = list(),
  active = list()
)
