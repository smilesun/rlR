Performance = R6::R6Class("Performance",
  public = list(
    list.reward.epi = NULL,  # take reward vector of each episode
    list.discount.reward.epi = NULL,  # discounted reward per episode
    list.rewardPerEpisode = NULL,  # sum up reward of each episode
    list.discountedRPerEpisode = NULL,
    rewardPerStep = NULL,
    list.stepsPerEpisode = NULL,
    list.infos = NULL,
    epi.idx = NULL,
    glogger = NULL,
    agent = NULL,
    r.vec.epi = NULL,
    epi_wait_ini = NULL,   # number of episode to wait until to reinitialize
    epi_wait_expl = NULL,  # number of episode to wait until to increase epsilon for exploration
    recent_win = NULL,
    epiLookBack = NULL,
    recent_door = NULL,
    bad_ratio = NULL,
    gamma = NULL,
    bad_reward = NULL,
    good_cnt = NULL,
    wait_epi = NULL,
    wait_cnt = NULL,
    reset_cnt = NULL,
    total.step = NULL,  # how many times model has been reset
    initialize = function(agent) {
      self$epiLookBack = 100L
      self$reset_cnt = 0L
      self$wait_epi = agent$conf$get("policy.epi_wait_ini")
      self$wait_cnt = 0L
      self$good_cnt = 0L
      self$recent_win = 20L
      self$recent_door = 40L
      self$bad_ratio = 0.99
      self$agent = agent
      if (!is.null(self$agent$env$bad_reward)) self$bad_reward = self$agent$env$bad_reward
      else self$bad_reward = -Inf

      self$epi_wait_ini = self$agent$conf$get("policy.epi_wait_ini")
      self$epi_wait_expl = self$agent$conf$get("policy.epi_wait_expl")
      self$gamma = self$agent$conf$get("agent.gamma")
      self$glogger = self$agent$glogger
      self$list.reward.epi = list()
      self$list.infos = list()
      self$list.discount.reward.epi = list()
      self$epi.idx = 0L
      self$list.rewardPerEpisode = list()
      self$list.discountedRPerEpisode = list()
      self$list.stepsPerEpisode = list()
      self$r.vec.epi = vector(mode = "numeric", length = 2000L)  # FIXME: how to set a reasonble number here?
    },

    success = function() {
      ok_reward = self$agent$env$ok_reward
      ok_step = self$agent$env$ok_step
      if (is.null(ok_reward) || is.null(ok_step)) {
        return(FALSE)
      }
      if (self$getAccPerf(ok_step) > ok_reward) {
        return(TRUE)
      }
      return(FALSE)
    },

    computeDiscount = function(rewardvec) {
      discounted_r = vector(mode = "double", length = length(rewardvec))
      running_add = 0
      i = length(rewardvec)
      while (i > 0) {
        running_add = running_add * self$gamma + rewardvec[i]
        discounted_r[i] = running_add
        i = i - 1L
      }
      discounted_r
    },

    persist = function(path) {
      perf = self$clone()
      save(perf, file = path)
    },


    getAccPerf = function(interval = 100L) {
      self$list.rewardPerEpisode = lapply(self$list.reward.epi, function(x) sum(x))
      epi.idx = length(self$list.rewardPerEpisode)
      winstart = max(1L, epi.idx - interval)
      vec = unlist(self$list.rewardPerEpisode)
      mean(vec[winstart:epi.idx], na.rm = TRUE)
    },

    isBad = function() {
      pwin = self$getAccPerf(self$recent_win)
      pdoor = self$getAccPerf(self$recent_door)
      self$agent$interact$toConsole("Last %d episodes average reward %f \n", self$recent_win, pwin)
      self$agent$interact$toConsole("Last %d episodes average reward %f \n", self$recent_door, pdoor)
      all_rewards = unlist(self$list.rewardPerEpisode)
      flag1 = pwin < self$bad_ratio * pdoor
      flag2 = pwin < (1/self$bad_ratio) * self$getAccPerf(100L)
      flag2old = flag2
      flag3 = pwin < median(all_rewards)
      flag4 = pwin < mean(all_rewards)
      flag22 = (flag2 || flag2old)
      if (!flag22)  self$good_cnt = self$good_cnt + 1L
      else self$good_cnt = 0L
      res = c(flag1, flag2, flag3, flag4, flag22)
      names(res) = c("bad_small", "bad_middle", "bad_big1", "bad_big2", "bad_middle2")
      self$agent$interact$toConsole("%s", toString(res))
      return(res)
    },

    rescue = function() {
      flag = self$isBad()
      self$wait_epi = min(self$agent$conf$get("policy.epi_wait_expl"), self$wait_epi + 1)
      if (flag[1]) {
        self$agent$interact$toConsole("\n bad perform for last window, %d times \n", self$wait_cnt + 1L)
        self$wait_cnt = self$wait_cnt + 1L
        ratio = exp(-self$agent$policy$logdecay * self$total_step)
        #self$agent$policy$epsilon = min(1, self$agent$policy$epsilon * ratio)  #FIXME: shall we increase explore here ? Again and again exporation will never converge
        flag_new_start = self$wait_cnt > self$agent$conf$get("policy.epi_wait_middle")
        flag_start = all(flag) && flag_new_start
        if (self$wait_cnt > self$wait_epi || flag_start) {
          if (flag[2] || flag[3]) {
            self$agent$interact$toConsole("\n\n### going to reset brain ###\n\n\n")
            self$agent$setBrain()
            self$wait_epi = self$agent$conf$get("policy.epi_wait_expl")
            self$reset_cnt = self$reset_cnt + 1L
            self$agent$policy$epsilon = self$agent$policy$maxEpsilon
            self$wait_cnt = 0
          } else {
            self$wait_cnt = max(0, self$wait_cnt - 1)
            self$agent$policy$epsilon = self$agent$policy$maxEpsilon
          }
        }
      } else {
        if (self$good_cnt > 5L) {
          self$agent$interact$toConsole("\n# success more than 5 \n")
          self$wait_cnt = max(0, self$wait_cnt - self$agent$conf$get("policy.epi_wait_ini"))
      }}
      #else if (flag["bad_middle2"])
      # self$wait_cnt = max(0, self$wait_cnt - 1)
      # }
      self$agent$interact$toConsole("\n wait cnt: %d times \n", self$wait_cnt)
    }, # fun

    toString = function() {
      s1 = sprintf("steps per episode:%s \n", toString(self$list.stepsPerEpisode))
      self$list.rewardPerEpisode = lapply(self$list.reward.epi, function(x) sum(x))
      s2 = sprintf("total reward per episode: %s \n", toString(self$list.rewardPerEpisode))
      self$rewardPerStep = unlist(self$list.rewardPerEpisode) / unlist(self$list.stepsPerEpisode)
      s3 = sprintf("reward per step per episode:%s \n", toString(self$rewardPerStep))
      paste(s1, s2, s3)
    },

    plot = function() {
      self$list.rewardPerEpisode = lapply(self$list.reward.epi, function(x) sum(x))
      rewards = unlist(self$list.rewardPerEpisode)
      df = data.frame(episode = seq_along(rewards),
        rewards = rewards)
      ggplot2::ggplot(df, aes(episode, rewards), col = "brown1") +
        geom_point(alpha = 0.2) +
        theme_bw() +
        labs(
          title = "Rewards Per Episode",
          x = "Episode",
          y = "Rewards per episode"
          ) +
        coord_cartesian(ylim = range(rewards)) +
        geom_smooth(se = FALSE, size = 1) +
        geom_hline(yintercept = median(rewards), size = 1, col = "black", lty = 2)
    },

    startApp = function() {
      # maybe put the code of this function into an extra source file

      # maybe put the code of this function into an extra source file
      library(DT)
      library(plotly)
      library(crosstalk)
      library(shiny)
      library(shinythemes)
      library(reticulate)

      data = cbind( perf$agent$replay.x, perf$agent$replay.y ) %>% #mtcars %>%
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
          theme = shinytheme("cerulean"),
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
            plot_ly(
              x = ~ col_index,
              y = ~ row_index,
              color = ~ value,
              mode = "markers",
              size  = I(10),
              type  = "scatter"
            ) %>%
            layout(
              showlegend = FALSE,
              xaxis = list(title = "Column Index", titlefont = list(size = 18)),
              yaxis = list(title = "Row Index", titlefont = list(size = 18))
            ) %>%
            add_trace(
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
            plot_ly(
              x = ~ col_index,
              y = ~ row_index,
              z = ~ value,
              mode = "markers",
              color = ~ value,
              size  = I(5),
              type  = "scatter3d"
            ) %>%
            layout(
              showlegend = TRUE,
              xaxis = list(title = "Column Index", titlefont = list(size = 25)),
              yaxis = list(title = "Row Index", titlefont = list(size = 25)),
              zaxis = list(title = "Weight Value", titlefont = list(size = 25))
            )
        })


        # highlight selected rows in the scatterplot
        output$plot_2d <- renderPlotly({

          req(input$x_axis_2d)
          s = input$x1_rows_selected

          state_space = data.frame(X_1 = 0:99)
          # using a loop, because the order of the columns is important
          for (i in 1:ncol(perf$agent$replay.x)) {
            if (paste0("X_", i) == input$x_axis_2d)
              # generate evenly distributed points between space borders
              state_space[input$x_axis_2d] = sapply((s_space_maxs$data()[input$x_axis_2d] - s_space_mins$data()[input$x_axis_2d]), function(x) 0:99/99 * x, USE.NAMES = FALSE) + s_space_mins$data()[input$x_axis_2d]
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
            plot_ly(
              x = ~ get(input$x_axis_2d),
              y = ~ get(input$y_axis_2d),
              mode = "markers",
              color = if (input$color_2d == "-") I("black") else ~ get(input$color_2d),
              size  = if (input$size_2d  == "-") I(8)       else ~ get(input$size_2d),
              name  = "Unfiltered",
              type  = "scatter"
            ) %>%
            layout(
              showlegend = TRUE,
              xaxis = list(title = input$x_axis_2d, titlefont = list(size = 18)),
              yaxis = list(title = input$y_axis_2d, titlefont = list(size = 18))
            ) %>%
            add_trace(
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
          state_space %<>% as.matrix
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
              Y_1 = matrix(df[[length(df)-1]], nrow = 100, ncol = 100),
              Y_2 = matrix(df[[length(df)]], nrow = 100, ncol = 100)
            )
          }
          state_space %<>% helper

          s_data %>%
            plot_ly(
              x = ~ get(input$x_axis_3d),
              y = ~ get(input$y_axis_3d),
              z = ~ get(input$z_axis),
              mode = "markers",
              color = if (input$color == "-") I("black") else ~ get(input$color),
              size  = if (input$size  == "-") I(3)       else ~ get(input$size),
              type  = "scatter3d"
            ) %>%
            layout(
              dragmode = "turntable",
              showlegend = TRUE,
              scene = list(
                xaxis = list(title = input$x_axis_3d, titlefont = list(size = 25)),
                yaxis = list(title = input$y_axis_3d, titlefont = list(size = 25)),
                zaxis = list(title = input$z_axis, titlefont = list(size = 30))
              )
            ) %>%
            add_surface(
              data = state_space,
              x = ~ x,
              y = ~ y,
              z = ~ get(input$z_axis)
            )
        })
      }


      shinyApp(ui, server)


    },

    toScalar = function() {
      self$getAccPerf(100L)
    },

    extractInfo = function() {
      self$list.infos = lapply(self$agent$mem$samples, function(x) x$info)
    },

    afterAll = function() {
      self$toString()   # print out performance
      if (self$glogger$flag) self$persist(self$agent$conf$conf.log.perf$resultTbPath)
      self$extractInfo()
    },

    afterEpisode = function() {
      self$agent$interact$idx.episode = self$agent$interact$idx.episode + 1L
      self$agent$interact$glogger$log.nn$info("Episode: %i, steps:%i\n", self$agent$interact$idx.episode, self$agent$interact$idx.step)
      self$agent$interact$toConsole("Episode: %i finished with steps:%i \n", self$agent$interact$idx.episode, self$agent$interact$idx.step)
      self$epi.idx = self$epi.idx + 1L
      self$list.reward.epi[[self$epi.idx]] = vector(mode = "list")
      self$list.reward.epi[[self$epi.idx]] = self$r.vec.epi[1L:self$agent$interact$idx.step]   # the reward vector
      self$list.discount.reward.epi[[self$epi.idx]] = self$computeDiscount(self$r.vec.epi[1L:self$agent$interact$idx.step])
      self$list.stepsPerEpisode[[self$epi.idx]] = self$agent$interact$idx.step  # the number of steps
      rew = self$getAccPerf(self$epiLookBack)
      self$agent$interact$toConsole("Last %d episodes average reward %f \n", self$epiLookBack, rew)

    }
    ),
  private = list(),
  active = list())
