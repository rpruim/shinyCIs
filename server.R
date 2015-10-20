
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
require(mosaic)
require(ggplot2)
theme_bare <- function(...) {
  theme(
    axis.line = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.y = element_blank(),
    # axis.ticks.length = unit(0, "lines"), # Error
    # axis.ticks.margin = unit(c(0,0,0,0), "lines"),
    legend.position = "none",
    panel.background = element_rect(fill = "white"),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.margin = unit(c(0,0,0,0), "lines"),
    plot.background = element_rect(fill = "white"),
    plot.margin = unit(c(0,0,0,0), "lines")
  )
}


shinyServer(function(input, output) {

  nsamples <- 100
  selected_sample <- nsamples
  coverTally <- c(0, 0)

  my_xlim <- reactive({
    res <- switch(
      input$population,
      norm = input$A + c(-1, 1) * 3.2 * input$B,
      beta = c(0,1),
      gamma = c(0, input$A / input$B + 3 * sqrt(input$A) / input$B)
    )
    res
  })

  output$paramControls <- renderUI({
    switch(
      input$population,
      norm =  list(
        sliderInput("A", "mean", min = -10, max = 10, value = 0, step = 0.2),
        sliderInput("B", "sd", min = 0.2, max = 10, value = 5, step = 0.2)
      ),
      beta =  list(
        sliderInput("A", "shape1", min = 0.8, max = 5, value = 1, step = 0.05),
        sliderInput("B", "shape2", min = 0.8, max = 5, value = 1, step = 0.05)
      ),
      gamma =  list(
        sliderInput("A", "shape", min = 0.02,  max = 5, value = 1, step = 0.02),
        sliderInput("B", "rate", min = 1, max = 10, value = 5, step = 0.2)
      )
    )
  })

  target <- reactive({
    switch(
      input$population,
      norm = input$A,
      beta = input$A / (input$A + input$B),
      gamma = input$A / input$B
    )
  })


  Data <- reactive({
    input$more
    data.frame(
      x =
        do.call(
          paste0("r", input$population),
          c(list(n = as.integer(input$n) * nsamples), list(input$A, input$B )))
    ) %>%
      mutate( idx = rep(1:nsamples, each = input$n) )
  })


  N <- reactive({
    as.integer(input$n)
  })

  popFunction <- reactive({
    function(x) {
      switch(
        input$population,
        norm = dnorm(x, mean = input$A, sd = input$B),
        beta = dbeta(x, shape1 = input$A, shape2 = input$B),
        gamma = dgamma(x, shape = input$A, rate = input$B)
      )
    }
  })

  OneSample <- reactive({
    Data() %>%
      filter( idx == selectedSample() )
  })

  alpha <- reactive({
    (1 - input$level) / 2
  })

  Intervals <- reactive({
    Data() %>%
      group_by( idx ) %>%
      summarise(
        mean = mean(x),
        lwr = mean + qt(alpha(), df = N() - 1) * sd(x) / sqrt(N()),
        upr = mean + qt(1-alpha(), df = N() - 1) * sd(x) / sqrt(N()),
        cover = (lwr < target()) & (target() < upr) ) %>%
      ungroup()
  })

  output$populationPlot<- renderPlot({
    ggplot( data.frame( x=my_xlim()), aes(x=my_xlim()) ) +
      stat_function( fun = popFunction(), n = 501 ) +
      expand_limits(x = my_xlim()) +
      theme_bare()
  })

  output$samplePlot<- renderPlot({
    ggplot( data = OneSample()) +
      geom_histogram( aes(x = x), bins = 15, fill = "navy", alpha=0.5) +
      expand_limits(x = my_xlim()) +
      theme_bare()
  })

  OneSampleColor <- reactive({
    colors <- c("TRUE" = "navy", "FALSE" = "red")
    covers <- (Intervals() %>% filter(idx == selectedSample()) )$cover
    colors[ as.character(covers) ]
  })

  selectedSample <- reactive({
    if (! is.null(input$plot_click)) {
      selected_sample <<- round(input$plot_click$y)
      if (selected_sample < 1) selected_sample <- 1
      if (selected_sample > nsamples) selected_sample <- nsamples
    }
    selected_sample
  })

  output$intervalsPlot <- renderPlot({
    ggplot(data = Intervals()) +
      geom_pointrange(
        aes(x=idx, ymin = lwr, ymax = upr, y = mean, colour = cover,
            alpha = idx == selectedSample(),
            size = idx == selectedSample()
        )) +
      geom_hline(yintercept = target(), size = 1, colour = "forestgreen", alpha = 0.7) +
      coord_flip() +
      scale_size_manual(values = c("TRUE" = 1.5, "FALSE" = .7), guide = FALSE) +
      scale_color_manual(values = c("TRUE" = "navy", "FALSE" = "red"), guide = FALSE) +
      scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = .5), guide = FALSE) +
      lims(y = my_xlim()) +
      labs(title = paste0(100 * input$level, "% Confidence Intervals for the mean"),
           x = "") +
      theme_bare()
  })

  output$bigPlot <- renderPlot({

    populationPlot <-
      ggplot( data.frame( x=my_xlim()), aes(x=my_xlim()) ) +
      stat_function( fun = popFunction(), n = 1001 ) +
      geom_vline(xintercept = target(), color = "forestgreen", size = 2, alpha = 0.7) +
      lims(x = my_xlim(), y = c(0, NA)) +
      labs(title = "Population", x ="") +
      theme_bare()

    samplePlot <-
      ggplot( data = OneSample()) +
      geom_histogram( aes(x = x), bins = 15,
                      fill = OneSampleColor(), alpha=0.5) +
      lims(x = my_xlim()) +
      geom_vline(xintercept = mean(OneSample()$x, color = "navy"), size = 2, alpha = 0.7) +
      labs(title = paste("One Sample; mean = ", round(mean(OneSample()$x), 2)), x="") +
      theme_bare()

    intervalsPlot <-
      ggplot(data = Intervals()) +
      geom_pointrange(
        aes(x=idx, ymin = lwr, ymax = upr, y = mean, colour = cover,
            alpha = idx == selectedSample(),
            size = idx == selectedSample()
        )) +
      geom_hline(yintercept = target(), color = "forestgreen", size = 5, alpha=0.7) +
      coord_flip() +
      scale_size_manual(values = c("TRUE" = 1.5, "FALSE" = .7), guide = FALSE) +
      scale_color_manual(values = c("TRUE" = "navy", "FALSE" = "red"), guide = FALSE) +
      scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = .5), guide = FALSE) +
      lims(y = my_xlim()) +
      labs(title = paste0(100 * input$level, "% Confidence Intervals for the mean"),
           x = "") +
      theme_bare()
    gridExtra::grid.arrange(
      populationPlot,
      samplePlot,
      heights = c(1, 1),
      ncol = 1
    )
  })


  observeEvent(input$A, {coverTally <<- c(0,0) })
  observeEvent(input$B, {coverTally <<- c(0,0) })
  observeEvent(input$population, {coverTally <<- c(0,0) })
  observeEvent(input$n, {coverTally <<- c(0,0) })
  observeEvent(input$level, {coverTally <<- c(0,0) })

  # text messages
  CoverTally <- reactive({
    cat("A: "); print(coverTally)
    coverTally <<- coverTally + c( sum(Intervals()$cover),  nrow(Intervals()) )
    cat("B: "); print(coverTally)
    coverTally
  })

  output$message <- renderText({
    paste(sum(Intervals()$cover), "of these",
          nrow(Intervals()), "intervals cover the parameter value.")
  })

  output$runningTotal <- renderText({
    if (CoverTally()[2] > 0) {
      paste0("Coverage rate: ",
             round(100 * CoverTally()[1]/CoverTally()[2],1), "% (", CoverTally()[2], " samples)")
    } else {
      ""
    }
  })


})
