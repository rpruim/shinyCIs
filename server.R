
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
require(mosaic)
require(ggplot2)

theme_bare <- function(...) {
  theme( complete = FALSE,
    axis.line = element_blank(), # element_line(colour = "gray80"),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.y = element_blank(),
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
  RV <- reactiveValues( cover = 0, total = 0 )

  my_xlim <- reactive({
    res <- switch(
      input$population,
      norm = c(-6, 6), # input$A + c(-1, 1) * 3.2 * input$B,
      beta = c(0,1),
      gamma = c(0, 9 / input$B) # input$A / input$B + 3 * sqrt(input$A) / input$B)
    )
    res
  })

  PopulationMean <- reactive({
    validate(
      need( is.character(input$population) & is.numeric(input$A) & is.numeric(input$B),
            message = "Just a moment.")
    )
    switch(
      input$population,
      norm = input$A,
      beta = input$A / (input$A + input$B),
      gamma = input$A / input$B
    ) %>% round(2)
  })

  PopulationSD <- reactive({
    switch(
      input$population,
      norm = input$B,
      beta = sqrt( input$A * input$B / ( (input$A + input$B)^2 * (input$A + input$B + 1)) ),
      gamma = sqrt(input$A / input$B^2)
    ) %>% round(2)
  })

  output$paramControls <- renderUI({
    switch(
      input$population,
      norm =  list(
        sliderInput("A", "mean", min = -2, max = 2, value = 0, step = 0.1),
        sliderInput("B", "sd", min = 0.2, max = 3, value = 1, step = 0.1)
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
    tryCatch(
      data.frame(
        x =
          do.call(
            paste0("r", input$population),
            c(list(n = as.integer(input$n) * nsamples), list(input$A, input$B )))
      ) %>%
        mutate( idx = rep(1:nsamples, each = input$n) ),
    error = function(e)
      data.frame(
        x =
          do.call(
            paste0("r", "norm"),
            c(list(n = 20 * nsamples), list(0, 1)))
      ) %>%
      mutate( idx = rep(1:nsamples, each = 20) )
  )
  })


  N <- reactive({
    as.integer(input$n)
  })

  popFunction <- reactive({
    switch(
      input$population,
      norm = function(x) dnorm(x, mean = input$A, sd = input$B),
      beta = function(x) dbeta(x, shape1 = input$A, shape2 = input$B),
      gamma = function(x) dgamma(x, shape = input$A, rate = input$B)
    )
  })

  selectedSample <- reactive({
    if (! is.null(input$plot_click)) {
      selected_sample <<- round(input$plot_click$y)
      if (selected_sample < 1) selected_sample <<- 1
      if (selected_sample > nsamples) selected_sample <<- nsamples
    }
    selected_sample
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
      geom_histogram( aes(x = x), bins = 40, fill = "navy", alpha=0.5) +
      expand_limits(x = my_xlim()) +
      theme_bare()
  })

  OneSampleColor <- reactive({
    colors <- c("TRUE" = "navy", "FALSE" = "red")
    covers <- (Intervals() %>% filter(idx == selectedSample()) )$cover
    colors[ as.character(covers) ]
  })

  output$intervalsPlot <- renderPlot({
    validate( need(is.numeric(target()), message = "Loading data...") )
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
    validate(
      need( is.numeric( PopulationMean() ), message = "Just a moment."),
      need( inherits(Intervals(), "data.frame"), message = "loading data"),
      need( is.numeric( my_xlim() ), message = "Just a moment."),
      need( is.numeric( PopulationSD() ), message = "Just a moment.")
    )
    populationPlot <-
      ggplot( data.frame( x=my_xlim()), aes(x=my_xlim()) ) +
      stat_function( fun = popFunction(), n = 1001 ) +
      geom_vline(xintercept = target(), color = "forestgreen", size = 2, alpha = 0.7) +
      lims(x = my_xlim(), y = c(0, NA)) +
      labs(
        title = paste0("Population (mean = ", PopulationMean(), ", sd =", PopulationSD(), ")"),
        x ="") +
      theme_bare()

    samplePlot <-
      ggplot( data = OneSample()) +
      geom_histogram( aes(x = x), bins = 15,
                      fill = OneSampleColor(), alpha=0.5) +
      lims(x = my_xlim()) +
      geom_vline(xintercept = mean(OneSample()$x, color = "navy"), size = 2, alpha = 0.7) +
      labs(title = paste("One Sample (mean = ",
                         round(mean(OneSample()$x), 2), ", sd = ",
                         round(sd(OneSample()$x), 2), ")"),
           x="") +
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

  observeEvent(input$more, {
    RV$cover <- RV$cover + sum(Intervals()$cover)
    RV$total <- RV$total + nrow(Intervals())
  })


  observeEvent( c(input$population, input$A, input$B, input$n, input$level),
                { RV$cover <- sum(Intervals()$cover); RV$total <- nrow(Intervals()) }
  )

  # text messages
  output$message <- renderText({
    paste(sum(Intervals()$cover), "of these",
          nrow(Intervals()), "intervals cover the parameter value.")
  })

  output$runningTotal <- renderText({
    if (TRUE){
      paste0("Coverage rate: ",
             round(100 *  RV$cover/ RV$total, 1),
             "% (",  RV$total, " samples)")
    } else {
      ""
    }
  })

})
