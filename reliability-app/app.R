library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Instant Reliability Significance"),
  h3(HTML("<b>How should we interpret reliability graphs?</b>")),
  p("What's important is consistency, not distance from the 'perfect calibration' line."),
  div(
    img(src = "silver-BRAGGING-01-1.png", height = "500px"),
    p(HTML("Source: <a href='https://fivethirtyeight.com/features/when-we-say-70-percent-it-really-means-70-percent/' target='_blank'>FiveThirtyEight</a>"), style = "text-align: left; font-style: italic;")
  ),
  p("This handy simulator tells you what the chances are that [N] forecasts of [p_gov] would be 'aligned' in only [threshold] or fewer of the outcomes, e.g. in this example from FiveThirtyEight, if the true probability of a home-team win were 85% and there were 383 games, what are the chances that only 79% of those games would be home-wins? Actually very small, suggesting we shouldn't be that impressed."),
  p(HTML("For more information, refer to Bröcker, J. and Smith, L.A. (2007) <a href='https://journals.ametsoc.org/view/journals/wefo/22/3/waf993_1.xml?tab_body=pdf'>'Increasing the reliability of reliability diagrams'</a>, Weather and Forecasting, 22(3): 651-661.")),
  sidebarLayout(
    sidebarPanel(
      numericInput("N", "Number of p_gov forecasts:", value = 383),
      numericInput("p_gov", "Governing probability (p_gov):", value = 0.85),
      numericInput("lessthan", "Threshold of interest:", value = 0.79),
      numericInput("mw", "Worlds:", value = 8192),
      actionButton("submit", "Submit")
    ),
    mainPanel(
      plotOutput("histogram")
    )
  )
)

server <- function(input, output) {
  output$histogram <- renderPlot({
    # Recreate the Python function in R
    exp_frac_aligned <- function(N = 383, p_gov = 0.85, lessthan = 0.79, mw = 4096) {
      fracaligned <- numeric(mw)
      for (i in 1:mw) {
        outcomes <- runif(N) < p_gov
        fracaligned[i] <- sum(outcomes) / N
      }
      res <- sum(fracaligned < lessthan) / mw

      # Plotting
      hist(fracaligned,
        breaks = 100, main = paste("Instant Reliability Significance\nN=", N, ", p_gov=", p_gov, ", mw=", mw),
        xlab = "Fraction Aligned", ylab = "Density", xlim = c(0, 1), col = "lightblue", freq = FALSE
      )
      abline(v = lessthan, col = "red")
      legend("topright", legend = paste("P(fracaligned <", lessthan, ") =", format(res, digits = 3, scientific = FALSE)), col = "black", lty = 1)
    }

    exp_frac_aligned(input$N, input$p_gov, input$lessthan, input$mw)
  })
}

shinyApp(ui = ui, server = server)
