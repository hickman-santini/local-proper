library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Instant Reliability Significance"),
  sidebarLayout(
    sidebarPanel(
      numericInput("p_gov", "Govering probability (p_gov):", value = 0.85),
      numericInput("N", "Number of p_gov forecasts:", value = 383),
      numericInput("lessthan", "Threshold of interest:", value = 0.79),
      numericInput("mw", "Worlds:", value = 4096),
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
