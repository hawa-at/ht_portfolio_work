library(shiny)
library(ggplot2)
library(dplyr)
library(ggrepel)
library(DT)


portfolio_risk <- read.csv("portfolio_risk_matrix.csv")

# UI
ui <- fluidPage(
  titlePanel("Digital Maturity vs. Public Project Outcomes Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("tier", "Select Investment Tier:",
                  choices = c("All", unique(portfolio_risk$Tier)),
                  selected = "All")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Risk Matrix", plotOutput("riskPlot", height = "600px")),
        tabPanel("Country Table", DTOutput("countryTable"))
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  filtered_data <- reactive({
    if (input$tier == "All") {
      portfolio_risk
    } else {
      filter(portfolio_risk, Tier == input$tier)
    }
  })
  
  output$riskPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = Avg_GTMI, y = Avg_Outcome, color = Tier, size = Projects)) +
      geom_point(alpha = 0.8) +
      geom_text_repel(aes(label = Country), size = 3, max.overlaps = 25, box.padding = 0.4, segment.color = "gray80") +
      geom_hline(yintercept = 2, linetype = "dashed", color = "gray60") +
      geom_vline(xintercept = 0.7, linetype = "dashed", color = "gray60") +
      scale_size_continuous(range = c(3, 10)) +
      labs(
        title = "Portfolio Risk Matrix: GTMI vs. Project Outcome",
        subtitle = "Countries grouped by recommended investment strategy",
        x = "Average GTMI (2022)",
        y = "Average Project Outcome Score",
        color = "Investment Tier",
        size = "Number of Projects"
      ) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom")
  })
  
  output$countryTable <- renderDT({
    datatable(filtered_data(), options = list(pageLength = 10, scrollX = TRUE))
  })
}

# Run the app
shinyApp(ui = ui, server = server)

