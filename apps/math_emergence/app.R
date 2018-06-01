library(shiny)
library(tidyverse)
library(scales)

ui <- fluidPage(

  titlePanel("Emergence of Math Subjects by Grade"),

  inputPanel(
    selectInput(
      "grade",
      label = "Grade",
      choices = c("K", 1:5, "6+"), selected = "K"
    )
  ),
  mainPanel(
    plotOutput("distPlot")
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  grade_summary <- read_csv("grade_summary.csv") %>%
    mutate(
      source = factor(
        source,
        levels = c(
          "All grade 6+",
          paste("All grade", 5:1),
          "All grade K"
        )
      )
    )

  subjects <- c("Division", "Multiplication", "Subtraction", "Addition")

  subject_data <- grade_summary %>%
    filter(field_label %in% subjects) %>%
    mutate(
      field_label = factor(
        field_label,
        levels = subjects
      )
    )

  output$distPlot <- renderPlot({
    ggplot(
      data = filter(subject_data, grade == input$grade),
      aes(
        x = field_label,
        y = mu,
        fill = field_label
      )
    ) +
      geom_bar(
        stat = "identity",
        width = 0.7,
        position = "dodge",
        show.legend = F
      ) +
      geom_text(
        aes(
          x = field_label,
          y = mu + 0.03,
          label = scales::percent(round(mu, 2))
        ),
        vjust = 0.5,
        position = position_dodge(0.7),
        size = 6
      ) +
      scale_fill_brewer(palette = "Set3") +
      guides(fill = guide_legend(reverse = T)) +
      ggtitle(
        paste0("Grade ", input$grade,
              ": N = ",
              unique(subject_data$n[subject_data$grade == input$grade]))
        ) +
      coord_flip() +
      scale_y_continuous(
        breaks = seq(from = 0, to = 1, by = 0.2),
        limits = c(0, 1.1),
        labels = scales::percent(seq(from = 0, to = 1, by = 0.2))
      ) +
      theme_classic() +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.title = element_blank(),
        strip.text = element_text(size = 6),
        axis.line.x = element_line(size = 0.5, color = "black"),
        plot.background = element_rect(fill = NA, color = "black"),
        legend.text = element_text(size = 6),
        legend.key.height = unit(8, "mm"),
        axis.text.y = element_text(size = 10, color = "black")
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)

