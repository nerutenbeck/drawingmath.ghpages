
library(shiny)
library(scales)
library(tidyverse)

field_types <- c(
  "Student Attributes",
  "Teacher Attributes",
  "Type of Math Depicted",
  "Tools and Technology Depicted",
  "Furniture"
)

# Define UI for application that draws a histogram
ui <- fluidPage(

   # Application title
   titlePanel("Drawing Math Image Features"),

   inputPanel(
     selectInput("type", label = "Image Features",
                 choices = c(field_types), selected = 1)
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

   output$distPlot <- renderPlot({
     ggplot(
       data = filter(grade_summary, field_type == input$type),
       aes(
         x = field_label,
         y = mu,
         fill = source
       )
     ) +
       geom_bar(
         aes(
           x = field_label,
           y = mu
         ),
         stat = "identity",
         width = 0.7,
         position = "dodge"
       ) +
       # geom_text(
       #   aes(
       #     x = field_label,
       #     y = mu + 0.05,
       #     label = scales::percent(round(mu, 2))
       #   ),
       #   vjust = 0.5,
       #   position = position_dodge(0.7),
       #   size = 1
       # ) +
       ggtitle(input$type) +
       guides(fill = guide_legend(reverse = T)) +
       coord_flip() +
       scale_fill_brewer(palette = "YlGn", direction = -1) +
       scale_y_continuous(
         breaks = seq(from = 0, to = 1, by = 0.2),
         limits = c(0, 1.1),
         labels = scales::percent(seq(from = 0, to = 1, by = 0.2))
       ) +
       theme(
         axis.text.y = element_text(size = 10, color = "black"),
         axis.title.x = element_blank(),
         axis.title.y = element_blank(),
         axis.ticks.y = element_blank(),
         legend.title = element_blank(),
         strip.text = element_text(size = 6),
         axis.line.x = element_line(size = 0.5, color = "black"),
         plot.background = element_rect(fill = NA, color = "black")
       )
   }, height = 850
   )
}

# Run the application
shinyApp(ui = ui, server = server)

