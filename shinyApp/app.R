# Load packages ----------------------------------------------------------------

library(shiny)
library(tidyverse)
library(ggthemes)
library(scales)
library(bslib)
library(bsicons)
library(rsconnect)

# Load data --------------------------------------------------------------------

test_wider <- read_csv('data/test_admindist_wider.csv')
ssi_percent_district <- read_csv('data/ssi_percent_district.csv')

ssi_percent_joined <- ssi_percent_district |>
  left_join(
    test_wider,
    by = join_by(school_district == sedaadminname, state == stateabb)
  )

# Find all industries ----------------------------------------------------------

subgroup_choices <- test_wider |>
  distinct(subgroup) |>
  arrange(subgroup) |>
  pull(subgroup)

# Randomly select a subgroup -----------------------------------

selected_subgroup <- "all"

# Define UI --------------------------------------------------------------------

ui <- page_navbar(
  title = "The Pandemic and Academic Performance in the US",
  bg = "#005587",
  sidebar = sidebar(
    selectizeInput(
      inputId = "subgroup",
      label = "Select one of the following subgroups:",
      multiple = FALSE,
      choices = subgroup_choices,
      selected = selected_subgroup,
      options = list(plugins = "remove_button")
    ),
    "Data comes from the Educational Opportunity Project at Stanford University."
  ),
  header = uiOutput("selected_subgroup"),
  nav_spacer(),
  nav_panel(
    title = "Math vs Reading Scores",
    card(
      card_body(plotOutput(outputId = "reading_vs_math_plot"))
    )
  ),
  
  nav_panel(
    title = "Academic Peformance by Subgroup",
    card(
      card_body(plotOutput(outputId = "ssi_vs_math_plot"))
    )
  ),

  nav_panel(
    title = "Data",
    card(card_body(DT::dataTableOutput(outputId = "data"))),
  ),
  footer = "Showing only results for school districts in the US."
  
)

# Define server function -------------------------------------------------------

server <- function(input, output, session) {

  # Filter data for selected subgroup
  test_wider_filtered <- reactive({
    test_wider |>
      filter(subgroup == input$subgroup)
  })
  
  ssi_percent_joined_filtered <- reactive({
    ssi_percent_joined |>
      filter(subgroup == input$subgroup)
  })
  
  # Plot of reading vs math scores
  output$reading_vs_math_plot <- renderPlot({
    test_wider_filtered() |>
      ggplot(aes(x = gys_mn_2023_ol_mth, y = gys_mn_2023_ol_rla)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      labs(
        x = "Math Scores",
        y = "Reading Scores",
        title = "Change in Reading vs Math Performance from 2019 to 2023"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)
      )
  })
  
  # Plot of math scores vs ssi percentage
  output$ssi_vs_math_plot <- renderPlot({
    ssi_percent_joined_filtered() |>
      ggplot(aes(x = ssi_percent, y = gys_mn_2023_ol_mth)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      labs(
        x = "Percentage of Families Receiving SSI",
        y = "Math Scores",
        title = "Change in Math Performance from 2019 to 2023 vs Percentage of Families Receiving SSI"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)
      )
  })

  
  output$data <- DT::renderDataTable({
    test_wider_filtered() |>
      select(
        sedaadminname, stateabb, 
        gys_mn_2019_ol_mth, gys_mn_2019_ol_rla, 
        gys_mn_2022_ol_mth, gys_mn_2022_ol_rla,
        gys_mn_2023_ol_mth, gys_mn_2023_ol_rla)
  })
}


# Create the Shiny app object --------------------------------------------------

shinyApp(ui = ui, server = server)


