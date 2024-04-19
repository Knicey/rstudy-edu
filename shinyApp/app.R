# Load packages ----------------------------------------------------------------

library(shiny)
library(tidyverse)
library(ggthemes)
library(scales)
library(bslib)
library(bsicons)
library(rsconnect)
library(dplyr)

# Load data --------------------------------------------------------------------

#Helper Functions -------------------------------------------------------------

convert_percentage <- function(x){
  as.numeric(sub("%", "", x, fixed=TRUE))/100
}

#Raw Dataset For Academic Performance -----------------------------------------
test_admindist_gys <- read_csv('data/seda2023_admindist_poolsub_gys_updated_20240205.csv')

test_selected_years <- test_admindist_gys |>
  select(
    sedaadmin, sedaadminname, stateabb, subject, subgroup, 
    gys_mn_2019_ol, gys_mn_2022_ol, gys_mn_2023_ol
  ) 

test_longer <- test_selected_years |>
  pivot_longer(
    cols = c(gys_mn_2019_ol, gys_mn_2022_ol, gys_mn_2023_ol),
    names_to = "year",
    values_to = "score"
  ) |>
  mutate(
    year = str_sub(year, -7, -4)
  )

test_wider <- test_selected_years |>
  pivot_wider(
    names_from = subject,
    values_from = c(gys_mn_2019_ol, gys_mn_2022_ol, gys_mn_2023_ol)
  )

#Cleaned Dataset for Income ----------------------------------------------------
acs_income_2021 <- read_csv('data/acs_income_2021.csv') 

acs_income_2021_narrow <- acs_income_2021 |>
  rename(
    ssi_percent = household_income_all_households_with_social_security_income,
    wages_salary = household_income_all_households_with_earnings_with_wages_or_salary_income,
    public_assistance = household_income_all_households_with_cash_public_assistance_income_or_food_stamps_snap,
    retirement = household_income_all_households_with_retirement_income,
  ) |>
  select_if(~ (is.character(.x))) |>
  relocate(state)

#Turn all percentages to numeric ---------------------------------------------
acs_income_2021_narrow[,-(1:2)] <- apply(acs_income_2021_narrow[, -(1:2)],2, function(x){
  as.numeric(sub("%", "", x, fixed=TRUE))/100})

#Join income and test data ---------------------------------------------------
acs_income_2021_joined <- acs_income_2021_narrow |>
  left_join(
    test_longer,
    by = join_by(school_district == sedaadminname, state == stateabb)
  )


# Find all choices ----------------------------------------------------------

subgroup_choices <- test_wider |>
  distinct(subgroup) |>
  arrange(subgroup) |>
  pull(subgroup)

subject_choices <- test_selected_years |>
  distinct(subject) |>
  arrange(subject) |>
  pull(subject)

income_choices <- acs_income_2021_narrow |>
  select(
    ssi_percent,
    wages_salary,
    public_assistance,
    retirement
  )  |>
  colnames()

#Take out year 2019 (this is the basis of comparison) ------------------------
year_choices <- test_longer |>
  distinct(year) |>
  arrange(year) |>
  filter(year != "2019") |>
  pull(year)

# Select default values -----------------------------------------------------

selected_subgroup <- "all"

selected_subject <- sample(subject_choices, 1)

selected_year <- "2022"

selected_income <- sample(income_choices, 1)

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
    selectizeInput(
      inputId = "year",
      label = "Select a year:",
      multiple = FALSE,
      choices = year_choices,
      selected = selected_year,
      options = list(plugins = "remove_button")
    ),
    selectizeInput(
      inputId = "subject",
      label = "Select a subject:",
      multiple = FALSE,
      choices = subject_choices,
      selected = selected_subject,
      options = list(plugins = "remove_button")
    ),
    selectizeInput(
      inputId = "income",
      label = "Select an economic variable:",
      multiple = FALSE,
      choices = income_choices,
      selected = selected_income,
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
    title = "Academic Peformance by Economic Variable",
    card(
      card_body(plotOutput(outputId = "ecnomic_vs_score_plot"))
    )
  ),

  nav_panel(
    title = "Data",
    card(card_body(DT::dataTableOutput(outputId = "data")))
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
  
  acs_income_joined_filtered <- reactive({
    acs_income_2021_joined |>
      filter(
        subgroup == input$subgroup, 
        subject == input$subject, 
        year == input$year
      )
  })
  
  # Plot of reading vs math scores
  output$reading_vs_math_plot <- renderPlot({
    test_wider_filtered() |>
      ggplot(aes(x = gys_mn_2023_ol_mth, y = gys_mn_2023_ol_rla)) +
      geom_point(aes(color = gys_mn_2023_ol_rla)) +
      #scale_color_gradientn(
      #  colors = c('white', "skyblue", 'red'),
      #  values = c(0, -1, 1)
      #) +
      scale_color_gradientn(
        colors = c("red", "#FFFFE4", "skyblue"), 
        limits = c(-7, 7)
      ) +
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
      ) +
      scale_y_continuous(limits = c(-7, 7) ) +
      scale_x_continuous(limits = c(-7, 7) )
  })
  
  # Plot of test scores vs economic variable
  output$ecnomic_vs_score_plot <- renderPlot({
    acs_income_joined_filtered() |>
      ggplot(aes(x = get(input$income), y = score)) +
      geom_point(aes(color = score)) +
      scale_color_gradientn(colors = c('white', "skyblue", 'red'),
                            values = c(0, -1, 1)) +
      geom_smooth(method = "lm", se = FALSE) +
      labs(
        x = paste("Percentage of Families", input$income),
        y = paste(input$subject , "Scores"),
        title = paste(
          "Change in", 
          input$subject,
          "Performance from 2019 to", 
          input$year, 
          "vs",
          input$income
          ),
        subtitle = paste("Among", input$subgroup, "students")
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)
      ) +
      scale_y_continuous(limits = c(-7, 7) )
  })

  
  output$data <- DT::renderDataTable({
    acs_income_joined_filtered() 
  })
}


# Create the Shiny app object --------------------------------------------------

shinyApp(ui = ui, server = server)


