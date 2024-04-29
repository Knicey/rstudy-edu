# Load packages ----------------------------------------------------------------

library(shiny)
library(tidyverse)
library(ggthemes)
library(scales)
library(bslib)
library(bsicons)
library(rsconnect)
library(janitor)

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
  ) |>
  mutate(
    subgroup = case_match(
      subgroup,
      "all" ~ "All Students",
      "ecd" ~ "Economically Disadvantaged",
      "blk" ~ "Black",
      "wht" ~ "White",
      "hsp" ~ "Hispanic",
      "nec" ~ "Non-Economically Disadvantaged",
    ),
    
    subject = case_match(
      subject,
      "mth" ~ "Mathematics",
      "rla" ~ "Reading"
    )
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

test_wider <- test_longer |>
  pivot_wider(
    names_from = subject,
    values_from = score
  ) 

#Cleaned Dataset for Income ----------------------------------------------------
acs_income_2021 <- read_csv('data/acs_income_2021.csv') 

acs_income_2021_cleaned <- acs_income_2021 |>
  rename(
    ssi_percent = household_income_all_households_with_social_security_income,
    wages_salary = household_income_all_households_with_earnings_with_wages_or_salary_income,
    #public_assistance = household_income_all_households_with_cash_public_assistance_income_or_food_stamps_snap,
    retirement = household_income_all_households_with_retirement_income,
  ) |>
  select_if(~ (is.character(.x))) |>
  relocate(state)

#Dataset for Child Characteristics -------------------------------------------
#acs_child_2021 <- read_csv('data/acs_child_characteristics_2021.csv', skip = 1)
#Lacks Porportional Data, may not use

#Turn all percentages to numeric ---------------------------------------------
acs_income_2021_cleaned[,-(1:2)] <- apply(acs_income_2021_cleaned[, -(1:2)],2, function(x){
  as.numeric(sub("%", "", x, fixed=TRUE))/100})

#Join income and test data ---------------------------------------------------
acs_income_2021_joined <- acs_income_2021_cleaned |>
  left_join(
    test_wider,
    by = join_by(school_district == sedaadminname, state == stateabb)
  ) |>
  select(
    state, 
    school_district, 
    subgroup, 
    year, 
    Mathematics, 
    Reading, 
    "Percentage of Families Receiving Social Security Income" = ssi_percent, 
    "Percentage of Families with Earnings from Salaries" = wages_salary, 
    #"Percentage of Families with Public Assistance Income or Food Stamps" = public_assistance, 
    "Percentage of Families with Retirement Income" = retirement
  )

acs_income_2021_joined[7:ncol(acs_income_2021_joined)]


# Find all choices ----------------------------------------------------------

subgroup_choices <- test_wider |>
  distinct(subgroup) |>
  arrange(subgroup) |>
  pull(subgroup)

subject_choices <- test_selected_years |>
  distinct(subject) |>
  arrange(subject) |>
  pull(subject)

income_choices <- acs_income_2021_joined[7:ncol(acs_income_2021_joined)] |>
  colnames()

#Calculate the year choices --------------------------------------------------
year_choices <- test_longer |>
  distinct(year) |>
  arrange(year) |>
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
    
    "All scores are calculated as a grade level difference from the 2019 national average in the US"
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
      card_body(plotOutput(outputId = "economic_vs_score_plot"))
    )
  ),

  nav_panel(
    title = "Data",
    card(card_body(DT::dataTableOutput(outputId = "data"))),
    downloadButton("downloadAllData", "Download All Data")
    
  ),
  
  nav_panel(
    title = "About",
    card(
      card_body(
        h3("About This Project"),
        p("This app allows you to explore the relationship between academic performance and socioeconomic factors in US school districts."),
        p("Use the sidebar to select a subgroup, year, subject, and economic variable. The app will display a scatter plot of reading vs math scores and a scatter plot of test scores vs the selected economic variable."),
        p("The data comes from the Educational Opportunity Project at Stanford University and the 2021 American Community Survey.")
      )
    )
  ),
  
  #footer = paste("Showing only results for school districts in the US.", "There are", textOutput(outputId = "num_schools"), "school districts in the dataset.")
  footer = textOutput(outputId = "num_schools")
)

# Define server function -------------------------------------------------------

server <- function(input, output, session) {

  # Filter data for selected subgroup
  test_wider_filtered <- reactive({
    test_wider |>
      filter(
        subgroup == input$subgroup,
        year == input$year
        )
  })
  
  acs_income_joined_filtered <- reactive({
    acs_income_2021_joined |>
      filter(
        subgroup == input$subgroup,
        year == input$year
      )
  })
  
  output$num_schools <- renderText({
    paste(
      "Showing",
      length(unique(acs_income_joined_filtered()$school_district)), 
      "school districts in the US."
      )
  })
  
  # Plot of reading vs math scores
  output$reading_vs_math_plot <- renderPlot({
    test_wider_filtered() |>
      ggplot(aes(x = Mathematics, y = Reading)) +
      geom_point(aes(color = Reading)) +
      #scale_color_gradientn(
      #  colors = c('white', "skyblue", 'red'),
      #  values = c(0, -1, 1)
      #) +
      scale_color_gradientn(
        colors = c("red", "#FFFFE4", "blue"), 
        limits = c(-7, 7)
      ) +
      geom_smooth(method = "lm", se = FALSE) +
      labs(
        x = "Math Scores",
        y = "Reading Scores",
        title = paste("Reading vs Math Performance in", input$year),
        subtitle = paste("Among", input$subgroup, "Students"),
        color = "Change in Score"
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
  output$economic_vs_score_plot <- renderPlot({
    acs_income_joined_filtered() |>
      ggplot(aes(x = get(input$income), y = get(input$subject))) +
      geom_point(aes(color = get(input$subject))) +
      scale_color_gradientn(
        colors = c("red", "#FFFFE4", "blue"), 
        limits = c(-7, 7)
      ) +
      geom_smooth(method = "lm", se = FALSE) +
      labs(
        x = paste("Percentage of Families", input$income),
        y = paste(input$subject , "Scores"),
        title = paste(
          input$subject,
          "Performance in", 
          input$year, 
          "vs",
          input$income
          ),
        subtitle = paste("Among", input$subgroup, "students"),
        color = "Change in Score"
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
  
  output$downloadFilteredData <- downloadHandler(
    filename = function() {
      # Use the selected dataset as the suggested file name
      paste0("data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Write the dataset to the `file` that will be downloaded
      write.csv(acs_income_2021_joined_filtered(), file)
    }
  )
  
  output$downloadAllData <- downloadHandler(
    filename = function() {
      # Use the selected dataset as the suggested file name
      paste0("data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Write the dataset to the `file` that will be downloaded
      write.csv(acs_income_2021_joined, file)
    }
  )
  
}


# Create the Shiny app object --------------------------------------------------

shinyApp(ui = ui, server = server)


