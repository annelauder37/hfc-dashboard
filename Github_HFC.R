#### HFC Dashboard Example #####

# Load packages 
library(shiny)
library(dplyr)
library(readxl)
library(DT)
library(bs4Dash)
library(ggplot2)


# Load data if not directly connected to API 
labor_el1<-read_excel("G:\\Shared drives\\XX\\XX.xlsx")
labor_el1 <- labor_el1 %>%
  # Rename columns
  rename(duration = `_duration`, income_source0 = `income_source/0`, income_source1 = `income_source/1`,
         income_source2 = `income_source/2`, income_source3 = `income_source/3`, income_source4 = `income_source/4`,
         income_source5 = `income_source/5`, income_source6 = `income_source/6`, income_source888 = `income_source/888`) %>%
  mutate(
    anon_enum = paste0("enum_", sapply(enunamelabel, function(x) substr(digest(x, algo="md5"), 1, 6))), # Create anonymous key for enumerator name
    anon_ID   = paste0("hh_", sapply(ID, function(x) substr(digest(x, algo="md5"), 1, 6))) # Create anonymous key for HH ID
  )


# UI 
ui <- fluidPage(
  titlePanel("Labor EL Round 1 Data Quality Check"),
  tabsetPanel(
    # Tab 1: Duplicates
    tabPanel(
      "Duplicates",
      sidebarLayout(
        sidebarPanel(
          radioButtons(
            "type", "Select display:",
            choices = c("All Duplicate Entries", "List of Duplicate IDs"),
            selected = "All Duplicate Entries"
          ),
          downloadButton("download_duplicates", "Download Duplicates")
        ),
        mainPanel(
          DTOutput("duplicates_table")
        )
      )
    ),
    
    # Tab 2: Survey Length
    tabPanel(
      "Survey Length",
      sidebarLayout(
        sidebarPanel(
          radioButtons(
            "survey_type",
            "Select Survey Length:",
            choices = c("Short Surveys (<= 10 minutes)", "Long Surveys (> 74 minutes)"),
            selected = "Short Surveys (<= 10 minutes)"
          ),
          sliderInput(
            "short_duration_slider",
            "Filter by Duration (Short Surveys, in minutes):",
            min = 0, max = 10, value = 10, step = 1
          ),
          sliderInput(
            "long_duration_slider",
            "Filter by Duration (Long Surveys, in minutes):",
            min = 75, max = 1700, value = 75, step = 1
          )
        ),
        mainPanel(
          DTOutput("short_surveys_table")
        )
      )
    ),
    
    # Tab 3: Skipped Questions
    tabPanel(
      "Skipped Questions",
      plotOutput("skip")
    ),
    
    # Tab 4: Unable to Interview
    tabPanel(
      "Unable to Interview",
      DTOutput("interview")
    ),
    
    # Tab 5: Outliers
    tabPanel(
      "Outliers",
      sidebarLayout(
        sidebarPanel(
          radioButtons(
            "outlier_choices", "Select question:",
            choices = c("Age", "Household size"),
            selected = "Age"
          )
        ),
        mainPanel(
          plotOutput("outliers")
        )
      )
    ),
    
    # Tab 6: Expenses Flags
    tabPanel(
      "Expenses Flags",
      sidebarLayout(
        sidebarPanel(
          radioButtons(
            "exp_type",
            "Select display:",
            choices = c(
              "Percentage of households which reported 0 across all expenses grouped by enumerator", 
              "0 across all expenses entries"
            ),
            selected = "Percentage of households which reported 0 across all expenses grouped by enumerator"
          ),
          downloadButton("download_exp", "Download Expenses Flags")
        ),
        mainPanel(
          DTOutput("exp")
        )
      )
    ),
    
    # Tab 7: Saving
    tabPanel(
      "Saving",
      sidebarLayout(
        sidebarPanel(
          radioButtons(
            "saving_type",
            "Select display:",
            choices = c(
              "Percentage of surveys in which households reported not saving and non-Blumont income grouped by enumerator", 
              "No savings and non-Blumont income entries"
            ),
            selected = "Percentage of surveys in which households reported not saving and non-Blumont income grouped by enumerator"
          )
        ),
        mainPanel(
          uiOutput("saving_note"),
          DTOutput("saving")
        )
      )
    ),
    
    # Tab 8: Rates of 666 and -777
    tabPanel(
      "Rates of 666 and -777",
      sidebarLayout(
        sidebarPanel(
          radioButtons(
            "alt_options",
            "Select display:",
            choices = c(
              "Percentage of surveys which include at least one occurrence of don't know or unsure grouped by enumerator",
              "Percentage of individual observations of don't know or unsure grouped by enumerator"
            ),
            selected = "Percentage of surveys which include at least one occurrence of don't know or unsure grouped by enumerator"
          )
        ),
        mainPanel(
          DTOutput("alternative")
        )
      )
    ),
    
    # Tab 9: Income Source
    tabPanel(
      "Income Source: Households Reporting No Income",
      DTOutput("income_source"),
      uiOutput("note2")
    )
  )
)


# Server
server <- function(input, output, session) {
  # Reactive data for duplicates
  duplicates_data <- reactive({
    if (input$type == "All Duplicate Entries") { # Display if user selects first option
      labor_el1 %>%
        group_by(anon_ID) %>% # Group by anonymous HH ID
        filter(n() > 1) %>% # Filter HH IDs that appear more than once
        ungroup() %>%
        dplyr::select(anon_ID, anon_enum) %>% # Retain relevant columns
        arrange(anon_ID)
    } else { # Display if user selects second option
      labor_el1 %>%
        group_by(anon_ID) %>%
        filter(n() > 1) %>%
        ungroup() %>%
        distinct(anon_ID) # Retain only one copy of duplicate IDs 
    }
  })
  
  # Render duplicates table
  output$duplicates_table <- renderDT({
    datatable(duplicates_data())
  })
  
  # Download handler for duplicates
  output$download_duplicates <- downloadHandler(
    filename = function() { "duplicates.csv" }, # Specify file name
    content = function(file) { # Specify what will be downloaded
      write.csv(duplicates_data(), file, row.names = FALSE) # Specify file format (CSV)
    }
  )
  
  # Reactive data for short surveys
  output$short_surveys_table <- renderDT({
    survey_data <- labor_el1 %>%
      mutate(duration_minutes = duration / 60) %>%
      filter(if (input$survey_type == "Short Surveys (<= 10 minutes)") {
        duration_minutes <= input$short_duration_slider
      } else {
        duration_minutes > input$long_duration_slider
      })
    datatable(survey_data)
  })
  
  # Render plot for skipped questions
  output$skip <- renderPlot({
    labor_el1$skipped <- apply(labor_el1[, -1], 1, function(x) sum(is.na(x))) # Create a new skips column calculating how many values are missing
    
    average_skips <- labor_el1 %>%
      group_by(anon_enum) %>%
      dplyr::summarize(
        Total_Surveys = n(),  # Count the number of surveys per enumerator
        Total_Skipped_Questions = sum(skipped),  # Total skipped questions per enumerator
        Average_Skipped_Questions = mean(skipped)  # Average skipped questions per survey
      )
    
    # Create graph
    ggplot(average_skips, aes(x = anon_enum, y = Average_Skipped_Questions, fill = anon_enum)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = "Average Number of Skipped Questions per Enumerator", x = "Enumerator", y = "Average Number of Skipped Questions", fill = "Enumerator") +
      theme(axis.text.x = element_blank())
  })
  
  # Render unable to interview table
  output$interview <- renderDT({
    labor_el1 %>%
      filter(interview == 0) %>%
      dplyr::select(anon_enum, anon_ID, interview, interview_reason, interview_reason_888_ar, cohort, camp) %>%
      arrange(camp)
  })
  
  # Render outliers boxplot
  output$outliers <- renderPlot({
    if (input$outlier_choices == "Age") {
      boxplot(labor_el1$age_new)
      title("Age Range (If respondent stated age listed is incorrect)")
    } else {
      labor_el1$hh_size_new<-as.numeric(labor_el1$hh_size_new)
      boxplot(labor_el1$hh_size_new)
      title("Household Size (If respondent stated that household size listed is incorrect)")
    }
  })
  
  # Percentage of each enumerator's surveys (of those they conducted) that has 0 across all expenses questions
  output$exp <- renderDT({
    if (input$exp_type == "0 across all expenses entries") {
      exp<-labor_el1 %>%
        filter(exp_1 == 0 & exp_2 == 0 & exp_3 == 0 & exp_4 == 0 & exp_5 == 0 & exp_6 == 0 & exp_7 == 0) %>%
        dplyr::select(anon_enum, ID, exp_1, exp_2, exp_3, exp_4, exp_5, exp_6, exp_7, income_total)
    } else {
      exp<-labor_el1 %>%
        filter(exp_1 == 0 & exp_2 == 0 & exp_3 == 0 & exp_4 == 0 & exp_5 == 0 & exp_6 == 0 & exp_7 == 0) %>%
        dplyr::select(anon_enum, exp_1, exp_2, exp_3, exp_4, exp_5, exp_6, exp_7)
      exp <- exp %>%
        group_by(anon_enum) %>%
        dplyr::summarize(SurveysMeetingCondition = n()) %>%
        left_join(
          labor_el1 %>%
            group_by(anon_enum) %>%
            dplyr::summarize(TotalSurveys = n()),
          by = "anon_enum"
        ) %>%
        mutate(
          Percentage = round(SurveysMeetingCondition / TotalSurveys * 100, 2)
        ) %>%
        arrange(Percentage)
    }
  })
  output$download_exp <- downloadHandler(
    filename = function() { "expenses.csv" }, # Specify file name
    content = function(file) {
      write.csv(exp(), file, row.names = FALSE) # Specify file format (CSV)
    }
  )
  
  # Render note for saving tab
  output$saving_note <- renderUI({
    saving <- labor_el1 %>%
      filter(saving == 0 & (
        income_source0 == TRUE |
          income_source1 == TRUE |
          income_source2 == TRUE |
          income_source3 == TRUE |
          income_source4 == TRUE |
          income_source5 == TRUE |
          income_source6 == TRUE |
          income_source888 == TRUE
      )) %>%
      select(anon_enum, ID, saving, income_source0, income_source1, income_source2, income_source3, income_source4, income_source5, income_source6, income_source888)
    
    if (nrow(saving) == 0) { # Note if the number of rows in saving equals 0
      HTML("<p style='color:red;'>There are no households that reported not saving any income and selected only non-Blumont income sources.</p>")
    } else { # Note if the number of rows in saving does NOT equal 0
      HTML("<p style='color:red;'>This displays households that reported not saving any income and selected only non-Blumont income sources. This is used to assess potential enumerator manipulation, as this skips to Section E.</p>")
    }
  })
  
  # Render saving tab 
  output$saving <- renderDT({
    if (input$saving_type == "Percentage of surveys in which households reported not saving and non-Blumont income grouped by enumerator") {
      saving <- labor_el1 %>%
        filter(saving == 0 & (
          income_source0 == TRUE |
            income_source2 == TRUE |
            income_source3 == TRUE |
            income_source4 == TRUE |
            income_source5 == TRUE |
            income_source6 == TRUE |
            income_source888 == TRUE
        )) %>%
        select(anon_enum, income_source0, income_source2, income_source3, income_source4, income_source5, income_source6, income_source888)
      saving <- saving %>%
        group_by(anon_enum) %>%
        dplyr::summarize(SurveysMeetingCondition = n()) %>%
        left_join(
          labor_el1 %>%
            group_by(anon_enum) %>%
            dplyr::summarize(TotalSurveys = n()),
          by = "anon_enum"
        ) %>%
        mutate(
          Percentage = round(SurveysMeetingCondition / TotalSurveys * 100, 2)
        ) %>%
        arrange(Percentage)
    } else {
      saving <- labor_el1 %>%
        filter(saving == 0 & (
          income_source0 == TRUE |
            income_source2 == TRUE |
            income_source3 == TRUE |
            income_source4 == TRUE |
            income_source5 == TRUE |
            income_source6 == TRUE |
            income_source888 == TRUE
        )) %>%
        select(anon_enum, ID, saving, income_source0, income_source1, income_source2, income_source3, income_source4, income_source5, income_source6, income_source888)
    }
  })
  output$alternative<-renderDT({
    if (input$alt_options == "Percentage of surveys which include at least one occurrence of don't know or unsure grouped by enumerator") {
      alt <- labor_el1 %>%
        filter(exp_1 == -777 | exp_2 == -777 | exp_3 == -777 |
                 exp_4 == -777 | exp_5 == -777 | exp_6 == -777 |
                 exp_7 == -777 | income_1 == -777 | income_2 == -777 |
                 income_3 == -777 |income_4 == -777 | income_5 == -777 |
                 income_6 == -777 | saving_amount == -777) %>%
        select(anon_enum, exp_1, exp_2, exp_3, exp_4, exp_5, exp_6, exp_7,income_1, income_2, income_3, income_4, income_5, income_6, saving_amount)
      alt <- alt %>%
        group_by(anon_enum) %>%
        dplyr::summarize(SurveysMeetingCondition = n()) %>%
        left_join(
          labor_el1 %>%
            group_by(anon_enum) %>%
            dplyr::summarize(TotalSurveys = n()),
          by = "anon_enum"
        ) %>%
        mutate(
          Percentage = round(SurveysMeetingCondition / TotalSurveys * 100, 2)
        ) %>%
        arrange(Percentage)
      
    } else {
      target_codes<-c(666, -777)
      alt <- labor_el1 %>%
        group_by(anon_enum) %>%
        summarize(
          TotalTargetCodes = sum(sapply(across(everything()), function(x) sum(x %in% target_codes, na.rm = TRUE))), # Total number of -666 and -777 entries
          TotalSurveys = n(), # Total number of surveys
          Percentage = round((TotalTargetCodes / TotalSurveys) * 100, 2) # Percentage of each enumerator's cells that meet criteria
        ) %>%
        arrange(desc(Percentage))
    }
  })
  
  # Income source tab 
  output$income_source<-renderDT({
    income_source_none<-labor_el1 %>%
      filter(income_source == 0) %>%
      select(anon_enum, income_source)
    income_source_none<-income_source_none %>%
      group_by(anon_enum) %>%
      dplyr::summarize(SurveysMeetingCondition = n()) %>%
      left_join( # Merge data
        labor_el1 %>%
          group_by(anon_enum) %>%
          dplyr::summarize(TotalSurveys = n()),
        by = "anon_enum"
      ) %>%
      mutate(
        Percentage = round(SurveysMeetingCondition / TotalSurveys * 100,2) # Percentage of each enumerator's surveys where respondent selected no income
      ) %>%
      arrange(Percentage)
  })
  output$note2<-renderUI({
    tags$p("Note: This shows the percentage of households that reported no income grouped by enumerator. This repsonse skips to the last section, so this is used to identify potential enumerator manipulation.")
  })
}


# Run the app
shinyApp(ui = ui, server = server)
