### Teacher Dashboard App User Interface -----

ui <- page_fluid(
  
  ### Page Header -----
  card(
    full_screen = FALSE,
    style = "padding: 1.5rem; background-color: #f8f9fa; border: none;",
    div(
      style = "text-align: center;",
      h2("Teacher Dashboard"),
      p(style = "color: #666; font-size: 1.1rem;", "Feb 2024 – Jun 2025")
    )
  ),
  
  ### Layout of 3 Main Filters -----
  layout_columns(
    ## Proficiency Filter ----
    card(
      full_screen = FALSE,
      style = "max-height: 200px; overflow: auto;",
      div(
        style = "text-align: center;",
        
        # Check box Row 01 ---
        checkboxGroupButtons(
          inputId = "level_filter_row1",
          label = "Choose Proficiencies",
          choices = c("A1", "A2"),
          selected = c("A1", "A2"),
          status = "primary",
          direction = "horizontal",
          checkIcon = list(
            yes = icon("check-circle"),
            no = icon("")  )  ),
        
        # Check box Row 02 ---
        checkboxGroupButtons(
          inputId = "level_filter_row2",
          label = NULL,
          choices = c("B1", "B2"),
          selected = c("B1", "B2"),
          status = "primary",
          direction = "horizontal",
          checkIcon = list(
            yes = icon("check-circle"),
            no = icon("")  )  )
      )
    ), #Card
    
    ## Date Filter ----
    card(
      full_screen = FALSE,
      style = "max-height: 200px; overflow: auto;",
      div(
        style = "
           display: flex;
           flex-direction: column;
            align-items: center;
           justify-content: center;
           height: 100%;
           text-align: center;
          ",
        
        # Date Layout Filter ---
        selectInput("date_layout", "Choose Accuracy of Graphics",
                    choices = c("Years", "Months", "Weeks", "Days"),
                    selected = "Months"),
        
        # Date Range Filter ---
        dateRangeInput("date_range", "Choose Time Range",
                       start = as.Date("2024-01-01"),
                       end = as.Date("2025-07-31") )
      )
    ), #Card
    
    ## Student Filter ----
    card(
      full_screen = FALSE,
      style = "max-height: 200px; overflow: auto; text-align: center;",
      "Filter by Students", # Text for Checkboxes..
      
      # All/None Buttons ---
      div(
        style = "display: flex; justify-content: center; gap: 10px; margin-bottom: 10px;",
        actionButton("select_all_students", "Select All"),
        actionButton("select_no_students", "Select None")
      ),
      
      # Unique Students Check box [helper.R - Shiny Preparations] ---
      div(
        style = "display: inline-block; column-count: 3; column-gap: 20px; text-align: left;",
        checkboxGroupInput(
          inputId = "student_filter",
          label = NULL,
          choices = student_names,
          selected = student_names)
      )
    ) #Card
  ), #layout_columns
  
  ### Layout of 6 KPI's ----
  layout_columns(
    style="text-align: center;",
    
    ## KPI 01 - Total Hours ----
    uiOutput("total_hours_box"),
    
    ## KPI 02 - Total Sessions ----
    uiOutput("total_sessions_box"),
    
    ## KPI 03 - Ratio Trial Classes ----
    uiOutput("ratio_trial_box"),
    
    ## KPI 04 - Total Students ----
    uiOutput("total_students_box"),
    
    ## KPI 05 - Ø Hours / Week ----
    uiOutput("total_hours_week_box"),
    
    ## KPI 06 - Ø Hours / Month ----
    uiOutput("total_hours_month_box")
  ), 
  
  ### Layout of 6 PLOTS -----
  layout_columns(
    col_widths = c(6, 6),
    
    ## 3 Line PLOTS, Vertically ----
    div(
      card(plotOutput("total_students_plot")),
      card(plotOutput("total_sessions_plot")),
      card(plotOutput("total_hours_plot"))
    ),
    
    ## 3 Var - Type PLOTS, Vertically ----
    div(
      card(plotOutput("proficiency_distribution_plot")),
      card(plotOutput("total_proficiency_destirbution_plot")),
      card(plotOutput("total_hours_by_student_plot"))
    )
  ) # layout_columns
) # page_fluid