### Teacher Dashboard App Server -----

server <- function(input, output, session) {
  
  ### All/None Buttons -----
  observeEvent(input$select_all_students, {
    updateCheckboxGroupInput(session, "student_filter", selected = student_names)})
  observeEvent(input$select_no_students, {
    updateCheckboxGroupInput(session, "student_filter", selected = character(0))})
  
  ### Reactive Selection [All 3 Filters] -----
  selected_proficiencies <- reactive({c(input$level_filter_row1, input$level_filter_row2)})
  selected_date_range <- reactive({input$date_range})
  selected_students <- reactive({input$student_filter})
  
  ### Reactive Subset of calendar data [All 3 Filters] -----
  filtered_data <- reactive({
    req(                           # Connecting Reactive Selection ---
      selected_proficiencies(), 
      selected_date_range(),
      selected_students()
    )
    calendar %>%                  # The actual filter code ---
      filter(
        Proficiency %in% selected_proficiencies(), 
        Student %in% selected_students(), # %in%, because several values ---
        Start_Time >= selected_date_range()[1],
        Start_Time <= selected_date_range()[2]
      )
  })
  
  ### KPI 01 - Total Hours -----
  output$total_hours_box <- renderUI({
    total_hours <- filtered_data() %>% 
      summarise(
        hours = sum(Duration_Hours, na.rm = TRUE)
        ) %>% # like helper.R
      pull(hours)
    value_box(
      title = "Total Hours",
      value = round(total_hours,0),
      showcase = bs_icon("clock-history"),
      theme = "cyan"
    )
  })
  
  ### KPI 02 - Total Sessions ----
  output$total_sessions_box <- renderUI({
    total_sessions <- nrow(filtered_data()) # like helper.R
    value_box(
      title = "Total Sessions",
      value = total_sessions,
      showcase = bs_icon("journal-text"), 
      theme = "primary"
    )
  })
  
  ### KPI 03 - Ratio Trial Classes -----
  output$ratio_trial_box <- renderUI({
    ratio_trial <- 
      paste0(round(100*(sum(
        filtered_data()$Student == "试听") / nrow(filtered_data())
        # 试听 = Trial Class
      ), digits=0), "%") # like helper.R
    value_box(
      title = "Ratio Trial Classes",
      value = ratio_trial,
      showcase = bs_icon("diagram-3"),
      theme = "secondary")
  })
  
  ### KPI 04 - Total Students -----
  output$total_students_box <- renderUI({
    total_students <- n_distinct(filtered_data()$Student) # like helper.R
    value_box(
      title = "Students",
      value = total_students,
      showcase = bs_icon("people-fill"),
      theme = "white")
  })
  
  ### KPI 05 - Ø Hours / Week -----
  
  ## Reactive Subset of Filtered Data ----
  filtered_weekly_summary <- reactive({
    filtered_data() %>% 
      group_by(Week) %>% 
      summarise(Total_Hours = sum(Duration_Hours, na.rm = TRUE)) %>% 
      arrange(Week)
    
  })  
  ## KPI Box Rendering ----
  output$total_hours_week_box <- renderUI({
    total_hours_week <- 
      round(mean(filtered_weekly_summary()$Total_Hours), digits=0)
    value_box(
      title = "Ø Hours / Week",
      value = total_hours_week, 
      showcase = bs_icon("calendar-week"),
      theme = "cyan")
  })

  ### KPI 06 - Ø Hours / Month ----- 
  
  ## Reactive Subset of Filtered Data ----
  filtered_monthly_summary <- reactive({
    filtered_data() %>% 
      group_by(Month) %>% 
      summarise(Total_Hours = sum(Duration_Hours, na.rm = TRUE)) %>% 
      arrange(Month)
  })  
  
  ## KPI Box Rendering ----
  output$total_hours_month_box <- renderUI({
    total_hours_month <- 
      round(mean(filtered_monthly_summary()$Total_Hours), digits=0)
    value_box(
      title = "Ø Hours / Month",
      value = total_hours_month, 
      showcase = bs_icon("calendar-month"),
      theme = "primary")
  })
  
  
  
  
  ### PLOT 1 - Number of Students -----
  
  ## Reactive Subset 1 of Filtered Data ----
  # Year/Month/Week/Day, User’s date layout choice. ---
  filtered_students_reactive <- reactive({
    if(input$date_layout == "Years") {
      filtered_data() %>% # like helper.R
        group_by(Year) %>% 
        summarise(Student_Count = n_distinct(Student)) %>% 
        arrange(Year)
    } else if(input$date_layout == "Months") {
      filtered_data() %>% 
        group_by(Month) %>% 
        summarise(Student_Count = n_distinct(Student)) %>% 
        arrange(Month)
    } else if(input$date_layout == "Weeks") {
      filtered_data() %>% 
        group_by(Week) %>% 
        summarise(Student_Count = n_distinct(Student)) %>% 
        arrange(Week)
    } else {
      filtered_data() %>% 
        group_by(Day) %>% 
        summarise(Student_Count = n_distinct(Student)) %>% 
        arrange(Day)
    } 
  })
  
  ## Render PLOT 1 ----
  # Year/Month/Week/Day, User’s date layout choice. ---
  output$total_students_plot <- renderPlot({
    if(input$date_layout == "Years") {
      ggplot(filtered_students_reactive()) +
        geom_line(mapping=aes(x=Year, y=Student_Count),
                  color="steelblue", linewidth=1.1) +
        theme_minimal() + 
        labs(x="", y="", title="Student Numbers") +
        theme(plot.title = element_text(
          hjust = 0.5, size = 16, face = "bold")) +
        scale_x_datetime(date_labels = "%Y")
    } else if(input$date_layout == "Months") {
      ggplot(filtered_students_reactive()) +
        geom_line(mapping=aes(x=Month, y=Student_Count),
                  color="steelblue", linewidth=1.1) +
        theme_minimal() + 
        labs(x="", y="", title="Student Numbers") +
        theme(plot.title = element_text(
          hjust = 0.5, size = 16, face = "bold")) +
        scale_x_datetime(date_labels = "%Y %b")
    } else if(input$date_layout == "Weeks") {
      ggplot(filtered_students_reactive()) +
        geom_line(mapping=aes(x=Week, y=Student_Count),
                  color="steelblue", linewidth=1.1) +
        theme_minimal() + 
        labs(x="", y="", title="Student Numbers") +
        theme(plot.title = element_text(
          hjust = 0.5, size = 16, face = "bold")) +
        scale_x_datetime(date_labels = "%y' %b W%V")
    } else {
      ggplot(filtered_students_reactive()) +
        geom_line(mapping=aes(x=Day, y=Student_Count),
                  color="steelblue", linewidth=1.1) +
        theme_minimal() + 
        labs(x="", y="", title="Student Numbers") +
        theme(plot.title = element_text(
          hjust = 0.5, size = 16, face = "bold")) +
        scale_x_datetime(date_labels = "%m.%d %a")
    }
  })
  
  ### PLOT 2 - Number of Sessions -----
  
  ## Reactive Subset 2 of Filtered Data ----
  # Year/Month/Week/Day, User’s date layout choice. ---
  filtered_sessions_reactive <- reactive({
    if(input$date_layout == "Years") {
      filtered_data() %>%  # like helper.R
        group_by(Year) %>% 
        summarise(Sessions = n()) %>% 
        arrange(Year)
    } else if(input$date_layout == "Months") {
      filtered_data() %>% 
        group_by(Month) %>% 
        summarise(Sessions = n()) %>% 
        arrange(Month)
    } else if(input$date_layout == "Weeks") {
      filtered_data() %>% 
        group_by(Week) %>% 
        summarise(Sessions = n()) %>% 
        arrange(Week)
    } else {
      filtered_data() %>% 
        group_by(Day) %>% 
        summarise(Sessions = n()) %>% 
        arrange(Day)
    } 
  })
  
  ## Render PLOT 2 ----
  # Year/Month/Week/Day, User’s date layout choice. ---
  output$total_sessions_plot <- renderPlot({
    if(input$date_layout == "Years") {
      ggplot(data = filtered_sessions_reactive()) +
        geom_line(mapping=aes(x=Year, y=Sessions), 
                  color="steelblue", linewidth=1.1) +
        theme_minimal() +
        labs(x = "", y = "", 
             title = "Teaching Sessions") +
        theme(plot.title = element_text(
          hjust = 0.5, size = 16, face = "bold")) +
        scale_x_datetime(date_labels = "%Y")
    } else if(input$date_layout == "Months") {
      ggplot(data = filtered_sessions_reactive()) +
        geom_line(mapping=aes(x=Month, y=Sessions), 
                  color="steelblue", linewidth=1.1) +
        theme_minimal() +
        labs(x = "", y = "", 
             title = "Teaching Sessions") +
        theme(plot.title = element_text(
          hjust = 0.5, size = 16, face = "bold")) +
        scale_x_datetime(date_labels = "%Y %b")
    } else if(input$date_layout == "Weeks"){
      ggplot(data = filtered_sessions_reactive()) +
        geom_line(mapping=aes(x=Week, y=Sessions), 
                  color="steelblue", linewidth=1.1) +
        theme_minimal() +
        labs(x = "", y = "", 
             title = "Teaching Sessions") +
        theme(plot.title = element_text(
          hjust = 0.5, size = 16, face = "bold")) +
        scale_x_datetime(date_labels = "%y' %b W%V")
    } else {
      ggplot(data = filtered_sessions_reactive()) +
        geom_line(mapping=aes(x=Day, y=Sessions), 
                  color="steelblue", linewidth=1.1) +
        theme_minimal() +
        labs(x = "", y = "", 
             title = "Teaching Sessions") +
        theme(plot.title = element_text(
          hjust = 0.5, size = 16, face = "bold")) +
        scale_x_datetime(date_labels = "%m.%d %a")
    }
  })
  
  
  ### PLOT 3 - Number of Hours ----
  
  ## Reactive Subset 3 of Filtered Data ----
  # Year/Month/Week/Day, User’s date layout choice. ---
  filtered_hours_reactive <- reactive({
    if(input$date_layout == "Years") {
      filtered_data() %>%  # like helper.R
        group_by(Year) %>% 
        summarise(Total_Hours = sum(Duration_Hours, na.rm = TRUE)) %>% 
        arrange(Year)
    } else if(input$date_layout == "Months") {
      filtered_data() %>% 
        group_by(Month) %>% 
        summarise(Total_Hours = sum(Duration_Hours, na.rm = TRUE)) %>% 
        arrange(Month)
    } else if(input$date_layout == "Weeks") {
      filtered_data() %>% 
        group_by(Week) %>% 
        summarise(Total_Hours = sum(Duration_Hours, na.rm = TRUE)) %>% 
        arrange(Week)
    } else {
      filtered_data() %>% 
        group_by(Day) %>% 
        summarise(Total_Hours = sum(Duration_Hours, na.rm = TRUE)) %>% 
        arrange(Day)
    } 
  })
  
  
  ## Render PLOT 3 ----
  # Year/Month/Week/Day, User’s date layout choice. ---
  output$total_hours_plot <- renderPlot({
    if(input$date_layout == "Years") {
      ggplot(data = filtered_hours_reactive()) +
        geom_line(mapping=aes(x=Year, y=Total_Hours), 
                  color="steelblue", linewidth=1.1) +
        theme_minimal() +
        labs(x = "", y = "", 
             title = "Teaching Hours") +
        theme(plot.title = element_text(
          hjust = 0.5, size = 16, face = "bold")) +
        scale_x_datetime(date_labels = "%Y")
    } else if(input$date_layout == "Months") {
      ggplot(data = filtered_hours_reactive()) +
        geom_line(mapping=aes(x=Month, y=Total_Hours), 
                  color="steelblue", linewidth=1.1) +
        theme_minimal() +
        labs(x = "", y = "", 
             title = "Teaching Hours") +
        theme(plot.title = element_text(
          hjust = 0.5, size = 16, face = "bold")) +
        scale_x_datetime(date_labels = "%Y %b")
    } else if(input$date_layout == "Weeks"){
      ggplot(data = filtered_hours_reactive()) +
        geom_line(mapping=aes(x=Week, y=Total_Hours), 
                  color="steelblue", linewidth=1.1) +
        theme_minimal() +
        labs(x = "", y = "", 
             title = "Teaching Hours") +
        theme(plot.title = element_text(
          hjust = 0.5, size = 16, face = "bold")) +
        scale_x_datetime(date_labels = "%y' %b W%V")
    } else {
      ggplot(data = filtered_hours_reactive()) +
        geom_line(mapping=aes(x=Day, y=Total_Hours), 
                  color="steelblue", linewidth=1.1) +
        theme_minimal() +
        labs(x = "", y = "", 
             title = "Teaching Hours") +
        theme(plot.title = element_text(
          hjust = 0.5, size = 16, face = "bold")) +
        scale_x_datetime(date_labels = "%m.%d %a")
    }
  })
  
  ### PLOT 4 - Proficiency Distribution ----

  ## Reactive Subset 4 of Filtered Data ----
  # Year/Month/Week/Day, User’s date layout choice. ---
  filtered_proficiency_distribution <- reactive({
    if(input$date_layout == "Years") {
      filtered_data() %>%  # like helper.R
        filter(!is.na(Year), !is.na(Proficiency)) %>% 
        group_by(Year, Proficiency) %>% 
        summarise(Count = n()) %>% 
        arrange(Year)
    } else if(input$date_layout == "Months") {
      filtered_data() %>% 
        filter(!is.na(Month), !is.na(Proficiency)) %>% 
        group_by(Month, Proficiency) %>% 
        summarise(Count = n()) %>% 
        arrange(Month)
    } else if(input$date_layout == "Weeks") {
      filtered_data() %>% 
        filter(!is.na(Week), !is.na(Proficiency)) %>% 
        group_by(Week, Proficiency) %>% 
        summarise(Count = n()) %>% 
        arrange(Week)
    } else {
      filtered_data() %>% 
        filter(!is.na(Day), !is.na(Proficiency)) %>% 
        group_by(Day, Proficiency) %>% 
        summarise(Count = n()) %>% 
        arrange(Day)
    } 
  })
  
  ## Render PLOT 4 ----
  # Year/Month/Week/Day, User’s date layout choice. ---
  output$proficiency_distribution_plot <- renderPlot({
    if(input$date_layout == "Years") {
      ggplot(data = filtered_proficiency_distribution()) +
        geom_line(mapping=aes(x=Year, y=Count, color=Proficiency), linewidth=1.1) +
        scale_color_manual(values = c(
          "A1" = "#AFEEEE",
          "A2" = "#CCCCFF",
          "B1" = "#6baed6",
          "B2" = "#4682B4"
        )) +
        theme_minimal() +
        labs(x = "", y = "", 
             title = "Sessions By Proficiency",
             color ="Proficiency") +
        theme(plot.title = element_text(
          hjust = 0.5, size = 16, face = "bold")) +
        scale_x_datetime(date_labels = "%Y")
    } else if(input$date_layout == "Months") {
      ggplot(data = filtered_proficiency_distribution()) +
        geom_line(mapping=aes(x=Month, y=Count, color=Proficiency), linewidth=1.1) +
        scale_color_manual(values = c(
          "A1" = "#AFEEEE",
          "A2" = "#CCCCFF",
          "B1" = "#6baed6",
          "B2" = "#4682B4"
        )) +
        theme_minimal() +
        labs(x = "", y = "", 
             title = "Sessions By Proficiency",
             color ="Proficiency") +
        theme(plot.title = element_text(
          hjust = 0.5, size = 16, face = "bold")) +
        scale_x_datetime(date_labels = "%Y %b")
    } else if(input$date_layout == "Weeks"){
      ggplot(data = filtered_proficiency_distribution()) +
        geom_line(mapping=aes(x=Week, y=Count, color=Proficiency), size=1.1) +
        scale_color_manual(values = c(
          "A1" = "#AFEEEE",
          "A2" = "#CCCCFF",
          "B1" = "#6baed6",
          "B2" = "#4682B4"
        )) +
        theme_minimal() +
        labs(x = "", y = "", 
             title = "Sessions By Proficiency",
             color ="Proficiency") +
        theme(plot.title = element_text(
          hjust = 0.5, size = 16, face = "bold")) +
        scale_x_datetime(date_labels = "%y' %b W%V")
    } else {
      ggplot(data = filtered_proficiency_distribution()) +
        geom_line(mapping=aes(x=Day, y=Count, color=Proficiency), linewidth=1.1) +
        scale_color_manual(values = c(
          "A1" = "#AFEEEE",
          "A2" = "#CCCCFF",
          "B1" = "#6baed6",
          "B2" = "#4682B4"
        )) +
        theme_minimal() +
        labs(x = "", y = "", 
             title = "Sessions By Proficiency",
             color ="Proficiency") +
        theme(plot.title = element_text(
          hjust = 0.5, size = 16, face = "bold")) +
        scale_x_datetime(date_labels = "%m.%d %a")
    }
  })
  
  ### PLOT 5 - Proficiency Distribution (%) ----
  
  ## Reactive Subset 5 of Filtered Data ----
  filtered_total_proficiency_destirbution <- reactive({
    filtered_data() %>%  # like helper.R
      group_by(Proficiency) %>%
      summarise(Count = n()) %>%
      ungroup() %>%
      mutate(
        Percent = Count / sum(Count) * 100,
        Label = paste0(round(Percent, 1), "%")  )
  })
  
  
  ## Render PLOT 5 ----
  output$total_proficiency_destirbution_plot <- renderPlot({
    ggplot(data = filtered_total_proficiency_destirbution(), 
           aes(x = "", y =Count, fill = Proficiency)) +
      geom_col(width = 1) +
      coord_polar(theta = "y") +
      scale_fill_manual(values = c(
        "A1" = "#AFEEEE", "A2" = "#CCCCFF", "B1" = "#6baed6", "B2" = "#4682B4")) +
      geom_text(aes(label = Label),
                position = position_stack(vjust = 0.5), size = 5) +
      theme_void() +
      labs(x = "", y = "", 
           title = "Sessions by Proficiency") +
      theme(plot.title = element_text(
        hjust = 0.5, size = 16, face = "bold"))
  })
  
  
  ### PLOT 6 - Hours per Student -----
  
  
  ## Reactive Subset 6 of Filtered Data ----
  filtered_total_hours_by_student <- reactive({
    filtered_data() %>%  # like helper.R
      group_by(Student) %>% 
      summarise(Duration = sum(Duration_Hours)) %>% 
      arrange(desc(Duration))
  })
  
  ## Render PLOT 6 ----
  output$total_hours_by_student_plot <- renderPlot({
    ggplot(data = 
             filtered_total_hours_by_student() %>%  
             slice_max(Duration, n = 20) %>% 
             mutate(Student = fct_reorder(Student, Duration))) +
      geom_col(mapping = aes(x = Student, y = Duration, fill = Duration)) +
      coord_flip() +
      scale_fill_gradient(low = "lightblue", high = "steelblue") +
      theme_minimal() +
      labs(x = "", y = "", 
           title = "Study Hours per Student") +
      theme(plot.title = element_text(
        hjust = 0.5, size = 16, face = "bold"),
        text = element_text(family = "noto")) #Fixing Chinese Characters
  })
  
}