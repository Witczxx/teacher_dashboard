### Teacher Dashboard App Helper R Script -----

### Connect to Real Data -----
jcalendar <- tryCatch({
  jcal <- read.csv("Exports/teacher_dataset.csv")
  
# If Error - Load Fake Data ---
}, error = function(e) {
  read.csv("Dashboard/fake_data.csv")
})

### Transform into Tibble -----
calendar <- as_tibble(jcalendar)

### Change Data Type -----
calendar$Start_Time <- as.POSIXct(calendar$Start_Time)
calendar$End_Time <- as.POSIXct(calendar$End_Time)

### Create Columns - Year/Month/Week/Day/Duration_Hours -----
calendar <- calendar %>% 
  mutate(
    Year = floor_date(Start_Time, unit= "year"),
    Month = floor_date(Start_Time, unit = "month"),
    Week = floor_date(Start_Time, unit = "week"),
    Day = floor_date(Start_Time, unit = "day"),
    Duration_Hours = round(as.numeric(End_Time - Start_Time, units ="hours"), digits=1))



### KPI 01 - Total Hours -----
total_hours <- 
  round(
    as.numeric(
      sum(calendar$End_Time - calendar$Start_Time), units = "hours"), digits=0)

### KPI 02 - Total Sessions -----
total_sessions <- 
  nrow(calendar)

### KPI 03 - Ratio Trial Classes -----
percent_trial <- 
  paste0(
    round(
      100*(
        sum(calendar$Student == "试听") / nrow(calendar)), digits = 0), "%")
        # 试听 = Trial Class

### KPI 04 - Total Students -----
total_students <- 
  n_distinct(calendar$Student)

### KPI 05 - Ø Hours / Week -----
total_hours_per_week <- calendar %>% 
  group_by(Week) %>% 
  summarise(Total_Hours = sum(Duration_Hours, na.rm = TRUE)) %>% 
  arrange(Week)

mean_hours_per_week <- 
  round(
    mean(total_hours_per_week$Total_Hours), digits=0)

### KPI 06 - Ø Hours / Month -----
total_hours_per_month <- calendar %>%
  group_by(Month) %>%
  summarise(Total_Hours = sum(Duration_Hours, na.rm = TRUE)) %>%
  arrange(Month)

mean_hours_per_month <- 
  round(
    mean(total_hours_per_month$Total_Hours), digits=0)



### PLOT 1 - Number of Students -----

# Group/Calcualte Students ---
total_students_per_month <- calendar %>%
  group_by(Month) %>%
  summarise(Student_Count = n_distinct(Student)) %>%
  arrange(Month)

# Create the Plot ---
total_students_plot <- ggplot(data = total_students_per_month) +
  geom_line(mapping=aes(x=Month, y=Student_Count), 
            color="steelblue", linewidth=1.1) +
  theme_minimal() +
  labs(x = "", y = "", 
       title = "Student Numbers") +
  theme(plot.title = element_text(
    hjust = 0.5, size = 16, face = "bold")) +
  scale_y_continuous(
    limits = c(5, 30), 
    breaks = seq(5, 30, by = 5)) +
  scale_x_datetime(
    date_labels = "%b %Y", 
    date_breaks = "2 month")

### PLOT 2 - Number of Sessions -----

# Group/Calculate Sessions ---
total_sessions_per_month <- calendar %>% 
  group_by(Month) %>% 
  summarise(Sessions = n()) %>% 
  arrange(Month)

# Create the PLOT ---
total_sessions_plot <- ggplot(data = total_sessions_per_month) +
  geom_line(mapping=aes(x=Month, y=Sessions), 
            color="steelblue", linewidth=1.1) +
  theme_minimal() +
  labs(x = "", y = "", 
       title = "Teaching Sessions") +
  theme(plot.title = element_text(
    hjust = 0.5, size = 16, face = "bold")) +
  scale_y_continuous(
    limits = c(40, 130), 
    breaks = seq(40, 130, by = 20)) +
  scale_x_datetime(
    date_labels = "%b %Y", 
    date_breaks = "2 month")

### PLOT 3 - Number of Hours -----

# Data Frame from KPI 06 - Ø Hours / Month ---

# Create the PLOT ---
total_hours_plot <- ggplot(data = total_hours_per_month) +
  geom_line(mapping=aes(x=Month, y=Total_Hours), 
            color="steelblue", linewidth=1.1) +
  theme_minimal() +
  labs(x = "", y = "", 
       title = "Teaching Hours") +
  theme(plot.title = element_text(
    hjust = 0.5, size = 16, face = "bold")) +
  scale_y_continuous(
    limits = c(60, 180), 
    breaks = seq(60, 180, by = 30), 
    labels= function(x) paste0(x,"h")) +
  scale_x_datetime(
    date_labels = "%b %Y", 
    date_breaks = "2 month")

### PLOT 4 - Proficiency Distribution -----

# Group/Calculate Sessions ---
proficiency_distribution <- calendar %>% 
  filter(!is.na(Month), !is.na(Proficiency)) %>% 
  group_by(Month, Proficiency) %>% 
  summarise(Count = n()) %>% 
  arrange(Month)

# Create PLOT ---
proficiency_distribution_plot <- 
  ggplot(data = proficiency_distribution) +
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
  scale_y_continuous(
    limits = c(0, 90), 
    breaks = seq(0, 90, by = 20)) +
  scale_x_datetime(
    date_labels = "%b %Y", 
    date_breaks = "2 month")

### PLOT 5 - Proficiency Distribution (%) -----

# Group/Calculate Sessions (%) ---
proficiency_distribution_percent <- proficiency_distribution %>%
  group_by(Proficiency) %>% 
  summarise(Count = sum(Count)) %>% 
  mutate(
    Percent = Count / sum(Count) * 100,
    Label = paste0(round(Percent, 1), "%")
  )

# Create PLOT ---
total_proficiency_destirbution_plot <-
  ggplot(data = proficiency_distribution_percent, 
         aes(x = "", y =Count, fill = Proficiency)) + # x="", because Pie Chart
  geom_col(width = 1) + # For Pie Chart
  coord_polar(theta = "y") + # Making it circular
  scale_fill_manual(values = c(
    "A1" = "#AFEEEE", "A2" = "#CCCCFF", "B1" = "#6baed6", "B2" = "#4682B4")) +
  geom_text(aes(label = Label),
            position = position_stack(vjust = 0.5), size = 5) +
  theme_void() +
  labs(x = "", y = "", 
       title = "Sessions by Proficiency") +
  theme(plot.title = element_text(
    hjust = 0.5, size = 16, face = "bold"))

### PLOT 6 - Hours per Student ----- 

# Group/Calculate Hours per Student ---
total_hours_by_student <- calendar %>%
  group_by(Student) %>% 
  summarise(Duration = sum(Duration_Hours)) %>% 
  arrange(desc(Duration))

# Chinese Characters visible [showtext Library] ---
font_add_google("Noto Sans SC", "noto")
showtext_auto()

# Create PLOT
total_hours_by_student_plot <- 
  ggplot(data = 
         total_hours_by_student %>%  
         slice_max(Duration, n = 20) %>% 
         mutate(Student = fct_reorder(Student, Duration))) + #Sort Students by Duration
  geom_col(mapping = aes(x = Student, y = Duration, fill = Duration)) +
  coord_flip() + # Flipped Bar Chart
  scale_fill_gradient(low = "lightblue", high = "steelblue") +
  theme_minimal() +
  labs(x = "", y = "", 
       title = "Study Hours per Student") +
  theme(plot.title = element_text(
    hjust = 0.5, size = 16, face = "bold"),
    text = element_text(family = "noto")) #Fixing Chinese Characters

### Create Variable for ui.R -----
## Preparing Student Filter ----
# Unique Students Variable ---
student_names <- 
  sort(unique(calendar$Student))