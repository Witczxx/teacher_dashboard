# Teacher Dashboard App (R Shiny)

## Introduction  
This project is a dashboard application designed for teachers to gain insights from their class session data.  

The goal was to provide an easy-to-use tool that enables teachers to quickly analyze and explore their data.  

The dashboard offers three main interactive filters, six key performance indicators (KPIs), and six visualizations.  
This allows teachers to perform efficient and flexible data analysis independently.  

Built with R Shiny (and a little HTML/CSS), the dashboard features a clean and modern design.

## Background  
The dashboard is based on a real use case, where only three key parameters were recorded by a teacher in a calendar app: 
student name, proficiency level, and session start/end times. 

This dataset shaped the design and functionalities of the app.  

## Screenshot of the Dashboard

![app_screenshot](https://github.com/user-attachments/assets/7158513c-df6c-4036-902e-434c28387636)

## Installation and Running the App

1. **Download the repository**  
   Clone or download all files from this repository and keep them in the same folder.

2. **Install R and RStudio**  
   Download and install [R](https://cran.r-project.org/) and [RStudio](https://posit.co/download/rstudio-desktop/).

3. **Open the project**  
   In RStudio, open the `.Rproj` file included in the repository.  
   This ensures the correct working environment so that all R scripts and file paths work together.

4. **Install required packages**  
   The following R packages are needed:  
   `tidyverse`, `RPostgres`, `DBI`, `showtext`, `shiny`, `bslib`, `bsicons`, `shinyWidgets`.  

   Install any missing packages by running in the RStudio console:  
   ```r
   install.packages("package-name")

## App Structure

The app layout is organized into three main sections:

1. **Header**  
   At the very top, a large card displays the title of the app.

2. **Filters**  
   Below the header, three filter cards are arranged side by side:  
   - **Card 1 – Proficiency filter:**  
     Select which proficiencies should be included. <br>
     Only students with the chosen proficiency levels will be considered in the KPIs and visualizations.  
   - **Card 2 – Accuracy of Graphics:**  
     Define the time scale for the x-axis in the visualizations: years, months, weeks, or days.  <br>
     Example: When comparing weeks or days in the range, it’s best not to keep "months" selected.
   - **Card 3 – Student filter:**  
     Include or exclude specific students from the analysis.

3. **KPIs and Visualizations**  
   Under the filter cards, six KPIs are displayed. These update based on the active filters.  
   Below the KPIs, six visualizations are shown, also responding to the filters.
   
   The **"Proficiency Distribution (%)"** and **"Hours per Student"** charts are unaffected <br>
   by the "Accuracy of Graphics" filter. They do not have a time-based x-axis.

## Author  
This dashboard app is my first project after completing the Google Data Analytics Certificate on Coursera.  
I really enjoyed the experience and it made me extend my knowledge by learning R Shiny from scratch.  
If you have any questions or recommendations, feel free to contact me on LinkedIn (link in my profile).
