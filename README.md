# Nichols XC 🏃🏽

This app was created with R Shiny💎 and serves as an interactive timetable for tracking and graphing runners' progress throughout the season.

## Requirements

### R and R packages:
You can copy and paste the following code into your R console:

```r
packages <- c("bsicons", "bslib", "dplyr", "DT", "googlesheets4",  
               "hms", "plotly", "rsconnect", "shiny", "shinyalert",
               "shinyBS", "shinyjs", "stringr", "tidyr", "writexl")
invisible(lapply(packages, install_and_load))
```
If you're new to Shiny, here's a great place to start: [Shiny Introduction](https://shiny.posit.co/).  
Completed apps can be run locally directly from the R console.  
To run apps online, you'll need an account with [Shiny Apps](https://login.shinyapps.io/login).
