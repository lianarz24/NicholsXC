#####
# Setup global environment and import data

# Set working directory
#setwd("C:/Users/liana/Downloads/NXC")

# Function to install and load packages
install_and_load <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, repos = "http://cran.us.r-project.org")
  }
  library(pkg, character.only = TRUE)
}

# List and install/load required packages
packages <- c("bsicons", "bslib", "dplyr", "DT", "googlesheets4", 
              "hms", "plotly", "rsconnect", "shiny", "shinyalert",
              "shinyBS", "shinyjs", "stringr", "tidyr", "writexl")
invisible(lapply(packages, install_and_load))

# Google Sheets authentication and data import
gs4_deauth()  # De-authenticate from any prior Google Sheets sessions
data <- read_sheet("1gEhUNJ_jVicikJOqn-Hrw8yO0nDuCg6rSy2RgCTLFmA")

# Specify and format time columns
time_columns <- c("pr24", "knoxF", "OGkno", "imp23", "redDr", "impKR", 
                  "mcqua", "OGmcq", "impRM", "stFra", "impM1", "stjBT", 
                  "imp12", "nwpa", "impAN", "caStM", 
                  "impN3", "caGow", "imp34", "allCa", "imp4A", "NYfed", "pr23", 
                  "red", "east", "impRE", "mc", "OGm23", "impEM", 
                  "ald", "impMA", "all", "impAA", "feder", "impAF", 
                  "avgTi", "avgMi")

# Function to format POSIXct time to MM:SS
format_time <- function(x) {
  if (inherits(x, "POSIXct")) {
    return(format(x, "%M:%S"))   # Extract only minutes and seconds
  } else if (is.character(x) && x == "-") {
    return(NA_character_)   # Handle missing or invalid data
  } else {
    return(x)
  }
}

# Apply formatting to time columns
data[time_columns] <- lapply(
  data[time_columns], function(col) {
    sapply(col, format_time)
  }
)

# Sort data with NA as the largest value
data <- data %>%
  arrange(across(everything(), ~ ifelse(is.na(.), Inf, .)))

# Define custom runner order (alphabetical)
custom_order <- c("Benji", "Chase", "Eli", "Hannes", "Jack", "Luca",
                  "Ned", "Oliver", "Oscar", "Rainer", "Reino", 
                  "Ryan J", "Ryan K", "Sam", "Tyler")

#####
## UI Header
header_ui <- div(class = "header-container",
                 img(src = "https://upload.wikimedia.org/wikipedia/en/7/7c/NicholsSchoolBuffaloNY.png",
                     alt = "Nichols School Logo"),
                 h1(class = "header-title", "Nichols Boys' Cross Country"))

# UI: Sidebar
sidebar_ui <- sidebarPanel(
  h4("Race Results👟", class = "race-results-title"),
  hr(class = "custom-hr"),
  
  # Year and meet selection inputs
  div(checkboxGroupInput("yearSelection", 
                         HTML("<b>Select year(s):🗓️</b>"),
                         choices = c("2024", "2023"), 
                         selected = "2024"),
      checkboxGroupInput("checkboxGroup", 
                         HTML("<b>Select meet(s):🏁</b>"),
                         choices = NULL, 
                         selected = NULL),
      selectInput("runnerSelect", 
                  HTML("<b>Progression Graph📈</b>"),
                  choices = c("Select runner" = ""), 
                  selected = "")
  ),
  hr(class = "custom-hr"),
  
  # Display team time phrase
  div(class = "time-take-off",
      textOutput("team_time_phrase")
  ),
  
  # Google Sheet and About buttons
  div(class = "centered-flex",
      tags$a(href = "https://docs.google.com/spreadsheets/d/1YY-00iMmU_6CxjMsQitXsVlarpuW5miZoDbUpiSMAoM", 
             "View the Google Sheet", target = "_blank", 
             class = "button-style"),
      actionButton("showAbout", "About")
  )
)

# UI: Main panel
mainpanel_ui <- mainPanel(
  
  # Images that displays before selections
  conditionalPanel(
    condition = "input.checkboxGroup.length === 0",
    div(
      class = "image-grid",
      style = "display: grid; grid-template-columns: repeat(2, 1fr); gap: 10px;",
      img(src = "https://raw.githubusercontent.com/lianarz24/NicholsXC/refs/heads/main/startline_NWPA.jpg",
          alt = "NWPA startline", style = "width: 100%; max-height: 400px; object-fit: cover;"),
      img(src = "https://raw.githubusercontent.com/lianarz24/NicholsXC/refs/heads/main/stride_back_NWPA.jpg",
          alt = "NWPA strides back", style = "width: 100%; max-height: 400px; object-fit: cover;"),
      img(src = "https://raw.githubusercontent.com/lianarz24/NicholsXC/refs/heads/main/stride_out_NWPA.jpg",
          alt = "NWPA stries out", style = "width: 100%; max-height: 400px; object-fit: cover;"),
      img(src = "https://raw.githubusercontent.com/lianarz24/NicholsXC/refs/heads/main/spikes_NWPA.jpg",
          alt = "NWPA spikes", style = "width: 100%; max-height: 400px; object-fit: cover;")
    )
  ),
  
  # Content displayed when selections are made
  conditionalPanel(
    condition = "input.checkboxGroup.length > 0",
    page_fillable(
      div(class = "card-container",
          # Race times card
          card(
            card_header(
              tags$span("Race Times", class = "card-header-title"),
              bsicons::bs_icon("question-circle", placement = "right",
                               title = "5k unless otherwise noted."),
              class = "d-flex card-header-bg"
            ),
            div(class = "card-body",
                DTOutput("filteredTable"),  # Display filtered race times
                uiOutput("footnote"),   # Display footnotes, if any
                div(class = "download-link",
                    tags$a(
                      href = "#",
                      downloadButton("downloadData","Download table as .xlsx",
                                     class = "button-style")
                    )
                ),
                textOutput("runnerSelectionMessage")
            )
          ),
          br(),
          # Runner progression card
          card(
            card_header(
              tags$span("Runner Progression", class = "card-header-title"),
              bsicons::bs_icon("question-circle", placement = "right",
                               title = "x-axis: [Left: Older, Right: Newer];\ny-axis: [Top: Faster; Bottom: Slower]"),
              class = "d-flex card-header-bg"
            ),
            div(class = "card-body",
                uiOutput("runnerPlotOutput")
            )
          ),
          br()
      )
    )
  ),
  
  # About section, initially hidden
  div(id = "AboutSection", style = "display: none;",
      card(
        card_header(
          tags$span("About",
                    style = "font-weight: bold; font-size: 18px; color: green;"),
          class = "d-flex align-items-center gap-1",
          style = "background-color: rgba(0, 128, 0, 0.2);"
        ),
        div(
          class = "about-body",
          HTML("<div class='text-content'>
         <p>This app was created with <a href='https://www.r-project.org/' target='_blank' rel='noopener noreferrer'>R</a> 
         using <a href='https://doi.org/10.32614/CRAN.package.shiny' target='_blank' rel='noopener noreferrer'>shiny</a> 
         by Lian Arzbecker.</p>
         <p>All photo credit goes to Head Coach Jim Cammarata.</p>
         <p>Last updated: 25 November 2024.</p>
       </div>"),
       hr(class = "about-divider"),
       HTML("<div class='text-content'>
         <p>For more information about R coding, visit: 
         <a href='https://lianjarzbecker.phd.sh/' target='_blank' rel='noopener noreferrer' 
            class='button-style'>lianjarzbecker.phd.sh</a></p>
         <p>To buy Suki, Lian's dog, a treat, visit: 
         <a href='https://buymeacoffee.com/lianjarzbecker' target='_blank' rel='noopener noreferrer' 
            class='woof-button'>
           <button class='button-style'>woof!🦴</button>
         </a></p>
       </div>"),
       
       hr(class = "about-divider")
        ),
       HTML("<div style='font-size: 14px; line-height: 1.5; margin-top: -20px; padding-left: 15px;'>
            <strong>R Packages Used:</strong>
            <p></p>
            <p>Atkins, A., Allen, T., Wickham, H., McPherson, J., & Allaire J. (2024). 
            <i>rsconnect: Deploy Docs, Apps, and APIs to 'Posit Connect', 'shinyapps.io', and 'RPubs' </i> (v1.3.1). Retrieved from 
            <a href='https://CRAN.R-project.org/package=rsconnect'>https://CRAN.R-project.org/package=rsconnect</a></p>
            <p>Attali, D. (2015).
            <i>shinyjs: Easily Improve the User Experience of Your Shiny Apps in Seconds</i> (v2.1.0). Retrieved from 
            <a href='https://doi.org/10.32614/CRAN.package.shinyjs'>https://doi.org/10.32614/CRAN.package.shinyjs</a></p>
            <p>Attali, D., & Edwards, T. (2024).
            <i>shinyalert: Easily Create Pretty Popup Messages (Modals) in 'Shiny'</i> (v3.1.0). Retrieved from 
            <a href='https://10.32614/CRAN.package.shinyalert'>https://10.32614/CRAN.package.shinyalert</a></p>
            <p>Bryan, J. (2019).
            <i>googlesheets4: Access Google Sheets using the Sheets API V4</i> (v1.1.1). Retrieved from 
            <a href='https://doi.org/10.32614/CRAN.package.googlesheets4'>https://doi.org/10.32614/CRAN.package.googlesheets4</a></p>
            <p>Chang, W., Cheng, J., Allaire, J., Sievert, C., Schloerke, B., Xie, Y., Allen, J., McPherson, J., Dipert, A., & Borges, B. (2012). 
            <i>shiny: Web Application Framework for R</i> (v1.9.1). Retrieved from 
            <a href='https://doi.org/10.32614/CRAN.package.shiny'>https://doi.org/10.32614/CRAN.package.shiny</a></p>
            <p>Bailey, E. (2014). 
            <i>shinyBS: Twitter Bootstrap Components for Shiny</i> (v0.61.1). Retrieved from 
            <a href='https://doi.org/10.32614/CRAN.package.shinyBS'>https://doi.org/10.32614/CRAN.package.shinyBS</a></p>
            <p>Müller, K. (2016). 
            <i>hms: Pretty Time of Day</i> (v1.1.3). Retrieved from 
            <a href='https://doi.org/10.32614/CRAN.package.hms'>https://doi.org/10.32614/CRAN.package.hms</a></p>
            <p>Ooms, J. (2017). 
            <i>writexl: Export Data Frames to Excel “xlsx” Format</i> (v1.5.0). Retrieved from 
            <a href='https://doi.org/10.32614/CRAN.package.writexl'>https://doi.org/10.32614/CRAN.package.writexl</a></p>
            <p>Sievert, C. (2022). 
            <i>bsicons: Easily Work with “Bootstrap” Icons</i> (v0.1.2). Retrieved from 
            <a href='https://doi.org/10.32614/CRAN.package.bsicons'>https://doi.org/10.32614/CRAN.package.bsicons</a></p>
            <p>Sievert, C., Cheng, J., & Aden-Buie, G. (2021). 
            <i>bslib: Custom “Bootstrap” “Sass” Themes for “shiny” and “rmarkdown”</i> (v0.8.0). Retrieved from 
            <a href='https://doi.org/10.32614/CRAN.package.bslib'>https://doi.org/10.32614/CRAN.package.bslib</a></p>
            <p>Sievert, C., Parmer, C., Hocking, T., Chamberlain, S., Ram, K., Corvellec, M., & Despouy, P. (2015). 
            <i>plotly: Create Interactive Web Graphics via “plotly.js”</i> (v4.10.4). Retrieved from 
            <a href='https://doi.org/10.32614/CRAN.package.plotly'>https://doi.org/10.32614/CRAN.package.plotly</a></p>
            <p>Wickham, H. (2009). 
            <i>stringr: Simple, Consistent Wrappers for Common String Operations</i> (v1.5.1). Retrieved from 
            <a href='https://doi.org/10.32614/CRAN.package.stringr'>https://doi.org/10.32614/CRAN.package.stringr</a></p>
            <p>Wickham, H., François, R., Henry, L., Müller, K., & Vaughan, D. (2014). 
            <i>dplyr: A Grammar of Data Manipulation</i> (v1.1.4). Retrieved from 
            <a href='https://doi.org/10.32614/CRAN.package.dplyr'>https://doi.org/10.32614/CRAN.package.dplyr</a></p>
            <p>Wickham, H., Vaughan, D., & Girlich, M. (2014). 
            <i>tidyr: Tidy Messy Data</i> (v1.3.1). Retrieved from 
            <a href='https://doi.org/10.32614/CRAN.package.tidyr'>https://doi.org/10.32614/CRAN.package.tidyr</a></p>
            <p>Xie, Y., Cheng, J., & Tan, X. (2015). 
            <i>DT: A Wrapper of the JavaScript Library “DataTables”</i> (v0.33). Retrieved from 
            <a href='https://doi.org/10.32614/CRAN.package.DT'>https://doi.org/10.32614/CRAN.package.DT</a></p>
            </div>")
      )
  ),
)

# Assembling all UI components into one fluidPage
ui <- fluidPage(
  tags$head(includeHTML("www/google-analytics.html")),
  includeCSS("www/custom_styles.css"),
  useShinyjs(),
  
  header_ui,  # Header UI component
  
  sidebarLayout(
    sidebar_ui,  # Sidebar UI component
    mainpanel_ui   # Main panel UI component
  ),
  
  tags$script(HTML("
  $(document).on('click', '#showAbout', function() {
    var AboutSection = $('#AboutSection');
    var button = $(this);
    
    if (AboutSection.is(':visible')) {
      AboutSection.hide();
      button.text('About');
    } else {
      AboutSection.show();
      button.text('Close About');
    }
  });
  "))
)

#####
## Server function
server <- function(input, output, session) {
  shinyalert(
    title = "Thanks for a great season!",
    #text = "and we're sending 2 to States",
    showConfirmButton = TRUE, confirmButtonText = "Go Vikings!",
    confirmButtonCol = "#1A6C36",
    imageUrl = "https://raw.githubusercontent.com/lianarz24/NicholsXC/refs/heads/main/Elis_fans.gif",
    imageWidth = 400, imageHeight = 250, animation = TRUE
  )
  
  ## Initial data setup
  col_mapALL <- list("Last Name" = "last", "First Name" = "first",
                     "Season's best" = "pr24", "Knox Farms" = "knoxF",
                     "Knox 4k" = "OGkno", "Improve1" = "imp23",
                     "Red Dragon" = "redDr", "Improve2" = "impKR",
                     "McQuaid" = "mcqua", "McQuaid 3mi" = "OGmcq",
                     "Improve3" = "impRM", "MML: St. Francis" = "stFra",
                     "Improve4" = "impM1", "MML: St. Joe's/Timon" = "stjBT",
                     "Improve5" = "imp12", "NWPA" = "nwpa",
                     "Improve7" = "impAN", "MML: Canisius/St. Mary's" = "caStM", 
                     "Improve8" = "impN3", "MML: Cardinal/Gow" = "caGow",
                     "Improve9" = "imp34", "All-Catholics" = "allCa",
                     "Improve10" = "imp4A", "Federation Champs" = "NYfed",
                     "2023 PR" = "pr23",
                     "Red Dragon23" = "red", "East Aurora23" = "east",
                     "Improve123" = "impRE", "McQuaid23" = "mc",
                     "McQuaid 3mi 23" = "OGm23", "Improve223" = "impEM",
                     "Alden23" = "ald", "Improve323" = "impMA",
                     "All-Catholics23" = "all", "Improve423" = "impAA",
                     "Federation23" = "feder", "Improve523" = "impAF",
                     "Average Time" = "avgTi", "Average Mile Pace" = "avgMi")
  col_map24 <- list("Season's best" = "pr24", "Knox Farms" = "knoxF", "Red Dragon" = "redDr",
                    "McQuaid" = "mcqua", "MML: St. Francis" = "stFra",
                    "MML: St. Joe's/Timon" = "stjBT",
                    "NWPA" = "nwpa", "MML: Canisius/St. Mary's" = "caStM",
                    "MML: Cardinal/Gow" = "caGow","All-Catholics" = "allCa",
                    "Federation Champs" = "NYfed")
  col_map23 <- list("Red Dragon23" = "red", "East Aurora23" = "east",
                    "McQuaid23" = "mc", "Alden23" = "ald",
                    "All-Catholics23" = "all", "Federation23" = "feder")
  
  ## Sidebar year and meet selection
  observe({
    selected_years <- input$yearSelection
    selected_years <- union(selected_years, "2024")
    updateCheckboxGroupInput(session, "yearSelection", selected = selected_years)
    
    # Determine available meet choices based on selected years
    meet_choices <- c()
    if ("2024" %in% selected_years) meet_choices <- c(meet_choices, names(col_map24))
    if ("2023" %in% selected_years) meet_choices <- c(meet_choices, names(col_map23))
    updateCheckboxGroupInput(session, "checkboxGroup", choices = meet_choices)
  })
  
  output$team_time_phrase <- renderText({
    "As a team, we've collectively taken off 36:09 this season!"
  })
  
  ## Populate runner choices
  observe({
    data <- data.frame(first = c("Benji", "Chase", "Eli", "Hannes", "Jack",
                                 "Luca", "Ned", "Oliver", "Oscar", "Rainer",
                                 "Reino", "Ryan J", "Ryan K", "Sam", "Tyler"))
    available_runners <- unique(trimws(data$first))
    ordered_runners <- custom_order[custom_order %in% available_runners]
    updateSelectInput(session, "runnerSelect",
                      choices = c("Select runner" = "", ordered_runners))
  })
  
  ## Reactive data for runners
  runner_data <- reactive({
    req(input$runnerSelect, input$checkboxGroup)
    data %>%
      filter(first == input$runnerSelect) %>%
      select(c("first", unlist(col_mapALL[input$checkboxGroup])))
  })
  
  ## Footnote Logic
  output$footnote <- renderUI({
    selected_meets <- input$checkboxGroup
    footnote_list <- list()
    
    if ("Knox Farms" %in% selected_meets) {
      footnote_list <- append(footnote_list, withMathJax(
        tags$div(
          tags$p(style = "font-size: 12px",
                 "†Any 4k times for Knox Farms have been converted ",
                 "to 5k using Riegel’s formula (1977)."),
          tags$p(style = "font-size: 10px",
                 "$$t_2 = t_1 \\times \\left( \\frac{d_2}{d_1} \\right)^{1.06}$$")
        )
      ))
    }
    
    if ("McQuaid" %in% selected_meets || "McQuaid23" %in% selected_meets) {
      footnote_list <- append(footnote_list, withMathJax(
        tags$div(
          tags$p(style = "font-size: 12px",
                 "‡All 3-mile times for McQuaid have been converted ",
                 "to 5k using Riegel’s formula (1977)."),
          tags$p(style = "font-size: 10px",
                 "$$t_2 = t_1 \\times \\left( \\frac{d_2}{d_1} \\right)^{1.06}$$")
        )
      ))
    }
    
    if (length(footnote_list) > 0) {
      do.call(tagList, footnote_list)
    }
  })
  
  # DT: Function to format time columns
  format_time_column <- function(column) {
    if (is.numeric(column)) {
      # Convert numeric values to time in HH:MM format
      as.character(format(as.POSIXct(column, origin = "1970-01-01",
                                     tz = "UTC"), "%H:%M"))
    } else {
      column
    }
  }
  
  ## Render data table
  output$filteredTable <- renderDT({
    req(input$checkboxGroup)  # Ensure checkboxes are selected
    
    # Always include the "first" (runner) column and selected meet columns
    selected_columns <- c("first", unlist(col_mapALL[input$checkboxGroup]))
    
    # Filter data
    filtered_data <- data %>%
      select(all_of(selected_columns)) %>%
      mutate(across(-first, format_time_column)) %>%  # Format time columns
      mutate(across(-first, ~ ifelse(is.na(.), "NA", .)))  # Replace NA with "NA"
    
    # Rename columns
    renamed_columns <- c("Runner", input$checkboxGroup)
    
    # Ensure the length of renamed_columns matches the number of columns in filtered_data
    if (length(renamed_columns) == ncol(filtered_data)) {
      colnames(filtered_data) <- renamed_columns
    } else {
      stop("Column name length does not match the number of columns in the data.")
    }
    
    # Check if "Runner" column exists
    if ("Runner" %in% colnames(filtered_data)) {
      # Order rows by the order of runners in the selectInput
      runner_order <- c("Select runner", custom_order)
      runner_order <- runner_order[runner_order %in% filtered_data$Runner]
      filtered_data <- filtered_data %>%
        arrange(factor(Runner, levels = runner_order))  # Arrange rows
    }
    
    # Display the filtered data without pagination and extra controls
    datatable(filtered_data, options = list(
      pageLength = nrow(filtered_data),  # Show all rows
      lengthChange = FALSE,  # Hide the "length" dropdown
      paging = FALSE,  # Disable pagination
      searching = FALSE,  # Hide the search box
      info = FALSE,  # Hide the information summary
      autoWidth = TRUE,  # Enable auto width for columns
      initComplete = JS(
        "function(settings, json) {",
        "  var api = this.api();",
        "  var header = api.table().header();",
        "  $(header).find('th').each(function(index) {",
        "    var text = $(this).text();",
        "    if (text === 'Knox Farms') {",
        "      $(this).html('Knox Farms&dagger;');",
        "    }",
        "    if (text === 'McQuaid' || text === 'McQuaid23') {",
        "      $(this).html(text + '&Dagger;');",
        "    }",
        "  });",
        "}"
      )
    ), rownames = FALSE)  # Hide row numbers
  }, server = FALSE)
  
  # Download filtered data as XLSX
  output$downloadData <- downloadHandler(
    filename = function() "Nichols_XC.xlsx",  # Filename function
    content = function(file) {
      req(input$checkboxGroup)  # Ensure checkboxes are selected
      
      # Always include the "first" (runner) column and selected meet columns
      selected_columns <- c("first",
                            unlist(col_mapALL[input$checkboxGroup]))
      
      # Filter, rename columns, and write the data to an Excel file
      data %>%
        select(all_of(selected_columns)) %>%
        rename_with(~ c("Runner", input$checkboxGroup)) %>%  # Rename columns
        write_xlsx(file)  # Write to file directly
    }
  )
  
  # Show the AboutSection when references are loaded
  output$referencesHTML <- renderUI({
    shinyjs::show("AboutSection")  # Show the section
    HTML(paste(formatted_references, collapse = ""))
  })
  
  # Check if a runner is selected
  output$runnerSelected <- reactive({
    !is.null(input$selectedRunner)  # Assuming you have an input for selecting a runner
  })
  
  # Prevent suspension of runnerSelected output when hidden
  outputOptions(output, "runnerSelected", suspendWhenHidden = FALSE)
  
  output$runnerPlotOutput <- renderUI({
    if (length(input$checkboxGroup) > 0) {
      # Check if a runner is selected
      if (input$runnerSelect != "") {
        plotlyOutput("runnerPlot")  # Show the plot if a runner is selected
      } else {
        # Show default message if no runner is selected
        tags$div("Please select a runner from the dropdown menu")
      }
    } else {
      # Show default message if no checkbox is selected
      tags$div("Please select a runner from the dropdown menu")
    }
  })
  
  output$runnerPlot <- renderPlotly({
    
    # Extract meet names and times for the selected runner
    meets <- names(col_mapALL[input$checkboxGroup])
    times <- runner_data()[1, -1]  # Skip the first column (runner name)
    
    # Reactive expression to get the selected runner's name
    selected_runner <- reactive({
      input$runnerSelect
    })
    
    convert_to_seconds <- function(t) {
      t <- as.character(t)  # Coerce to character
      if (!is.na(t) && t != "-" && grepl("^[0-9]+:[0-9]+$", t)) {  # Validate format
        parts <- strsplit(t, ":")[[1]]
        as.numeric(parts[1]) * 60 + as.numeric(parts[2])
      } else {
        NA  # Return NA for invalid or non-conforming entries
      }
    }
    
    times_in_seconds <- sapply(times, convert_to_seconds)
    valid_indices <- !is.na(times_in_seconds)
    
    # Validate that there are valid times for the selected runner
    validate(
      need(sum(valid_indices) > 0, 
           paste(selected_runner(), "has not raced in any of the selected meets."))
    )
    
    # Filter meets and times based on valid indices
    meets <- meets[valid_indices]
    times_in_seconds <- times_in_seconds[valid_indices]
    times <- times[valid_indices]
    
    # Convert seconds back to MM:SS format for hover text
    hover_text <- sprintf("%02d:%02d", 
                          floor(times_in_seconds / 60), 
                          times_in_seconds %% 60)
    
    # Identify the fastest time
    fastest_time_index <- which.min(times_in_seconds)
    
    # Set colors: yellow for the fastest time, black for others
    colors <- rep('black', length(times_in_seconds))
    colors[fastest_time_index] <- 'yellow'
    
    # Set outline color for all points
    outlines <- rep('black', length(times_in_seconds))
    outlines[fastest_time_index] <- 'black'
    
    # Reverse the order of the x-axis categories
    meets <- factor(meets, levels = rev(meets))
    
    # Determine min and max times for y-axis range
    y_min <- min(times_in_seconds, na.rm = TRUE)
    y_max <- max(times_in_seconds, na.rm = TRUE)
    
    # Add some padding to the y-axis range
    padding <- 30  # 30 seconds of padding
    y_range <- c(y_max + padding, y_min - padding)  # Reverse range for descending time values
    
    title_reactive <- reactive({
      selected_runner <- input$runnerSelect
      
      # If no runner is selected, provide a default title
      if (selected_runner == "") {
        return("Progression")
      }
      paste(selected_runner)
    })
    
    # Define the meet order and convert to factor
    meet_order <- c("Red Dragon23", "East Aurora23", "McQuaid23", "Alden23", 
                    "All-Catholics23", "Federation23", 
                    "Knox Farms", "Red Dragon", "McQuaid", 
                    "MML: St. Francis", "MML: St. Joe's/Timon", 
                    "NWPA", "MML: Canisius/St. Mary's", 
                    "MML: Cardinal/Gow", "All-Catholics","Season's best")
    meets <- factor(meets, levels = meet_order)
    
    # Create a data frame with meets and times, order by factor levels
    df <- data.frame(meets, times_in_seconds, hover_text, colors, outlines)
    df <- df[order(df$meets), ]
    
    # Create the plot
    fig <- plot_ly(df, x = ~meets, y = ~times_in_seconds,
                   type = 'scatter', mode = 'lines+markers',
                   text = ~hover_text, hoverinfo = 'text',
                   marker = list(color = df$colors, size = 11, symbol = "star",
                                 line = list(color = 'black', width = 1.5)),
                   line = list(color = 'green')) %>%
      layout(
        title = list(
          text = title_reactive(),  
          font = list(size = 20, family = "Arial", color = "black", weight = "bold")
        ),
        xaxis = list(
          title = "Meet",
          titlefont = list(size = 16, family = "Arial", color = "black", weight = "bold"),
          tickfont = list(size = 14),
          automargin = TRUE
        ),
        yaxis = list(
          title = list(
            text = "5k Time",
            font = list(size = 16, family = "Arial", color = "black", weight = "bold"),
            standoff = 10
          ),
          tickfont = list(size = 14),
          automargin = TRUE,
          tickvals = seq(y_min - padding, y_max + padding, by = 30),
          ticktext = sprintf("%02d:%02d", 
                             floor(seq(y_min - padding,
                                       y_max + padding, by = 30) / 60),
                             seq(y_min - padding,
                                 y_max + padding, by = 30) %% 60),
          range = y_range
        ),
        margin = list(t = 60),
        hovermode = "closest",
        autosize = TRUE,
        plot_bgcolor = '#e5ecf6',
        xaxis = list(
          zerolinecolor = '#ffff',
          zerolinewidth = 2,
          gridcolor = 'ffff'
        ),
        yaxis = list(
          zerolinecolor = '#ffff',
          zerolinewidth = 2,
          gridcolor = 'ffff'
        )
      ) %>%
      config(displaylogo = FALSE,
             displayModeBar = FALSE)  # Hide the Plotly logo
    
    fig
    
  })
  # Reactive value to track if About should be shown
  About_visible <- reactiveVal(FALSE)
  
  # Toggle About visibility when button is clicked
  observeEvent(input$showAbout, {
    About_visible(!About_visible())
  })
  
  # Output to control the visibility of the About section
  output$showAbout <- reactive({
    About_visible()
  })
  
  # Must make the output available to the conditionalPanel
  outputOptions(output, "showAbout", suspendWhenHidden = FALSE)
}

#####
## Run app
# Run the application
shinyApp(ui = ui, server = server)