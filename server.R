# Names: Anvitha Ghanta and Janhavi Lonari 
# Last Updated: 07/08/2025
# Details: This is the UI file for the Shiny app made for MMI Textiles 

# call the needed libraries
library(shiny)
library(googlesheets4)
library(ggplot2)
library(plotly)
library(tidyverse)
library(dplyr)
library(RColorBrewer)
if (!require(stringr)) install.packages("stringr")
library(readxl)
library(stringr)
# this authenticates the service account JSON
gs4_auth(path = "the-service-account.json")

# here is the ID to the google sheet for the service account JSON to access it
sheet_id <- "15ixLLlQ4rz1dVibUBTokF5_hik6YnYoWsA9EzufGVL0"
sheet_id1 <- "1rNB8QILMhG0HDRxR3vmR691BfwWdI9tUS0hLskqP3Io"
sheet_id2 <- "1PSIYIUxkNARzIIlPuOJvvZwgJp77hbeBAZyewsP94yI"
server <- function(input, output, session) {
  
  # this makes it so that the data can be stored reactively 
  vals <- reactiveValues(data = NULL)
  pal <- reactiveValues(data = NULL)
  tal<-reactiveValues(data = NULL)
 

  
  
  # when the button "Reload Data" on the Tolerance Graphs tab is clicked the data is updated 
  observeEvent(input$reload_tolerance, {
    vals$data <- read_sheet(sheet_id)
  }, ignoreNULL = FALSE)
  
  observeEvent(input$reload_preheat, {
    raw_data <- read_sheet(sheet_id1)
    pal$data <- na.omit(raw_data$Date)
    
  }, ignoreNULL = FALSE)
  
  observeEvent(input$reload_sheet, {
    tal$data  <- read_sheet(sheet_id2)
    tal$data <- tal$data[!apply(is.na(tal$data) | trimws(tal$data) == "", 1, all), ]
    
    data_subset_style <- tal$data %>%
      select(c(18:49))  # This selects 24 + 8 = 32 columns


    # Step 3: Optionally verify the number of columns
    stopifnot(ncol(data_subset_style) == 32)
    
    # Step 4: If needed, clean column names (e.g., set to "1" through "32")
    colnames(data_subset_style) <- as.character(1:32)
    
    
    # Assume data_subset_style has 32 columns
    block_size_style <- 8
    
    # Collapse into 8 columns
    collapsed_style <- lapply(1:block_size_style, function(i) {
      col_indices <- seq(i, ncol(data_subset_style), by = block_size_style)
      values <- unlist(data_subset_style[, col_indices], use.names = FALSE)
      return(values)
    })
    
    # Create tibble from the collapsed list and name columns 1 to 8
    final_tibble_style <- as_tibble(setNames(collapsed_style, as.character(1:block_size_style)))
    
    
    apple <- (nrow(final_tibble_style)) / 4  # assuming final_tibble is a data frame/tibble
    list1 <- list()
    list2 <- list()
    list3 <- list()
    list4 <- list()
    list5 <- list()
    list6 <- list()
    list7 <- list()
    list8 <- list()
    style_list <- list()
    product_list <- list()
    date_list <- list()
    minimum_list <- list()
    maximum_list <- list()
    heatset_list <- list()
    endnum_list <- list()
    the1list <- list()
    the2list <- list()
    the3list <- list()
    the4list <- list()
    combined_list <- list()
    
    
    banana = nrow(data_subset_style)
    
    
    for (i in 1:banana) {
      for (g in 0:(4)) {
        row_index <- banana * g + (i)
        list1 <- c(list1, final_tibble_style[row_index, 1])
        list2 <- c(list2, final_tibble_style[row_index, 2])
        list3 <- c(list3, final_tibble_style[row_index, 3])
        list4 <- c(list4, final_tibble_style[row_index, 4])
        list5 <- c(list5, final_tibble_style[row_index, 5])
        list6 <- c(list6, final_tibble_style[row_index, 6])
        list7 <- c(list7, final_tibble_style[row_index, 7])
        list8 <- c(list8, final_tibble_style[row_index, 8])
      }
      the1list <- c(the1list, list1[1], list2[1], list3[1], list4[1], list5[1], list6[1], list7[1], list8[1])
      the2list <- c(the2list, list1[2], list2[2], list3[2], list4[2], list5[2], list6[2], list7[2], list8[2])
      the3list <- c(the3list, list1[3], list2[3], list3[3], list4[3], list5[3], list6[3], list7[3], list8[3])
      the4list <- c(the4list, list1[4], list2[4], list3[4], list4[4], list5[4], list6[4], list7[4], list8[4])
      if (g == 4) {
        list1 <- Filter(Negate(is.na), list1)
        list3 <- Filter(Negate(is.na), list3)
        list5 <- Filter(Negate(is.na), list5)
        list7 <- Filter(Negate(is.na), list7)
        if (length(list1) == 0) {
          endnum_list <- c(endnum_list)
          style_list <- c(style_list)
          product_list <- c(product_list)
          date_list <- c(date_list)
          minimum_list <- c(minimum_list)
          maximum_list <- c(maximum_list)
          heatset_list <- c(heatset_list)
        } else if (length(list3) == 0) {
          endnum_list <- c(endnum_list, 1, 2)
          style_list <- c(style_list, rep(tal$data[i, 5], 2))
          product_list <- c(product_list, rep(tal$data[i, 4], 2))
          date_list <- c(date_list, rep(tal$data[i, 7], 2))
          minimum_list <- c(minimum_list, rep(tal$data[i, 8], 2))
          maximum_list <- c(maximum_list, rep(tal$data[i, 9], 2))
          heatset_list <- c(heatset_list, rep(tal$data[i, 6], 2))
        } else if (length(list5) == 0) {
          endnum_list <- c(endnum_list, 1, 2, 3, 4)
          style_list <- c(style_list, rep(tal$data[i, 5], 4))
          product_list <- c(product_list, rep(tal$data[i, 4], 4))
          date_list <- c(date_list, rep(tal$data[i, 7], 4))
          minimum_list <- c(minimum_list, rep(tal$data[i, 8], 4))
          maximum_list <- c(maximum_list, rep(tal$data[i, 9], 4))
          heatset_list <- c(heatset_list, rep(tal$data[i, 6], 4))
        } else if (length(list7) == 0) {
          endnum_list <- c(endnum_list, 1, 2, 3, 4, 5, 6)
          style_list <- c(style_list, rep(tal$data[i, 5], 6))
          product_list <- c(product_list, rep(tal$data[i, 4], 6))
          date_list <- c(date_list, rep(tal$data[i, 7], 4))
          minimum_list <- c(minimum_list, rep(tal$data[i, 8], 6))
          maximum_list <- c(maximum_list, rep(tal$data[i, 9], 6))
          heatset_list <- c(heatset_list, rep(tal$data[i, 6], 6))
        } else {
          endnum_list <- c(endnum_list, 1, 2, 3, 4, 5, 6, 7, 8)
          style_list <- c(style_list, rep(tal$data[i, 5], 8))
          product_list <- c(product_list, rep(tal$data[i, 4], 8))
          date_list <- c(date_list, rep(tal$data[i, 7], 8))
          minimum_list <- c(minimum_list, rep(tal$data[i, 8], 8))
          maximum_list <- c(maximum_list, rep(tal$data[i, 9], 8))
          heatset_list <- c(heatset_list, rep(tal$data[i, 6], 8))
        }
        list1 <- list()
        list2 <- list()
        list3 <- list()
        list4 <- list()
        list5 <- list()
        list6 <- list()
        list7 <- list()
        list8 <- list()
      }
    }
    
    the1list <- unname(unlist(the1list))
    the2list <- unname(unlist(the2list))
    the3list <- unname(unlist(the3list))
    the4list <- unname(unlist(the4list))

    
    
    # Step 2: Extract columns 7–30 and 39–46
    data_subset_loom <- tal$data %>%
      select(10:13)  # This selects 4 columns
    
    # Step 3: Optionally verify the number of columns
    stopifnot(ncol(data_subset_loom) == 4)
    
    # Step 4: If needed, clean column names (e.g., set to "1" through "32")
    colnames(data_subset_loom) <- as.character(1:4)
    
    # Assume df_subset has 4 columns
    block_size_loom <- 1
    
    # Collapse into 8 columns
    collapsed <- lapply(1:block_size_loom, function(i) {
      col_indices <- seq(i, ncol(data_subset_loom), by = block_size_loom)
      values <- unlist(data_subset_loom[, col_indices], use.names = FALSE)
      return(values)
    })
    
    # Create tibble from the collapsed list and name columns 1 to 8
    final_tibble_loom <- as_tibble(setNames(collapsed, as.character(1:block_size_loom)))
    
    
    
    #Evaluate for the values
    grape <- (nrow(final_tibble_loom)) / 4  # assuming final_tibble is a data frame/tibble
    #initalize list
    loom_list <- list()
    
    #iterate to reformat the data
    for (i in 1:4) {
      for (g in 0:(grape - 1)) {
        row_index <- 4 * g + (i)
        value1 <- final_tibble_loom[row_index,1]
        loom_list <- c(loom_list, value1, value1)
      }
    }
    
    loom_list = unname(unlist(loom_list))
    
    
    
    # Step 2: Extract columns 7–30 and 39–46
    data_subset_ddate <- tal$data %>%
      select(14:17)  # This selects 4 columns
    
    # Step 3: Optionally verify the number of columns
    stopifnot(ncol(data_subset_ddate) == 4)
    
    # Step 4: If needed, clean column names (e.g., set to "1" through "32")
    colnames(data_subset_ddate) <- as.character(1:4)
    
    # Assume df_subset has 32 columns
    block_size_ddate <- 1
    
    # Collapse into 8 columns
    collapsed_ddate <- lapply(1:block_size_ddate, function(i) {
      col_indices <- seq(i, ncol(data_subset_ddate), by = block_size_ddate)
      values <- unlist(data_subset_ddate[, col_indices], use.names = FALSE)
      return(values)
    })
    
    # Create tibble from the collapsed list and name columns 1 to 8
    final_tibble_ddate <- as_tibble(setNames(collapsed_ddate, as.character(1:block_size_ddate)))
    
    kiwi <- (nrow(final_tibble_ddate)) / 4  # assuming final_tibble is a data frame/tibble
    
    mango = nrow(data_subset_ddate)
    
    ddate_list <- list()
    
    
    for (i in 1:4) {
      for (g in 0:(kiwi - 1)) {
        row_index <- 4 * g + (i)
        value2 <- final_tibble_ddate[row_index,1]
        ddate_list <- c(ddate_list, value2, value2)
      }
    }
    
    
    ddate_list = unname(unlist(ddate_list))
    
    # Convert to POSIXct datetime object
    converted_dates <- as.POSIXct(ddate_list, origin = "1970-01-01", tz = "UTC")
    
    # Format to MM/DD/YYYY or any desired format
    ddate_list <- format(converted_dates, "%m/%d/%Y")
    
    
    
    up <- nrow(tal$data)
    
    
    for (i in 1:up) {
      
      #endnum_list <- c(endnum_list, 1, 2, 3, 4, 5, 6, 7, 8)
      #style_list <- c(style_list, tal$data[i, 5], tal$data[i, 5], tal$data[i, 5], tal$data[i, 5], tal$data[i, 5], tal$data[i, 5], tal$data[i, 5], tal$data[i, 5])
      #product_list <- c(product_list, tal$data[i, 4], tal$data[i, 4], tal$data[i, 4], tal$data[i, 4], tal$data[i, 4], tal$data[i, 4], tal$data[i, 4], tal$data[i, 4])
      #date_list <- c(date_list, tal$data[i, 7], tal$data[i, 7], tal$data[i, 7], tal$data[i, 7], tal$data[i, 7], tal$data[i, 7], tal$data[i, 7], tal$data[i, 7])
      #minimum_list <- c(minimum_list, tal$data[i, 8], tal$data[i, 8], tal$data[i, 8], tal$data[i, 8], tal$data[i, 8], tal$data[i, 8], tal$data[i, 8], tal$data[i, 8])
      #maximum_list <- c(maximum_list, tal$data[i, 9], tal$data[i, 9], tal$data[i, 9], tal$data[i, 9], tal$data[i, 9], tal$data[i, 9], tal$data[i, 9], tal$data[i, 9])
      #heatset_list <- c(heatset_list, tal$data[i, 6], tal$data[i, 6], tal$data[i, 6], tal$data[i, 6], tal$data[i, 6], tal$data[i, 6], tal$data[i, 6], tal$data[i, 6])
    }
    
    endnum_list <- unname(unlist(endnum_list))
    style_list <- unname(unlist(style_list))
    product_list <- unname(unlist(product_list))
    minimum_list <- unname(unlist(minimum_list))
    maximum_list <- unname(unlist(maximum_list))
    heatset_list <- unname(unlist(heatset_list))
    date_list <- unname(unlist(date_list))
    loom_list <- Filter(Negate(is.na), loom_list)
    ddate_list <- Filter(Negate(is.na), ddate_list)
    the1list <- Filter(Negate(is.na), the1list)
    the2list <- Filter(Negate(is.na), the2list)
    the3list <- Filter(Negate(is.na), the3list)
    the4list <- Filter(Negate(is.na), the4list)
    
    # Convert to POSIXct datetime object
    converted_datess <- as.POSIXct(date_list, origin = "1970-01-01", tz = "UTC")
    
    # Format to MM/DD/YYYY or any desired format
    date_list <- format(converted_datess, "%m/%d/%Y")
    
    print(style_list)
    print(product_list)
    print(date_list)
    print(minimum_list)
    print(maximum_list)
    print(loom_list)
    print(heatset_list)
    print(endnum_list)
    print(ddate_list)
    print(the1list)
    
    
    resultant_df <- data.frame(
      Style = style_list,
      Product = product_list,
      Date = date_list,
      Minimum = minimum_list,
      Maximum = maximum_list,
      Loom = loom_list,
      Heat_Set_MO = heatset_list,
      End_Number = endnum_list,
      Doff_Date = ddate_list,
      Width_Result_1 = the1list,
      Width_Result_2 = the2list,
      Width_Result_3 = the3list,
      Width_Result_4 = the4list
    )
    
    # Combine all lists into one for checking
    lists <- list(
      style_list, product_list, date_list, minimum_list, maximum_list,
      loom_list, heatset_list, endnum_list, ddate_list,
      the1list, the2list, the3list, the4list
    )
    
    # Check if all lists are non-empty and of equal length
    valid_lengths <- sapply(lists, function(x) !is.null(x) && length(x) > 0)
    same_length <- length(unique(sapply(lists, length))) == 1
    
    if (all(valid_lengths) && same_length) {
        sheet_append(ss= sheet_id, resultant_df)
        tal_empty <- tal$data[0,]
      
        range_clear(ss = sheet_id2)
      
        range_write(ss = sheet_id2, data = tal_empty, col_names = TRUE )
      # Append to sheet here
    } else {
      message("Skipping append: One or more lists are empty or lengths don't match.")
    }

    
    
}, ignoreNULL = FALSE)
  
  
  # when the button "Reload Data" on the CPK Graphs tab is clicked the data is updated
  observeEvent(input$reload_cpk, {
    vals$data <- read_sheet(sheet_id)
  }, ignoreNULL = FALSE)
  
  # this creates the histogram on the CPK Graph tab 
  # this creates the histogram on the CPK Graph tab 
  output$mistPlot <- renderPlot({
    req(filtered_data(), input$styleChoice_cpk)
    
    # Filter by style from filtered date range
    new_data <- filtered_data() %>%
      filter(grepl(input$styleChoice_cpk, Style, ignore.case = TRUE))
    
    #requires these values to exist before proceeding
    
    
    # filters the data by the style selected in the dropdown with the ID 'styleChoice_cpk'
   
    
    # filters the data and leaves columns the data points (ex: "Width Result #...")
    other_data <- new_data[, c("Width Result #1", "Width Result #2",
                               "Width Result #3", "Width Result #4")]
    
    # combined the 4 columns into one long numeric vector which is needed to plot
    flattened <- as.numeric(unlist(other_data))
    
    #this calculates mean and sigma using the long numeric vector
    mu <- mean(flattened, na.rm = TRUE) # removes missing values
    sigma <- sd(flattened, na.rm = TRUE) # removes missing values 
    
    # Pull min/max tolerance from first matching row
    USL <- as.numeric(new_data$Maximum[1])
    LSL <- as.numeric(new_data$Minimum[1])
    
    # this solves for CPK calculation 
    cpu <- (USL - mu) / (3 * sigma)
    cpl <- (mu - LSL) / (3 * sigma)
    cpk <- min(cpu, cpl)
    cpk <- round(cpk,3)
    
    # create and return the CPK histogram
    plot_data <- data.frame(Value = flattened)
    
    ggplot(plot_data, aes(x = Value)) +
      geom_histogram(aes(y = ..density..), binwidth = 0.05, fill = "violet", color = "black") +
      stat_function(fun = dnorm, args = list(mean = mu, sd = sigma), color = "black", size = 1.2,aes(x = Value),inherit.aes = FALSE) +
      geom_vline(xintercept = c(LSL, USL), color = "red", linetype = "dashed") +
      geom_vline(xintercept = mu, color = "black", linetype = "dotted", linewidth = 1.2) +
      annotate("label", x = mu - 1, y = Inf, label = paste("CPK =", cpk),
               vjust = 2, hjust = 0.05, size = 5, color = "blue", fontface = "bold") +
      labs(title = paste(input$styleChoice_cpk, "CPK Distribution"),
           x = "Edge Width (mm)", y = "Count") +
      theme_minimal()
  })
  
  # this creates the histogram on the Tolerance Graph tab 
  output$distPlot <- renderPlot({
    req(vals$data, input$styleChoice_tolerance)
    
    # filters the data by the style selected in the dropdown with the ID 'styleChoice_tolerance'
    new_data <- vals$data[grepl(input$styleChoice_tolerance, vals$data$Style, ignore.case = TRUE), ]
    
    # Then optionally filter by Loom only if selected
    if (!is.null(input$loom_tolerance) && input$loom_tolerance != "Unselected") {
      new_data <- new_data[new_data$Loom == input$loom_tolerance, ]
    }
    
    # filters the data and leaves columns the data points (ex: "Width Result #...")
    other_data <- new_data[, c("Width Result #1", "Width Result #2",
                               "Width Result #3", "Width Result #4")]
    
    # combined the 4 columns into one long numeric vector which is needed to plot
    flattened <- as.numeric(unlist(other_data))
    
    # Pull min/max tolerance from first matching row
    USL <- as.numeric(new_data$Maximum[1])
    LSL <- as.numeric(new_data$Minimum[1])
    
    outCount <- sum(flattened < LSL | flattened > USL)
    
    paste("Out of Range", outCount)
    
    # create and return the tolerance levels histogram
    ggplot(data.frame(Value = flattened), aes(x = Value)) +
      # plots the histogram bars
      geom_histogram(binwidth = 0.05, fill = "skyblue", color = "black", boundary = 0) +
      # creates the lines for the upper and lower bound
      geom_vline(xintercept = c(LSL, USL), color = "red", linetype = "dashed") +
      labs(
        # create labels for title, x-axis, and y-axis 
        title = paste(input$styleChoice_tolerance, "Tolerance Distribution"),
        x = "Edge Width (mm)",
        y = "Count"
      ) +
      theme_minimal()
  })
  
  # using the style column in the data this creates the dropdown choices 
  observe({
    req(vals$data)
    updateSelectInput(session, "styleChoice_tolerance", #on the Tolerance Graphs tab
                      choices = unique(vals$data$Style),
                      selected = unique(vals$data$Style)[1])
  })
  
  # using the style column in the data this creates the dropdown choices   
  observe({
    req(vals$data)
    updateSelectInput(session, "styleChoice_cpk", #on the CPK Graphs tab
                      choices = unique(vals$data$Style),
                      selected = unique(vals$data$Style)[1])
  })
  
  observe({
    req(vals$data, input$styleChoice_tolerance)
    
    # Filter data by selected Style
    filtered_data <- vals$data[vals$data$Style == input$styleChoice_tolerance, ]
    
    # Get unique Loom values for that Style
    loom_options <- unique(filtered_data$Loom)
    
    # Update Loom dropdown
    updateSelectInput(session, "loom_tolerance",
                      choices =  c("Unselected", loom_options),
                      selected = loom_options[1])
  })
  
  observe({
    req(pal$data, input$styleChoice_tolerance_pre)
    
    # Filter data by selected Style
    filtered_data <- pal$data[pal$data$Style == input$styleChoice_tolerance_pre, ]
    
    # Get unique Loom values for that Style
    loom_options <- unique(filtered_data$Loom)
    
    # Update Loom dropdown
    updateSelectInput(session, "loom_tolerance",
                      choices =  c("Unselected", loom_options),
                      selected = loom_options[1])
  })
  
  output$outCount <- renderText({
    req(vals$data, input$styleChoice_tolerance)
    
    # Filter the data based on selected style
    new_data <- vals$data[grepl(input$styleChoice_tolerance, vals$data$Style, ignore.case = TRUE), ]
    
    if (!is.null(input$loom_tolerance) && input$loom_tolerance != "Unselected") {
      new_data <- new_data[new_data$Loom == input$loom_tolerance, ]
    }
    
    # Extract numeric columns (replace with correct column names)
    other_data <- new_data[, c("Width Result #1", "Width Result #2",
                               "Width Result #3", "Width Result #4")]
    
    # Flatten to single numeric vector
    flattened <- as.numeric(unlist(other_data))
    
    # Get LSL and USL
    USL <- as.numeric(new_data$Maximum[1])
    LSL <- as.numeric(new_data$Minimum[1])
    
    # Remove NAs
    flattened <- flattened[!is.na(flattened)]
    
    # Count in and out of range
    in_range <- sum(flattened >= LSL & flattened <= USL)
    out_range <- sum(flattened < LSL | flattened > USL)
    total <- length(flattened)
    
    # Display result
    paste0("Total Points: ", total, 
           " | In Range: ", in_range, 
           " | Out of Range: ", out_range)
  })
 
  
  # when the button "Reload Data" on the CPK tab data is updated 
  observeEvent(input$reload_cpk_table, {
    vals$data <- read_sheet(sheet_id)
  }, ignoreNULL = FALSE)
  
  output$tableMaker <- renderDT({
    req(vals$data)
    
    style_cpk_table <- vals$data %>%
      group_by(Style) %>%
      summarise(
        # Extract all numeric measurements per group
        measurements = unlist(select(cur_data(), matches("Width Result"))),
        mu = mean(measurements, na.rm = TRUE),
        sigma = sd(measurements, na.rm = TRUE),
        USL = first(Maximum),
        LSL = first(Minimum),
        CPK = round(pmin((USL - mu) / (3 * sigma), (mu - LSL) / (3 * sigma)), 3),
        .groups = "drop"
      ) %>%
      distinct(Style, .keep_all = TRUE) %>%    # Ensure one row per style
      select(Style, CPK) %>%
      arrange(desc(CPK))                       # 🔥 Sort by descending CPK
    
    datatable(style_cpk_table, options = list(pageLength = 10))
  })
  dateRangeInput("date_range",
                 label = "Select Date Range:",
                 start = Sys.Date() - 30,
                 end = Sys.Date(),
                 min = "2020-01-01",
                 max = Sys.Date())
  filtered_data <- reactive({
    req(vals$data, input$date_range)
    
    vals$data %>%
      filter(Date >= input$date_range[1] & Date <= input$date_range[2])
  })
  
  
}