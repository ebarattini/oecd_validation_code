# add a vector of Indicator names for better visualisation
# Create a combined named vector
indicator_choices <- c(
  # AA indicators
  "ADMRASTH - Asthma hospital admission" = "ADMRASTH",
  "ADMRCOPD - Chronic Obstructive Pulmonary Diseases (COPD) hospital admission" = "ADMRCOPD",
  "ADMRCHFL - Congestive Heart Failure (CHF) hospital admission" = "ADMRCHFL",
  "ADMRHYPT - Hypertension hospital admission" = "ADMRHYPT",
  "ADMRDBUC - Diabetes hospital admission" = "ADMRDBUC",
  "PATRDBLE - Diabetes lower extremity amputation using linked data" = "PATRDBLE",
  
  # PR indicators
  "PRDMPCDD - Adequate use of cholesterol lowering treatment in people with diabetes" = "PRDMPCDD",
  "PRDMPADD - First choice antihypertensives for people with diabetes" = "PRDMPADD",
  "PRBZOZDD - Long-term use of benzodiazepines and related drugs in older people" = "PRBZOZDD",
  "PRBZLAOP - Use of long-acting benzodiazepines in older people" = "PRBZLAOP",
  "PRABCQDD - Volume of cephalosporines and quinolones as a proportion of all antibiotics" = "PRABCQDD",
  "PRABOUDD - Overall volume of antibiotics for systemic use prescribed" = "PRABOUDD",
  "PRACCUNS - Any anticoagulating drug in combination with an oral NSAID" = "PRACCUNS",
  "PRPPOPFM - Proportion of 75 years and over taking more than 5 medications" = "PRPPOPFM",
  "PROPOUDD - Overall volume of opioids prescribed" = "PROPOUDD",
  "PROPPCOU - Proportion of the population who are chronic opioid users" = "PROPPCOU",
  "PRPPANTI - Proportion of people 65 and over prescribed antipsychotics" = "PRPPANTI",
  
  # AC indicators
  "MORTAMIO - AMI 30 day mortality using linked data" = "MORTAMIO",
  "MORTAMII - AMI 30 day mortality using unlinked data" = "MORTAMII",
  "MORTHSTO - Hemorrhagic stroke 30 day mortality using linked data" = "MORTHSTO",
  "MORTHSTI - Hemorrhagic stroke 30 day mortality using unlinked data" = "MORTHSTI",
  "MORTISTO - Ischemic stroke 30 day mortality using linked data" = "MORTISTO",
  "MORTISTI - Ischemic stroke 30 day mortality using unlinked data" = "MORTISTI",
  "IHWTHIPS - Hip fracture surgery initiated within 2 days after admission" = "IHWTHIPS",
  
  # IC indicators
  "ICISCACR - Ischemic Stroke – All-cause hospital readmissions within 365 days after discharge" = "ICISCACR",
  "ICISCDSR - Ischemic Stroke – Disease-specific hospital readmissions within 365 days after discharge" = "ICISCDSR",
  "ICISCACM - Ischemic Stroke – All-cause mortality within 365 days after discharge" = "ICISCACM",
  "ICISCMACR - Ischemic Stroke – Mortality or all-cause readmission within 365 days after discharge" = "ICISCMACR",
  "ICISCMDSR - Ischemic Stroke – Mortality or disease-specific readmission within 365 days after discharge" = "ICISCMDSR",
  "ICCHFACR - CHF – All-cause hospital readmissions within 365 days after discharge" = "ICCHFACR",
  "ICCHFDSR - CHF – Disease-specific hospital readmissions within 365 days after discharge" = "ICCHFDSR",
  "ICCHFACM - CHF – All-cause mortality within 365 days after discharge" = "ICCHFACM",
  "ICCHFMACR - CHF – Mortality or all-cause readmission within 365 days after discharge" = "ICCHFMACR",
  "ICCHFMDSR - CHF – Mortality or disease-specific readmission within 365 days after discharge" = "ICCHFMDSR",
  "ICCHFCF - CHF – Case fatality within 30 days of the admission date" = "ICCHFCF"
  
)

# ++ others to be added



# define the indicator sets
group_indicators <- list(
  AA = c(
    "ADMRASTH - Asthma hospital admission" = "ADMRASTH",
    "ADMRCOPD - Chronic Obstructive Pulmonary Diseases (COPD) hospital admission" = "ADMRCOPD",
    "ADMRCHFL - Congestive Heart Failure (CHF) hospital admission" = "ADMRCHFL",
    "ADMRHYPT - Hypertension hospital admission" = "ADMRHYPT",
    "ADMRDBUC - Diabetes hospital admission" = "ADMRDBUC",
    "PATRDBLE - Diabetes lower extremity amputation using linked data" = "PATRDBLE"
  ),
  PR = c(
    "PRDMPCDD - Adequate use of cholesterol lowering treatment in people with diabetes" = "PRDMPCDD",
    "PRDMPADD - First choice antihypertensives for people with diabetes" = "PRDMPADD",
    "PRBZOZDD - Long-term use of benzodiazepines and related drugs in older people" = "PRBZOZDD",
    "PRBZLAOP - Use of long-acting benzodiazepines in older people" = "PRBZLAOP",
    "PRABCQDD - Volume of cephalosporines and quinolones as a proportion of all antibiotics" = "PRABCQDD",
    "PRABOUDD - Overall volume of antibiotics for systemic use prescribed" = "PRABOUDD",
    "PRACCUNS - Any anticoagulating drug in combination with an oral NSAID" = "PRACCUNS",
    "PRPPOPFM - Proportion of 75 years and over taking more than 5 medications" = "PRPPOPFM",
    "PROPOUDD - Overall volume of opioids prescribed" = "PROPOUDD",
    "PROPPCOU - Proportion of the population who are chronic opioid users" = "PROPPCOU",
    "PRPPANTI - Proportion of people 65 and over prescribed antipsychotics" = "PRPPANTI"
  ),
  AC = c(
    "MORTAMIO - AMI 30 day mortality using linked data" = "MORTAMIO",
    "MORTAMII - AMI 30 day mortality using unlinked data" = "MORTAMII",
    "MORTHSTO - Hemorrhagic stroke 30 day mortality using linked data" = "MORTHSTO",
    "MORTHSTI - Hemorrhagic stroke 30 day mortality using unlinked data" = "MORTHSTI",
    "MORTISTO - Ischemic stroke 30 day mortality using linked data" = "MORTISTO",
    "MORTISTI - Ischemic stroke 30 day mortality using unlinked data" = "MORTISTI",
    "IHWTHIPS - Hip fracture surgery initiated within 2 days after admission" = "IHWTHIPS"
  ),
  IC = c(
    "ICISCACR - Ischemic Stroke – All-cause hospital readmissions within 365 days after discharge" = "ICISCACR",
    "ICISCDSR - Ischemic Stroke – Disease-specific hospital readmissions within 365 days after discharge" = "ICISCDSR",
    "ICISCACM - Ischemic Stroke – All-cause mortality within 365 days after discharge" = "ICISCACM",
    "ICISCMACR - Ischemic Stroke – Mortality or all-cause readmission within 365 days after discharge" = "ICISCMACR",
    "ICISCMDSR - Ischemic Stroke – Mortality or disease-specific readmission within 365 days after discharge" = "ICISCMDSR",
    "ICCHFACR - CHF – All-cause hospital readmissions within 365 days after discharge" = "ICCHFACR",
    "ICCHFDSR - CHF – Disease-specific hospital readmissions within 365 days after discharge" = "ICCHFDSR",
    "ICCHFACM - CHF – All-cause mortality within 365 days after discharge" = "ICCHFACM",
    "ICCHFMACR - CHF – Mortality or all-cause readmission within 365 days after discharge" = "ICCHFMACR",
    "ICCHFMDSR - CHF – Mortality or disease-specific readmission within 365 days after discharge" = "ICCHFMDSR",
    "ICCHFCF - CHF – Case fatality within 30 days of the admission date" = "ICCHFCF"
  ),
  
  MP = c(
    
  ),
  PS = c(
  ),
  
  EC = c(
    
  )
  
)

viz_df <- merged_df %>%
  select(MEASURE, SEX, REF_AREA, TIME_PERIOD, AGE, UNIT, OBS_VALUE) %>%
  mutate(is_ci = if_else(UNIT %in% c("up_ci", "lo_ci"), TRUE, FALSE)) %>%
  group_by(MEASURE, SEX, REF_AREA, TIME_PERIOD, AGE) %>%
  summarise(
    Rate = round(mean(OBS_VALUE[!is_ci], na.rm = TRUE), 2),  # Round the mean rate to two decimal places
    CI_Lower = if(all(is.na(OBS_VALUE[is_ci]))) NA_real_ else round(min(OBS_VALUE[is_ci], na.rm = TRUE), 2),  # Round the CI lower bound
    CI_Upper = if(all(is.na(OBS_VALUE[is_ci]))) NA_real_ else round(max(OBS_VALUE[is_ci], na.rm = TRUE), 2),  # Round the CI upper bound
    .groups = "drop"
  )



# Add a option for codes without predefined names
all_codes <- unique(viz_df$MEASURE)

# Add undefined codes to indicator_choices
for (code in all_codes) {
  if (!code %in% indicator_choices) {
    indicator_choices[paste(code, "")] <- code
  }
}


# the ui determines which components are displayed on the dashboard
ui <- fluidPage(
  titlePanel("Healthcare Data Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("grouping", "Indicator Group", choices = names(group_indicators)),
      selectInput("indicator", "Indicator", choices = NULL),  # the selection will be updated based on group selection
      selectInput("SEX", "SEX", choices = c("All" = "All", "M" = "M", "F" = "F")),
      selectInput("REF_AREA", "REF_AREA", choices = c("All" = "All", unique(viz_df$REF_AREA))),
      radioButtons("graphType", "Graph Type", choices = c("Bar" = "bar", "Line" = "line")),
      checkboxInput("showCI", "Show Confidence Intervals", value = FALSE)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Table", DTOutput("table")),
        tabPanel("Graph", plotOutput("graph"))
      )
    )
  )
)


colors <- c("F" = "#008080", "M" = "#E69500", "T" = "#BEBEBE") 


server <- function(input, output, session) {
  observe({
    group <- input$grouping
    updateSelectInput(session, "indicator", choices = group_indicators[[group]])
  })
  
  filtered_data <- reactive({
    data <- viz_df
    
    if (input$SEX != "All") {
      data <- data %>% filter(SEX == input$SEX)
    }
    
    if (input$REF_AREA != "All") {
      data <- data %>% filter(REF_AREA == input$REF_AREA)
    }
    
    if (input$indicator != "") {
      data <- data %>% filter(MEASURE == input$indicator)
    }
    
    data
  })
  
  output$table <- renderDT({
    filtered_data()
  })
  
  output$graph <- renderPlot({
    data <- filtered_data()
    y_max <- max(data$Rate, na.rm = TRUE) + 0.05 * max(data$Rate, na.rm = TRUE)  # Adjust y_max to include CI
    y_limits <- c(0, y_max)
    
    p <- ggplot(data, aes(x = TIME_PERIOD, y = Rate, group = SEX)) +
      scale_y_continuous(limits = y_limits) +
      labs(x = "TIME_PERIOD", y = "Rate", title = paste("Graph for", input$indicator)) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
            axis.text.y = element_text(size = 12))
    
    if (input$graphType == "bar") {
      p <- p + geom_bar(stat = "identity", position = position_dodge(), aes(fill = SEX)) +
        scale_fill_manual(values = colors)  # Apply custom colors
      if (input$showCI) {
        p <- p + geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2, position = position_dodge(0.9))
      }
    } else {  # For line graph
      p <- p + geom_line(aes(color = SEX)) +
        geom_point(aes(color = SEX)) +
        scale_color_manual(values = colors)  # Apply custom colors
      if (input$showCI) {
        p <- p + geom_ribbon(aes(ymin = CI_Lower, ymax = CI_Upper, fill = SEX), alpha = 0.2)
      }
    }
    
    p
  })
  
}


shinyApp(ui, server)