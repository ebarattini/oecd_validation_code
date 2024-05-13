####################################################################################################
## dashboard setup function
# this code establishes the settings that are called when the dashboard is displayed

# it contains (in order):
# 1 - vectors of all the indicators by group, necessary for the dropdown menu
# 2 - the UI settings, which determines which components are displayed on the dashboard
# 3 - a call the shiny app to display the dashboard

####################################################################################################

# 1 - vectors of all the indicators by group, necessary for the dropdown menu
# Create a combined named vector
indicator_choices <- c(
  # AA indicators
  "ADMRASTH - Asthma hospital admission" = "ADMRASTH",
  "ADMRCOPD - Chronic Obstructive Pulmonary Diseases (COPD) hospital admission" = "ADMRCOPD",
  "ADMRCHFL - Congestive Heart Failure (CHF) hospital admission" = "ADMRCHFL",
  "ADMRHYPT - Hypertension hospital admission" = "ADMRHYPT",
  "ADMRDBUC - Diabetes hospital admission" = "ADMRDBUC",
  "ADMRDBLE - Diabetes lower extremity amputation using unlinked data" = "ADMRDBLE",
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
  "ICCHFCF - CHF – Case fatality within 30 days of the admission date" = "ICCHFCF",
  
  # MH indicators
  "SUICMENT - In-patient suicide among patients diagnosed with a mental disorder" = "SUICMENT",
  "MORTSUMD - Suicide within 1 year after discharge among patients diagnosed with a mental disorder" = "MORTSUMD",
  "MORTSUMS - Suicide within 30 days after discharge among patients diagnosed with a mental disorder" = "MORTSUMS",
  "EXCESCHI - Excess mortality in people diagnosed with schizophrenia" = "EXCESCHI",
  "EXCEBIPO - Excess mortality in people diagnosed with bipolar disorder" = "EXCEBIPO",
  
  # PE indicators
  "COSKCOST - Consultation skipped due to costs" = "COSKCOST",
  "MTSKCOST - Medical tests treatment or follow-up skipped due to costs" = "MTSKCOST",
  "PMSKCOST - Prescribed medicines skipped due to costs" = "PMSKCOST",
  "HPRTIPAT - Doctor spending enough time with patients during the consultation" = "HPRTIPAT",
  "RHPTIPAT - Regular doctor spending enough time with patients during the consultation" = "RHPTIPAT",
  "HPREXCLA - Doctor providing easy-to-understand explanations" = "HPREXCLA",
  "RHPEXCLA - Regular doctor providing easy-to-understand explanations" = "RHPEXCLA",
  "HPRGOASK - Doctor giving opportunity to ask questions or raise concerns" = "HPRGOASK",
  "RHPGOASK - Regular doctor giving opportunity to ask questions or raise concerns" = "RHPGOASK",  
  "HPRIPDEC - Doctor involving patients in decisions about care or treatment" = "HPRIPDEC",
  "RHPIPDEC - Regular doctor involving patients in decisions about care or treatment" = "RHPIPDEC",
  "HPRCORES - Doctor treating patients in decisions about care or treatment" = "HPRCORES",
  "RHPCORES - Regular doctor involving patients in decisions about care or treatment" = "RHPCORES",
  
  # MP indicators
  "MPIPRESP - Care providers treating mental health patients with courtesy and respect (inpatient care)" = "MPIPRESP",
  "MPCBRESP - Care providers treating mental health patients with courtesy and respect (community-based care)" = "MPCBRESP",
  "MPIPTIME - Care providers spending enough time with mental health patients (inpatient care)" = "MPIPTIME",
  "MPCBTIME - Care providers spending enough time with mental health patients (community-based care)" = "MPCBTIME",
  "MPIPEXPL - Care providers providing easy-to-understand explanations to mental health patients (inpatient care)" = "MPIPEXPL",
  "MPCBEXPL - Care providers providing easy-to-understand explanations to mental health patients (community-based care)" = "MPCBEXPL",
  "MPIPINVO - Care providers involving mental health patient in decisions about care and treatment (inpatient care)" = "MPIPINVO",
  "MPCBINVO - Care providers involving mental health patient in decisions about care and treatment (community-based care)" = "MPCBINVO",
  
  
  # PS indicators
  "FORBPROC - Retained surgical item or unretrieved device fragment" = "FORBPROC",
  "POSTPESP - Postoperative pulmonary embolism - hip and knee replacement discharges" = "POSTPESP",
  "POSTDVSP - Postoperative deep vein thrombosis - hip and knee replacement discharges" = "POSTDVSP",
  "POSTSESP - Postoperative sepsis - abdominal discharges" = "POSTSESP",
  "OBSTVDWI - Obstetric trauma vaginal delivery with instrument" = "OBSTVDWI",
  "OBSTVDWO - Obstetric trauma vaginal delivery without instrument" = "OBSTVDWO",
  
  # EC indicators
  "MORTACUT - Deaths in inpatient acute care" = "MORTACUT",
  "MORTHOSP - Deaths in hospital" = "MORTHOSP",
  "ADMMOCAN - Unplanned/urgent in-patient admissions during the last 30 days of life cancer deaths" = "ADMMOCAN",
  "ADMMOCAR - Unplanned/urgent in-patient admissions during the last 30 days of life cardiovascular diseases’ deaths" = "ADMMOCAR",
  "ADMMOCAN - Unplanned/urgent in-patient admissions during the last 30 days of life for cancer deaths" = "ADMMOCAN",
  "ADMMOCAR - Unplanned/urgent in-patient admissions during the last 30 days of life for cardiovascular diseases’ deaths" = "ADMMOCAR",
  "ADMMOCHR - Unplanned/urgent in-patient admissions during the last 30 days of life for chronic respiratory diseases’ deaths" = "ADMMOCHR",
  "ADMMOALZ - Unplanned/urgent in-patient admissions during the last 30 days of life for Alzheimer's and other dementias’ deaths" = "ADMMOALZ",
  "ADMMOALL - Unplanned/urgent in-patient admissions during the last 30 days of life for all causes of death" = "ADMMOALL",
  "ADMDECAN - Unplanned/urgent in-patient admissions during the last 180 days of life for cancer deaths" = "ADMDECAN",
  "ADMDECAR - Unplanned/urgent in-patient admissions during the last 180 days of life for cardiovascular diseases’ deaths" = "ADMDECAR",
  "ADMDECHR - Unplanned/urgent in-patient admissions during the last 180 days of life for chronic respiratory diseases’ deaths" = "ADMDECHR",
  "ADMDEALZ - Unplanned/urgent in-patient admissions during the last 180 days of life for Alzheimer's and other dementias’ deaths" = "ADMDEALZ",
  "ADMDEALL - Unplanned/urgent in-patient admissions during the last 180 days of life for all causes of death" = "ADMDEALL"
  
)



# define the indicator sets, in alphabetical order
group_indicators <- list(
  AA = c(
    "ADMRASTH - Asthma hospital admission" = "ADMRASTH",
    "ADMRCOPD - Chronic Obstructive Pulmonary Diseases (COPD) hospital admission" = "ADMRCOPD",
    "ADMRCHFL - Congestive Heart Failure (CHF) hospital admission" = "ADMRCHFL",
    "ADMRHYPT - Hypertension hospital admission" = "ADMRHYPT",
    "ADMRDBUC - Diabetes hospital admission" = "ADMRDBUC",
    "ADMRDBLE - Diabetes lower extremity amputation using unlinked data" = "ADMRDBLE",
    "PATRDBLE - Diabetes lower extremity amputation using linked data" = "PATRDBLE"
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
  
  
  EC = c(
    "MORTACUT - Deaths in inpatient acute care" = "MORTACUT",
    "MORTHOSP - Deaths in hospital" = "MORTHOSP",
    "ADMMOCAN - Unplanned/urgent in-patient admissions during the last 30 days of life cancer deaths" = "ADMMOCAN",
    "ADMMOCAR - Unplanned/urgent in-patient admissions during the last 30 days of life cardiovascular diseases’ deaths" = "ADMMOCAR",
    "ADMMOCAN - Unplanned/urgent in-patient admissions during the last 30 days of life for cancer deaths" = "ADMMOCAN",
    "ADMMOCAR - Unplanned/urgent in-patient admissions during the last 30 days of life for cardiovascular diseases’ deaths" = "ADMMOCAR",
    "ADMMOCHR - Unplanned/urgent in-patient admissions during the last 30 days of life for chronic respiratory diseases’ deaths" = "ADMMOCHR",
    "ADMMOALZ - Unplanned/urgent in-patient admissions during the last 30 days of life for Alzheimer's and other dementias’ deaths" = "ADMMOALZ",
    "ADMMOALL - Unplanned/urgent in-patient admissions during the last 30 days of life for all causes of death" = "ADMMOALL",
    "ADMDECAN - Unplanned/urgent in-patient admissions during the last 180 days of life for cancer deaths" = "ADMDECAN",
    "ADMDECAR - Unplanned/urgent in-patient admissions during the last 180 days of life for cardiovascular diseases’ deaths" = "ADMDECAR",
    "ADMDECHR - Unplanned/urgent in-patient admissions during the last 180 days of life for chronic respiratory diseases’ deaths" = "ADMDECHR",
    "ADMDEALZ - Unplanned/urgent in-patient admissions during the last 180 days of life for Alzheimer's and other dementias’ deaths" = "ADMDEALZ",
    "ADMDEALL - Unplanned/urgent in-patient admissions during the last 180 days of life for all causes of death" = "ADMDEALL"
    
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

  
  MH = c(
    "SUICMENT - In-patient suicide among patients diagnosed with a mental disorder" = "SUICMENT",
    "MORTSUMD - Suicide within 1 year after discharge among patients diagnosed with a mental disorder" = "MORTSUMD",
    "MORTSUMS - Suicide within 30 days after discharge among patients diagnosed with a mental disorder" = "MORTSUMS",
    "EXCESCHI - Excess mortality in people diagnosed with schizophrenia" = "EXCESCHI",
    "EXCEBIPO - Excess mortality in people diagnosed with bipolar disorder" = "EXCEBIPO"
  ),
  
  
  MP = c(
    "MPIPRESP - Care providers treating mental health patients with courtesy and respect (inpatient care)" = "MPIPRESP",
    "MPCBRESP - Care providers treating mental health patients with courtesy and respect (community-based care)" = "MPCBRESP",
    "MPIPTIME - Care providers spending enough time with mental health patients (inpatient care)" = "MPIPTIME",
    "MPCBTIME - Care providers spending enough time with mental health patients (community-based care)" = "MPCBTIME",
    "MPIPEXPL - Care providers providing easy-to-understand explanations to mental health patients (inpatient care)" = "MPIPEXPL",
    "MPCBEXPL - Care providers providing easy-to-understand explanations to mental health patients (community-based care)" = "MPCBEXPL",
    "MPIPINVO - Care providers involving mental health patient in decisions about care and treatment (inpatient care)" = "MPIPINVO",
    "MPCBINVO - Care providers involving mental health patient in decisions about care and treatment (community-based care)" = "MPCBINVO"
    
  ),
  
  
  PE = c(
    "COSKCOST - Consultation skipped due to costs" = "COSKCOST",
    "MTSKCOST - Medical tests treatment or follow-up skipped due to costs" = "MTSKCOST",
    "PMSKCOST - Prescribed medicines skipped due to costs" = "PMSKCOST",
    "HPRTIPAT - Doctor spending enough time with patients during the consultation" = "HPRTIPAT",
    "RHPTIPAT - Regular doctor spending enough time with patients during the consultation" = "RHPTIPAT",
    "HPREXCLA - Doctor providing easy-to-understand explanations" = "HPREXCLA",
    "RHPEXCLA - Regular doctor providing easy-to-understand explanations" = "RHPEXCLA",
    "HPRGOASK - Doctor giving opportunity to ask questions or raise concerns" = "HPRGOASK",
    "RHPGOASK - Regular doctor giving opportunity to ask questions or raise concerns" = "RHPGOASK",  
    "HPRIPDEC - Doctor involving patients in decisions about care or treatment" = "HPRIPDEC",
    "RHPIPDEC - Regular doctor involving patients in decisions about care or treatment" = "RHPIPDEC",
    "HPRCORES - Doctor treating patients in decisions about care or treatment" = "HPRCORES",
    "RHPCORES - Regular doctor involving patients in decisions about care or treatment" = "RHPCORES"
  ),
  

  PS = c(
    "FORBPROC - Retained surgical item or unretrieved device fragment" = "FORBPROC",
    "POSTPESP - Postoperative pulmonary embolism - hip and knee replacement discharges" = "POSTPESP",
    "POSTDVSP - Postoperative deep vein thrombosis - hip and knee replacement discharges" = "POSTDVSP",
    "POSTSESP - Postoperative sepsis - abdominal discharges" = "POSTSESP",
    "OBSTVDWI - Obstetric trauma vaginal delivery with instrument" = "OBSTVDWI",
    "OBSTVDWO - Obstetric trauma vaginal delivery without instrument" = "OBSTVDWO"
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

# 2 - the UI settings
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
      labs(x = "Year", y = "Rate", title = paste("Graph for", input$indicator)) +
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

# 3 - a call the shiny app to display the dashboard
shinyApp(ui, server)