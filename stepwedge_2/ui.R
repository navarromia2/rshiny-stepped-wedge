
# MN edited ui 3/25/24

library(shiny)
library (ggplot2)
library (stringr)
#library (grid) 
library (gridBase)
library (gridExtra)
library (shinythemes)
library (textclean)
library (viridis)


options(shiny.fullstacktrace = TRUE)

source("loadFunctions.R", local=F)

DefaultValues <- list(
  num_ICs <- 3,
  #IC names
  H2H_YesNo <- FALSE,
  CohortName <- "Cohort",
  EntityName <- "Sites",
  NumberOfSteps <- 4,
  InterventionConditions <- "Control,Intervention,Sustain",
  InterventionBlocksWhereStepsOccur <- 1
)

# Define UI for app that simulates Stepped Wedge  ----
fluidPage ( theme = shinytheme("united"),
            sidebarLayout(
              sidebarPanel ( "updated 3/25/2024 by Mia Navarro",
                             tabsetPanel (
                               tabPanel ( "ROLL-OUT DESIGN",
                                          tabsetPanel (id = "WhichDesignTabPanel" ,
                                                       tabPanel ( "Intervention: General" , value = "INT_GeneralTabPanel" ,
                                                                  
                                                                  numericInput("num_ICs",
                                                                               "Number of Implementation Conditions",
                                                                               min = 2,
                                                                               value = DefaultValues$num_ICs),
                                                                  
                                                                  uiOutput("ICtextboxes"),
                                                                  
                                                                  checkboxInput( "H2H_YesNo", 
                                                                                 "Is this a head-to-head roll-out trial?" ,
                                                                                 value = DefaultValues$H2H_YesNo) ,
                                                                  
                                                                  conditionalPanel(condition = "input.H2H_YesNo == true",
                                                                                   checkboxGroupInput( "H2H_Groups",
                                                                                                       "Which implementation conditions are being compared?", 
                                                                                                       c("#1" = "1", "#2" = "2", "#3" = "3", "#4" = "4", "#5" = "5"))),
                                                                  
                                                                  textInput("CohortName",
                                                                            "Unit of Assignment",
                                                                            value = DefaultValues$CohortName, placeholder="How are assignment sub-units grouped?"),
                                                                  
                                                                  # Name for Entities (Implementer Unit)
                                                                  textInput("EntityName",
                                                                            "Sub-Unit of Assignment",
                                                                            value = DefaultValues$EntityName, placeholder="Who is implementing the intervention?"),
                                                                  
                                                                  # Number of Cohorts
                                                                  numericInput("NumberOfSteps",
                                                                               "Number of Units",
                                                                               value = DefaultValues$NumberOfSteps, min = 2, step = 1),
                                                                  
                                                                  # How many units per cohort
                                                                  uiOutput("NumberEntitiesperCohort")
                                                                  
                                                       ) ,
                                                       
                                                       tabPanel ( "Intervention: Timing" , value = "INT_TimingTabPanel" ,
                                                                  
                                                                  #Start Date - value is yyyy-mm-dd, even if display format is different
                                                                  dateInput("time_start", 
                                                                            "Intervention Start Date",
                                                                            format = "mm/dd/yyyy"),
                                                                  
                                                                  # Time unit
                                                                  selectInput("TimeUnit", 
                                                                              "Time Unit", 
                                                                              choices = c("Month", "Quarter", "Year")),
                                                                  
                                                                  # # Start Year 
                                                                  # numericInput("time_startYEAR",
                                                                  #              "Year of Intervention Start Date",
                                                                  #              value = 2023, min = 1999, step = 1),
                                                                  # # Start Month
                                                                  # numericInput("time_startMONTH",
                                                                  #              "Month of Intervention Start Date",
                                                                  #              value = 1, min = 1, max = 12, step = 1),
                                                                  
                                                                  #FIX Duration of Conditions
                                                                  numericInput("int_durationCOND1",
                                                                               "Duration of Implementation Condition #1 (# Time Units)",
                                                                               value = 1, min = 1, step = 1),
                                                                  numericInput("int_durationCOND2",
                                                                               "Duration of Implementation Condition #2 (# Time Units)",
                                                                               value = 1, min = 1, step = 1),
                                                                  numericInput("int_durationCOND3",
                                                                               "Duration of Implementation Condition #3 (# Time Units)",
                                                                               value = 1, min = 1, step = 1),
                                                                  numericInput("int_durationCOND4",
                                                                               "Duration of Implementation Condition #4 (# Time Units)",
                                                                               value = 1, min = 1, step = 1),
                                                                  numericInput("int_durationCOND5",
                                                                               "Duration of Implementation Condition #5 (# Time Units)",
                                                                               value = 1, min = 1, step = 1),
                                                                  
                                                                  h5(strong("Customize Timing by Assignment Unit")),
                                                                  checkboxInput("Checkbox_CustomizeTimingbyCohort",
                                                                                "Do you want to adjust timings of specific cohorts?"),
                                                                  
                                                                  conditionalPanel("input.Checkbox_CustomizeTimingbyCohort==true",
                                                                                   textInput("List_CustomTimingCohorts",
                                                                                             "")),
                                                                  
                                                                  checkboxInput("Checkbox_StaggerInitialSteps",
                                                                                "Are starting points staggered across cohorts?"),
                                                                  
                                                                  checkboxInput("Checkbox_CatchUpEndingSteps",
                                                                                "Are endpoints staggered across cohorts?"),
                                                                  
                                                                  checkboxInput("Checkbox_InterventionBlocksWhereStepsOccur",
                                                                                "Are end periods staggered across cohorts?")
                                                                  
                                                       ), 
                                                       
                                                       tabPanel ( "Graph: Settings" , value = "GRAPH_SettingsTabPanel" ,
                                                                  
                                                                  textInput("AssignmentPlotTitle",
                                                                            "Plot Title"), 

                                                                  textInput("AssignmentPlotYaxisLabel",
                                                                            "Y Axis Title"), 

                                                                  textInput("AssignmentPlotXaxisLabel",
                                                                            "X Axis Title"), 

                                                                  textInput("NamesOfEachStep",
                                                                            "Names of Each Step"), 

                                                                  selectInput("NamingYAxisTickMarks",
                                                                              "Y Axis Tick Mark Labels",
                                                                              choices = list("Assignment Units",
                                                                                             "Default",
                                                                                             "Custom"), #currently does not work
                                                                              selected = "Assignment Units"),
                                                                  
                                                                  selectInput("NamingXAxisTickMarks",
                                                                              "X Axis Tick Mark Labels",
                                                                              choices = list("Time Units",
                                                                                             "Dates",
                                                                                             "Default",
                                                                                             "Custom"), #currently does not work
                                                                              selected = "Time Units"),
                                                                  
                                                       ),
                                                       
                                                       tabPanel ( "Graph: Download" , value = "GRAPH_DownloadTabPanel" ,
                                                                  
                                                                  textInput("AssignmentPlotDirectory",  
                                                                            "Directory"), 
                                                                  
                                                                  textInput("AssignmentPlotFileName",  
                                                                            "Base File Name"), 
                                                                  
                                                                  checkboxInput("AddDateAndTime",  
                                                                                "Add Date and Time to FileName"), 
                                                                  
                                                                  selectInput( "PlotFileType",  "Plot File Type", 
                                                                               choices = list(".pdf",
                                                                                              ".emf",
                                                                                              ".wmf" ,
                                                                                              ".jpg"),
                                                                               selected = ".pdf" ) ,
                                                                  
                                                                  numericInput("maxXLabelValues",
                                                                               "Maximum Number of X Labels on Assignment Plot ", value = 10),
                                                                  
                                                                  actionButton ("SaveAssignmentPlot", "Download Graph" )
                                                                  
                                                       ),
                                                       tabPanel ( "ADVANCED" , value = "ADV" ,
                                                                  
                                                                  strong(h4("General")),
                                                                  textInput ( "InterventionConditions_ORIG" , 
                                                                              "List of Conditions (comma-separated list; use colon for head-to-head comparisons)"  ,
                                                                              value = DefaultValues$InterventionConditions , placeholder = "e.g., Control,Intervention A:Intervention B,Sustain" ) ,
                                                                  
                                                                  strong(h4("Timing")),
                                                                  textInput( "StaggerInitialSteps" ,
                                                                             "ADJUST BEGINNING: Are Start Periods Staggered across Cohorts/Steps (y/n)?\n(If start times differ by Cohorts/Steps, give number separated by ',')",
                                                                             value = "n") ,
                                                                  
                                                                  textInput( "CatchUpEndingSteps" ,
                                                                             "ADJUST ENDING: Are Ending Periods Staggered across Steps (y/n)?\n(If different starting periods by Step give number separated by ',')",
                                                                             value = "n"  ) ,
                                                                  
                                                                  textInput("InterventionBlocksWhereStepsOccur",
                                                                            "ADJUST STEPS: Number of Time Periods Where Steps Occur (if more than one separate by ',')",
                                                                            value =  DefaultValues$InterventionBlocksWhereStepsOccur )
                                                                  
                                                       )
                                          )))),
              mainPanel(  ###: : :  Open mainPanel ---- 
                          
                          # verbatimTextOutput( "ReturnDefinition" ),
                          textOutput("FilePrintedOK") ,
                          plotOutput("AssignmentPlot")  ##: : : : plotOutput("AssignmentPlot") ----
                          
                          
              )   ###: : : Close MainPanel  ----
            )
            )
