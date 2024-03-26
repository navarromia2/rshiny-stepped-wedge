#
# This is the server logic of a Shiny web application. 

library(shiny)
library (ggplot2)
library (stringr)
#library (grid) 
library (gridBase)
library (gridExtra)
library (shinythemes)
library (textclean)
library (viridis)

source("loadFunctions.R", local=F)

# Define server logic required to draw a histogram
function(input, output, session) {

  # CONSTANTS
  
  ValidMessage <- CreateValidMessage()
  InputValueFileStem <- "inputVals"
  InputValueFileType <- ".RData" # Default Settings for the Assignment Plot
  
  
  LineWidth <- 20  # standard when all the steps have same number of entities
  the.colors <- c( "blue" , "green" , "yellow" , "orange" , "pink" , "red" , "purple" , sample ( colors( distinct = FALSE ) , size = length (colors( distinct = FALSE ) )))
  stem.InterventionName <- "Implementation Conditions"
  defaultTimeUnit <- "Time Unit"
  defaultEntityName <- "Assigned Units"
  defaultLanguage <- "English"
  default.maxXLabelValues <- 12
  default.WhereToPutLegend <- "bottom"
  
  # Initialize 
  Checks <- list ()
  inputCheck <- list ()
  
  
  # Initialize and provide constants
  
  All.Intervention.Conditions <- NULL
  
  
  #Reactives----
  
  makeFileName_reactive <- reactive (  {
    if ( trimws (input$AssignmentPlotDirectory) == "" )
      slash <- ""
    else
      slash <- "/"
    
    if ( input$AddDateAndTime ) 
      AddDateAndTime <- paste0 ( "." , format( Sys.time(), "%y-%m-%d.%H-%M-%S") )
    else
      AddDateAndTime <- ""
    
    paste0 ( input$AssignmentPlotDirectory , slash , input$AssignmentPlotFileName , AddDateAndTime , ".pdf" )
    
  } )
  
  count.reactive <- eventReactive ( input$SaveAssignmentPlot , { input$SaveAssignmentPlot })
  
  # reactive expression
  fullFileName_reactive <- eventReactive ( input$SaveAssignmentPlot , {
    if ( input$AddDateAndTime  )
      AddDateAndTime <- paste0 ( "." , format( Sys.time(), "%y-%m-%d.%H-%M-%S") )
    else
      AddDateAndTime <- ""
    
    if ( trimws (input$AssignmentPlotDirectory) == "" )
      slash <- ""
    else {
      if ( lastChar (input$AssignmentPlotDirectory ) != .Platform$file.sep )
        slash <- .Platform$file.sep
    }
    
    
    if ( input$AddDateAndTime ) 
      AddDateAndTime <- paste0 ( "." , format( Sys.time(), "%y-%m-%d.%H-%M-%S") )
    else
      AddDateAndTime <- ""
    
    AssignPltTitle <- input$AssignmentPlotTitle 
    if (! is.null ( AssignPltTitle ) || trimws (AssignPltTitle) != "")
      AssignPltTitle <- paste( "." , AssignPltTitle , ".")
    
    paste0 ( input$AssignmentPlotDirectory , slash ,
             input$AssignmentPlotFileName , 
             AddDateAndTime , 
             input$AssignmentPlotTitle ,
             ".",
             input$PlotFileType )
    
  })
  
  savePlot_reactive <- eventReactive( input$SaveAssignmentPlot, {
    
    makeFileName <- function ( AddDateAndTime , 
                               AssignmentPlotDirectory,
                               FileNameStem  ,
                               FileDescription ,
                               FileType ,
                               ReplaceSpace 
    ) {
      if ( AddDateAndTime  )
        AddDateAndTime <- paste0 ( "." , format( Sys.time(), "%y-%m-%d.%H-%M-%S") )
      else
        AddDateAndTime <- ""
      
      if ( trimws ( AssignmentPlotDirectory) == "" )
        slash <- ""
      else {
        if ( lastChar (AssignmentPlotDirectory ) != .Platform$file.sep )
          slash <- .Platform$file.sep
      }
      
      FileDescr <- FileDescription
      if ( ! not.Present ( FileDescr ))
        FileDescr <- paste0( "." , FileDescr )
      if ( ReplaceSpace ) { 
        FileDescr <- gsub ( " " , "_" , FileDescr)
        FileDescr <- gsub ( "\n" , "_" , FileDescr)
        FileDescr <- gsub ( "\t" , "_" , FileDescr)
      }
      
      
      rslt <- paste0 ( AssignmentPlotDirectory , slash , FileNameStem , AddDateAndTime , FileDescr , FileType )
      return ( rslt )
      
    }
    
    validate ( need ( length (input$AssignmentPlotDirectory) == 0 || 
                        trimws (input$AssignmentPlotDirectory) == "" ||
                        ( length(input$AssignmentPlotDirectory) > 0  ) && 
                        dir.exists (input$AssignmentPlotDirectory )  , 
                      paste ("Error:" , input$AssignmentPlotDirectory ,  
                             "Directory does not exist or cannot write to it") 
    ) 
    ) 
    
    ggsave ( fullFileName_reactive ( ))
    
    LatestInputValueFile <- makeFileName ( TRUE , 
                                           input$AssignmentPlotDirectory,
                                           FileNameStem =   "inputVals" ,
                                           FileDescription = input$AssignmentPlotTitle ,
                                           FileType = InputValueFileType , 
                                           ReplaceSpace = TRUE )
    
    save (inputCheck , file = LatestInputValueFile )
    
  })
  
  #### uiOutput("ICtextboxes") ####
  output$ICtextboxes <- renderUI({
    n_IC <- as.integer(input$num_ICs)
    
    lapply(1:n_IC, function(i) {
      textInput(paste0("InterventionCondition",i),
                paste0("Implementation Condition #",i),
                value = paste0("IC #",i))
    })
  })
  
  observe(print(input$InterventionCondition1))
  
  output$FilePrintedOK <- renderText({
    
    savePlot_reactive () 
    
    if (file.exists ( fullFileName_reactive () ))
      paste ( "Saved Assessment Plot Number" , count.reactive ( ) , fullFileName_reactive() )
    else
      paste ( "File " , fullFileName_reactive() , "not saved" )
  })
  
  #### uiOutput(NumberEntitiesperCohort) ####
  # output$NumberEntitiesperCohort <- renderUI({
  #   n_cohorts <- as.integer(input$NumberOfSteps)
  #   str_units <- make_plural(input$CohortName)[1]
  #   str_subunits <- make_plural(input$EntityName)
  #   
  #   lapply(1:n_cohorts, function(i) {
  #     textInput(paste0("NumberEntitiesperCohort",i),
  #               paste("Number of",str_subunits,"in",input$EntityName,i,),
  #               value = 10)
  #   })
  # })
  
  output$InputEntityName <- renderUI({  #### DE-BUG: Number of Entities per Cohort; originally output$InputEntityName ####
    print ("input$EntityName =" )
    print (input$EntityName )
    
    GetExistingInputCheck <- list( )
    StartingInputValueFilename <- "inputVals.Rdata"
    
    
    if ( file.exists ( StartingInputValueFilename )) {
      load  (file = StartingInputValueFilename )
      GetExistingInputCheck <- inputCheck
    }
    
    
    
    
    
    if ( length (GetExistingInputCheck$EntitiesPerStep) == 0 ) {
      NbrEntitiesDefault <- 1
    } else
      NbrEntitiesDefault <- GetExistingInputCheck$EntitiesPerStep
    
    tagList (
      textInput("EntitiesPerStep",
                paste0("Number of ", getName ( unlist (input$EntityName) , "Entity" , plural = TRUE ) , " for each ", getName ( unlist (input$CohortName) , "Cohort" , plural = F ), " \n(If different across Steps separate with ',')" ),
                value = NbrEntitiesDefault )
    )
  })
  
  
  
  
  outputOptions(output,"InputEntityName", suspendWhenHidden = TRUE)
  
  output$UseTimeUnitForTimeElementsPerPeriod <- renderUI({ # output$UseTimeUnitForTimeElementsPerPeriod ----
    
    GetExistingInputCheck <- list( )
    StartingInputValueFilename <- "inputVals.Rdata"
    
    
    if ( file.exists ( StartingInputValueFilename )) {
      load  (file = StartingInputValueFilename )
      GetExistingInputCheck <- inputCheck
    } 
    
    
    
    
    pr (input$WhichDesignTabPanel )
    
    
    if ( length (GetExistingInputCheck$TimeElementsPerPeriod ) == 0 ) { 
      NbrTimeElementsDefault <- 1
    } else
      NbrTimeElementsDefault <- GetExistingInputCheck$TimeElementsPerPeriod 
    
    tagList (
      textInput("TimeElementsPerPeriod",
                paste0("ADJUST TIME SCALE: Number of ", getName ( input$TimeUnit, "Time Element" , plural = TRUE ) , " Between Time Periods\n(If different across Time Periods separate by ',')" ),
                value = NbrTimeElementsDefault ) 
    )
  })
  
  output$UseTimeUnitForNumberPeriodsEachCondition <- renderUI({ # output$UseTimeUnitForNumberPeriodsEachCondition ----
    
    print ("input$TimeUnit =" )
    print (input$TimeUnit )
    
    GetExistingInputCheck <- list( )
    StartingInputValueFilename <- "inputVals.Rdata"
    
    
    if ( file.exists ( StartingInputValueFilename )) {
      load  (file = StartingInputValueFilename )
      GetExistingInputCheck <- inputCheck
    } 
    
    
    
    if ( length (GetExistingInputCheck$NumberPeriodsEachCondition ) == 0 ) { 
      NbrPeriodsEachConditionDefault  <- 1
    } else
      NbrPeriodsEachConditionDefault <- GetExistingInputCheck$NumberPeriodsEachCondition
    
    
    
    moreThanOne <- length (  SeparateStringByCommas (InterventionConditions ) )
    
    tagList (
      
      textInput("NumberPeriodsEachCondition",
                paste0("Number of Time Periods for:  " ,
                       getName ( InterventionConditions  , "Intervention/Implementation Conditions(s)" , plural = FALSE ) , "\n(If different across Interventions separate by ',')") ,
                value =  NbrPeriodsEachConditionDefault  )
    )
  })
  
  
  ###  output$AssignmentPlot ----
  output$AssignmentPlot <- renderPlot({  
    #  Enter text??.R below
    
    
    # set tabsetPanel appropriately for validate input$WhichDesignTabPanel ) > 0 && 
    #           input$WhichDesignTabPanel  == "DesignNumbersTabPanel" )
    
    
    saveFile <- eventReactive( input$SaveAssignmentPlot , {
      ggsave( fileName )
    })
    
    # Functions
    ### copy from here into server 
    
    
    # new 2020 01 31
    
    
    ## new revised function 2020 01 31
    
    
    
    #### Intervention Conditions List ####
    # InterventionConditions_list <- c(input$InterventionCondition1,input$InterventionCondition2,input$InterventionCondition3,input$InterventionCondition4,input$InterventionCondition5)
    
    InterventionConditions_raw <- c()
    
    n_IC <- as.integer(input$num_ICs)
    
    for (k in 1:n_IC) {
      InterventionConditions_raw <- c(InterventionConditions_raw, input[[paste0("InterventionCondition",k)]])
    }
    
    InterventionConditions_list <- c()
    
    for (i in 1:n_IC) {
      currentCondition <- InterventionConditions_raw[i]
      currentCondition <- trimws(currentCondition)
      
      if ((nchar(currentCondition) > 0)) { # && (!(currentCondition %in% InterventionConditions_list))) {
        InterventionConditions_list <- c(InterventionConditions_list,currentCondition)
      }
    }
    
    InterventionConditions <- gsub(", ",",",toString(InterventionConditions_list))
    
    ####Head-to-head comparisons####
    # Need to figure out how to do if they list the head-to-head ICs not next to each other.
    
    
    if ((input$H2H_YesNo==TRUE) && (!is.null(input$H2H_Groups)) && length(input$H2H_Groups)>=2){
      
      H2H_indices <- as.numeric(input$H2H_Groups) #Convert the H2H_Groups to list of integers
      commaPos <- unlist(gregexpr(',', InterventionConditions)) #List of comma positions in list
      
      #Swap the i'th comma/s for a colon
      for (i in 1:(length(H2H_indices)-1)){
        substr(InterventionConditions,
               start=commaPos[H2H_indices[i]],
               stop=commaPos[H2H_indices[i]]) <- ":"
      }
      
    }
    
    Intervention.Blocks <- SeparateStringByCommas (InterventionConditions )
    All.Intervention.Conditions <- SeparateStringByBoth (InterventionConditions )
    
    
    
    # Run first validations
    
    validate(
      need(  ! is.null (input$NumberOfSteps) && input$NumberOfSteps >= 2 & is.wholenumber (input$NumberOfSteps ) , 
             ValidationMessage ( "NumberOfSteps" , Language = copyValue  (input$Language, defaultLanguage ) ) )
    )
    
    
    
    NumberOfSteps <- copyValue (input$NumberOfSteps, 4 )  # defaults to 4 if missing
    
    
    
    
    
    
    # Note, default number of time periods is NumberOfSteps
    
    default.NumberOfTimePeriodsDuringRollout <- NumberOfSteps
    
    ## Retrieve Intervention Names
    
    validate(
      need( ! not.Present (InterventionConditions), 
            ValidationMessage ( "InterventionConditions" , Language = copyValue  (input$Language, defaultLanguage ) ) )
    )
    
    validate(
      need( ! not.Present (InterventionConditions) && 
              length ( SeparateStringByBoth (InterventionConditions ))  <= length ( the.colors ), 
            "Too many interventions, shorten or extend number of colors under Controls" )
    )
    
    validate(
      need( not.Present (input$NumberPeriodsEachCondition) ||
              ( ! not.Present (input$NumberPeriodsEachCondition) && 
                  ! not.Appropriate ( SeparateStringByBoth  ( input$NumberPeriodsEachCondition  ) ) )  , 
            "Please enter Non-Negative Number(s) of Time Periods for Each Condition\n(Single Numeric or Separate by Commas and Colons for Blocking)")
    )
    
    ####Set duration of each Condition.####
    
    NumberPeriodsEachCondition_raw <- c(input$int_durationCOND1,
                                        input$int_durationCOND2,
                                        input$int_durationCOND3,
                                        input$int_durationCOND4,
                                        input$int_durationCOND5)[1:length(All.Intervention.Conditions)]
    
    NumberPeriodsEachCondition.List <- c()
    
    commaPos <- unlist(gregexpr(',', InterventionConditions)) #List of comma positions in list
    colonPos <- unlist(gregexpr(':', InterventionConditions)) #List of comma positions in list
    
    commasAndColons_df <- data.frame(index = c(commaPos, colonPos),
                                     type = c(rep(",",length(commaPos)), rep(":",length(colonPos))))
    
    
    for (i in NumberPeriodsEachCondition_raw) {
      currentCondition_D <- i
      
      if ((!is.null(currentCondition_D))) { # && (!(currentCondition %in% InterventionConditions_list))) {
        NumberPeriodsEachCondition.List <- c(NumberPeriodsEachCondition.List,currentCondition_D)
      } else {
        NumberPeriodsEachCondition.List <- c(NumberPeriodsEachCondition.List,1)
      }
    }
    
    print(NumberPeriodsEachCondition.List)
    
    NumberPeriodsEachCondition.String <- ReplaceColonCommaPositions(InterventionConditions,
                                                                    toString(NumberPeriodsEachCondition.List),1)
    
    #
    
    
    
    # Expand or Contract NumberPeriodsEachCondition to match InterventionConditions
    
    # NumberPeriodsEachCondition.String <- ReplaceColonCommaPositions  ( InterventionConditions , input$NumberPeriodsEachCondition , 1 )
    NumberPeriodsEachCondition <- as.numeric (SeparateStringByBoth (NumberPeriodsEachCondition.String))
    
    sepBlocks <- SeparateStringByCommas ( NumberPeriodsEachCondition.String )
    
    firstWithinBlock <- list ()
    the.Block.Lengths <- list ()
    the.Block <- list ( )
    Numeric.Intervention.Blocks <- NULL
    lastN <- 0
    for ( nblock in 1 : length ( sepBlocks)) {
      the.Block [[ nblock ]] <- SeparateStringByCharacter ( sepBlocks [ nblock ] , ":")
      the.Block.Lengths [[ nblock ]] <- length ( the.Block [[ nblock ]] )
      firstWithinBlock [[ nblock ]] <-  the.Block [[ nblock ]] [ 1 ]
      numeric.text <- paste0 ( ((1 : the.Block.Lengths[[ nblock ]]) + lastN) , collapse = ":")
      Numeric.Intervention.Blocks <-  c ( Numeric.Intervention.Blocks , numeric.text )
      lastN <- lastN + the.Block.Lengths[[ nblock ]]
    }
    NumberPeriodsEachBlock <- as.numeric ( unlist ( firstWithinBlock ))
    
    #Replace all the names with numbers, ignoring ones that are repetitions
    Numeric.All.Intervention.Conditions <- 1 : length ( All.Intervention.Conditions ) # needed if names not all unique e.g. aba design
    
    
    
    
    
    NumberOfInterventionBlocks <- length( Intervention.Blocks )
    NumberOfAll.Intervention.Conditions <- length ( All.Intervention.Conditions )
    
    
    onlyConditionsWithinBlocks <- list()  # initialize, keep blank if no blocks of conditions
    NumericonlyConditionsWithinBlocks <- list ( )
    BlockLocations <- NULL  # vector of all intervention.block locations where there is a block of more than 1 condition
    NumberOfBlockContrasts <- 0
    productOfBlockSizes <- 1
    if ( NumberOfAll.Intervention.Conditions == NumberOfInterventionBlocks ) {
      HasBlocks <- FALSE
      BlockLocations <- NULL
      for ( i in 1 : length ( Intervention.Blocks ))
        onlyConditionsWithinBlocks [[ i ]] <- ""
    } else {
      HasBlocks <- TRUE
      
      for ( i in 1 : length ( Intervention.Blocks)) {
        splitUpBlock <- trimws (SeparateStringByCharacter ( Intervention.Blocks [ i ] , ":"))
        NumericSplitUpBlock <- SeparateStringByCharacter ( Numeric.Intervention.Blocks [ i ] , ":")
        if ( length ( splitUpBlock ) > 1 ) {
          NumberOfBlockContrasts <- NumberOfBlockContrasts  + 1
          BlockLocations <- c( BlockLocations , i )
          onlyConditionsWithinBlocks [[ i ]] <- splitUpBlock
          NumericonlyConditionsWithinBlocks [[ i ]] <- NumericSplitUpBlock
          productOfBlockSizes <- productOfBlockSizes * length ( splitUpBlock )
        }
      }
      
    }
    
    nr <- NumberBlockCombinations  ( Intervention.Blocks, BlockLocations , onlyConditionsWithinBlocks )
    
    
    validate(
      need( not.Present (input$EntitiesPerStep) || ( ! not.Present (input$EntitiesPerStep)  && 
                                                       (! not.Appropriate ( SeparateStringByBoth  ( input$EntitiesPerStep   ) ) ) )  , 
            "Please enter Non-Negative Number(s) of Entities Per Step\n(Single Numeric or Separate by Commas and Colons for Blocking)")
    )
    
    # Make this into a vector of the number of entities in every step
    EntitiesEveryStep <- CombineSteps2  ( input$EntitiesPerStep , 1 , NumberOfSteps * productOfBlockSizes )
    
    
    TimeUnit <- copyValue ( input$TimeUnit , defaultTimeUnit )
    
    
    
    validate(
      need( not.Present (input$TimeElementsPerPeriod) ||
              (!not.Present ( input$TimeElementsPerPeriod ) &&
                 length (input$TimeElementsPerPeriod ) == 1 ) , 
            labelWithPluralName (TimeUnit , "Time Element" ) )
    )
    
    # need to wait until later to correctly assign TimeElementsPerPeriod 
    
    
    
    validate(
      need( checkNonNegWholenumber ( as.numeric ( SeparateStringByCommas  ( input$InterventionBlocksWhereStepsOccur  ) ) )  , 
            "Please enter Number(s) of Time Periods Between Steps\n(Single Numeric >= 0 or Separate by Commas)")
    )
    
    
    
    
    
    validate(
      need( trimws ( input$StaggerInitialSteps )  == "" ||
              length ( SeparateStringByCommas  ( input$StaggerInitialSteps  ) ) == 1 || 
              ( is.numeric ( SeparateStringByCommas  ( input$StaggerInitialSteps  ) ) && 
                  checkNonNegWholenumber ( as.numeric ( SeparateStringByCommas  ( input$StaggerInitialSteps  ) ) ) )  , 
            "Error, Are Start Periods Staggered...: Please Enter 'y/n' or Numerics >= 0 Separated by Commas for each Step)")
    )
    
    StaggerInitialSteps <- ExpandOrContract ( StaggerStepsStart (input$StaggerInitialSteps , "yes" , NumberOfSteps ) , NumberOfSteps )
    
    validate(
      need( not.Present ( input$CatchUpEndingSteps)  ||
              length ( SeparateStringByCommas  ( input$CatchUpEndingSteps  ) ) == 1 || 
              ( is.numeric ( SeparateStringByCommas  ( input$CatchUpEndingSteps ) ) &&
                  checkNonNegWholenumber ( as.numeric ( SeparateStringByCommas  ( input$CatchUpEndingSteps  ) ) ) )  , 
            "Error, ADJUST ENDING: Are Ending Periods Staggered...:Please Enter 'y/n' or Numerics >= 0 Separated by Commas for each Step)")
    )
    
    
    CatchUpEndingSteps <- StaggerStepsCatchup (input$CatchUpEndingSteps , "yes" , StaggerInitialSteps , NumberOfSteps )
    
    
    validate ( need ( not.Present (input$MaxTimePeriodsInStudy)  ||
                        (! is.na (as.numeric ( input$MaxTimePeriodsInStudy)) &
                           as.numeric ( input$MaxTimePeriodsInStudy) >= 1) ,
                      "Error, enter the Last Time Period to display on assignment plot, either blank or > 0" ) )
    
    pr (! not.Present (input$MaxTimePeriodsInStudy) )
    pr ( !is.na ( as.numeric (input$MaxTimePeriodsInStudy ) ) )
    if ( !  not.Present (input$MaxTimePeriodsInStudy) && !is.na ( as.numeric (input$MaxTimePeriodsInStudy ) ) ) {
      MaxTimePeriodsInStudy <- as.numeric (input$MaxTimePeriodsInStudy)
      validate ( need ( MaxTimePeriodsInStudy >= 1 ,
                        "Error, enter Last Time Period >= 1"))
    } else
      MaxTimePeriodsInStudy <- NA
    
    pr ( MaxTimePeriodsInStudy )
    
    validate ( need ( not.Present (input$MinStartTimePeriodInStudy) || 
                        ! is.na (as.numeric ( input$MinStartTimePeriodInStudy) ) ,
                      "Error, enter Starting Time Period either blank or numeric (may be negative)" ) )
    
    
    if ( not.Present( input$MinStartTimePeriodInStudy) ) {
      MinStartTimePeriodInStudy <- 0
    } else
      MinStartTimePeriodInStudy <- as.numeric ( input$MinStartTimePeriodInStudy)
    
    if ( !not.Present ( MaxTimePeriodsInStudy) && !not.Present  (MinStartTimePeriodInStudy  ))
      validate ( need (MaxTimePeriodsInStudy > MinStartTimePeriodInStudy ,
                       "Last Time Period to Display Must be > First Time Period to Display (ASSIGN Save Plot/---Change Plot Labels and Dimensions/Set the Last First ...)"))
    ##
    #   Periods - chronological time sequence, width in Time Elements can vary
    #      Total.Periods = PeriodsDuringRollout  + PeriodsDuringStaggerBaseline +
    #                     PeriodsDuringCatchUpEnding 
    #      Period.TimeElementSizes -- vector of length Total.Periods 
    #                       = TimeElementsPerPeriod + ...
    #      Total.TimeElements <- sum ( Period.TimeElementSizes )
    ##
    #   Periods and Time Elements, work outwards from the Steps to assign interventions
    #    
    #    Stagger Beginning                                    Steps        Catch Up Ending     
    #Times                        TimeElementsEveryStep                 
    #        max(StaggerInitialSteps)                                             max(CatchUpEndingSteps)
    # Periods # pos incrs              NumberPeriodsEachBlock      0/1           Periods # pos incrs   
    #  #unique (StaggerInitialSteps )
    
    ##     interventions c , d , e 
    
    #     (staggerStart == FALSE)   step    {StaggerCatch up == FALSE}         
    #    c  c   c    d  d  d  e  e   e   e  {e} {e} {e}    
    #   (c)  c  c    c  d  d  d  e   e   e  {e} {e} {e} 
    #   (c) (c) c    c  c  d  d  d   e   e   e  {e} {e}
    #   (c) (c) (c)  c  c  c  d  d   d   e   e   e   e   
    
    
    #  
    # Start with Step Section and work outwards
    
    #  NumberPeriodsEachBlock - minimum number of each block of intervention condition (during step section )
    
    lastCondition <- Intervention.Blocks [ length ( Intervention.Blocks ) ]
    NumericlastCondition <- Numeric.Intervention.Blocks [ length ( Numeric.Intervention.Blocks ) ]
    
    #ORIG
    if ( NumberOfSteps > 1 ) {
      
      # start at 0
      InterventionBlocksWhereStepsOccur <- c  ( 0 , CombineSteps( input$InterventionBlocksWhereStepsOccur , 1 , NumberOfSteps - 1 ))# 
    } else  
      InterventionBlocksWhereStepsOccur <- 0 
    
    pr( InterventionBlocksWhereStepsOccur )
    PeriodsDuringRollout <- sum ( NumberPeriodsEachBlock ) + sum(InterventionBlocksWhereStepsOccur)  # how many periods for step section to complete
    
    
    
    TimeElementsPerPeriod <- CombineSteps ( input$TimeElementsPerPeriod , 1 , NumberOfSteps ) # expand/contract to periods during rollout
    pr ( TimeElementsPerPeriod )
    
    #MN 
    # if ( NumberOfSteps > 1 ) {
    #   
    #   # start at 0 THIS IS WHERE THE CURRENT BUG/ERROR IS -- WITH INTERVENTION BLOCKS WHERE STEPS OCCUR
    #   InterventionBlocksWhereStepsOccur <- 
    #     SeparateStringByCommas(input$InterventionBlocksWhereStepsOccur)# 
    #   InterventionBlocksWhereStepsOccur_sum <- InterventionBlocksWhereStepsOccur
    # } else  {
    #   InterventionBlocksWhereStepsOccur <- as.integer(input$InterventionBlocksWhereStepsOccur)
    #   InterventionBlocksWhereStepsOccur_sum <- InterventionBlocksWhereStepsOccur * NumberOfSteps
    # }
    # 
    # pr( InterventionBlocksWhereStepsOccur )
    # PeriodsDuringRollout <- sum ( NumberPeriodsEachBlock ) + sum(InterventionBlocksWhereStepsOccur)  # how many periods for step section to complete
    # 
    # 
    # 
    # TimeElementsPerPeriod <- CombineSteps ( input$TimeElementsPerPeriod , 1 , NumberOfSteps ) # expand/contract to periods during rollout
    # pr ( TimeElementsPerPeriod )
    
    
    #   
    #  1. Calculate Periods during Rollout 
    #
    
    
    
    #   This is a count of all different rows with non-zero entities
    NumberOfNonVacuousRows <- sum ( EntitiesEveryStep > 0 )
    
    # run through creation of CombinedRollout matrix with numbers referring to intervention conditions
    AssignmentMatrix <- Repeat ( Numeric.Intervention.Blocks , NumberPeriodsEachBlock , HasBlocks , BlockLocations ,
                                 NumericonlyConditionsWithinBlocks  )
    
    AssignmentRollout <- ExpandBlockToSubblock0 ( NumberOfSteps , nrow ( AssignmentMatrix) , InterventionBlocksWhereStepsOccur , AssignmentMatrix  ) 
    pr("ExpandBlockToSubblock0")
    pr(AssignmentRollout)
    
    EntitiesEveryNonZeroStep <- EntitiesEveryStep [ EntitiesEveryStep > 0 ] # use later
    
    NumberFinalrows <-  ( EntitiesEveryStep > 0 ) # 
    
    FirstNonBlankIntvn <- getNonBlank ( AssignmentRollout , fctn = min )
    LastNonBlankIntvn <- getNonBlank ( AssignmentRollout , fctn = max )
    
    
    CombinedRollout <- AssignmentRollout
    
    totalRows <- nrow ( CombinedRollout  )
    
    #  2. Calculate Periods Stagger Baseline  -- 
    
    if ( length ( StaggerInitialSteps ) > 1 ||
         ! ( length ( StaggerInitialSteps ) == 1 && 
             StaggerInitialSteps[ 1 ]   %in% c( "yes" , "y" , "t" , "true" ) ) )  {
      
      
      startPeriod.Step <-  StaggerInitialSteps 
      
      theBlankCols <- BlankColumns ( CombinedRollout , "start"  )  # list of initial blanks
      
      for ( tau in 1 : NumberOfSteps ) {
        the.Rows <- ExpandSingleStep ( tau , nr )
        
        ### fixed
        starting <- max ( startPeriod.Step[ tau ] , 1 ) 
        
        LastBeginningBlank <- 0
        if ( length ( theBlankCols [[ ( tau - 1 ) * nr + 1 ]] ) > 0 )
          LastBeginningBlank <-  max ( theBlankCols [[ ( tau - 1 ) * nr + 1 ]])
        
        if ( starting  > 0 && LastBeginningBlank  > 0 && starting <= LastBeginningBlank )
          CombinedRollout [ the.Rows , (starting : LastBeginningBlank )  ]  <- FirstNonBlankIntvn [ the.Rows  ]
        
        
      }
      
    }
    
    
    #  get time elements and periods
    
    
    
    TimeElementsPerPeriod <- ExpandOrContract ( TimeElementsPerPeriod , PeriodsDuringRollout )
    pr ( PeriodsDuringRollout)
    pr(TimeElementsPerPeriod)
    
    Total.Time.Periods <- PeriodsDuringRollout   # number of periods in total design
    
    Total.TimeElements <- sum ( TimeElementsPerPeriod ) # number of time elements in total design
    
    Period.TimeElementSizes <- TimeElementsPerPeriod
    
    # calculate number of time elements for each step
    TimeElementsEveryStep <- ExpandOrContract( Period.TimeElementSizes  , Total.Time.Periods  )
    
    
    
    #  3. Calculate Periods catch up
    
    # if any positive values: 
    
    pr ( CatchUpEndingSteps )
    
    if ( length ( CatchUpEndingSteps ) > 1 ||
         ! ( length ( CatchUpEndingSteps ) == 1 && 
             CatchUpEndingSteps[ 1 ]  %in% c( "yes" , "y" , "t" , "true" ) ) )  { 
      
      
      endPeriod.Step <-  ncol ( CombinedRollout ) -   CatchUpEndingSteps 
      
      
      endPeriod.Step <- ifelse ( endPeriod.Step < 1 , 1 , endPeriod.Step )
      
      pr ( endPeriod.Step)
      
      theBlankCols <- BlankColumns ( CombinedRollout , "end"  )  # list of initial blanks
      
      pr ( theBlankCols )
      for ( tau in 1 : NumberOfSteps ) {
        the.Rows <- ExpandSingleStep ( tau , nr )
        
        ending <-  endPeriod.Step [ tau ] 
        
        FirstEndingBlank <- Total.Time.Periods + 1 
        if ( length ( theBlankCols [[ ( tau - 1 ) * nr + 1  ]] ) > 0 ) 
          FirstEndingBlank <- min ( theBlankCols [[ ( tau - 1 ) * nr + 1  ]]) 
        
        
        if ( FirstEndingBlank  <= Total.Time.Periods &&  ending  <= Total.Time.Periods &&  FirstEndingBlank  <= ending ) {
          pr ( tau )
          pr ( FirstEndingBlank)
          pr ( ending)
          pr (LastNonBlankIntvn [ the.Rows  ] )
          CombinedRollout [ the.Rows , (FirstEndingBlank  : ending )  ]  <- LastNonBlankIntvn [ the.Rows  ]
          pr ( CombinedRollout )
        } else {
          if ( ( FirstEndingBlank == Total.Time.Periods + 1 )  && ending < Total.Time.Periods ) {
            CombinedRollout [ the.Rows , ( ( ending + 1)  : Total.Time.Periods )  ]  <- ""
          }
          
        }
        
      }
    }
    
    #  4. Get time Periods and Time Elements, allowing expansion or contraction
    #     depending on MaxTimePeriodsInStudy
    
    if ( ! not.Present ( MaxTimePeriodsInStudy) ) {
      if ( ncol ( CombinedRollout) > MaxTimePeriodsInStudy ) { 
        CombinedRollout <- CombinedRollout [ , 1 : MaxTimePeriodsInStudy ]
        Period.TimeElementSizes <- Period.TimeElementSizes[1 : MaxTimePeriodsInStudy ]
        cutcolumns <- "cut columns"
        pr (cutcolumns)
        pr ( CombinedRollout )
      }
      else { 
        nbr <- MaxTimePeriodsInStudy - ncol ( CombinedRollout)
        if ( nbr > 0 ) {
          
          extraBlank <- matrix ( "" , nrow = nrow ( CombinedRollout ) ,
                                 ncol = nbr  )
          CombinedRollout <- cbind ( CombinedRollout , extraBlank )
          
          
        }
      }
      lineNbr <- 2152
      pr(lineNbr)
      pr(CombinedRollout)
      # recalculate
      PeriodsDuringRollout <- ncol ( CombinedRollout )
      pr (PeriodsDuringRollout )
      TimeElementsPerPeriod <- CombineSteps ( input$TimeElementsPerPeriod , 1 , PeriodsDuringRollout ) # expand/contract to periods during rollout
      pr (TimeElementsPerPeriod )
      Total.TimeElements <- sum ( TimeElementsPerPeriod )
    }
    
    #5.  Add Additional Steps if this is chosen - fill in this code later.
    #    Split Blocks, making separate rows with different interventions but same timing
    ##  Need to revise EntitiesEveryStep when there is a split
    NumberOfDifferentRows <- nrow ( CombinedRollout) 
    
    #6. Produce matrix of time elements w interventions, expanding from periods to time elements
    Condition.Time.Entities <- matrix ( "" , nrow = totalRows  , ncol = Total.TimeElements )
    pr (  Period.TimeElementSizes)
    
    for ( tt in 1 : totalRows ) {
      startVal <- 1
      pr ( tt )
      pr ( startVal)
      for ( k in 1 : length ( Period.TimeElementSizes ) ) {
        pr ( k )
        pr ( Period.TimeElementSizes)
        
        Condition.Time.Entities [  tt , ( startVal : ( startVal + Period.TimeElementSizes [ k ] - 1 ))] <- CombinedRollout [ tt , k ] 
        startVal <- startVal + Period.TimeElementSizes [ k ]
        pr (startVal )
      }
    }
    
    
    pr ( Condition.Time.Entities )
    # Text in the plot
    ####Define x axis tick mark names.####
    
    xAxis <- 0 :  Total.TimeElements 
    
    validate ( need ( is.null (input$maxXLabelValues ) ||
                        trimws ( input$maxXLabelValues ) == "" ||
                        is.PositiveWholenumber (input$maxXLabelValues)  ,
                      "Error: max number of X label values must be > 0 ") )
    
    if ( length (input$maxXLabelValues ) > 0 && trimws ( input$maxXLabelValues ) != "") {
      maxXLabelValues <- input$maxXLabelValues
    } else
      maxXLabelValues <- default.maxXLabelValues
    
    #Set default x axis names (1,2,3,4,...)
    theXlength <- min ( Total.TimeElements + 1 , maxXLabelValues )
    
    
    if (identical(input$NamingXAxisTickMarks,"Time Units")){
      
      print(toString(input$TimeUnit))
      xAxis2 <- paste(toString(input$TimeUnit),1:theXlength)
      xAxis.labelVals <- xAxis2
      
    } else if (identical(input$NamingXAxisTickMarks,"Dates")){
      
      # time0 <- as.Date(paste0(input$time_startYEAR,"/",input$time_startMONTH,"/01"),"%Y/%m/%d")
      time0 <- input$time_start
      
      theTimeNames <- seq.Date(from=time0,by=tolower(toString(input$TimeUnit)),length.out = theXlength)
      xAxis.labelVals <- format(theTimeNames,"%D")
      
    } else if (identical(input$NamingXAxisTickMarks,"Default")){
      
      xAxis <- round (seq ( 0 , Total.TimeElements , length.out = theXlength))
      xAxis.labelVals <- xAxis
      
    } else if (identical(input$NamingXAxisTickMarks,"Custom")){
      
      theTimeNames <- SeparateStringByCommas ( input$NamesOfEachTimePoint  )
      
      if ( any ( trimws (theTimeNames) != "" ) ) {
        theTimeNames.length <- length ( theTimeNames )
        if ( theTimeNames.length < theXlength )
          xAxis.labelVals <- c ( theTimeNames , xAxis [ (theTimeNames.length + 1) : theXlength  ])
      }
      
    }
    
    ####Define y axis tick mark names.####
    
    
    yAxis <- nr * ( 1 : NumberOfSteps ) 
    
    if (identical(input$NamingYAxisTickMarks,"Assignment Units")){
      
      yAxis2 <- paste0(toString(input$CohortName),
                       " ", NumberOfSteps:1,
                       "\n(", tolower(getName(unlist(input$EntityName), "Sub-unit", plural = T)),
                       ")")
      yAxis.labelVals <- yAxis2
      
    } else if (identical(input$NamingYAxisTickMarks,"Default")){
      
      yAxis.labelVals <-  ( NumberOfSteps : 1 ) 
      
    } else if (identical(input$NamingYAxisTickMarks,"Custom")){ #Not working yet
      
      
      
    }
    
    
    NamesOfEachStep <- SeparateStringByCommas ( input$NamesOfEachStep )
    nFill <- NumberOfSteps - length (NamesOfEachStep )
    if ( any (NamesOfEachStep != "") &&  nFill <= 0 ) {
      yAxis.labelVals <- rev ( NamesOfEachStep [ 1 : NumberOfSteps ] )
    } else {
      if ( any (NamesOfEachStep != "") && nFill > 0   )
        yAxis.labelVals <- rev ( c( NamesOfEachStep , rep ( "" , nFill )) )
    }   
    
    
    if ( input$AssignmentPlotYaxisLabel != "" ) {
      ylabText <- input$AssignmentPlotYaxisLabel
    } else
      ylabText <- makeYlabel (copyValue ( input$EntityName , defaultEntityName ) , EntitiesEveryStep ) #Y Axis Title
    
    
    if ( all (TimeElementsPerPeriod == 1) ) {
      xlabText <- paste ( "Time (" , TimeElementsPerPeriod [ 1 ] , TimeUnit, "Per Time Interval)" ) #X Axis Title
    } else {
      if ( var ( TimeElementsPerPeriod ) > 0 ) {
        rest <- paste ( TimeElementsPerPeriod , collapse = ",")
      } else
        rest <- TimeElementsPerPeriod [ 1 ]
      xlabText <- paste ( "Time (" , rest, paste0 (TimeUnit, "s"), "Per Period)" ) #X Axis Title
    }
    
    
    the.main <- paste ( "Rollout Design\n" , NumberOfSteps , "Steps in" , Total.TimeElements )
    if ( length ( TimeUnit) > 0 ) {
      the.main <- paste ( the.main  , paste0 (TimeUnit, "s"))
    } else
      the.main <- paste ( the.main , "Times" )
    
    
    UseGGPlot <- TRUE 
    if ( ! UseGGPlot ) { 
      
      plot ( xAxis , xAxis , ylim = range ( yAxis ),  type = "n" , axes = FALSE , 
             main = the.main  ,
             xlab = xlabText , 
             ylab = makeYlabel (copyValue ( input$EntityName , defaultEntityName) , 
                                CombineStepsChar (  input$EntitiesPerStep , 1, NumberOfSteps ) )
      )
      
      axis ( 1 , at = xAxis , labels = xAxis.labelVals , pos = NA )
      axis ( 2 , at = yAxis , labels = yAxis.labelVals , pos = NA )
    }
    
    
    TimesCondition <- list()
    minVal <- matrix ( NA , nrow = totalRows , ncol = NumberOfAll.Intervention.Conditions  )
    maxVal <- matrix ( NA , nrow = totalRows , NumberOfAll.Intervention.Conditions )
    
    
    for ( tau in 1 :  NumberOfSteps ) {
      for ( nrCount in 1 : nr ) {
        indx <- ( tau - 1 ) * nr + nrCount
        TimesCondition [[ indx ]] <- list ()
        
        for ( j in 1 :  NumberOfAll.Intervention.Conditions ) {
          
          TimesCondition [[ indx ]] [[ j ]] <- ( 1 :  Total.TimeElements ) [ Condition.Time.Entities [ indx , ] == as.character(Numeric.All.Intervention.Conditions [ j ] ) ]
          if ( length (TimesCondition [[ indx ]] [[ j ]]) > 0 ) {
            minVal[ indx , j ] <- min (TimesCondition [[ indx ]] [[ j ]] , na.rm = TRUE) 
            maxVal [ indx , j ] <- max (TimesCondition [[ indx ]] [[ j ]] , na.rm = TRUE) 
          } 
          
          
          if ( ! is.na ( minVal [ indx , j ] )) {
            
            if ( EntitiesEveryStep [ indx ] > 0 )
              if ( ! UseGGPlot )
                lines ( c ( minVal [ indx , j ] -1  , maxVal [ indx , j ]  )  , rep ( totalRows - indx + 1 , 2 ) , col = the.colors [ j ] , 
                        lwd = LineWidth * EntitiesEveryStep [ indx ] / mean (EntitiesEveryStep  ) , lend = 2 )
          }
        }
      } # end InterventionConditions
      
    } # end Steps
    
    
    # helpful vertical lines to emphasize time changes
    
    for ( tau in 1 :  NumberOfSteps ) {
      if ( NumberOfAll.Intervention.Conditions >= 2 ) {
        for ( j in 1 :  ( NumberOfAll.Intervention.Conditions )  ) {
          if ( ! UseGGPlot )
            lines ( c( maxVal [ tau , j ]   , maxVal [ tau , j ]  ) , c( 0 , NumberOfSteps - tau + 1 ) , lty = 3 )
        }
      }
    }
    
    # legend (  c(5,8) , c( length(All.Intervention.Conditions) -2 , -2 ) , 
    #        legend = All.Intervention.Conditions ,
    #         fill =  the.colors [ 1 : length ( All.Intervention.Conditions )] , bty = "n")
    
    
    if (  UseGGPlot ) {
      
      
      # Simple Design ggplot.R
      # provides legend with lines at the bottom by default
      
      xStart <- as.vector ( minVal -1 )
      yStart <- as.vector ( NumberOfDifferentRows - row ( minVal ) + 1 ) 
      xEnd <- as.vector ( maxVal )
      yEnd <- yStart 
      arrange.colors <-  as.vector ( the.colors [ col ( minVal ) ])
      the.width <- EntitiesEveryStep [ as.vector  ( row ( minVal ) ) ] * LineWidth / mean ( EntitiesEveryStep)
      
      qq <- All.Intervention.Conditions [ col (minVal)]
      
      pr ( xEnd)
      
      
      # if MaxTimePeriodsInStudy < number of periods, then chop off these from plot
      
      if ( !is.na (MaxTimePeriodsInStudy) ) {
        # add last dashed line but remove values if they are > MaxTimePeriodsInStudy
        smallerThan <- ( xEnd <=  MaxTimePeriodsInStudy )
      } else 
        smallerThan <- rep ( TRUE , length ( xEnd ))
      
      xStartA <- xStart [ smallerThan ]
      yStartA <- yStart [ smallerThan ]
      xEndA <- xEnd [ smallerThan ]
      yEndA <- yEnd [ smallerThan ]
      
      arrange.colorsA <- arrange.colors [ smallerThan ]
      
      the.widthA <- the.width [ smallerThan ]
      
      qqA <- factor ( qq [ smallerThan ] , levels = unique ( qq [ smallerThan ] ) )
      
      pr ( MaxTimePeriodsInStudy )
      pr(xStartA)
      pr ( xStart )
      pr ( xEnd)
      pr ( xEndA )
      pr ( arrange.colorsA )
      pr ( the.widthA )
      pr ( qqA )
      
      uniqueInterventions <- unique ( All.Intervention.Conditions )
      
      pr ( unique ( All.Intervention.Conditions ) )
      
      mapToUniqueInterventionNumber <- match ( All.Intervention.Conditions , uniqueInterventions  )
      
      df.Interventions <- data.frame ( xStart = xStartA , 
                                       yStart = yStartA ,
                                       xEnd = xEndA  ,
                                       yEnd =  yEndA ,
                                       arrange.colors = arrange.colorsA ,
                                       width = the.widthA ,
                                       theIntervention = qqA )
      
      
      if ( input$AssignmentPlotXaxisLabel != "" ) {
        xlabText <- input$AssignmentPlotXaxisLabel 
      } else { 
        if ( all (TimeElementsPerPeriod == 1) ) {
          xlabText <- paste ( "Time (" , TimeElementsPerPeriod [ 1 ] , TimeUnit, "Per Time Period )" )
        } else {
          if ( var ( TimeElementsPerPeriod ) > 0 ) {
            rest <- paste ( TimeElementsPerPeriod , collapse = ",")
          } else
            rest <- TimeElementsPerPeriod [ 1 ]
          
          xlabText <- paste ( "Time (" , rest , paste0 (TimeUnit, "s"), "Per Period )" )
        }
      }
      
      
      if ( input$AssignmentPlotTitle != "" ) {
        the.main <- input$AssignmentPlotTitle
      } else {
        
        the.main <- paste ( "Rollout Design\n" , NumberOfSteps , "Steps in" , Total.TimeElements )
        if ( length ( TimeUnit) > 0 ) {
          the.main <- paste ( the.main  , paste0 (TimeUnit, "s"))
        } else
          the.main <- paste ( the.main , "Times" )
      }
      
      # remove any intervention.conditions that have missing start or end values
      pr ( df.Interventions )
      
      Missing <- is.na ( df.Interventions [ , c ( "xStart" , "yStart" , "xEnd" , "yEnd")] ) 
      
      
      MissingVector <- apply ( Missing , 1 , any ) | df.Interventions [ , "width"] == 0
      dfNoNA <- df.Interventions [  ! MissingVector ,  ] 
      
      pr ( dfNoNA )
      
      if ( not.Present ( input$WhereToPutLegend )) {
        WhereToPutLegend <- default.WhereToPutLegend
      } else 
        WhereToPutLegend <- input$WhereToPutLegend
      
      
      #  draw vertical guides 
      
      xStartb <- NULL
      xEndb <- NULL
      yStartb <- NULL
      yEndb <- NULL
      
      NumberofNonZeroEntities <- sum ( EntitiesEveryStep > 0)
      
      
      indx2 <- 0
      for ( tau in 1 :  NumberOfSteps ) {
        for ( nrCount in 1 : nr ) {
          indx <- ( tau - 1 ) * nr + nrCount
          if ( EntitiesEveryStep [ indx ] > 0 )  {
            
            indx2 <- indx2 + 1 
            for ( j in 1 :  ( NumberOfAll.Intervention.Conditions)  ) {
              xStartb <- c( xStartb , maxVal [ indx , j ] )
              xEndb <- c( xEndb , maxVal [ indx , j ]  )
              yStartb <- c ( yStartb , 0 )
              yEndb <- c ( yEndb , NumberofNonZeroEntities - indx2 + 1 )
            }
          }
        }
      }
      
      pr(MaxTimePeriodsInStudy)
      if ( !is.na (MaxTimePeriodsInStudy) ) {
        # add last dashed line but remove values if they are > MaxTimePeriodsInStudy
        smallerThan <- ( xEndb <=  MaxTimePeriodsInStudy )
        pr ( smallerThan )
        xStartb <- c( xStartb [smallerThan ] , MaxTimePeriodsInStudy )
        xEndb <- c( xEndb [smallerThan ] , MaxTimePeriodsInStudy  )
        yStartb <- c ( yStartb [smallerThan ] , 0 )
        yEndb <- c ( yEndb [smallerThan ] , NumberOfSteps )
      }
      
      if ( !is.na (MinStartTimePeriodInStudy) ) {
        # add last dashed line
        xStartb <- c( MinStartTimePeriodInStudy , xStartb  )
        xEndb <- c( MinStartTimePeriodInStudy , xEndb   )
        yStartb <- c ( 0 , yStartb  )
        yEndb <- c ( NumberOfSteps  , yEndb )
      }
      
      pr(xStartb)
      pr(xEndb)
      pr (yStartb)
      pr (yEndb)
      
      
      VerticalGuides <- data.frame ( xStartb = xStartb ,
                                     xEndb = xEndb ,
                                     yStartb = yStartb ,
                                     yEndb = yEndb )
      
      pr ( VerticalGuides)
      
      MsgVert <- is.na ( VerticalGuides [ , c( "xStartb" , "xEndb")] )
      MissingVecVert <- apply ( MsgVert , 1 , any )
      
      VerticalGuidesNoNA <- VerticalGuides [ ! MissingVecVert , ]
      
      pr( VerticalGuidesNoNA)
      
      BiggerSize <- 15
      
      print(dfNoNA)
      
      b <- ggplot( dfNoNA   ) +
        theme_classic ( base_size = BiggerSize )  + labs ( title = the.main , x = xlabText , y = ylabText ) +
        geom_segment(data = dfNoNA , mapping = aes(  x = xStart , y = yStart, xend = xEnd, yend = yEnd , col = theIntervention  , 
                                                     size =  width  ) , lineend = "butt" ) +
        geom_segment ( data = VerticalGuidesNoNA , aes ( x = xStartb , y = yStartb, xend = xEndb, yend = yEndb ), 
                       linetype = 3 ) +
        theme ( plot.title = element_text ( hjust = 0.5 , size = BiggerSize ) ,
                axis.title.x = element_text ( size = BiggerSize ) ,
                axis.title.y = element_text ( size = BiggerSize )  ) +
        scale_y_continuous ( labels =  yAxis.labelVals  , breaks = yAxis ) +
        scale_x_continuous ( labels = xAxis.labelVals , breaks = xAxis  ) +
        theme(legend.position=  WhereToPutLegend ) +
        
        # the following doesn't produce legend, there is one when left out
        
        theme(legend.key.width = unit(3 ,"cm")) +
        
        scale_color_manual(values= the.colors [ 1 : length ( uniqueInterventions)]) +
        scale_size("size" , guide = "none") +
        
        theme(legend.text=element_text(size= BiggerSize )) +
        theme(legend.direction='vertical') + 
        labs( color = 'Implementation Conditions'  ) + 
        guides(colour = guide_legend(override.aes = 
                                       list(color = 
                                              factor  (the.colors [ 1 : length ( uniqueInterventions)])  ,
                                            linetype = 1 , size = 4 ))) 
      
      
      print ( b )
      
    }
    
    inputCheck <- list ()
    for ( i in names ( input )) {
      inputCheck [[ i ]] <- input [[ i ]]
    }
    
    LatestInputValueFile <- makeFileName ( TRUE , 
                                           input$AssignmentPlotDirectory,
                                           FileNameStem =   InputValueFileStem ,
                                           FileDescription = input$AssignmentPlotTitle ,
                                           FileType = InputValueFileType , 
                                           ReplaceSpace = TRUE )
    
    #  save (inputCheck , file = LatestInputValueFile )

  })
  
  
}
