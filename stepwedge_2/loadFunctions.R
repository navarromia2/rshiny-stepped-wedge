#loadFunctions.R


CreateValidMessage <- function() { #MN
  
  ValidMessage <- list ( )
  ValidMessage [[ "English"]] <- list ()
  ValidMessage [[ "English"]] [[ "NumberOfSteps"]] <- list()
  
  ValidMessage [[ "English"]] [[ "NumberOfSteps"]]$What <- "Please enter Number of Steps "
  
  ValidMessage [[ "English"]] [[ "NumberOfSteps"]]$Valid <- "Integer >= 2"
  
  ValidMessage [[ "English"]] [[ "NumberOfSteps"]]$Where <- "(Input for Design Figure/Numbers in the Design/Total Number of Steps...)"
  
  ValidMessage [[ "Spanish" ]] <- list ()
  ValidMessage [[ "Spanish" ]] [[ "NumberOfSteps"]] <- list()
  
  ValidMessage [[ "Spanish" ]] [[ "NumberOfSteps"]]$What <- "(Spanish) Please enter Number of Steps "
  
  ValidMessage [[ "Spanish" ]] [[ "NumberOfSteps"]]$Valid <- "Integer >= 2"
  
  ValidMessage [[ "Spanish" ]] [[ "NumberOfSteps"]]$Where <- "(Input for Design Figure/Numbers in the Design/Total Number of Steps...)"
  
  
  ValidMessage [[ "English"]] [[ "InterventionConditions"]] <- list()
  
  ValidMessage [[ "English"]] [[ "InterventionConditions"]]$What <- "Required: Please enter Name(s) of Intervention/Implementation Condition(s) in order of appearance"
  
  ValidMessage [[ "English"]] [[ "InterventionConditions"]]$Valid <- "Enter as Text; if more than 1 separate names by ','"
  
  ValidMessage [[ "English"]] [[ "InterventionConditions"]]$Where <- "Location ASSIGN: Names/ Name(s) of Intervention...)"
  
  
  ValidMessage [[ "Spanish" ]] <- list ()
  ValidMessage [[ "Spanish" ]] [[ "NumberOfSteps"]] <- list()
  
  ValidMessage [[ "Spanish" ]] [[ "InterventionConditions"]]$What <- "(Spanish ) Required: Please enter Name(s) of Intervention(s)/Implementation(s) in order of appearance"
  
  ValidMessage [[ "Spanish" ]] [[ "InterventionConditions"]]$Valid <- "Enter as: Text(s), if more than 1 then separate names by ','"
  
  ValidMessage [[ "Spanish" ]] [[ "InterventionConditions"]]$Where <- "Location: (Input for Design Figure/Setup Design Names/Name(s) of Intervention...)"
  
  return(ValidMessage)
  
}
lastChar <- function ( x ) {
  rslt <- NULL
  if ( length ( x ) > 0 && nchar ( x ) > 0 ) {
    ending <- nchar ( x )
    rslt <- substring ( x , ending , ending )
  }
  
  return (rslt )
}

not.Present <- function ( x ) {
  # return TRUE if a value is not available
  if (  length ( x ) == 0 || is.null ( x )  || trimws (x) == "")
    rslt <- TRUE
  else 
    rslt <- FALSE
  return ( rslt )
}

pr <- function ( Unquoted ) {
  # 
  Quoted = substitute ( Unquoted)
  print ( Quoted )
  print ( Unquoted )
}

SeparateStringByCommas <- function ( string , single.character = "," ) {
  g <-SeparateStringByCharacter  ( string , single.character )
  if (is.null ( g ))
    g <- ""
  return ( g )
}

SeparateStringByCharacter  <- function ( string , single.character ) {
  # return a character vector
  # cat ( "***" , string , "\n")
  if ( not.Present ( string ))
    the.vector <- NULL
  else
    the.vector <- unlist ( strsplit (as.character (unlist ( string )) , single.character ))
  return ( the.vector )
}

#output$InputEntityName----

getName <- function ( a , Default , plural = TRUE ) {
  # if a is missing then use Default
  # for plural, if text a ends in y (e.g. County) then convert to Countie for plural
  # otherwise just add an s
  if ( length ( a ) == 0 || trimws (a) == "") {
    rslt <- Default
  } else
    rslt <- a
  
  if ( plural ) {
    the.size <- nchar ( rslt )
    the.last.char <- substr ( rslt , the.size, the.size)
    
    
    if (the.last.char != tolower ( the.last.char)   ) {
      rslt <- toupper ( make_plural ( rslt ) )
    } else
      rslt <- make_plural ( rslt )
    
  }
  return ( rslt )
}


getName <- function ( a , Default , plural = TRUE ) {
  # if a is missing then use Default
  # for plural, if text a ends in y (e.g. County) then convert to Countie for plural
  # otherwise just add an s
  if ( length ( a ) == 0 || trimws (a) == "") {
    rslt <- Default 
  } else
    rslt <- a 
  
  if ( plural ) {
    the.size <- nchar ( rslt )
    the.last.char <- substr ( rslt , the.size, the.size)
    
    
    if (the.last.char != tolower ( the.last.char)   ) {
      rslt <- toupper ( make_plural ( rslt ) )
    } else
      rslt <- make_plural ( rslt )
    
  }
  return ( rslt )
}


#01312020----
ReplaceColonCommaPositions <- function ( string , replacement , default = 1 ) {
  # Expand or Contract replacement, which can have colons and commas, to same order as string
  # string:  text string with : and , separators, all have to be non-blank
  # replacement: either starts with a non-null character or takes default value
  #  both of these input criteria are tested 
  # find colon and comma locations
  # return replacement text string that has same : and , positions as string does
  
  BlocksInString <- SeparateStringByCommas ( string )
  
  if ( length ( BlocksInString) == 1 && trimws ( BlocksInString ) == "" ) {
    rebuild <- NULL  # this should not happen
  } else { 
    
    if ( !is.null ( replacement)) {
      BlocksInReplacement <- SeparateStringByCommas ( replacement )
    } else
      BlocksInReplacement <- default
    
    
    # look at each block separated by commas
    
    lastReplacement <- 1
    CurrentString <- list ( )
    CurrentReplacement <- list ( )
    for ( i in 1 : length ( BlocksInString )) {
      cat ( i , " t1\n")
      CurrentString [[ i ]] <- SeparateStringByCharacter( BlocksInString [ i ] , ":")
      if (! is.na ( BlocksInReplacement [ i ] ) ) {
        cat ( i , " t2\n")
        CurrentReplacement [[ i ]] <- SeparateStringByCharacter( BlocksInReplacement [ i ] , ":")
      } else {
        cat ( i , " t3\n")
        CurrentReplacement [[ i ]] <- 1
      }
      
      
      lngth <- length ( CurrentString [[ i ]])
      if ( lngth <= length ( CurrentReplacement [[ i ]])) {
        cat ( i , " t4\n" )
        CurrentReplacement [[ i ]] <- CurrentReplacement [[ i ]] [ 1 : lngth ]
      } else { # need to expand CurrentReplacement
        if ( length ( CurrentReplacement [[ i ]] ) == 0 ) { 
          cat ( i , " t5\n" )
          CurrentReplacement [[ i ]] <- 1
        }
        
        
        
        # make same length as CurrentString [[ i ]]
        lngthRepl <- length ( CurrentReplacement [[ i ]] )
        lastOne <- CurrentReplacement [[ i ]] [ lngthRepl ]
        nbrToAdd <- lngth - lngthRepl
        cat ( i , " t6\n")
        CurrentReplacement [[ i ]] <- c ( CurrentReplacement [[ i ]] , rep ( lastOne , nbrToAdd ) )
      } # end need to expand CurrentReplacement
    } # end for loop
    
    
    rebuild <- paste ( CurrentReplacement [[ 1 ]] , collapse = ":")
    if ( length ( BlocksInString ) > 1 ) 
      for ( i in 2 : length ( BlocksInString )) {
        newpart <- paste0 (CurrentReplacement [[ i ]] , collapse = ":")
        rebuild <- paste0 ( rebuild , "," , newpart  )
      }
  } 
  return ( rebuild )
}


SeparateStringByBoth <- function ( string  ) {
  #  string -  text string surrounded by quotes
  #  characters - , characters = c("," , ":")
  # return a vector of strings
  
  
  characters = c("," , ":") 
  
  rslt <- unlist ( strsplit (as.character (unlist ( string )) , characters[1]))
  rslt <- unlist ( strsplit ( rslt , characters[ 2 ]) )
  
  return ( rslt )
}


not.Present <- function ( x ) {
  # return TRUE if a value is not available
  if (  length ( x ) == 0 || is.null ( x )  || trimws (x) == "")
    rslt <- TRUE
  else 
    rslt <- FALSE
  return ( rslt )
}



BlocksOfTextsSeparatedbyComma <- function ( Separated ) {
  rslt <- unlist ( Separated$first)
  return ( rslt)
}


getName <- function ( a , Default , plural = TRUE ) {
  # if a is missing then use Default
  # for plural, if text a ends in y (e.g. County) then convert to Countie for plural
  # otherwise just add an s
  if ( length ( a ) == 0 || trimws (a) == "") {
    rslt <- Default 
  } else
    rslt <- a 
  
  if ( plural ) {
    the.size <- nchar( rslt )
    the.last.char <- substr ( rslt , the.size, the.size)
    
    
    if (the.last.char != tolower ( the.last.char)   ) {
      rslt <- toupper ( make_plural ( rslt ) )
    } else
      rslt <- make_plural ( rslt )
    
  }
  return ( rslt )
}


is.wholenumber <- function (x, tol = .Machine$double.eps^0.5)  {
  # is.wholenumber :  adapted from library ( FRACTION )
  if ( is.character (x ) || ! is.vector ( x ) ) {
    rslt <- FALSE
  } else 
    rslt <- all ( abs(x - round(x)) < tol )
  return ( rslt )
}


not.Present <- function ( x ) {
  # return TRUE if a value is not available
  if (  length ( x ) == 0 || is.null ( x )  || trimws ( x ) == "" || is.na (x ))
    rslt <- TRUE
  else 
    rslt <- FALSE
  return ( rslt )
}


SeparateStringByCharacter  <- function ( string , single.character ) {
  # return a character vector
  # cat ( "***" , string , "\n")
  if ( not.Present ( string ))
    the.vector <- NULL
  else 
    the.vector <- unlist ( strsplit (as.character (unlist ( string )) , single.character ))
  return ( the.vector )
}

SeparateStringByCommas <- function ( string , single.character = "," ) {
  g <-SeparateStringByCharacter  ( string , single.character )
  if (is.null ( g ))
    g <- ""
  return ( g )
}

# handle input$values that are not set
copyValue <- function ( x , default  ) {
  if ( is.null (x ) || length ( x ) == 0 || ( trimws (as.character (x) ) == "")) {
    y <- default
  } else 
    y <- x
  return ( y )
}

checkAllNumericNonNegativeIntegerWsumPositive <- function ( x ) {
  # check a text vector separated by commas is all non-negative numeric when commas are removed and at least one is positive
  # don't give any warning about non-numeric input values
  old.warn <- getOption ( "warn")
  options(warn = -1)  # don't print out 
  y <- as.numeric (SeparateStringByCommas ( x ))
  rslt <- ( all ( ! is.na ( y ) ) & is.wholenumber ( y ) & sum ( y ) > 0 )
  options(warn = old.warn ) # return "warn" to initial state
  return ( rslt )
} 

checkAllPositiveIntegers <- function ( x ) {
  # check a text vector separated by commas is all positive integers when commas are removed
  # don't give any warning about non-numeric input values
  old.warn <- getOption ( "warn")
  options(warn = -1)  # don't print out 
  y <- as.numeric (SeparateStringByCommas ( x ))
  rslt <- ( all ( ! is.na ( y ) ) & is.wholenumber ( y ) & all ( y ) > 0 )
  options(warn = old.warn ) # return "warn" to initial state
  return ( rslt )
} 

is.NonNegWholenumber <- function ( x ) {
  rslt <- ( all ( ! is.na ( x )) && is.wholenumber( x ) && all ( x >= 0 ))
  return ( rslt )
}

is.PositiveWholenumber <- function ( x ) {
  rslt <- ( all ( ! is.na ( x )) && is.wholenumber( x ) && all ( x > 0 ))
  return ( rslt )
}

checkNonNegWholenumber <- function ( x ) {
  old.warn <- getOption ( "warn")
  options(warn = -1)  # don't print out 
  y <- as.numeric (SeparateStringByCommas ( x ))
  rslt <- ( length ( y ) >= 1 & is.NonNegWholenumber ( y ) )
  options(warn = old.warn )
  return ( rslt )
}

checkUnique <- function ( x ) {
  return ( length ( x ) == length ( unique ( x )))
}

not.Completed <- function ( text , char = ":") {
  rslt <- FALSE
  if ( lastCharacter ( text ) == char )
    rslt <- TRUE
  return ( rslt )
}

not.Appropriate <- function ( x ) {
  # not present OR not a valid non neg interger w at least 1 positive
  # cat ( "  Entered not.Appropriate section B\n")
  rslt <- (  ! checkAllNumericNonNegativeIntegerWsumPositive ( x ) )
  return ( rslt )
} 

#   new got rid of revised function 2020 01 31 also renamed from not.Right.Lengths  
not.Matching.Lengths <- function ( x , y ) {
  # lengths of x and y when separated by commas are not the same
  # first argument is InterventionConditions which now need not have unique elements!
  x1 <- trimws (SeparateStringByCommas ( x ))
  y1 <- SeparateStringByCommas ( y )
  return (  not.Present ( x ) || ! checkUnique  ( x1 ) || not.Present ( y ) || length ( x1 ) != length ( y1 ))
}

ExpandOrContract <- function ( x , len ) {
  # expand a vector to len by copying last element or contract by truncating
  rslt <- x
  if ( length ( rslt ) < len ) {
    last <- rslt [ length ( rslt )]
    rslt <- c ( rslt , rep ( last , len - length ( rslt )  ))
  }
  if ( length ( rslt ) > len ) {
    rslt <- rslt [ 1 :  len ]
  }
  
  if ( len <= 0 ) 
    rslt <- NULL
  
  return ( rslt )
}

labelWithPluralName <- function ( text , Default ) {
  if ( length ( text ) == 0 ) {
    val <- paste0 ( Default , "s")
  } else 
    val <- paste0 ( text , "s")
  
  the.label <- paste ( "Please enter numeric", val , "Per Period\n(If different across Steps separate by ',')" )
  return ( the.label )
}

#  this isn't complete yet but need for general way to 
#  pass truth status and text message to validate()

TruthOf <- function ( obj , Quoted , conjunction = "and") {
  # evaluate the truth of whether obj satisfies Checks[[Quoted]]
  # use as first element of input to validate ()
  # input:  obj  input$SOMETHING
  #      :  Quoted     "SOMETHING"
  if ( conjunction == "and") {
    rslt <- TRUE  # 
  } else # this means "OR" so start with FALSE
    rslt <- FALSE # 
  
  
  for ( i in seq ( along = Checks [[ Quoted ]]))
    if ( conjunction == "and") {
      rslt <- rslt  & Clecks [[ Quoted ]][[ i ]] ( obj ) # check this!
    } else
      rslt <- rslt |  Clecks [[ Quoted ]][[ i ]] ( obj )
    
    return ( rslt ) 
    
}

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

SimpleConcat <- function ( StartQuote = "" , obj , EndQuote = "" , sep = " " ) {
  # concatenate Start Quote, value of an object, EndQuote
  # use in validate ( , ?? )
  return ( paste ( StartQuote , obj , EndQuote , sep = "" ))
}

ValidationMessage <- function ( Quoted , Location = ValidMessage , Language = defaultLanguage ) {
  # Quoted - object name in quotes
  # Location - list that provides 
  What <- Location[[ Language ]][[ Quoted ]]$What # what information is needed
  Valid <- Location[[ Language ]][[ Quoted ]]$Valid # provide text describing valid value
  Where <- Location[[ Language ]][[ Quoted ]]$Where # where do you find this info
  
  return ( paste ( c (What , Valid, Where) , collapse = "\n"))
}


not.AllUnique <- function ( string  ) {
  cat ( paste ("Entered not.AllUnique string = ", string , "\n"))
  the.names <- SeparateStringByBoth( string )
  return ( ! all.unique ( the.names ))
}

has.DuplicateBeforeEnding <- function ( string ) {
  the.names <- SeparateStringByBoth( string )
  rslt <- FALSE
  if ( length ( the.names) > 1 && not.AllUnique ( string )) {
    dropLastName <- the.names [ - length ( the.names )]
    the.names2 <- paste ( dropLastName , collapse = ",")
    if ( not.AllUnique ( the.names2 ))
      rslt <- TRUE
  }
  return ( rslt )
}

CombineSteps <- function ( x , Default , n ) {
  # returns numeric vector of length n
  ans <- ExpandOrContract (as.numeric (SeparateStringByCommas ( copyValue ( x , Default ) )) , n )
  return( ans )
}

CombineStepsText <- function ( x , Default , n ) {
  # returns  vector of length n
  ans <- ExpandOrContract (SeparateStringByCommas ( copyValue ( x , Default ) ) , n )
  return( ans )
}

CombineSteps2 <- function ( x , Default , n ) {
  # returns NUMERIC vector of length n, separating out both : ,
  ans <- ExpandOrContract (as.numeric  ( SeparateStringByBoth ( copyValue ( x , Default ) )) , n )
  return( ans )
}


CombineStepsChar <- function ( x , Default , n ) {
  # returns character vector of length n, separating out : ,
  ans <- ExpandOrContract ( SeparateStringByBoth ( copyValue ( x , Default ) ) , n )
  return( ans )
}


StaggerStepsStart <- function ( x , Default , n ) {
  # return values of first columns nonblank for each step
  lowerX <- tolower (  SeparateStringByCommas( (x) ) ) 
  if ( length ( x ) == 0 || trimws (lowerX ) == "" ) { # default is stagger
    rslt <- 1 : n
  } else {  # length ( x ) > 0
    
    if ( length ( lowerX ) == 1 ) {  
      if ( lowerX %in% c( "no" , "n" , "t" , "true" , "0")) {
        rslt <- rep ( 1 , n )
        
      } else { 
        rslt <- 1 : n
      }
      
    } else {  # length is more than 1
      rslt <- CombineSteps ( lowerX , 1 , n )
    }
  }
  return ( rslt )
}

StaggerStepsCatchup <- function ( x , Default , StaggerInitialSteps, n ) {
  # 
  # return number of columns to leave blank at the end
  #  output:  0,0,...,0 -- fill out to the end
  #           set of numbers - fill out to these values
  #           NULL - don't do anything
  # gently allow all reasonable numeric values, yes, no true, false
  # if not.Present ( x )
  #   return NULL
  # if any positive numerical values, e.g., 3,2
  #   expand or contract to n and return
  # if length(x) == 1 and x == TRUE or 1 then the ending is staggerered  
  #   if StaggerInitialSteps not all 1 (no beginning stagger)
  #        nothing to do - return NULL  and don't do anything after this call
  #   else  return ( (n-1):0)
  # if length(x) == 1 and x == FALSE
  #   return rep(0,n) - fill in to the end
  
  # 
  
  lowerX <- tolower (  SeparateStringByCommas( (x) ) ) 
  
  if ( not.Present ( lowerX )) {
    rslt <- NULL
  } else {
    
    if ( length ( lowerX ) == 1 ) {  
      if ( lowerX %in% c( "no" , "n" , "f" , "false" , "0")) { # no staggering
        rslt <- rep ( 0 , n )
        
      } else {  # not a false or no value customized stagger
        if ( ! is.numeric ( lowerX )) {  # then it's true yes or blank  
          rslt <- ( (n - 1 ) :  0 )
        } else {  # length is more than 1 
          rslt <- CombineSteps ( lowerX , n , n )
        }
      }
    }
  }
  return ( rslt )
}




sortFixNull <- function ( x ) {
  z <- x [ x > 0 ]
  if ( length ( z ) > 0 ) {
    rslt <- sort( z )
  } else
    rslt <- NULL
  return ( rslt )
}

all.unique <- function ( x ) {
  rslt <- TRUE
  if ( length ( x ) > 0 )
    rslt <- ( length ( x ) == length ( unique( x )))
  return ( rslt)
}

lastCharacter <- function ( string ) {
  n <- str_length ( string )
  rslt <- str_sub ( string , start = n , end = n )
  return ( rslt )
}

NumberBlockCombinations <- function ( conditions, BlockLocations , onlyConditionsWithinBlocks ) {
  # return total product of number of block combinations
  nr <- 1  # number of rows as the product of all these
  # message ( paste ( "conditions = " , paste ( conditions, collapse = "\t") , "\n"))
  BlockSizes <- NumberBlockSizes ( conditions, BlockLocations , onlyConditionsWithinBlocks )
  for ( k in BlockLocations) {
    nr <- nr * BlockSizes [ k ]
  }
  return ( nr )
}

NumberBlockSizes <- function ( conditions, BlockLocations , onlyConditionsWithinBlocks ) {
  BlockSizes <- rep ( 1 , length ( conditions ))
  for ( k in BlockLocations) {
    
    BlockSizes [ k ] <- length ( onlyConditionsWithinBlocks [[ k ]])
    
  }
  return ( BlockSizes )
}


####How we add each row into the matrix (Repeat)####
Repeat <- function ( conditions , periods , HasBlocks , BlockLocations ,
                     onlyConditionsWithinBlocks  )  {
  # input: conditions - only 1 value for each block, so a character string separated by ,
  # periods:  number of periods for each intervention Block
  # HasBlock: t/f
  # BlockLocations: which places start blocks with more than 1 value
  # onlyConditionsWithinBlocks: what are the intervention conditions within blocks w more than 1 value
  # 
  # output: returns either a vector or a matrix
  #         number of columns = sum ( periods )
  #         number of rows = product ( length (onlyConditionsWithinBlocks[j]))
  # do special work if HasBlocks
  # This requires conditions and periods to be same lengths
  if ( ! HasBlocks ) {
    rslt <- NULL
    for ( j in 1 : length (conditions) ) {
      rslt <- c ( rslt , rep ( conditions [ j ] , periods [ j ] ))
    }
    rslt <- matrix ( rslt , nrow = 1 , ncol = length ( rslt ) )
  } else {  # has blocks
    nr <- NumberBlockCombinations  ( conditions, BlockLocations , onlyConditionsWithinBlocks )
    BlockSizes <- NumberBlockSizes  ( conditions, BlockLocations , onlyConditionsWithinBlocks )
    
    # for ( uu in 1 : length ( BlockSizes ))
    #   message  ( paste ( "** uu =" , uu , "BlockSizes [ uu ] = ", BlockSizes [ uu ] , "\n") )  
    rslt <- matrix ( "" , nrow = nr  , ncol = sum ( periods ))
    
    startColumn <- 0
    endColumn <- 0
    for ( k in 1 : length ( conditions )) {  # actually over Blocks 
      
      startColumn <- endColumn + 1
      endColumn <- startColumn + periods [ k ] - 1
      
      
      if (! (k %in% BlockLocations )) {
        message  ( paste ( "** k =" , k , "\n") )  
        rslt [  , startColumn : endColumn  ] <-   rep ( conditions [ k ] , periods [ k ] )
        
      } else {  # k involves a block of interventions
        for ( m in 1 : length ( onlyConditionsWithinBlocks [[ k ]] ) ) { 
          message  ( paste ( "&~~ k =" , k , "m = " , m , "nr = " , nr , "BlockSizes [ k ] = " , BlockSizes [ k ] , "\n"))
          message  ( paste ( " WhichExpandedRows = ", paste ( WhichExpandedRows ( k , m , nr , BlockSizes ) , collapse = "\t" ) , "\n") )  
          WhichRows <- WhichExpandedRows ( k , m , nr , BlockSizes  )
          message  ( paste ( "~~**WhichRows =" , paste ( WhichRows , collapse = "\t" ) , "onlyConditionsWithinBlocks value = " , onlyConditionsWithinBlocks[[ k ]][ m ] , "periods value = " , periods [ k ] ,"\n") ) 
          rslt [ WhichRows , startColumn : endColumn  ] <-   rep ( onlyConditionsWithinBlocks [[ k ]][ m ] , periods [ k ] )
          
          
        } # end over m
      } # end over else -- k is a block
    }  # end over k
  }  # end HasBlock
  
  return ( rslt )
}

WhichExpandedRows <- function ( k , m , nr , BlockSizes ) {
  # From a subblock m within a Block or step k, return all the values of 
  #  rows needed to be filled across all intervention conditions
  # k - which Block
  # m - which value within kth Block
  
  
  NumberRepetitions <- nr  / BlockSizes [ k ]
  
  if ( k > 1 ) {
    AdjacentRepeats <- prod (  BlockSizes [ 1 : ( k -1 )] )
    
    skips <- AdjacentRepeats *  BlockSizes [ k ]
    
    if ( k == length (BlockSizes )) {
      NonAdjacentRepeats <- 0
    } else {
      NonAdjacentRepeats <- prod ( BlockSizes [ k : length ( BlockSizes )])
      
    }
    
    
    startRow <-  AdjacentRepeats * ( m - 1 ) + 1
    endRow <- startRow + AdjacentRepeats - 1
    
    
    beginFillin <- seq ( startRow , endRow )
    
    nbrReps <- NumberRepetitions / length ( beginFillin)
    
    rslt <- NULL
    startSeq <- beginFillin
    for ( mm in 1 : nbrReps ) {
      startSeq <- beginFillin + skips * ( mm - 1 )
      rslt <- c ( rslt , startSeq )
    }
    
    # checkLengthOK <- length ( rslt ) * BlockSizes [ k ] / nr  # should be 1
    
    
  } else { # k = 1
    
    Z <- nr / BlockSizes [ 1 ]
    rslt <- m + ( BlockSizes [ 1 ] ) * ( 0 : ( Z - 1 ))
  }
  
  
  return ( rslt ) 
}

FindTimePeriods <- function ( x ) {
  # x is vector of non-neg time element durations
  # convert these to time Periods and return a vector with the length being the largest time Period
  # if x [ j ] = 0 then return 0
  #  exampleFindTimePeriods  
  #  x =       2 5 0 4
  #  return    1 3 0 2
  MaxPeriod <- rank ( x, ties.method = "min" ) - sum ( x == 0 )
  MaxPeriod <- ifelse ( MaxPeriod < 0 , 0 , MaxPeriod )
  
  return ( MaxPeriod )
}

TimeElementsForPeriods <- function ( x ) {
  # x a vector of non neg time element durations, not necessary in any sequence
  # return how many time elements for each period ordered 
  #  example
  #  x =       2 5 0 4
  #  return    2 2 1, e.g., 2nd element goes through 3 time periods 
  y <- unique ( sort ( x [ x > 0 ]) )
  NumberTimeElements <- rep ( 0 , length ( y ))
  the.sum <- 0
  for ( i in 1 : length ( y )) {
    NumberTimeElements [ i ] <- y [ i ] - the.sum
    the.sum <- y [ i ]
  }
  
  return ( NumberTimeElements )
}

FirstBlankAfterLastCondition <- function ( x , condn ) {
  u <- ( x == condn )
  lastCondition <- max ( ( 1 : length ( x ) ) [ u ] )
  if ( lastCondition < length ( x ) && x [ lastCondition + 1 ] == "" ) {
    w <- lastCondition + 1 
  } else
    w <- 0  # error
  
  return ( w )
}

FillIn <- function ( firstPart , laterPart ) {
  #  change tx1 tx1 - - tx1 to tx1 tx1 tx1 tx1 tx1
  combine <- cbind ( firstPart , laterPart )
  if ( ncol ( combine ) >= 2 ) {
    for ( r in 1 : nrow ( combine )) {
      # check for pattern
      start <- 0
      end <- 0
      for ( k in 2 : ( ncol ( combine ) - 1 ) ) {
        if ( start == 0 & combine [ r , k ] == "" ) {
          start <- k
        }
        if ( start >= 2 &&  (combine [ r , k ] == combine [ r , (start - 1) ]) ) {
          end <- k - 1
        }
        
        if ( start >= 2 & end > start )
          combine [ r , start : end ] <- combine [ r , start - 1 ]
      }
    }
    
    return ( combine )
  }
  
}

ExpandSingleStep <- function ( tau , nr ) {
  rows.start <- nr * ( tau - 1 ) + 1
  rows.end <- nr * tau 
  return ( rows.start : rows.end )
}

RowsAccountingFor0Entities <- function ( NumberOfSteps , nr , EntitiesEveryStep ) {
  # returns matrix expanded rows for all steps
  rslt <- data.frame ( matrix  ( 0 , nrow = NumberOfSteps * nr , ncol = 3 ) )
  names( rslt ) <- c( "Step" , "Keep" , "SeqNumber" ) 
  
  rslt [ , "Keep"] <- ( EntitiesEveryStep > 0 )
  rslt [ , "SeqNumber"] <- cumsum( rslt [ , "Keep"])
  
  for ( tau in 1 : NumberOfSteps )
    
    the.Rows <- ExpandSingleStep ( tau , nr )
  rslt [  the.Rows , "Step" ] <- tau
  
}

ExpandBlockToSubblock <- function ( NumberOfSteps , nr , InterventionBlocksWhereStepsOccur , AssignmentMatrix , maxPeriods ) {
  # expand rows in assignment matrix to account for all possible combinations of subblocks
  #  nr is product of number of subblocks 
  #  note, some of these may have 0 units assigned, account for this later
  theRows <- nr * NumberOfSteps 
  if ( is.na (maxPeriods) | is.null ( maxPeriods ) )
    maxPeriods <- 0
  theCols <- max  ( c ( ncol ( AssignmentMatrix ) +  sum ( InterventionBlocksWhereStepsOccur) , maxPeriods ) )
  rslt <- matrix ( "", nrow = theRows , ncol = theCols )
  for ( tau in 1 : NumberOfSteps ) {
    cat ("tau = ",tau, "\n")
    startPeriod <- cumsum ( InterventionBlocksWhereStepsOccur) [ tau ] + 1 
    pr(startPeriod)
    endingPeriod <- startPeriod + ncol ( AssignmentMatrix ) - 1
    pr(endingPeriod)
    rslt [ ExpandSingleStep ( tau , nr ) , startPeriod :  endingPeriod ] <- AssignmentMatrix 
    pr(rslt )
  }
  
  
  return ( rslt )
}

ExpandBlockToSubblock0 <- function ( NumberOfSteps , nr , InterventionBlocksWhereStepsOccur , AssignmentMatrix  ) {
  # expand rows in assignment matrix to account for all possible combinations of subblocks
  #  nr is product of number of subblocks 
  #  note, some of these may have 0 units assigned, account for this later
  theRows <- nr * NumberOfSteps 
  
  theCols <-  ncol ( AssignmentMatrix ) +  sum ( InterventionBlocksWhereStepsOccur) 
  rslt <- matrix ( "", nrow = theRows , ncol = theCols )
  for ( tau in 1 : NumberOfSteps ) {
    pr( tau )
    startPeriod <- cumsum ( InterventionBlocksWhereStepsOccur) [ tau ] + 1 
    pr ( startPeriod )
    endingPeriod <- startPeriod + ncol ( AssignmentMatrix ) - 1
    pr ( endingPeriod )
    
    rslt [ ExpandSingleStep ( tau , nr ) , startPeriod :  endingPeriod ] <- AssignmentMatrix 
    pr ( rslt )
  }
  
  return ( rslt )
}

makeYlabel <- function ( the.name ,  values ) {
  
  if ( length ( values ) > 1  ) {
    if ( var ( values ) > 0 ) {
      zz <- paste0 ( "[" , paste ( values , collapse = ",") , "]"  )
    } else
      zz <-  paste ( length(values) , "*" , values[ 1 ] ,"For"  )
  } else
    zz <- values [ 1 ]
  the.text <- paste ( the.name , " (", zz , " Each Step)", collapse = "," )
  return ( the.text )
}

getNonBlank <- function ( matrix  , fctn = min ) {
  firstNonBlankIntvn <- rep (NA , nrow ( matrix ))
  for ( i in 1 : nrow ( matrix )) {
    all.nonBlank <- ( matrix [ i , ] != "")
    whichCol <- fctn (( 1 : ncol ( matrix ) )[ all.nonBlank ])
    firstNonBlankIntvn [ i ] <- matrix [ i , whichCol ]
  }
  return ( firstNonBlankIntvn )
}

LocateNonBlank <- function ( matrix , fctn ) {
  # return column location of first or last nonblank
  #  fctn is either min for first or max for last
  whichCol <- rep (NA , nrow ( matrix ))
  for ( i in 1 : nrow ( matrix )) {
    all.nonBlank <- ( matrix [ i , ] != "")
    whichCol [ i ] <- fctn (( 1 : ncol ( matrix ) )[ all.nonBlank ])
  }
  return ( whichCol )
}

BlankColumns <- function ( matrix , StartEnd  ) {
  # return list of size nrow(matrix) of columns with blank values either at beginning or end
  # StartEnd = "start" use for stagger start
  # StartEnd = "end" use for catch up at the end
  if ( StartEnd == "start") {
    NonBlank <- getNonBlank ( matrix , min )
    FirstNonBlankLocation <- LocateNonBlank ( matrix , min )
  } else {
    NonBlank <- getNonBlank ( matrix , max )
    LastNonBlankLocation <- LocateNonBlank ( matrix , max )
  }
  
  blanksCols <- list ()
  
  for ( i in 1 : nrow ( matrix )) {
    
    blanksCols [[ i ]]  <- ( 1 : ncol ( matrix ) )[ matrix [ i , ] == ""]
    if ( StartEnd == "start")   {
      blanksCols [[ i ]] <- blanksCols [[ i ]] [ blanksCols [[ i ]] < FirstNonBlankLocation [ i ] ] 
    } else
      blanksCols [[ i ]] <- blanksCols [[ i ]] [ blanksCols [[ i ]] > LastNonBlankLocation [ i ] ] 
  }
  
  return ( blanksCols)
}