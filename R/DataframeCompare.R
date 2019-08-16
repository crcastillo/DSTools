#' Find Mode of a Vector
#'
#' This function finds the most frequently occuring element within a
#' vector. This will remove any NA elements before calculation.
#'
#' @param x The vector of values to reference
#'
#' @return The most frequently occuring value
#'
#' @export
fx_mode <- function(x){

  #* Find unique values
  unique_x <- unique(na.omit(x))

  #* Return the most frequently occuring value
  return(unique_x[
    which.max(
      tabulate(
        match(
          na.omit(x)
          , unique_x
        )
      )
    )
    ]
  )

} # Close Function



#' List Feature Classes
#'
#' This function returns a character vector of the classes of the columns
#' of a given data.frame
#'
#' @param x The data.frame to be evaluated
#'
#' @return The classes of the data.frame columns
#'
#' @export
fx_allClass <- function(x){

  return(
    unlist(
      lapply(
        unclass(x)
        , class
        )
      )
    )

} # Close Function



#' Fix Factors with '_Other_' Levels
#'
#' This function addresses factors that contain '_Other_' as one of the
#' levels and coerces extra levels to '_Other_'
#'
#' @keywords internal
#'
#' @noRd
fx_OtherFix <- function(){

  #* Find factors that have new levels that need to be coerced to _Other_
  Other_Match_Fix <- Other_Match[
  unlist(
    lapply(
      1:length(Other_Match)
      , function(x){
        length(
          setdiff(
            x = Transform_Factors_Levels[[ Other_Match[x] ]]
            , y = Source_Factors_Levels[[ Other_Match[x] ]]
          )
        )
      }
    )
  ) > 0
  ]

  #* Handle factors that include _Other_ by imputing new factor levels to _Other_
  if(length(Other_Match_Fix) > 0){

    #* Loop through factors that need to have extra levels imputed to _Other_
    for(i in 1:length(Other_Match_Fix)){

      #* Store levels to be coerced
      Other_Levels <- setdiff(
        x = Transform_Factors_Levels[[ Other_Match_Fix[i] ]]
        , y = Source_Factors_Levels[[ Other_Match_Fix[i] ]]
      )


      #* Ensure transform_dataframe factor contains _Other_
      levels(transform_dataframe[[ Other_Match_Fix[i] ]]) <- c(
        levels(transform_dataframe[[ Other_Match_Fix[i] ]])
        , '_Other_'
      )


      #* Change levels to _Other_
      transform_dataframe[
        transform_dataframe[[ Other_Match_Fix[i] ]] %in% Other_Levels
        , Other_Match_Fix[i]
        ] <- '_Other_'

    } # Close loop

  } # Close boolean

  #* Return the transform_dataframe
  return(transform_dataframe)

} # Close Function



#' Fix Extra/Missing Factor Levels with EDF
#'
#' This function replaces any extra/missing factor levels within
#' transform_dataframe with a sample from the empirical distribution
#' of the factor from original_dataframe
#'
#' @keywords internal
#'
#' @noRd
fx_FactorEDF <- function(){

  #* Loop through factors that need to be imputed
  for(i in 1:length(Source_Factors_Fix)){

    #* Define the levels that need to be imputed
    Extra_Levels <- setdiff(
      x = Transform_Factors_Levels[[ Source_Factors_Fix[i] ]]
      , y = Source_Factors_Levels[[ Source_Factors_Fix[i] ]]
    )


    #* Define the records that need imputation
    Level_Index <- which(
      transform_dataframe[[ Source_Factors_Fix[i] ]] %in% Extra_Levels |
        is.na(transform_dataframe[[ Source_Factors_Fix[i] ]])
    )


    #* Convert factor to character type
    transform_dataframe[[ Source_Factors_Fix[i] ]] <- as.character(transform_dataframe[[ Source_Factors_Fix[i] ]])


    #* Change levels to randomly sample from the distribution of original_dataframe levels
    transform_dataframe[
      Level_Index
      , Source_Factors_Fix[i]
      ] <- as.character(
        sample(
          x = na.omit(original_dataframe[[ Source_Factors_Fix[i] ]])
          , size = length(Level_Index)
          , replace = TRUE
        )
      )


    #* Convert character attribute to factor type
    transform_dataframe[[ Source_Factors_Fix[i] ]] <- as.factor(transform_dataframe[[ Source_Factors_Fix[i] ]])

  } # Close EDF imputation loop

  #* Return the transform_dataframe
  return(transform_dataframe)

} # Close Function



#' Fix Extra/Missing Factor Levels with Mode
#'
#' This function replaces any extra/missing factor levels within
#' transform_dataframe with a the most frequently occurring level
#' of the factor from original_dataframe
#'
#' @keywords internal
#'
#' @noRd
fx_FactorMode <- function(){

  #* Loop through factors that need to be imputed
  for(i in 1:length(Source_Factors_Fix)){

    #* Define the levels that need to be imputed
    Extra_Levels <- setdiff(
      x = Transform_Factors_Levels[[ Source_Factors_Fix[i] ]]
      , y = Source_Factors_Levels[[ Source_Factors_Fix[i] ]]
    )


    #* Define the records that need imputation
    Level_Index <- which(
      transform_dataframe[[ Source_Factors_Fix[i] ]] %in% Extra_Levels |
        is.na(transform_dataframe[[ Source_Factors_Fix[i] ]])
    )


    #* Convert factor to character type
    transform_dataframe[[ Source_Factors_Fix[i] ]] <- as.character(transform_dataframe[[ Source_Factors_Fix[i] ]])


    #* Change levels to the most frequently occuring level
    transform_dataframe[
      Level_Index
      , Source_Factors_Fix[i]
      ] <- as.character(fx_mode(original_dataframe[[ Source_Factors_Fix[i] ]]))


    #* Convert character attribute to factor type
    transform_dataframe[[ Source_Factors_Fix[i] ]] <- as.factor(transform_dataframe[[ Source_Factors_Fix[i] ]])

  } # Close Mode imputation loop

  #* Return the transform_dataframe
  return(transform_dataframe)

} # Close Function



#' Fix Extra/Missing Factor Levels with Uniform
#'
#' This function replaces any extra/missing factor levels within
#' transform_dataframe with a uniform sample from all possible
#' levels of the factor from original_dataframe
#'
#' @keywords internal
#'
#' @noRd
fx_FactorUniform <- function(){

  #* Loop through factors that need to be imputed
  for(i in 1:length(Source_Factors_Fix)){

    #* Define the levels that need to be imputed
    Extra_Levels <- setdiff(
      x = Transform_Factors_Levels[[ Source_Factors_Fix[i] ]]
      , y = Source_Factors_Levels[[ Source_Factors_Fix[i] ]]
    )


    #* Define the records that need imputation
    Level_Index <- which(
      transform_dataframe[[ Source_Factors_Fix[i] ]] %in% Extra_Levels |
        is.na(transform_dataframe[[ Source_Factors_Fix[i] ]])
    )


    #* Convert factor to character type
    transform_dataframe[[ Source_Factors_Fix[i] ]] <- as.character(transform_dataframe[[ Source_Factors_Fix[i] ]])


    #* Change levels to randomly sample from the available original_dataframe levels
    transform_dataframe[
      Level_Index
      , Source_Factors_Fix[i]
      ] <- as.character(
        sample(
          x = na.omit(Source_Factors_Levels[[ Source_Factors_Fix[i] ]])
          , size = length(Level_Index)
          , replace = TRUE
        )
      )


    #* Convert character attribute to factor type
    transform_dataframe[[ Source_Factors_Fix[i] ]] <- as.factor(transform_dataframe[[ Source_Factors_Fix[i] ]])

  } # Close Uniform imptation loop

  #* Return the transform_dataframe
  return(transform_dataframe)

} # Close Function



#' Fix Extra/Missing Factor Levels with NA
#'
#' This function replaces any extra/missing factor levels within
#' transform_dataframe with an NA
#'
#' @keywords internal
#'
#' @noRd
fx_FactorNA <- function(){

  #* Loop through factors that need to be imputed
  for(i in 1:length(Source_Factors_Fix)){

    #* Define the levels that need to be imputed
    Extra_Levels <- setdiff(
      x = Transform_Factors_Levels[[ Source_Factors_Fix[i] ]]
      , y = Source_Factors_Levels[[ Source_Factors_Fix[i] ]]
    )


    #* Define the records that need imputation
    Level_Index <- which(
      transform_dataframe[[ Source_Factors_Fix[i] ]] %in% Extra_Levels |
        is.na(transform_dataframe[[ Source_Factors_Fix[i] ]])
    )


    #* Convert factor to character type
    transform_dataframe[[ Source_Factors_Fix[i] ]] <- as.character(transform_dataframe[[ Source_Factors_Fix[i] ]])


    #* Change levels to NA
    transform_dataframe[
      Level_Index
      , Source_Factors_Fix[i]
      ] <- NA


    #* Convert character attribute to factor type
    transform_dataframe[[ Source_Factors_Fix[i] ]] <- as.factor(transform_dataframe[[ Source_Factors_Fix[i] ]])

  } # Close NA imptation loop

  #* Return the transform_dataframe
  return(transform_dataframe)

} # Close Function



#' Fix NA Numerical Elements with Mean
#'
#' This function replaces any NA numerical attribute elements from
#' transform_dataframe with the mean value from original_dataframe
#'
#' @keywords internal
#'
#' @noRd
fx_NumericMean <- function(){

  #* Loop through attributes that need to be imputed
  for(i in 1:length(Source_Numerical_Fix)){

    #* Define the records that need imputation
    NA_Index <- which(
      is.na(transform_dataframe[[ Source_Numerical_Fix[i] ]])
    )


    #* Change NAs to the mean value from original_dataframe
    transform_dataframe[
      NA_Index
      , Source_Numerical_Fix[i]
      ] <- mean(original_dataframe[[ Source_Numerical_Fix[i] ]], na.rm = TRUE)

  } # Close mean imputation loop

  #* Return the transform_dataframe
  return(transform_dataframe)

} # Close Function



#' Fix NA Numerical Elements with Median
#'
#' This function replaces any NA numerical attribute elements from
#' transform_dataframe with the median value from original_dataframe
#'
#' @keywords internal
#'
#' @noRd
fx_NumericMedian <- function(){

  #* Loop through attributes that need to be imputed
  for(i in 1:length(Source_Numerical_Fix)){

    #* Define the records that need imputation
    NA_Index <- which(
      is.na(transform_dataframe[[ Source_Numerical_Fix[i] ]])
    )


    #* Change NAs to the median value from original_dataframe
    transform_dataframe[
      NA_Index
      , Source_Numerical_Fix[i]
      ] <- median(original_dataframe[[ Source_Numerical_Fix[i] ]], na.rm = TRUE)

  } # Close median imputation loop

  #* Return the transform_dataframe
  return(transform_dataframe)

} # Close Function



#' Fix NA Numerical Elements with Mode
#'
#' This function replaces any NA numerical attribute elements from
#' transform_dataframe with the most frequent occurring value from
#' the orginal_dataframe
#'
#' @keywords internal
#'
#' @noRd
fx_NumericMode <- function(){

  #* Loop through attributes that need to be imputed
  for(i in 1:length(Source_Numerical_Fix)){

    #* Define the records that need imputation
    NA_Index <- which(
      is.na(transform_dataframe[[ Source_Numerical_Fix[i] ]])
    )


    #* Change NAs to the most freuqently occurring value from original_dataframe
    transform_dataframe[
      NA_Index
      , Source_Numerical_Fix[i]
      ] <- fx_mode(original_dataframe[[ Source_Numerical_Fix[i] ]])

  } # Close mode imputation loop

  #* Return the transform_dataframe
  return(transform_dataframe)

} # Close Function



#' Fix NA Numerical Elements with EDF
#'
#' This function replaces any NA numerical attribute elements from
#' transform_dataframe with a sample of the empirical distribution
#' from the orginal_dataframe
#'
#' @keywords internal
#'
#' @noRd
fx_NumericEDF <- function(){

  #* Loop through attributes that need to be imputed
  for(i in 1:length(Source_Numerical_Fix)){

    #* Define the records that need imputation
    NA_Index <- which(
      is.na(transform_dataframe[[ Source_Numerical_Fix[i] ]])
    )


    #* Change levels to randomly sample from the distribution of original_dataframe levels
    transform_dataframe[
      NA_Index
      , Source_Numerical_Fix[i]
      ] <- sample(
        x = na.omit(original_dataframe[[ Source_Numerical_Fix[i] ]])
        , size = length(NA_Index)
        , replace = TRUE
      )

  } # Close EDF imputation loop

  #* Return the transform_dataframe
  return(transform_dataframe)

} # Close Function



#' Align Attribute Classes, Factor Levels, and Impute Missing Values
#'
#' This function takes a reference data.frame and alters a target data.frame to resemble the reference. The
#' classes, factor levels, and imputed values will be taken from the reference data.frame unless otherwise
#' specified in the parameters.
#'
#' @param original_dataframe The source dataframe to be referenced
#' @param transform_dataframe The dataframe to be transformed
#' @param exclude_attribute_list Optional list of attributes/columns to be ignore from both data.frames
#' @param factor_imputation Optional method for handling extra levels for factor attributes: "edf", "mode", "uniform", "NA", "ignore"
#' @param numerical_imputation Optional method for handling NAs for numerical attributes: "mean", "median", "mode", "edf", "ignore"
#'
#' @return A modified data.frame that resembles the original data.frame with aligned classes, factor levels, and
#' imputed values for NA elements
#'
#' @export
fx_Align_Impute <- function(
  original_dataframe
  , transform_dataframe
  , exclude_attribute_list
  , factor_imputation
  , numerical_imputation
){

  #**********************************
  ##### Load required libraries #####
  #**********************************

  require(lubridate) # Will not throw an error if lubridate cannot be found
  require(bit64) # Will not throw an error if bit64 cannot be found


  #************************************
  ##### Handle optional arguments #####
  #************************************

  #* Missing exclude_attribute_list, create empty element
  if(missing(exclude_attribute_list)){
    exclude_attribute_list <- NULL
  }


  #* Missing numerical_imputation, default to "mean"
  if(missing(numerical_imputation)){
    numerical_imputation <- "mean"
  }


  if(missing(factor_imputation)){
    factor_imputation <- "edf"
  }


  #**********************************************************************************************************
  ##### Determine the classes of the original_dataframe fields and align the transform_dataframe fields #####
  #**********************************************************************************************************

  #* Determine the classes of the original_dataframe fields, exclude any fields contained within exclude_attribute_list
  Source_Classes <- fx_allClass(original_dataframe)
  Source_Classes <- Source_Classes[ !names(Source_Classes) %in% exclude_attribute_list ]


  #* Determine the classes of the transform_dataframe fields, exclude any fields contained within exclude_attribute_list
  Transform_Classes <- fx_allClass(transform_dataframe)
  Transform_Classes <- Transform_Classes[ !names(Transform_Classes) %in% exclude_attribute_list ]


  #* Ensure the curated attribute lists are identical between Source_Classes and Transform_Classes
  if(
    length(Source_Classes) == length(Transform_Classes) &
    !anyNA(match(names(Transform_Classes), names(Source_Classes))) &
    !anyNA( match(names(Source_Classes), names(Transform_Classes)))
   ){

    #* Align transform_dataframe according to Source_Classes order
    transform_dataframe <- transform_dataframe[ , names(Source_Classes) ]

  } else {
    stop("Error: Extra attributes exist within the compared dataframes")
  } # Close boolean


  #* Reclassify attribute types from transform_dataframe to align with Source_Classes
  for( Iter in 1:length(Source_Classes)){

    #* Check to see if class of dataframe attribute is identical to the original_dataframe class
    if( class(transform_dataframe[[Iter]]) != Source_Classes[Iter]){

      #* If the classes don't align then alter per Source_Classes
      transform_dataframe[[Iter]] <- do.call(
        paste0(
          "as."
          , Source_Classes[Iter]
        )
        , list(transform_dataframe[[Iter]])
      )

      #* Print the attribute that is being adjusted
      # print(names(Source_Classes)[Iter])

    } # Close boolean

  } # Close loop


  #* Remove Iter if it exists
  if(exists("Iter")){
    rm(Iter)
  } # Close boolean


  #******************************************************************
  ##### Run check to ensure Source_Classes == Transform_Classes #####
  #******************************************************************

  #* Determine the classes of the transform_dataframe fields, exclude any fields contained within exclude_attribute_list
  Transform_Classes <- fx_allClass(transform_dataframe)
  Transform_Classes <- Transform_Classes[ !names(Transform_Classes) %in% exclude_attribute_list ]


  #* Stop the function and provide an error message if the attributes types do not align
  if(identical(Source_Classes, Transform_Classes)==FALSE){

    stop("Error: Attribute types do not align within the compared dataframes")

  } # Close boolean


  #************************************************
  ##### Store the levels of factor attributes #####
  #************************************************

  #* Identify factor attributes within Source_Classes
  Source_Factors <- names(Source_Classes)[ Source_Classes == "factor" ]


  #* Run check to verify there are factor attributes
  if(length(Source_Factors != 0)){

    #* Create a list that stores the levels of the individual factors
    Source_Factors_Levels <- lapply(
      1:length(Source_Factors)
      , function(x){
        levels(original_dataframe[[Source_Factors[x]]])
      }
    ) # Close lapply

    #* Provide names to Source_Factors_Levels elements
    names(Source_Factors_Levels) <- Source_Factors

  } # Close boolean


  #* Run check to verify there are factor attributes
  if(length(Source_Factors != 0)){

    #* Create a list that stores the levels of the individual factors
    Transform_Factors_Levels <- lapply(
      1:length(Source_Factors)
      , function(x){
        levels(transform_dataframe[[Source_Factors[x]]])
      }
    ) # Close lapply

    #* Provide names to Transform_Factors_Levels elements
    names(Transform_Factors_Levels) <- Source_Factors

  } # Close boolean


  #*****************************************
  ##### Store the numerical attributes #####
  #*****************************************

  #* Identify numerical attributes within Source_Classes
  Source_Numerical <- names(Source_Classes)[ Source_Classes %in% c("numeric", "integer", "double", "integer64") ]


  #*************************************************
  ##### Store the factors that contain _Other_ #####
  #*************************************************

  #* Create a vector that will store factors that contain _Other_ | typical value replacement for grouped factor levels
  Other_Match <- Source_Factors[
    unlist(
      lapply(
        1:length(Source_Factors)
        , function(x){
          !is.na(match('_Other_', Source_Factors_Levels[[x]]))
        }
      )
    )
    ]


  #*************************************************************************************************
  ##### Ensure _Other_ factors within transform_dataframe with extra levels are set to _Other_ #####
  #*************************************************************************************************

  #* Check to see Other_Match is populated
  if(length(Other_Match) > 1){

    #* Run fx_OtherFix()
    transform_dataframe <- fx_OtherFix()

  } # Close boolean


  #****************************************
  #****************************************
  ##### Categorical Factor Imputation #####
  #****************************************
  #****************************************

  #* Find factors that have new levels that need to be imputed
  Source_Factors_Fix <- Source_Factors[
    unlist(
      lapply(
        1:length(Source_Factors)
        , function(x){
          length(
            setdiff(
              x = Transform_Factors_Levels[[ Source_Factors[x] ]]
              , y = Source_Factors_Levels[[ Source_Factors[x] ]]
            )
          )
        }
      )
    ) > 0
  ]


  #* Append on factors that have NA to Source_Factors_Fix
  Source_Factors_Fix <- unique(
    c(
      Source_Factors_Fix
      , Source_Factors[
        unlist(
          lapply(
            1:length(Source_Factors)
            , function(x){
              anyNA(Transform_Factors_Levels[[ Source_Factors[x] ]])
            }
          )
        )
      ]
    )
  )


  #* Check to see if Source_Factors_Fix has elements
  if(length(Source_Factors_Fix) > 0){


    #*************************************
    ##### factor_imputation == "edf" #####
    #*************************************
    if(factor_imputation == "edf"){

      #* Display message
      print("factor_imputation == 'edf', extra factor levels will be set to the empirical random distribution of available levels")

      #* Run fx_FactorEDF()
      transform_dataframe <- fx_FactorEDF()

    } # Close EDF boolean


    #**************************************
    ##### factor_imputation == "mode" #####
    #**************************************
    if(factor_imputation == "mode"){

      #* Display message
      print("factor_imputation == 'mode', extra factor levels will be set to the most frequent available level")

      #* Run fx_FactorMode()
      transform_dataframe <- fx_FactorMode()

    } # Close Mode boolean


    #*****************************************
    ##### factor_imputation == "uniform" #####
    #*****************************************
    if(factor_imputation == "uniform"){

      #* Display message
      print("factor_imputation == 'uniform', extra factor levels will be set to the uniform random distribution of available levels")

      #* Run fx_FactorUniform()
      transform_dataframe <- fx_FactorUniform()

    } # Close Uniform boolean


    #************************************
    ##### factor_imputation == "NA" #####
    #************************************
    if(factor_imputation == "NA"){

      #* Display message
      print("factor_imputation == 'NA', extra factor levels will be set to NA")

      #* Run fx_FactorNA()
      transform_dataframe <- fx_FactorNA()

    } # Close NA boolean


    #****************************************
    ##### factor_imputation == "ignore" #####
    #****************************************
    if(factor_imputation == "ignore"){

      #* Display message
      print("factor_imputation == 'ignore', extra factor levels will NOT be altered")

    } # Close Ignore boolean


    #*****************************************************************************
    ##### Ensure transform_dataframe factor levels mirror original_dataframe #####
    #*****************************************************************************

    #* Create a list that stores the levels of the individual factors
    Transform_Factors_Levels <- lapply(
      1:length(Source_Factors)
      , function(x){
        levels(transform_dataframe[[Source_Factors[x]]])
      }
    ) # Close lapply


    #* Provide names to Transform_Factors_Levels elements
    names(Transform_Factors_Levels) <- Source_Factors


    #* Store the extra levels for transform_dataframe
    Transform_Extra_Levels <- lapply(
      1:length(Source_Factors)
      , function(x){
        setdiff(
          x = Transform_Factors_Levels[[ Source_Factors[x] ]]
          , y = Source_Factors_Levels[[ Source_Factors[x] ]]
        )
      }
    )


    #* Provide names to Extra_Levels elements
    names(Transform_Extra_Levels) <- Source_Factors


    #* Loop through factors that need to have their levels aligned
    for(i in 1:length(Source_Factors)){

      #* Change tranform_dataframe levels to mirror original_dataframe
      levels(transform_dataframe[[ Source_Factors[i] ]]) <- c(
        Source_Factors_Levels[[ Source_Factors[i] ]]
        , Transform_Extra_Levels[[ Source_Factors[i] ]]
      )

    } # Close factor level alignment loop


    #******************************************************************
    ##### Run checks to identify factors with extra levels or NAs #####
    #******************************************************************

    #* Store index of factors with extra attributes
    Transform_Extra_Levels_Index <- which(
      lapply(
        Transform_Extra_Levels
        , function(x){
          length(x)
        }
      ) > 0
    )


    #* Check to see Transform_Extra_Levels_Index has elements
    if(length(Transform_Extra_Levels_Index) > 0){

      #* Print the factors and extra levels
      lapply(
        1:length(Transform_Extra_Levels_Index)
        , function(x){

          print(
            paste0(
              names(Transform_Extra_Levels_Index)[x]
              , ' has the following extra levels: '
              , paste(
                # Transform_Extra_Levels[[ Transform_Extra_Levels_Index[[x]] ]]
                Transform_Extra_Levels[[ names(Transform_Extra_Levels_Index)[x] ]]
                , collapse = ", "
              )
              )
          )

        }
      )

    } # Close boolean


    #* Store names of transform_dataframe factors with NA
    Transform_NA_Factors <- Source_Factors[
      unlist(
        lapply(
          1:length(Source_Factors)
          , function(x){
            anyNA( transform_dataframe[[ Source_Factors[x] ]] )
            }
          )
        )
      ]


    #* Check to see Transform_NA_Factors has elements
    if(length(Transform_NA_Factors) > 0){

      #* Print the factors and extra levels
      print(
        paste0(
          'The following factors have NAs: '
          , paste(
            Transform_NA_Factors
            , collapse = ", "
          )
        )
      )

    } # Close boolean

  } # Close factor imputation boolean


  #**************************************
  #**************************************
  ##### Numerical Factor Imputation #####
  #**************************************
  #**************************************

  #* Append on factors that have NA to Source_Factors_Fix
  Source_Numerical_Fix <-
    Source_Numerical[
        unlist(
          lapply(
            1:length(Source_Numerical)
            , function(x){
              anyNA(transform_dataframe[[ Source_Numerical[x] ]])
            }
          )
        )
        ]


  #* Check to see if Source_Numerical_Fix has elements
  if(length(Source_Numerical_Fix) > 0){


    #*****************************************
    ##### numerical_imputation == "mean" #####
    #*****************************************
    if(numerical_imputation == "mean"){

      #* Display message
      print("numerical_imputation == 'mean', numerical NAs will be imputed with the mean value")

      #* Run fx_NumericMean()
      transform_dataframe <- fx_NumericMean()

    } # Close mean boolean


    #*******************************************
    ##### numerical_imputation == "median" #####
    #*******************************************
    if(numerical_imputation == "median"){

      #* Display message
      print("numerical_imputation == 'median', numerical NAs will be imputed with the median value")

      #* Run fx_NumericMedian()
      transform_dataframe <- fx_NumericMedian()

    } # Close median boolean


    #*****************************************
    ##### numerical_imputation == "mode" #####
    #*****************************************
    if(numerical_imputation == "mode"){

      #* Display message
      print("numerical_imputation == 'mode', numerical NAs will be imputed with the most frequently occurring value")

      #* Run fx_NumericMode()
      transform_dataframe <- fx_NumericMode()

    } # Close mode boolean


    #****************************************
    ##### numerical_imputation == "edf" #####
    #****************************************
    if(numerical_imputation == "edf"){

      #* Display message
      print("numerical_imputation == 'edf', NA records will be set to the empirical random distribution of available values")

      #* Run fx_NumericEDF()
      transform_dataframe <- fx_NumericEDF()

    } # Close EDF boolean


    #*******************************************
    ##### numerical_imputation == "ignore" #####
    #*******************************************
    if(numerical_imputation == "ignore"){

      #* Display message
      print("numerical_imputation == 'ignore', NA records will not be imputed")

    } # Close NA boolean


    #*****************************************************
    ##### Run checks to identify attributes with NAs #####
    #*****************************************************

    #* Store names of transform_dataframe attributes with NA
    Transform_NA_Numerics <- Source_Numerical[
      unlist(
        lapply(
          1:length(Source_Numerical)
          , function(x){
            anyNA( transform_dataframe[[ Source_Numerical[x] ]] )
          }
        )
      )
      ]


    #* Check to see Transform_NA_Numerics has elements
    if(length(Transform_NA_Numerics) > 0){

      #* Print the attributes
      print(
        paste0(
          'The following factors have NAs: '
          , paste(
            Transform_NA_Numerics
            , collapse = ", "
          )
        )
      )

    } # Close boolean


  } # Close numerical imputation boolean


  #*************************************
  ##### Return transform_dataframe #####
  #*************************************

  return(transform_dataframe)

}  # Close Function
