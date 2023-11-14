#' GetMissingColumns
#' 
#' Returns a character vector of missing required columns
#' from a given dataframe
#' 
#' Required columns are:
#' author, year, TP, FN, FP, TN
#'
#' @param df a dataframe
#'
#' @return a character vector of missing columns 
#' (NULL if none are missing)
GetMissingCols <- function(df) {
    col_names <- names(df)
    required_cols <- c("author", "year", "TP", "FN", "FP", "TN")
    missing_cols <- c()
    for (col in required_cols) {
        if (!col %in% col_names) {
            missing_cols <- c(missing_cols, col)
        }
    }
    return(missing_cols)
}

#' IsValid
#' 
#' A function to validate that all the required columns
#' are present in a dataframe
#' see GetMissingColumns(df) for details
#' on the required columns
#'
#' @param df a dataframe to be validated
#'
#' @return boolean - TRUE if there are no missing columns
#' False if any columns are missing.
IsValid <- function(df) {
    if(is.null(GetMissingCols(df))){
        return(TRUE)
    }
    else {
        return(FALSE)
    }
}

#' DefaultFileInput
#' 
#' Function to return a shiny file input for a given namespace id
#'
#' @param id namespace id
#'
#' @return a shiny file input to be used with renderUI
DefaultFileInput <- function(id) {
    ns <- NS(id)
    return(
        fileInput(inputId = ns("data_input"), label="Please select a file", buttonLabel="Select", placeholder="No file selected", accept = c(".csv", ".xlsx"))
    )
}