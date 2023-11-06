# Function to check data is valid
get_missing_cols <- function(df) {
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

is_valid <- function(df) {
    if(is.null(get_missing_cols(df))){
        return(TRUE)
    }
    else {
        return(FALSE)
    }
}