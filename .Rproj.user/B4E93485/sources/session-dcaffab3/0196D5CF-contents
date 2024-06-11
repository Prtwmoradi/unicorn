#' Show CSV Head or Null Values and Preprocess Data
#'
#' This function allows users to input the path of a CSV file and then choose to view the head of the data,
#' count the null values in each column, or preprocess the data.
#' @export
unicorn <- function() {
  # Prompt the user to enter the file path
  file_path <- readline(prompt = "Please enter the path of the CSV file: ")

  # Check if the 'knitr' package is installed, and install it if necessary
  if (!requireNamespace("knitr", quietly = TRUE)) {
    install.packages("knitr")
  }

  # Load the 'knitr' package
  library(knitr)

  # Attempt to read the CSV file from the path
  tryCatch({
    df <- read.csv(file_path)

    while(TRUE) {
      # Present options to the user
      cat("\nOptions:\n")
      cat("1. See the head of the data\n")
      cat("2. See the null values\n")
      cat("3. Preprocess the data\n")
      cat("4. Exit\n")

      # Ask the user to choose an option
      user_input <- readline(prompt = "Please enter your choice (1, 2, 3, or 4): ")

      # Check the user's input and perform the corresponding action
      if (user_input == "1") {
        # Display the head of the data frame as a table
        print(kable(head(df), format = "simple"))
      } else if (user_input == "2") {
        # Calculate and display the count of null values in each column
        null_counts <- colSums(is.na(df))
        print(kable(data.frame(Column = names(null_counts), Null_Count = null_counts), format = "simple"))
      } else if (user_input == "3") {
        preprocess_data(df)
      } else if (user_input == "4") {
        cat("Exiting.\n")
        break
      } else {
        cat("Invalid input. Please try again.\n")
      }
    }
  }, error = function(e) {
    cat("An error occurred while reading the CSV file:\n")
    cat(e$message, "\n")
  })
}

# Preprocessing function
preprocess_data <- function(df) {
  while(TRUE) {
    # Present preprocessing options to the user
    cat("\nPreprocessing Options:\n")
    cat("1. Fill missing values with a specified value\n")
    cat("2. Remove rows with missing values\n")
    cat("3. Convert data types of columns\n")
    cat("4. Return to main menu\n")

    # Ask the user to choose an option
    preprocess_input <- readline(prompt = "Please enter your choice (1, 2, 3, or 4): ")

    if (preprocess_input == "1") {
      column <- readline(prompt = "Enter the column name to fill missing values: ")
      value <- readline(prompt = "Enter the value to fill missing values with: ")
      df[[column]][is.na(df[[column]])] <- value
      cat("Missing values in column", column, "have been filled with", value, ".\n")
    } else if (preprocess_input == "2") {
      df <- df[complete.cases(df), ]
      cat("Rows with missing values have been removed.\n")
    } else if (preprocess_input == "3") {
      column <- readline(prompt = "Enter the column name to convert data type: ")
      dtype <- readline(prompt = "Enter the target data type (e.g., numeric, character): ")
      df[[column]] <- switch(dtype,
                             numeric = as.numeric(df[[column]]),
                             character = as.character(df[[column]]),
                             factor = as.factor(df[[column]]),
                             as(df[[column]], dtype))
      cat("Column", column, "has been converted to", dtype, "data type.\n")
    } else if (preprocess_input == "4") {
      break
    } else {
      cat("Invalid input. Please try again.\n")
    }
  }
}
