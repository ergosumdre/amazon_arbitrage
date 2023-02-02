# Load the necessary libraries
library(httr)
library(dplyr)
library(rvest)
library(stringr)

# Define the main function upcAndPrice
upcAndPrice <- function(webpage){
  # Use tryCatch to handle errors
  tryCatch({
    # Use httr to retrieve the content of the webpage
    html <- httr::GET(url = webpage)
    
    # Extract the UPC number from the content of the webpage
    upc_num <- str_sub(content(html, "text"), 
                       start = str_locate(content(html, "text"), "upc")[1] +6,
                       end = str_locate(content(html, "text"), "mpn")[1] -4)
    
    # Extract the price from the content of the webpage
    myPrice <- str_sub(content(html, "text"), 
                       start = str_locate(content(html, "text"), "Your Price")[1] +6,
                       end = str_locate(content(html, "text"), "Your Price")[1] + 189)
    # Use regular expressions to extract the numeric value from the price
    myPrice <- regmatches(myPrice, gregexpr("[[:digit:]]+", myPrice))
    # Convert the extracted value to a numeric
    myPrice <- as.numeric(unlist(myPrice))
    # Collapse the extracted value to a single string
    myPrice <- str_c(myPrice, collapse = ".") 
    
    # Create a data frame with the UPC number and price
    df <- data.frame(upc = upc_num,
                     price = myPrice)
  }, 
  # If there is an error, return a data frame with NAs for the UPC number and price
  error = function(e) {
    upc_num <- NA
    myPrice <- NA
    
    df <- data.frame(upc = upc_num,
                     price = myPrice)
  })
  # Return the data frame
  return(df)
}

# Call the upcAndPrice function with a specific webpage URL
upcAndPrice("URL")
