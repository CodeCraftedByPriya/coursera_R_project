# TASK 1
# Load the required package
library(httr)

# Function to get the HTML page
get_wiki_covid19_page <- function() {
  # Specify the URL
  url <- "https://en.wikipedia.org/w/index.php?title=Template:COVID-19_testing_by_country"
  
  # Make the GET request
  response <- GET(url)
  
  # Check if the request was successful
  if (status_code(response) == 200) {
    # Return the response
    return(content(response, "text"))
  } else {
    # Print an error message if the request failed
    stop("Failed to retrieve the page. Status code: ", status_code(response))
  }
}

# Call the function and store the result
wiki_page <- get_wiki_covid19_page()

# Print the first 1000 characters of the retrieved HTML content
cat(substr(wiki_page, 1, 1000))


# TASK 2
# Load the required packages
library(httr)
library(rvest)

# Call the function and store the result
wiki_page <- get_wiki_covid19_page()

# Parse the HTML content
root_html <- read_html(wiki_page)

# Get the tables in the HTML root node
table_nodes <- html_nodes(root_html, "table")

# Extract the specific table (index 2) and convert it into a data frame
covid_testing_data <- html_table(table_nodes[2], fill = TRUE)[[1]]

# Rename the columns to match the structure you want
colnames(covid_testing_data) <- c("Country or region", "Date[a]", "Tested", "Units[b]", "Confirmed (cases)", "Confirmed / tested, %", 
                                  "Tested / population, %", "Confirmed / population, %", "Ref.")

# Print the formatted data frame
print(covid_testing_data)


# TASK 3
# Function to preprocess the COVID-19 data frame
preprocess_covid_data_frame <- function(raw_df) {
  # Rename columns
  colnames(raw_df) <- c("country", "date", "tested", "units", "confirmed", 
                        "confirmed.tested.ratio", "tested.population.ratio", 
                        "confirmed.population.ratio", "ref")
  # Convert 'tested' column to numeric (handling commas)
  raw_df$tested <- as.numeric(gsub(",", "", raw_df$tested))
  # Convert 'confirmed' column to numeric (handling commas)
  raw_df$confirmed <- as.numeric(gsub(",", "", raw_df$confirmed))
  # Calculate new columns if needed
  raw_df$confirmed.tested.ratio <- raw_df$confirmed / raw_df$tested * 100
  raw_df$tested.population.ratio <- raw_df$tested / 1000  # Example calculation
  raw_df$confirmed.population.ratio <- raw_df$confirmed / 10000  # Example calculation
  # Return processed data frame
  return(raw_df)
}
# Call preprocess_covid_data_frame function with the extracted data frame
processed_df <- preprocess_covid_data_frame(covid_testing_data)
# Print summary of the processed data frame
summary(processed_df)

# Export the processed data frame to a CSV file
write.csv(processed_df, "covid.csv", row.names = FALSE)

# Check if the file exists and print its path
wd <- getwd()
file_path <- file.path(wd, "covid.csv")
print(file_path)
file.exists(file_path)

# Download the provided CSV file
covid_csv_file <- download.file("https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0101EN-Coursera/v2/dataset/covid.csv", destfile="covid.csv")

# Read the CSV file into a data frame
covid_data_frame_csv <- read.csv("covid.csv", header=TRUE, sep=",")


# TASK 4
# Read covid_data_frame_csv from the csv file
covid_data_frame_csv <- read.csv("covid.csv", header=TRUE)

# Get the 5th to 10th rows with "country" and "confirmed" columns
subset_data <- covid_data_frame_csv[5:10, c("country", "confirmed")]

# Print the subset data frame
print(subset_data)



# TASK 5
# Get the total confirmed cases worldwide
total_confirmed <- sum(covid_data_frame_csv$confirmed, na.rm = TRUE)

# Get the total tested cases worldwide
total_tested <- sum(covid_data_frame_csv$tested, na.rm = TRUE)

# Calculate the positive ratio (confirmed / tested)
positive_ratio <- total_confirmed / total_tested

# Print the results
cat("Total confirmed cases worldwide:", total_confirmed, "\n")
cat("Total tested cases worldwide:", total_tested, "\n")
cat("Overall positive ratio:", positive_ratio, "\n")


# TASK 6
# Extract the country column
countries <- covid_data_frame_csv$country
# Check the class of the country column (should be Factor)
class(countries)
# Convert the country column to character
countries <- as.character(countries)
# Sort the countries A to Z
countries_AtoZ <- sort(countries)
# Sort the countries Z to A
countries_ZtoA <- sort(countries, decreasing = TRUE)
# Print the sorted Z to A list
print(countries_ZtoA)


# TASK 7
# Read covid_data_frame_csv from the csv file if not already done
covid_data_frame_csv <- read.csv("covid.csv", header=TRUE)
# Use grep with regular expression to find matches
matches <- grep("^United", covid_data_frame_csv$country, value = TRUE)
# Print the matched country names
print(matches)



# TASK 8
# Select data for two countries (replace "Country1" and "Country2" with actual country names)
country1 <- covid_data_frame_csv[covid_data_frame_csv$country == "Country1", c("country", "confirmed", "confirmed.population.ratio")]
country2 <- covid_data_frame_csv[covid_data_frame_csv$country == "Country2", c("country", "confirmed", "confirmed.population.ratio")]
# Print data for Country 1
print(country1)

# Print data for Country 2
print(country2)


# Task 9
# Select data for Australia

country1 <- covid_data_frame_csv[covid_data_frame_csv$country == "Australia", c("country", "confirmed", "confirmed.population.ratio")]

# Select data for Austria
country2 <- covid_data_frame_csv[covid_data_frame_csv$country == "Austria", c("country", "confirmed", "confirmed.population.ratio")]

# Extract confirmed population ratio for Australia
country1_ratio <- country1$confirmed.population.ratio
# Extract confirmed population ratio for Austria
country2_ratio <- country2$confirmed.population.ratio

# Compare ratios using if-else statement
if (country1_ratio > country2_ratio) {
  cat("Australia has a larger ratio of confirmed cases to population:", country1_ratio, "\n")
} else if (country2_ratio > country1_ratio) {
  cat("Austria has a larger ratio of confirmed cases to population:", country2_ratio, "\n")
} else {
  cat("Both countries have the same ratio of confirmed cases to population:", country1_ratio, "\n")
}


# Task 9
# Prompt the user to enter the first country
cat("Enter the first country: ")
country1_name <- readline()

# Prompt the user to enter the second country
cat("Enter the second country: ")
country2_name <- readline()

# Select data for the first country entered by the user
country1 <- covid_data_frame_csv[covid_data_frame_csv$country == country1_name, c("country", "confirmed", "confirmed.population.ratio")]

# Select data for the second country entered by the user
country2 <- covid_data_frame_csv[covid_data_frame_csv$country == country2_name, c("country", "confirmed", "confirmed.population.ratio")]

# Extract confirmed population ratio for the first country
country1_ratio <- country1$confirmed.population.ratio

# Extract confirmed population ratio for the second country
country2_ratio <- country2$confirmed.population.ratio

# Compare ratios using if-else statement
if (country1_ratio > country2_ratio) {
  cat(country1_name, "has a larger ratio of confirmed cases to population:", country1_ratio, "\n")
} else if (country2_ratio > country1_ratio) {
  cat(country2_name, "has a larger ratio of confirmed cases to population:", country2_ratio, "\n")
} else {
  cat("Both countries have the same ratio of confirmed cases to population:", country1_ratio, "\n")
}


# TASK 10
# Define the threshold ratio (1% in this case)
threshold <- 1  # 1% threshold

# Filter countries with confirmed.population.ratio less than the threshold
countries_below_threshold <- covid_data_frame_csv[covid_data_frame_csv$confirmed.population.ratio < threshold, c("country", "confirmed", "confirmed.population.ratio")]

# Print the countries with ratios less than the threshold
print("Countries with confirmed to population ratio less than 1%:")
print(countries_below_threshold)
