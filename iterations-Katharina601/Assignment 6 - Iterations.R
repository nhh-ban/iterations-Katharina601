#Assignment 6 - Iterations
##########################

#Problem 2
##########
library(httr)
library(jsonlite)
library(ggplot2)
library(DescTools)
library(tidyverse)
library(magrittr)
library(rlang)
library(lubridate)
library(anytime)
library(readr)
library(yaml)

#### 1: Beginning of script

# Load function for posting GQL-queries and retrieving data: 
source("functions/GQL_function.r")

# The URL we will use is stored below: 

configs <- 
  read_yaml("vegvesen_configs.yml")


gql_metadata_qry <- read_file("gql-queries/station_metadata.gql")

# Let's try submitting the query: 

stations_metadata <-
  GQL(
    query=gql_metadata_qry,
    .url = configs$vegvesen_url
  ) 


#### 2: Transforming metadata

source("functions/data_transformations.r")

stations_metadata_df <- 
  stations_metadata %>% 
  transform_metadata_to_df(.)


#### 3: Testing metadata
source("functions/data_tests.r")
test_stations_metadata(stations_metadata_df)


### 5: Final volume query: 

source("gql-queries/vol_qry.r")

stations_metadata_df %>% 
  filter(latestData > Sys.Date() - days(7)) %>% 
  sample_n(1) %$% 
  vol_qry(
    id = id,
    from = to_iso8601(latestData, -4),
    to = to_iso8601(latestData, 0)
  ) %>% 
  GQL(., .url = configs$vegvesen_url) %>%
  transform_volumes() %>% 
  ggplot(aes(x=from, y=volume)) + 
  geom_line() + 
  theme_classic()

#Verify the solution of Problem 2
source("functions/data_transformations.r")

stations_metadata_df <- 
  stations_metadata %>% 
  transform_metadata_to_df(.)


#Problem 3: Testing metadata
############################

# This file contains tests to be applied to 
# the Vegvesen stations-data *after* being transformed
# to a data frame. 
# 
# All tests are packed in a function test_stations_metadata that apples
# all the aforementioned tests


#Proof if the data frame has the expected column names.
test_stations_metadata_colnames <-
  function(df) {
    
    expected_colnames <- c("id", "name", "latestData", "lat", "lon")
    
    if (all(colnames(df) == expected_colnames) == TRUE) {
      print("PASS: Data has the correct columns")
    } else{
      print("FAIL: Columns do not match the correct specification")
    }
  }


#Test whether the number of rows 
#of the data frame is within expected limits.
test_stations_metadata_nrows <-
  function(df) {
    
    min_expected_rows <- 5000
    max_expected_rows <- 10000
    
    if (nrow(df) > min_expected_rows & nrow(df) < max_expected_rows) {
      print("PASS: Data has a reasonable number of rows")
    } else if (nrow(df) <= min_expected_rows) {
      print("FAIL: Data has suspiciously few rows")
    } else {
      print("FAIL: Data has suspiciously many rows")
    }
  }


#Test whether the column data typs in the data frame match the expected
#data types.
test_stations_metadata_coltypes <-
  function(df) {
    expected_coltypes <-
      c("character", "character", "double", "double", "double")
    
    if (all(df %>%
            map_chr( ~ typeof(.)) == expected_coltypes) == TRUE) {
      print("PASS: All cols have the correct specifications")
    } else{
      print("FAIL: Columns do not have the correct specification")
    }
  }


#Check if the amount of missing values in the data frame is within
#an acceptable range.
test_stations_metadata_nmissing <-
  function(df) {
    max_miss_vals <- 200
    
    if (df %>% map_int( ~ sum(is.na((.)))) %>% sum(.) < max_miss_vals) {
      print("PASS: Amount of missing values is reasonable")
    } else {
      print("FAIL: Too many missing values in data set")
    }
  }


#Check if the "latestData" column has the expected UTC-time zone
test_stations_metadata_latestdata_timezone <-
  function(df) {
    
    if (attr(df$latestData,"tzone")=="UTC") {
      print("PASS: latestData has UTC-time zone")
    } else {
      print("FAIL: latestData does not have expected UTC-time zone")
    }
  }


#Joined function to run all the individual tests defined previously
#on the data frame.
test_stations_metadata <- 
  function(df){
    test_stations_metadata_colnames(df)
    test_stations_metadata_coltypes(df)
    test_stations_metadata_nmissing(df)
    test_stations_metadata_nrows(df)
    test_stations_metadata_latestdata_timezone(df)
  }


#Problem 4 - getting volume data
################################

#Problem 4
##########

#Task 4a - time-function
########################
to_iso8601 <- function(datetime, offset) {
  # Convert datetime to a POSIXct object
  datetime <- as_datetime(datetime)
  
  # Add the offset in days to the datetime
  new_datetime <- datetime + days(offset)
  
  # Format the datetime in ISO8601 with UTC time zone
  iso8601_time <- format(anytime(new_datetime), format = "%Y-%m-%dT%H:%M:%SZ")
  
  return(iso8601_time)
}

#Example test #1:
to_iso8601(as_datetime("2016-09-01 10:11:12"),0)

#Example test #2:
to_iso8601(as_datetime("2016-09-01 10:11:12"),-4)


#Task 4b - GQL for volumes
##########################

library(glue)

#Verify that the assignment works:
GQL(
  vol_qry(
    id=stations_metadata_df$id[1], 
    from=to_iso8601(stations_metadata_df$latestData[1],-4),
    to=to_iso8601(stations_metadata_df$latestData[1],0)
  ),
  .url = configs$vegvesen_url
)


#Task 5 - finalizing a traffic volume call!
###########################################


library(jsonlite)

transform_volumes <- function(json_response) {
  response_list <- fromJSON(json_response)
  
  # Extract the relevant data from the list
  data <- response_list$data$trafficData$volume$byHour$edges$node
  
  # Convert the data to a data frame
  traffic_data_df <- as.data.frame(data)
  
  return(traffic_data_df)
}


#Task 6 - making the plot prettier
##################################

library(ggplot2)

plot_traffic_volume <- function(traffic_data_df, station_name) {
  pretty_plot <- ggplot(traffic_data_df, aes(x = as.POSIXct(from), y = total.volumeNumbers.volume)) +
    geom_line() +
    labs(
      title = paste("Traffic Volume for Station:", station_name),
      x = "Time",
      y = "Volume"
    ) +
    theme_minimal() +
    theme(legend.title = element_text("Station Name"))
  
  print(pretty_plot)
}

plot_traffic_volume(traffic_data_df, "Station Name")


