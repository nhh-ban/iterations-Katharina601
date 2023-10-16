transform_metadata_to_df <-function(stations_metadata)

 {
  # Convert the latestData to UTC format
  stations_metadata$latestData <- as.POSIXct(stations_metadata$latestData, tz = "UTC")
  
  # Create a data frame
  stations_metadata_df <- data.frame(
    id = stations_metadata$id,
    name = stations_metadata$name,
    latestData = stations_metadata$latestData,
    lat = stations_metadata$lat,
    lon = stations_metadata$lon
  )
  
  return(stations_metadata_df)
}

stations_metadata[[1]] %>%  
  map_dfr(as_tibble)  %>% 
  mutate(latestData = map_chr(latestData, 1, .default = NA_character_)) %>% 
  mutate(latestData = as_datetime(latestData, tz = "UTC"))  %>% 
  unnest_wider(location) %>% 
  unnest_wider(latLon)

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


#Task 5
########

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

