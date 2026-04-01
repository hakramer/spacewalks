# Anu Kramer
# 20260331
# test code for workshop
# data pulled from: https://data.nasa.gov/resource/eva.json (with modifications) on DATE

#############
### SETUP ###
#############
input_file = 'eva-data.json'
output_file = 'eva-data.csv'
graph_file = 'cumulative_eva_graph.png'

library(jsonlite) # functionality to upload data from json format
library(lubridate) # interpret dates from data file
library(tidyverse) # ggplot for making fig

#################
### functions ###
#################


#' Read EVA data from a JSON file into a tibble
#'
#' Reads a JSON file containing an array of records (objects) and returns the
#' contents as a tibble for downstream analysis.
#'
#' @param input_file Path to a JSON file (character scalar). The file is expected
#'   to contain a JSON array of objects, e.g. `[{"eva":"1", ...}, {"eva":"2", ...}]`.
#' @return A tibble with one row per JSON record and one column per field.
#' @examples
#' eva_tbl <- read_json_to_dataframe("./eva-data.json")
#' dplyr::glimpse(eva_tbl)
read_json_to_dataframe <- function(json_file) {
  eva_tbl <- jsonlite::fromJSON(json_file) |> # make input into a table
    as_tibble()
}


#' Update the json and add cols and formatting, then save
#'
#' Reads a JSON file containing an array of records (objects) and turns it into a tibble, then
#' updates the tibble by: adding formatting and variable types, remove any rows that don't have a duration or date
#' ordering by date, adding hours column and populating, calculating cumulative hours, and writing the table to a csv
#'
#' @param input_file Path to a JSON file (character scalar). The file is expected
#'   to contain a JSON array of objects, e.g. `[{"eva":"1", ...}, {"eva":"2", ...}]`. 
#' @param output_file CSV filename to save the output table to
#' @returns A tibble with one row per JSON record and one column per field, sorted by date.
#' @export output_file saves the above sorted table as a csv
#' @examples 
#' eva_tbl <- update_format_json_dataframe(input_file, output_file)
update_format_json_dataframe <- function(input_file,output_file) {
  eva_tbl <- read_json_to_dataframe(input_file)
  eva_tbl <- eva_tbl |> # add formatting to table columns
    mutate(
      eva  = as.numeric(eva),
      date = ymd_hms(date, quiet = TRUE) ) |>
    filter(!is.na(duration), duration != "", !is.na(date)) # remove any rows that don't have a duration or date
  # order table by date
  eva_tbl <- eva_tbl |> 
    arrange(date)
  
  # add hours column and populate & calculate cumulative hours
  eva_tbl <- eva_tbl |> 
    mutate(
      duration_hours = {
        parts <- str_split(duration, ":", n = 2, simplify = TRUE)
        as.numeric(parts[, 1]) + as.numeric(parts[, 2]) / 60
      },
      cumulative_time = cumsum(duration_hours)
    )
  readr::write_csv(eva_tbl, output_file) # write the table to a csv
  return(eva_tbl)
}

#' Plot cumulative time in space vs. year & save
#'
#' @param df a dataframe containing 'date' and 'cumulative_time' cumulative time in space
#' @param graph_file a path to save the plot
#'
#' @returns the plot
#' @export graph_file saves the plot to the 'graph file' filename
#'
#' @examples
#' plot_cumulative_time_in_space(eva_tbl,graph_file)
plot_cumulative_time_in_space <- function(df, graph_file) {
  p <- ggplot(df, aes(x = date, y = cumulative_time)) +
    geom_point() +
    geom_line() +
    labs(
      x = "Year",
      y = "Total time spent in space to date (hours)"
    ) +
    theme_minimal()
  
  ggsave(graph_file, plot = p, width = 9, height = 5, dpi = 300)
  print(p)
  return(p)
}

####################
### run the code ###
####################
eva_tbl <- update_format_json_dataframe(input_file, output_file)
plot_cumulative_time_in_space(eva_tbl,graph_file)
