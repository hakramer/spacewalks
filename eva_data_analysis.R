# Anu Kramer
# 20260331
# test code for workshop
# data pulled from: https://data.nasa.gov/resource/eva.json (with modifications) on DATE

input_file = 'eva-data.json'
output_file = 'eva-data.csv'
graph_file = 'cumulative_eva_graph.png'

library(jsonlite) # functionality to upload data from json format
library(lubridate) # interpret dates from data file
library(tidyverse) # ggplot for making fig

eva_tbl <- jsonlite::fromJSON(input_file) |> # make input into a table
  as_tibble()


eva_tbl <- eva_tbl |> # add formatting to table columns
  mutate(
    eva  = as.numeric(eva),
    date = ymd_hms(date, quiet = TRUE) ) |>
  filter(!is.na(duration), duration != "", !is.na(date)) # remove any rows that don't have a duration or date


readr::write_csv(eva_tbl, output_file) # write the table to a csv


eva_tbl <- eva_tbl |> #order table by date
  arrange(date)


eva_tbl <- eva_tbl |> # add hours column and populate & calculate cumulative hours
  mutate(
    duration_hours = {
      parts <- str_split(duration, ":", n = 2, simplify = TRUE)
      as.numeric(parts[, 1]) + as.numeric(parts[, 2]) / 60
    },
    cumulative_time = cumsum(duration_hours)
  )


# plot cumulative time in space vs. year
cumulative_spacetime_plot <- ggplot(eva_tbl, aes(x = date, y = cumulative_time)) +
  geom_point() +
  geom_line() +
  labs(
    x = "Year",
    y = "Total time spent in space to date (hours)"
  ) +
  theme_minimal()

ggsave(graph_file, plot = cumulative_spacetime_plot, width = 9, height = 5, dpi = 300)
print(cumulative_spacetime_plot)
