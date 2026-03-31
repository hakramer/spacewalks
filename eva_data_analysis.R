# Anu Kramer
# 20260331
# test code for workshop
# data pulled from: https://data.nasa.gov/resource/eva.json (with modifications) on DATE

input_file = 'eva-data.json'
output_file = 'eva-data.csv'
graph_file = 'cumulative_eva_graph.png'
# fieldnames <- c("EVA #", "Country", "Crew    ", "Vehicle", "Date", "Duration", "Purpose")

library(jsonlite) # functionality to upload data from json format
library(lubridate) # interpret dates from data file
library(tidyverse) # ggplot for making fig

eva_tbl <- jsonlite::fromJSON(input_file) |>
  as_tibble()

subset=c('duration','date')
eva_tbl <- eva_tbl |>
  mutate(
    eva  = as.numeric(eva),
    date = ymd_hms(date, quiet = TRUE)
  ) |>
  filter(!is.na(duration), duration != "", !is.na(date))


readr::write_csv(eva_tbl, output_file)


eva_tbl <- eva_tbl |>
  arrange(date)


eva_tbl <- eva_tbl |>
  mutate(
    duration_hours = {
      parts <- str_split(duration, ":", n = 2, simplify = TRUE)
      as.numeric(parts[, 1]) + as.numeric(parts[, 2]) / 60
    },
    cumulative_time = cumsum(duration_hours)
  )


p <- ggplot(eva_tbl, aes(x = date, y = cumulative_time)) +
  geom_point() +
  geom_line() +
  labs(
    x = "Year",
    y = "Total time spent in space to date (hours)"
  ) +
  theme_minimal()

ggsave(graph_file, plot = p, width = 9, height = 5, dpi = 300)
print(p)



---------------------------------------------

eva_tbl <- jsonlite::fromJSON(input_file)|>
  as_tibble()


# j_l <- read_json(input_file)
# data=as.data.frame(j_l[[1]]) # turn the input data into a dataframe
# head(data)

for( i in 2:374){ #use only rows 2-374
  r = j_l[[i]]
    print(r)
    data =merge(data, as.data.frame(r),  all=TRUE)
}
#data.pop(0)
## Comment out this bit if you don't want the spreadsheet
write.csv(output_file) # write the data to an easily readable format



time <- c() # make the "time" variable as an empty list
date = Date() # initiate the date variable as a date type


j=1
for (i in rownames(data)){
    print(data[j, ])
    # and this bit
    # w.writerow(data[j].values())
    if (!is.na(data[j,]$duration)){ # make sure a duration is listed in the table
        duration_str=data[j,]$duration # pull the duration
        if(duration_str == ''){
          #do nothing
        }else{ # confirm the duration is not an empty string
            duration_dt=as.POSIXlt(duration_str,format='%H:%M') # save duration in the format HH:MM
            duration_hours <- as.numeric(as.difftime(hour(duration_dt), units = 'hours')+as.difftime(minute(duration_dt), units='mins')+as.difftime(second(duration_dt), units='secs'))/(60*60)
            print(duration_dt,duration_hours)
            time <- c(time, duration_hours) #record the time and duration for each time step
            if(!is.na(data[j,]$date)){ # confirm there's a date entered
                date= c(date, as.Date(substr(data[j,'date'], 1, 10), format = '%Y-%m-%d')) #reformat the date
                #date.append(data[j]['date'][0:10])

            }else{
              time <- time[1:length(time) -1] # if no date listed, use the last time listed (and don't add a row for this entry)
                }
            }
        }
    j = j+1
}

duration_dt=0
for(i in time)
  duration_dt <- c(duration_dt, duration_dt[length(duration_dt)]+i)


df <- data.frame(
date, time
)[order(date, time), ]

date <- df$date
time <- df$time


# png(graph_file)
# plot(date,duration_dt[2:length(duration_dt)],
# xlab = 'Year', ylab= 'Total time spent in space to date (hours)'
# )
# dev.off()
# plot(date,duration_dt[2:length(duration_dt)],
# xlab = 'Year', ylab= 'Total time spent in space to date (hours)'
# )

# make plot of cumulative spacetime by year
cumulative_time <- duration_dt[2:length(duration_dt)]

cumulative_spacetime_plot <- ggplot(df, aes(x = date, y = cumulative_time)) +  
  geom_point() +
  geom_line() +
  labs(x = "Year", y = "Total time spent in space to date (hours)") +
  theme_minimal()

ggsave(graph_file, plot = cumulative_spacetime_plot, width = 9, height = 5, dpi = 300)

print(cumulative_spacetime_plot)
