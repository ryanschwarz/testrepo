# Logs grouped by User_ID
# min.max of timestamp sum of timespent count of activity (act id uniques)

# Libraries ----
library(dplyr)

# Pull Data/File Location/Fix Data Types ----
logs <- read.csv("C:/Users/Vretta-Silver/Desktop/mtic2_data/BiWeekly Data Release June 23/01 Data/01 log-data/00 Student_Logs.csv",stringsAsFactors = F)
URL <- "C:/Users/Vretta-Silver/Desktop/mtic2_data/BiWeekly Data Release/01 Activity_Sessions.csv"
logs$time.stamp <- as.numeric(logs$time.stamp)
logs$activity.id <- as.numeric(logs$activity.id)
logs$time.before_next <- as.integer(logs$time.before_next)
logs$user.id <- as.numeric(logs$user.id)

# Functions ----
make.string_list <- function(...) paste0(unique(...),collapse = ',') #Used in compute

# Main function to compute relevant activity sessions stats ----
compute_learning_sessions <- function(logs){
  Activity_Sessions <- logs %>%
    filter(!is.na(activity.session.id)) %>% 
    filter(time.before_next >0) %>%
    group_by(user.id) %>% 
    summarise(activity.time.start = min(time.stamp),
              activity.time.end = max(time.stamp),
              activity.time.total = sum(time.before_next),
              count.activity = n_distinct(activity.id))
}
actses <- compute_learning_sessions(logs)

# Write to csv
write.table(actses,
            sep = ",",
            row.names = FALSE,
            file = URL)


