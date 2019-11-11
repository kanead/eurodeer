#'##########################################################################
#' ame1 project - studying the relation between home range size and activity
#' 2019/11/04
#' Adam Kane, Enrico Pirotta & Barry McMahon
#' https://mecoco.github.io/ame1.html
#' merging data
############################################################################

#' make it tidy
summary_data_tidy <- summary_data %>% gather(key = 'statistic', value = 'value', ndvi_Mean:corine_Mode)

#' merge with home range data
hr <- read_csv("results/home_range_month.csv" , col_names = TRUE)
head(hr)

hr_env_data <- merge(hr,summary_data,c("identifier"))

#' merge with activity data
activity <- read_csv("results/Activity_data_byMonth.csv" , col_names = TRUE)

#' make an identifier so it matches the other output
#' add a leading zero 
activity$month <- sprintf("%02d",activity$month) # fix to 2 characters 
activity$yr_month <- paste(activity$year, activity$month, sep = "/")
activity$identifier <- paste(activity$animals_id, activity$yr_month, sep = "_")
activity <- activity %>% select(-yr_month)
head(activity)

#' we want to merge this with hr_env_data
head(hr_env_data)
all_data <- merge(hr_env_data,activity,c("identifier")) %>% select(-id.x, -id.y)
head(all_data)

#' make it tidy
all_data_tidy1 <- all_data %>% gather(key = 'accelerometry', value = 'measure', mean_act:min_act)

#' make it tidier
all_data_tidy2 <- all_data_tidy1 %>% gather(key = 'home_range', value = 'area', kdearea:mcp)

#' go from long format to wide
data_wide <- spread(all_data_tidy2, statistic, value)
data_wide

data_wider <- spread(data_wide, statistic, value)
data_wide

#' export the results
write.csv(summary_data, "results/env_data.csv", row.names = F)
write.csv(summary_data_tidy, "results/env_data_tidy.csv", row.names = F)
write.csv(hr_env_data, "results/hr_env_data.csv", row.names = F)
write.csv(activity, "results/activity.csv", row.names = F)
write.csv(all_data, "results/combined_data.csv", row.names = F)
