#' @name fake_database.R
#' @author Tim Fraser

# Let's make a fake database!

source("R/connect.R")
conn = connect("mysql", "greentechdb")

conn %>% dbListTables()

conn %>% tbl("policies")


# Add a 'policies' table

policies = tribble(
  ~policy_id, ~cmaq_category, ~type,                  ~level,
  1,          "Ride Sharing",  "activity",            "county",
  2,          "Alternative Fuels and Vehicles", "ef", "county",
  3,  "Congestion Reduction and Traffic Flow Improvements", "ef", "county",
)

dbWriteTable(
  conn = conn,
  name = "policies", 
  value = policies, 
  fieldtypes = c("policy_id" = "INT(4)", "cmaq_category" = "CHAR(50)", "type" = "CHAR(8)", "level" = "CHAR(6)"),
  row.names = FALSE,
  overwrite = TRUE
)

# Add an 'varsperpolicy' table
varsperpolicy = tribble(
  ~policy_id, ~var,     ~type,
  1,          "vmt",   "activity",
  2,          "ef",    "ef",
  3,          "ef",    "ef",
  4,          "vmt",   "activity",
  4,          "ef",    "ef"
) %>%
  mutate(policy_id = as.integer(policy_id))

dbWriteTable(
  conn = conn,
  name = "varsperpolicy", 
  value = varsperpolicy, 
  fieldtypes = c("policy_id" = "INT(4)", "var" = "CHAR(11)", "type" = "CHAR(8)"),
  row.names = FALSE,
  overwrite = TRUE,
  append = FALSE
)

# Add an effects table
effects = tribble(
  ~policy_id, ~var, ~stat,    ~se,
  1,         "vmt", 0.000107, 0.000021,
  2,         "ef",  0.895,    0.472,
  3,         "ef",  27.4,     23.3,
  4,         "vmt", 0.0005,   0.000011,
  4,         "ef", 10.3,   5.2
) %>%
  mutate(policy_id = as.integer(policy_id))

dbWriteTable(
  conn = conn,
  name = "effects", 
  value = effects, 
  fieldtypes = c("policy_id" = "INT(4)", "var" = "CHAR(11)", "stat" = "DOUBLE(15)", se = "DOUBLE(15)"),
  row.names = FALSE,
  overwrite = TRUE
)

conn %>% tbl("effects")

dbDisconnect(conn)

rm(list = ls())

###################################
library(stats)
library(dplyr)
library(readr)
library(ggplot2)

path_old <- "z/quality_checker/test1/data_old.csv"
path_new_good <- "z/quality_checker/test1/data_new_good.csv"
path_new_bad <- "z/quality_checker/test1/data_new_bad.csv"

data_old_many <- read_csv("z/quality_checker/test1/data_old_many.csv")
data_old <- read_csv(path_old)
data_new_good <- read_csv(path_new_good)
data_new_bad <- read_csv(path_new_bad)

# Create a list of variables to filter
variables <- c("emissions", "vmt", "vehicles", "sourcehours", "starts")
variance_values <- c(2, 1.5, 1, 0.5)

# Create an empty list to store filtered datasets
filtered_datasets <- list()

# Loop over each variance value
for (variance_val in variance_values) {
  # Create a temporary dataframe to store filtered data
  filtered_data <- data_old_many
  
  # Loop over each variable
  for (variable in variables) {
    
    # Calculate mean and standard deviation
    mean_val <- mean(data_old_many[[variable]])
    sd_val <- sd(data_old_many[[variable]])
    
    # Filter the data based on the mean, standard deviation, and current variance value for each variable
    filtered_data <- filtered_data %>%
      filter({{variable}} < (mean_val - variance_val * sd_val) | {{variable}} > (mean_val + variance_val * sd_val))
  }
  
  # Store the filtered dataset in the list
  filtered_datasets[[paste0("data_bad_", variance_val)]] <- filtered_data
  
  # Save the filtered data to the environment
  assign(paste0("data_bad_", variance_val), filtered_data, envir = .GlobalEnv)
}

# Print the filtered datasets
print(filtered_datasets)




