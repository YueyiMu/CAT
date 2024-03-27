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

# Filter data:  It creates separate data frames for each filtered variable.
emission_new <- data_old_many %>%
  filter(emissions > data_new_good$emissions)
vmt_new <- data_old_many %>%
  filter(vmt > data_new_good$vmt)
veh_new <- data_old_many %>%
  filter(vehicles > data_new_good$vehicles)
sour_new <- data_old_many %>%
  filter(sourcehours > data_new_good$sourcehours)
starts_new <- data_old_many %>%
  filter(starts > data_new_good$starts)

# Create variables
variables <- c("emissions", "vmt", "vehicles", "sourcehours", "starts")
# Create a vector
result_df <- data.frame(variable = character(), z_value = numeric(), p_value = numeric(), stringsAsFactors = FALSE)

for (variable in variables) {
  
  # Calculate the statistics of the data set
  mean_old <- mean(data_old_many[[variable]])
  sd_old <- sd(data_old_many[[variable]])
  n_old <- length(data_old_many[[variable]])
  mean_new_good <- mean(data_new_good[[variable]])
  n_new_good <- length(data_new_good[[variable]])
  
  # Calculate SE
  se <- sqrt((sd_old^2 / n_old) + (sd_old^2 / n_new_good))
  # Calculate z-value
  z <- (mean_old - mean_new_good) / se
  # Calculate p-value
  p_value <- 2 * pnorm(-abs(z))
  
  # Save results
  result_df <- rbind(result_df, data.frame(variable = variable,p_value = p_value))
}
print(result_df)


# Extract individual p-values
p_values <- result_df$p_value

# Calculate joint p-value using Fisher's method
joint_p_value <- 1 - prod(1 - p_values)

# Output joint p-value
print(paste("Joint p-value:", joint_p_value))
if (joint_p_value < 0.05){
  print("statistically significant")
} else{
  print("Not statistically significant")
}
#there is a meaningful effect or relationship present in the data being analyzed.

#####################################################################################

# Set significance level
alpha <- 0.05

# Create an empty list to store plots
plots <- list()

# Loop through each variable
for (variable in variables) {
  # Extract p-value from result_df
  p_value <- result_df$p_value[result_df$variable == variable]
  
  # Print p_value for debugging
  print(p_value)
  
  # Check if p-value is less than alpha
  if (length(p_value) > 0 && p_value < alpha) {
    # Create a density plot for the variable
    plot <- ggplot(data_old_many, aes_string(x = variable)) +
      geom_density(color = "black") +
      labs(title = paste("Density Plot of", variable),
           x = variable,
           y = "Density") +
      # Add vertical line at mean of 
      geom_vline(xintercept = (data_new_bad[[variable]]), color = "blue", linetype = "dashed") +
      # Add vertical line at mean of 
      geom_vline(xintercept = (data_new_good[[variable]]), color = "red", linetype = "dashed")
    
    # Store the plot in the list
    plots[[variable]] <- plot
  }
}

# Display the plots
for (i in 1:length(plots)) {
  print(plots[[i]])
}
