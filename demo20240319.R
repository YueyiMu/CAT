# Here's a quick demo of the idea I have...

library(dplyr)
library(readr)
library(ggplot2)

path_old = "z/quality_checker/test1/data_old.csv"
path_new_good = "z/quality_checker/test1/data_new_good.csv"
path_new_bad = "z/quality_checker/test1/data_new_bad.csv"

data_old = read_csv(path_old)
data_new_good = read_csv(path_new_good)
data_new_bad = read_csv(path_new_bad)

ggplot() +
  geom_density(data = data_old, 
               mapping = aes(x = emissions, fill = "OLD")) + 
  geom_vline(data = data_old %>% filter(year == 2025), 
             mapping = aes(xintercept = emissions, color = "OLD")) +
  geom_vline(data = data_new_good, 
             mapping = aes(xintercept = emissions, color = "GOOD")) +
  geom_vline(data = data_new_bad, 
             mapping = aes(xintercept = emissions, color = "BAD")) +
  labs(x = "metric (emissions)", y = "Probability Density Function (PDF)",
       fill = "Old Data (All Years)", color = "New 2025 Data Quality") 

# How do we select the good data and discern it from the bad data,
# when it is SO FAR OFF from the rest?

rm(list = ls())

# Some initial ideas....

# CHECKER - CONCEPTUAL VALIDITY #############################

# Conceptual validity - does the data show associations that it should in theory?
# eg. if vehicle miles traveled in the new data for a row is above the mean for vmt,
#     then emissions in the new data for a row should also be above the mean for emissions.

path_old = "z/quality_checker/test1/data_old.csv"
path_new_good = "z/quality_checker/test1/data_new_good.csv"
path_new_bad = "z/quality_checker/test1/data_new_bad.csv"

data_old = read_csv(path_old)
data_new_good = read_csv(path_new_good)
data_new_bad = read_csv(path_new_bad)

data_old
data_new_good
data_new_bad

stat_emissions = data_old %>% 
  summarize(
    mu = mean(emissions), 
    sd = sd(emissions), 
    n = n(), 
    se = sd / sqrt(n)) %>%
  # Add in the new emissions estimate
  mutate(new = data_new_good$emissions) %>%
  # How many se greater than the mean is it?
  mutate(z = (new - mu) / se) %>%
  # Label the variable
  mutate(var = "emissions") 


# repeat for vmt
stat_vmt = data_old %>% 
  summarize(
    mu = mean(vmt), 
    sd = sd(vmt), 
    n = n(), 
    se = sd / sqrt(n)) %>%
  # Add in the new vmt estimate
  mutate(new = data_new_good$vmt) %>%
  # How many se greater than the mean is it?
  mutate(z = (new - mu) / se) %>%
  # Label the variable
  mutate(var = "vmt") 

# Stack them
stat = bind_rows(stat_emissions, stat_vmt) %>%
  # Check if z is 0
  mutate(check = z > 0)

# Are the estimates always all greater than the mean (z > 0)?
stat 

# Cleanup
rm(list = ls())


# CHECKER - RELATED #####################################

# What's the full range of estimates from a large sample of counties? (in this case, all PA counties?)
data_new_good = read_csv("z/quality_checker/test1/data_new_good.csv")
data_new_bad = read_csv("z/quality_checker/test1/data_new_bad.csv")

data_old_many = read_csv("z/quality_checker/test1/data_old_many.csv") %>%
  filter(year == 2025)

ggplot() +
  geom_density(data = data_old_many, mapping = aes(x = emissions)) +
  geom_vline(data = data_new_good, mapping = aes(xintercept = emissions, color = "GOOD")) +
  geom_vline(data = data_new_bad, mapping = aes(xintercept = emissions, color = "BAD"))


# Er.... this doesn't work well either....

rm(list = ls())



# IDEAS ################


# SINGLE PROBABILITY METRIC
# - what's the chance that emissions was extreme?
# -----> BAD

# JOINT PROBABILITY METRIC
# - what's the chance that emissions AND vmt were BOTH extreme?

# COMPARE AGAINST PEERS
# - Find 100 peer counties at the same year
# - how different is your statistic from theirs?

# COMPARE AGAINST PEERS... IN TERMS OF POPULATION, AND ETC.
# - Find 100 peer counties... using a cool technique
# - how different is your statistic from theirs?

# HOW MANY STANDARD ERRORS IS TOO MANY?
# - 3? 6? 1?

# CONCEPTUAL VALIDITY
# - When vmt (miles) increases, does emissions increase too?

# ONLY IF WE HAVE TO:
# Quality check the INPUTS, not the OUTPUTS

data_old_many %>% glimpse()



# What's the full range of estimates from a large sample of counties? (in this case, all PA counties?)
data_new_good = read_csv("z/quality_checker/test1/data_new_good.csv")
data_new_bad = read_csv("z/quality_checker/test1/data_new_bad.csv")

data_old_many = read_csv("z/quality_checker/test1/data_old_many.csv") 


ggplot() +
  geom_density(data = data_old_many, mapping = aes(x = emissions)) +
  geom_vline(data = data_new_good, mapping = aes(xintercept = emissions, color = "GOOD")) +
  geom_vline(data = data_new_bad, mapping = aes(xintercept = emissions, color = "BAD")) +
  scale_x_log10()

ggplot() +
  geom_density(data = data_old_many, mapping = aes(x = vmt)) +
  geom_vline(data = data_new_good, mapping = aes(xintercept = vmt, color = "GOOD")) +
  geom_vline(data = data_new_bad, mapping = aes(xintercept = vmt, color = "BAD")) +
  scale_x_log10()


ggplot() +
  geom_density(data = data_old_many, mapping = aes(x = starts)) +
  geom_vline(data = data_new_good, mapping = aes(xintercept = starts, color = "GOOD")) +
  geom_vline(data = data_new_bad, mapping = aes(xintercept = starts, color = "BAD")) +
  scale_x_log10()




# BUILD 4 TESTS IN ONE TABLE

# make a function?

tests = tibble(
  id = 1,
  t1 = TRUE,
  t2 = FALSE,
  t3 = TRUE,
  t4 = FALSE
)

#?dplyr::case_when()
tests %>% 
  mutate(accept = case_when(
    # Some condition
    t1 == TRUE & t2 == TRUE & t3 == FALSE ~ FALSE,

    TRUE ~ TRUE
  ))
#HOW MANY STANDARD ERRORS IS TOO MANY?

calculate_se_difference <- function(data_old, data_new, variable) {
  mu_old <- mean(data_old[[variable]], na.rm = TRUE)
  sd_old <- sd(data_old[[variable]], na.rm = TRUE)
  n_old <- nrow(data_old)
  se_old <- sd_old / sqrt(n_old)
  mu_new <- mean(data_new[[variable]], na.rm = TRUE)
  se_diff <- abs(mu_new - mu_old) / se_old
  return(se_diff)
}


se_diff_emissions = calculate_se_difference(data_old_many, data_new_good, "emissions")
se_diff_vmt = calculate_se_difference(data_old_many, data_new_good, "vmt")

threshold_2se = 2
threshold_3se = 3
threshold_4se = 4
threshold_5se = 5
threshold_6se = 6


exceeds_2se_emissions = se_diff_emissions > threshold_2se
exceeds_3se_emissions = se_diff_emissions > threshold_3se
exceeds_4se_emissions = se_diff_emissions > threshold_4se
exceeds_5se_emissions = se_diff_emissions > threshold_5se
exceeds_6se_emissions = se_diff_emissions > threshold_6se
exceeds_2se_vmt = se_diff_vmt > threshold_2se
exceeds_3se_vmt = se_diff_vmt > threshold_3se
exceeds_4se_vmt = se_diff_vmt > threshold_4se
exceeds_5se_vmt = se_diff_vmt > threshold_5se
exceeds_6se_vmt = se_diff_vmt > threshold_6se

data_quality_check <- tibble(
  id = 1,
  exceeds_2se_emissions = exceeds_2se_emissions,
  exceeds_3se_emissions = exceeds_3se_emissions,
  exceeds_4se_emissions = exceeds_4se_emissions,
  exceeds_5se_emissions = exceeds_5se_emissions,
  exceeds_6se_emissions = exceeds_6se_emissions,
  exceeds_2se_vmt = exceeds_2se_vmt,
  exceeds_3se_vmt = exceeds_3se_vmt,
  exceeds_4se_vmt = exceeds_4se_vmt,
  exceeds_5se_vmt = exceeds_5se_vmt,
  exceeds_6se_vmt = exceeds_6se_vmt
) %>%
  mutate(
    quality = case_when(
      exceeds_6se_emissions | exceeds_6se_vmt ~ "Very Bad - Highly Extreme Values",
      exceeds_5se_emissions | exceeds_5se_vmt ~ "Bad - Very Extreme Values",
      exceeds_4se_emissions | exceeds_4se_vmt ~ "Questionable - Moderate Extreme Values",
      exceeds_3se_emissions | exceeds_3se_vmt ~ "Potentially Problematic - Mild Extreme Values",
      TRUE ~ "Good" # If none of the above conditions are met, the data is considered Good
    )
  )


print(data_quality_check)
glimpse(data_quality_check)
      
      
      
# Calculate Pearson correlation coefficient
correlation_coefficient <- cor(data_old_many$emissions, data_old_many$vmt, use = "complete.obs")

# Check if the correlation coefficient is NA
if (is.na(correlation_coefficient)) {
  message("Correlation coefficient cannot be calculated; check data for completeness and variability.")
} else {
  # Print the correlation coefficient
  print(correlation_coefficient)
  
  # Assess conceptual validity based on correlation
  if (correlation_coefficient > 0.5) {
    message("Positive association between VMT and emissions suggests good conceptual validity.")
  } else if (correlation_coefficient <= 0.5 & correlation_coefficient >= 0) {
    message("Weak or no positive association between VMT and emissions; review data for conceptual validity.")
  } else {
    message("Negative association between VMT and emissions; conceptual validity is questionable.")
  }
}
