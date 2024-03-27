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

