#### Bachelor Thesis ####


# Loading packages
library(tidyverse)
library(vdemdata)



###### V-DEM DATASET with WC indicator ######
#############################################

# Saving V-dem dataset
vdem -> vdem_data

# Selecting variables and filtering for observations newer than 1950. 
vdem_data %>% select(country_name, country_text_id, country_id,
                     year, v2elembaut, v2psoppaut, v2psbars,
                     v2x_ex_hereditary, v2x_ex_military,
                     v2x_ex_party) %>% filter(year >= 1950) -> Vdem_data_2


view(Vdem_data_2)

# Checking for missing values in specific variables
sum(is.na(Vdem_data_2$v2elembaut)) # 55 NAs
sum(is.na(Vdem_data_2$v2psoppaut)) # 404 NAs 
sum(is.na(Vdem_data_2$v2psbars)) # 0 NAs 


# Coding the missing estimates for non-electoral polities as if they were #
# in the 1st percentile in the distribution on each of these variables. #


# Calculate the 1st percentile for v2elembaut
first_percentile_v2elembaut <- quantile(Vdem_data_2$v2elembaut, 0.01, na.rm = TRUE)

# Calculate the 1st percentile for v2psoppaut
first_percentile_v2psoppaut <- quantile(Vdem_data_2$v2psoppaut, 0.01, na.rm = TRUE)

# Replace NA values with the 1st percentile in v2elembaut
Vdem_data_2$v2elembaut[is.na(Vdem_data_2$v2elembaut)] <- first_percentile_v2elembaut

# Replace NA values with the 1st percentile in v2psoppaut
Vdem_data_2$v2psoppaut[is.na(Vdem_data_2$v2psoppaut)] <- first_percentile_v2psoppaut

## Checking if it worked. (it did)
sum(is.na(Vdem_data_2$v2elembaut)) # 0 NAs
sum(is.na(Vdem_data_2$v2psoppaut)) # 0 NAs 
sum(is.na(Vdem_data_2$v2psbars)) # 0 NAs 



##########
# Constructing our final component to the W from the variables:
# v2x_ex_hereditary, v2x_ex_military, and v2x_ex_party.

# Taking the maximum value for each country_year from among hereditary, military,
# and party and reversing its direction


Vdem_data_2 %>%
  mutate(
    # Reverse the values of the control variables
    hereditary_reversed = 1 - v2x_ex_hereditary,
    military_reversed = 1 - v2x_ex_military,
    party_reversed = 1 - v2x_ex_party
  ) -> Vdem_data_2_updated

# View the updated dataset to check the new columns. It checks out!!!!
view(Vdem_data_2_updated)

#### Combining hereditary, military and party into one component

Vdem_data_2_updated %>%
  mutate(
    # Summing reversed values into a single component
    combined_component = hereditary_reversed + military_reversed + party_reversed
  ) -> Vdem_data_3

# View the dataset to check the new combined component
view(Vdem_data_3)


## Standardizing all 4 components to range from 0 to 1. 
# combined_component, v2elembaut, v2psoppaut, v2psbars. 

Vdem_data_3 %>%
  mutate(
    # Standardizing variables to range from 0 to 1
    combined_component = rescale(combined_component, to = c(0, 1)),
    v2elembaut = rescale(v2elembaut, to = c(0, 1)),
    v2psoppaut = rescale(v2psoppaut, to = c(0, 1)),
    v2psbars = rescale(v2psbars, to = c(0, 1))
  ) -> Vdem_data_4

# View the standardized dataset to check the new scaled variables
view(Vdem_data_4)

###### Winning coalition size is constructed by summing the 4 standardized parts
# and then dividing by 4 to take their average standardized score. 

# Summing the 4 parts and dividing by 4.
Vdem_data_4 %>%
  mutate(
    # Calculating the average of the four standardized variables
    W = (combined_component + v2elembaut + v2psoppaut + v2psbars) / 4
  ) -> Vdem_data_W

# View the final dataset to check the new average component
view(Vdem_data_W)

# Cleaning up dataset to only include relevant variables
Vdem_data_W %>% 
  select(country_name, country_text_id, country_id, year, W) -> Winning_coalition

# View dataset with Winning coalition variable
view(Winning_coalition)

##### Saving dataset as a .csv file. 
write.csv(Winning_coalition, "winning_coalition.csv", row.names = FALSE)

# importing winning coalition dataset
WC <- read_csv("winning_coalition.csv")


### Distribution of Winning Coalition size. 1950-2022. 
WC %>%
  ggplot(aes(x = W)) + 
  geom_histogram(binwidth = 0.01, color = "black", fill = "gray") +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1), limits = c(0, 1)) +
  labs(title = "Distribution of Winning Coalition Size, 1950-2022",
       x = "Winning Coalition Size",
       y = "Frequency") +
  theme_linedraw()




