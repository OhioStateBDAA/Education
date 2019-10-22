# R BASICS

# this is a comment

# variable assignments
min_var = 0
max_var <- 500

# run code by putting your cursor on the line
# then hit run (or Ctr/Cmd + Enter)

# Functions are called with a name followed by a set of parenthesis
# Function arguments are inside the parenthesis
# this prints ‘0’
print(min_var)

# this prints a string of characters announcing yourself to the world
print("Hello World")




# Import tidyverse
library(tidyverse)

# Importing the Data
# Data URL: https://bdaa-workshops.s3.us-east-2.amazonaws.com/AB_NYC_2019.csv
AB_NYC = read_csv("https://bdaa-workshops.s3.us-east-2.amazonaws.com/AB_NYC_2019.csv")


# ------
# ggplot
ggplot(data = AB_NYC) # creates a blank plot

# one variable plots
# density of price
ggplot(data = AB_NYC) + geom_density(mapping = aes(x=price))
# density of price, with x axis limited to $2,000
ggplot(data = AB_NYC) + 
  geom_density(mapping = aes(x=price)) + 
  xlim(0,2000)
# density of price, with x axis limited to $500, and colored by neighboorhood_group
# can try 'fill' instead of 'color'
ggplot(data = AB_NYC) +
  geom_density(mapping = aes(x=price, color=neighbourhood_group)) + 
  xlim(0,500)

# bar plot to see count of rentals by room_type
ggplot(data = AB_NYC) + 
  geom_bar(mapping = aes(x=room_type))
# bar plot to see count of rentals by neighbourhood_group, with room_type as a fill
ggplot(data = AB_NYC) + 
  geom_bar(mapping = aes(x = neighbourhood_group, fill = room_type)) 

# simplifying things
base = ggplot(data = AB_NYC)


# 2 variable plots
# boxplot to see price by neighbourhood_group
base + 
  geom_boxplot(mapping = aes(x = neighbourhood_group, y = price)) + 
  ylim(0, 500)

# boxplot to see price by neighbourhood_group and room_type (via color or fill)
base + 
  geom_boxplot(mapping = aes(x = neighbourhood_group, y = price, fill = room_type)) + 
  ylim(0, 500)

# 'mapping' the neighbourhoods 
# you can set aesthetic values in the ggplot function ("globally") or in each layer
ggplot(data = AB_NYC, mapping = aes(x = longitude, y = latitude)) + 
  geom_point(aes(color = neighbourhood_group)) + 
  coord_quickmap()

# scatterplot of number_of_reviews and price
ggplot(data = AB_NYC) +
  geom_point(mapping = aes(x = price, y = number_of_reviews, color = room_type))




# ------------
# Adding dplyr into the mix
# filtering data
manhattan_data = 
  AB_NYC %>% 
  filter(neighbourhood_group == "Manhattan", room_type == "Private room")

# scatterplot of number_of_reviews and price
# looking only at Private Rooms in Manhattan
# add availability_365 as a color
ggplot(data = manhattan_data) +
  geom_point(mapping = aes(x = price, y = number_of_reviews, color = availability_365))

# adding a column with mutate()
# let's calculate the length of the listing's title
# on the manhattan_data 
manhattan_data = 
  manhattan_data %>% 
  mutate(title_length = str_length(name))

# generating summary statistics with group_by() and summarise()
# group_by() the calculated_host_listings_count 
# to see differences in listings based on how many other listings the host has
# things to 'summarise' for each calculated_host_listings_count:
#   - number of listings
#   - mean price 
#   - mean number of reviews 
#   - mean availability_365 
summary = 
  AB_NYC %>% 
  group_by(calculated_host_listings_count) %>% 
  summarise(
    num_listings = n(),
    mean_price = mean(price),
    mean_num_reviews = mean(number_of_reviews),
    mean_availability_365 = mean(availability_365)
  )

# calculate the number of hosts at each calculated_host_listings_count
# and remove rows where there are 2 or less hosts that host that many listings
# for the summary dataframe
summary = 
  summary %>% 
  mutate(num_hosts = num_listings / calculated_host_listings_count) %>% 
  filter(num_hosts > 2)

# let's see if hosts that host more properties have 
# more/less availability on avg. than hosts that only have a few listings
# scatter plot (geom_smooth()) using summary dataframe
# calculated_host_listings_count compared to mean_availability_365
# size as num_listings
# addding a "smoother" as an additional layer
final_graph = 
  ggplot(summary, aes(x = calculated_host_listings_count, y = mean_availability_365)) + 
  geom_point(aes(size = num_listings)) +
  geom_smooth()
final_graph

# finishing touches
# xlab, ylab, ggtitle
final_graph = final_graph +
  xlab("Number of Host's Listings") +
  ylab("Mean Days Available in the Year") +
  ggtitle("Listing Availablility by Number of Host's Listings")
final_graph
  

