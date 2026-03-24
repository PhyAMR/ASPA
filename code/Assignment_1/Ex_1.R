
lake_names <- c("Loch Ness", "Loch Lomond", "Loch Morar", "Loch Tay", 
                "Loch Awe", "Loch Maree", "Loch Ericht", "Loch Lochy", 
                "Loch Rannoch", "Loch Shiel", "Loch Katrine", "Loch Arkaig", 
                "Loch Shin")
volume <- c(7.45, 2.6, 2.3, 1.6, 1.2, 1.09, 1.08, 1.07, 0.97, 0.79, 0.77, 0.75, 0.35) 

# Area in km^2
area <- c(56, 71, 27, 26.4, 39, 28.6, 18.6, 16, 19, 19.5, 12.4, 16, 22.5) 

# Length in km
length_km <- c(39, 36, 18.8, 23, 41, 20, 23, 16, 15.7, 28, 12.9, 19.3, 27.8) 

# Maximum depth in m
max_depth <- c(230, 190, 310, 150, 94, 114, 156, 162, 134, 128, 151, 109, 49) 

# Mean depth in m
mean_depth <- c(132, 37, 87, 60.6, 32, 38, 57.6, 70, 51, 40, 43.4, 46.5, 15.5) 


# Create a data frame
scottish.lakes <- data.frame(Name = lake_names, Volume_km3 = volume, Area_km2 = area, 
                         Length_km = length_km, Max_Depth_m = max_depth, Mean_Depth_m = mean_depth)
str(scottish.lakes)

max_volume <- scottish.lakes$Volume_km3[which.max(scottish.lakes$Volume_km3)]
max_volume_lake <- scottish.lakes$Name[which.max(scottish.lakes$Volume_km3)]
cat("The lake with the largest volume is", max_volume_lake, "with a volume of", max_volume, "km^3.\n")
min_volume <- scottish.lakes$Volume_km3[which.min(scottish.lakes$Volume_km3)]
min_volume_lake <- scottish.lakes$Name[which.min(scottish.lakes$Volume_km3)]
cat("The lake with the smallest volume is", min_volume_lake, "with a volume of", min_volume, "km^3.\n")


max_area <- scottish.lakes$Area_km2[which.max(scottish.lakes$Area_km2)]
max_area_lake <- scottish.lakes$Name[which.max(scottish.lakes$Area_km2)]
cat("The lake with the largest area is", max_area_lake, "with an area of", max_area, "km^2.\n")
min_area <- scottish.lakes$Area_km2[which.min(scottish.lakes$Area_km2)]
min_area_lake <- scottish.lakes$Name[which.min(scottish.lakes$Area_km2)]
cat("The lake with the smallest area is", min_area_lake, "with an area of", min_area, "km^2.\n")


scottish.lakes_ord <- scottish.lakes[order(scottish.lakes$Area_km2), ]

largest_two <- tail(scottish.lakes_ord, 2)[, c("Name", "Area_km2")]
cat("The two lakes with the largest area are: \n")
largest_two

tot_area <- sum(scottish.lakes_ord$Area_km2)
cat("The total area of the ", length(scottish.lakes$Name)," lakes is", tot_area, "km^2).\n")


# Exercise 2: 100 m Atheltics Records 

library(rvest)
library(tidyverse)
library(ggplot2)
# For the men's 100m records, we can scrape the data from the All-Time Athletics website.
men100m_html <- read_html("https://www.alltime-athletics.com/m_100ok.htm")
men100m_html |> html_nodes(xpath = "//pre") |> html_text() -> men100m_list
men100m_tbl <- read_fwf(men100m_list)
str(men100m_tbl)

men100m_tbl <- men100m_tbl %>%
  mutate(across(c(X2, X3,X7), as.numeric),X9 = as.Date(X9, format = "%d.%m.%Y"), X6 = as.Date(X6, format = "%d.%m.%Y")) 
men100m_tbl <- men100m_tbl %>% 
  rename(Rank = X1, Time = X2, Wind = X3, Athlete = X4, Country = X5, Venue = X7, Date = X6, Notes = X8, DOB = X9)

str(men100m_tbl)

# Find the best time for each country each year
fastest_per_country <- men100m_tbl %>%
  mutate(Year = year(Date)) %>%
  group_by(Year) %>%
  summarise(Best_Time = min(Time), .groups = 'drop')

# Plot as a line chart
ggplot(fastest_per_country, aes(x = Year, y = order(Best_Time))) + #, color = Country
  geom_line(size = 1) +
  geom_point() +
  scale_y_reverse() +
  theme_bw() +
  labs(title = "Fastest 100m Runner per Country Over Time for men",
       subtitle = "Tracking the evolution of top performances",
       y = "Best Time (s)")

men100m_tbl <- men100m_tbl %>%
  mutate(Year = year(Date), Decade_Label = paste0(floor(Year / 10) * 10, "s")) 

ggplot(men100m_tbl, aes(x = fct_infreq(Country), fill = as.factor(Decade_Label))) +
  geom_bar() +
  coord_flip() +
  theme_minimal() +
  labs(title = "Top 100m Times by Country and Decade for men",
       x = "Country",
       y = "Number of Records",
       fill = "Decade")

# No we do the same for the women's 100m records.
women100m_html <- read_html("https://www.alltime-athletics.com/w_100ok.htm")
women100m_html |> html_nodes(xpath = "//pre") |> html_text() -> women100m_list
women100m_tbl <- read_fwf(women100m_list)
str(women100m_tbl)


women100m_tbl <- women100m_tbl %>%
  mutate(across(c(X2, X3,X7), as.numeric),X9 = as.Date(X9, format = "%d.%m.%Y"), X6 = as.Date(X6, format = "%d.%m.%Y")) 
women100m_tbl <- women100m_tbl %>% 
  rename(Rank = X1, Time = X2, Wind = X3, Athlete = X4, Country = X5, Venue = X7, Date = X6, Notes = X8, DOB = X9)

str(women100m_tbl)

# Find the best time for each country each year
fastest_per_country <- women100m_tbl %>%
  mutate(Year = year(Date)) %>%
  group_by(Year) %>%
  summarise(Best_Time = min(Time), .groups = 'drop')

# Plot as a line chart
ggplot(fastest_per_country, aes(x = Year, y = order(Best_Time))) + #, color = Country
  geom_line(size = 1) +
  geom_point() +
  scale_y_reverse() +
  theme_bw() +
  labs(title = "Fastest 100m Runner per Country Over Time for women",
       subtitle = "Tracking the evolution of top performances",
       y = "Best Time (s)")

women100m_tbl <- women100m_tbl %>%
  mutate(Year = year(Date), Decade_Label = paste0(floor(Year / 10) * 10, "s")) 

ggplot(women100m_tbl, aes(x = fct_infreq(Country), fill = as.factor(Decade_Label))) +
  geom_bar() +
  coord_flip() +
  theme_minimal() +
  labs(title = "Top 100m Times by Country and Decade for women",
       x = "Country",
       y = "Number of Records",
       fill = "Decade")

