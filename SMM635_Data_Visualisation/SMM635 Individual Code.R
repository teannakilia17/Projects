## SMM635 DATA VISUALISATION PROJECT
# Load libraries
library(ggplot2)
library(tidyverse)
library(dplyr)

# Load the data----------------------------------------------------------------
reviews <-read.csv("platform_economy_reviews.csv")
mobility <-read.csv("platform_economy_mobility.csv")

# Inspect the Data
sum(is.na(reviews) == TRUE)
sum(is.na(mobility) == TRUE)

# Visualise the Data
head(reviews)
head(mobility)

reviews <- reviews %>%
  rename(restaurant = id)

# Code Shared in R
ds <- reviews %>%
  group_by(time, location) %>%
  summarise(counts = n(), .groups = 'drop')

ds$time <- as.Date(paste0(ds$time, "-01"), format = "%Y-%m-%d")

ggplot(ds, aes(x = time, y = counts, color = location)) +
  geom_line() +  # Line plot
  scale_color_manual(values = c("austin" = "red", "dallas" = "blue")) +  # Custom colors
  labs(
    title = "Counts of Yelp Reviews Over Time",
    y = "Counts of Yelp Reviews",
    color = "Location"
  ) +
  geom_vline(xintercept = as.Date("2016-05-01"), color = "purple", linetype = "dashed") +
  annotate("text", x = as.Date("2016-02-01"), y = 0, label = "Uber & Lyft\nexit Austin", 
           color = "purple", angle = 90, hjust = 0, size =3) +
  geom_vline(xintercept = as.Date("2017-02-01"), color = "green", linetype = "dashed") +
  annotate("text", x = as.Date("2016-11-01"), y = 0, label = "Uber & Lyft\nre-entry Austin", 
           color = "green", angle = 90, hjust = 0, size = 3) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +  # Rotate x-axis labels
  scale_x_date(date_labels = "%Y-%m", breaks = "6 month") +  # Show month-year labels
  guides(color = guide_legend(title = "City"))


# Graph 2 Cumulative Employee Quitting ----------------------------------------

#change "True" and "False into binary data
mobility$quit <- ifelse(mobility$quit == "True", 1, 0)

#create dataframe
data2 <- mobility %>%
  mutate(time = as.Date(paste0(time, "-01"), format = "%Y-%m-%d")) %>%  # Ensure time is a Date
  arrange(time) %>%  # Arrange by time
  group_by(dma) %>% 
  mutate(quit = cumsum(quit)) %>%  
  select(restaurant_cat, dma, time, quit)

#set factors of restaurant categories for facet wrap to use as levels
data2$restaurant_cat = factor(data2$restaurant_cat, levels=c("Quick Service","Fast Casual","Casual Dining","Upscale Casual","Fine Dining"))

ggplot(data2, aes(x=time, y= quit, color = dma))+
  geom_line() +  # Line plot
  scale_color_manual(values = c("AUSTIN" = "red", "DALLAS" = "blue")) +  
  labs(
    title = "Cumulative Employee's Quitting over Time",
    x = "Time",
    y = "Cumulative Quits",
    color = "Location" 
  )+
  facet_grid(rows =vars(restaurant_cat)) +
  # add intercepts
  geom_vline(xintercept = as.Date("2016-05-01"), color = "purple", linetype = "dashed") +
  annotate("text", x = as.Date("2016-03-01"), y = 45, label = "Uber & Lyft\nexit Austin", 
           color = "purple", angle = 90, hjust = 0, size =3) +
  geom_vline(xintercept = as.Date("2017-02-01"), color = "green", linetype = "dashed") +
  annotate("text", x = as.Date("2016-12-01"), y = 45, label = "Uber & Lyft\nre-entry Austin", 
           color = "green", angle = 90, hjust = 0, size = 3) + 
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5, size = 14))+
  geom_smooth(aes(group=dma), method = "lm", se = FALSE, linetype = "dashed", linewidth =0.8, color="grey50")


## Finds the slope values of regression lines
regressionresults <- data2 %>%
  group_by(dma, restaurant_cat) %>%
  summarise(
    intercept = coef(lm(quit ~ as.numeric(time)))[1],
    slope = coef(lm(quit ~ as.numeric(time)))[2]
  )
print(regressionresults)

# Average of Bad Food & Service Reviews ---------------------------------------------------------------
# Create a dataframe that compares the different period, 
data4 <- reviews %>%
  mutate(time = format(as.Date(paste0(time, "-01"), format = "%Y-%m-%d"))) %>%  
  filter(time >= as.Date("2015-05-01") & time < as.Date("2018-05-01")) %>% # filter the dates to only look at 3 year period
  group_by(time, location) %>%
  summarise(
    avg_bad_service = mean(bad_service, na.rm = TRUE)*100,  # convert values to a percentage and find the mean
    avg_bad_food = mean(bad_food, na.rm = TRUE)*100        
  ) %>% 
  mutate(
    period = case_when(
      time >= as.Date("2015-05-01") & time < as.Date("2016-05-01") ~ "Before Exit",  # 1 year before exit
      time >= as.Date("2016-05-01") & time <= as.Date("2017-05-01") ~ "During Absence", # 1 year during exit
      time > as.Date("2017-05-01") & time <= as.Date("2018-05-01") ~ "After Re-entry", # 1 year after re-entry
    )
) 

data5 <- data4 %>% 
  group_by(period, location) %>%
  summarise(
    mean_bad_service = mean(avg_bad_service, na.rm = TRUE),  #takes the average per time period
    mean_bad_food = mean(avg_bad_food, na.rm = TRUE)
  ) %>%
  # allocated proxy dates to arrange the points along the x-axis
  mutate(
    time = case_when(
      period == "Before Exit" ~ as.Date("2016-04-01"),
      period == "During Absence" ~ as.Date("2016-11-01"),
      period == "After Re-entry" ~ as.Date("2017-06-01")
    )
  ) %>%
  arrange(time)
  
ggplot(data5, aes(x = as.Date(time))) +
  geom_point(aes(y = mean_bad_service, color = "Bad Service"), size = 3) +  
  geom_point(aes(y = mean_bad_food, color = "Bad Food"), size = 3) +
  #lines to join the average points - better shows the relationship between points. 
  geom_line(aes(y = mean_bad_service, color = "Bad Service")) +  
  geom_line(aes(y = mean_bad_food, color = "Bad Food")) +
  scale_color_manual(values = c("Bad Food" = "forestgreen", "Bad Service" = "firebrick")) +
  #add in the intercepts
  geom_vline(xintercept = as.Date("2016-05-01"), color = "purple", linetype = "dashed") +
  annotate("text", x = as.Date("2016-04-16"), y=17, angle=90, na.rm = TRUE, 
           label = "Uber & Lyft Exit\nexit Austin", color = "purple", hjust = 0, size =3) +
  geom_vline(xintercept = as.Date("2017-05-01"), color = "green", linetype = "dashed") +
  annotate("text", x = as.Date("2017-04-16"), y = 17,angle=90, na.rm = TRUE, 
           label = "Uber & Lyft Re-entry\nexit Austin", color = "green", hjust = 0, size=3) +
  facet_grid(rows = vars(location))+
  labs(
    title = "Trends in % of Bad Yelp Reviews Over Time",
    x = "Time",
    y = "Average Negative Yelp Reviews (%)",
    colour = "Legend"
  ) +
  scale_x_date(date_labels = "%Y-%m", breaks = "2 months") +
  ylim(min(data5$mean_bad_service,data5$mean_bad_food),max(data5$mean_bad_service,data5$mean_bad_food))+
  theme_minimal() +
  theme(
    strip.background = element_rect(fill = "lightblue", color = "black"),
    axis.text.x = element_text(angle = 90, hjust = 1),
    strip.text = element_text(size = 10, face = "bold"),
    legend.position = "top",
    plot.title = element_text(hjust = 0.5, size = 14)  
  )