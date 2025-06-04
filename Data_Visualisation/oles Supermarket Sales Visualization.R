#install packages
install.packages("reshape2")
install.packages("ggplot2")
install.packages("dplyr")

#load library
library(ggplot2)
library(readr)
library(tidyverse)  

#load datasets
datasetA <- read.csv("ColesSalesData.csv")
datasetB <- read.csv("ColesStoreData.csv")

#renaming columns
colnames(datasetA)[colnames(datasetA) == "Coles_StoreIDNo"] ="Coles_StoreID"

#merging datasets
data <- merge(datasetA,datasetB,by= c("X","Coles_StoreID"))
summary(data)

#viewing the dataset
data
ncol(data) #11 columns
nrow(data) #682 observations

#cleaning the dataset
data1 <- na.omit(data)
#611 observations remaining
sum(is.na(data1)==TRUE) #check for any remaining na

write.csv(data1, "data1.csv")
#Plot 1------------------------------------------------------------------------------------------------------------------------------
# Transforming the data into long format
# Choosing numerical data to explore

kt_pivot <- data1 %>%
  select(Expec_Revenue, Gross_Sale, Sales_Cost, Customer_Count, Staff_Count) %>%
  pivot_longer(cols = everything(), names_to = "variable")

# Adding a new column to categorise variables into two groups based on the y-value range
kt_pivot <- kt_pivot %>%
  mutate(group = ifelse(variable %in% c("Customer_Count"), "Customer_Count", "Other Variables"))

# Plot boxplots using defined grouping to create a split graph
ggplot(kt_pivot, aes(x = variable, y = value)) +
  geom_boxplot(outlier.color = "red", shape = 20) +
  facet_wrap(~ group, scales = "free_y") +
  labs(x = "Variables", y = "Values", title = "Boxplot of Numerical Variables") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Plot 2------------------------------------------------------------------------------------------------------------------------------
#log exp rev and log sales 
Log_Rev = log(data1['Expec_Revenue'])
colnames(Log_Rev) <- "Log_Rev"
Log_G_Sales = log(data1['Gross_Sale'])
colnames(Log_G_Sales) <- "Log_G_Sales"
#add these to our data 
data2 = bind_cols(data1, Log_Rev)
data3 = bind_cols(data2, Log_G_Sales)

ggplot(data3, aes(x = Store_Area, y = Log_G_Sales, color = Coles_Forecast)) +
  geom_point() +
  geom_smooth(aes(x = Store_Area, y = Log_G_Sales), method = "lm", color = "black", se = FALSE) +
  labs(title = "Gross Sales by Store Size",
       x = "Store Size (sqft)",
       y = "Log Gross Sales",
       color = "Coles Forecast") +
  scale_color_manual(values = c("On Target" = "forestgreen", "Below Target" = "firebrick")) +
  theme_minimal()

#Plot 3------------------------------------------------------------------------------------------------------------------------------
# Summarises and groups data by Location, Store Size, and Forecast
location_size_summary <- data1 %>%
  group_by(Store_Location, Store_Area, Coles_Forecast) %>%
  mutate(Store_Area = paste(Store_Area, "sqft"))%>%
  summarize(Count = n()) %>%
  ungroup()
  
# Plot: Store performance by location and store size
ggplot(location_size_summary, aes(x = Store_Location, y = Count, fill = Coles_Forecast)) +
  geom_bar(stat = "identity", position = "stack", width= 0.6) +
  facet_wrap(~ Store_Area, scales = "free_y") +
  labs(
    title = "Store Performance by Location and Size Category",
    x = "Store Location",
    y = "Number of Stores",
    fill = "Forecast Status",
  ) +
  scale_fill_manual(values = c("On Target" = "forestgreen", "Below Target" = "firebrick")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 15, face = "bold")
 ) 

#Appendix - Heat map-----------------------------------------------------------------------------
library(reshape2)  # For melting the correlation matrix

# Select the relevant numerical columns
numerical_data <- data3[, c("Store_Area", "Gross_Sale", "Log_Rev", 
                            "Sales_Cost")]

# Calculate the correlation matrix
cor_matrix <- cor(numerical_data, use = "complete.obs")  # Use complete.obs to handle missing values

# Melt the correlation matrix into long format
cor_melt <- melt(cor_matrix)

# Plot the heatmap
ggplot(cor_melt, aes(x = Var1, y = Var2, fill = value, color = Store_Size_Category)) +
  geom_tile(color = "white") +                          # Add tile borders for readability
  scale_fill_gradient2(low = "red", high = "blue", mid = "white",
                       midpoint = 0, limit = c(-1, 1),   # Center the gradient at 0
                       name = "Correlation") +           # Legend title
  theme_minimal() +
  labs(title = "Correlation Heatmap of Store Metrics") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + # Rotate x-axis labels
  coord_fixed()
