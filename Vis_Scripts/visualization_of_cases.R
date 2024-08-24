library(ggplot2)
library(dplyr)
library(lubridate)  

setwd("")

pitt_global_seq_data <- read.csv(".")

df <- pitt_global_seq_data %>% 
  mutate(Date = as.Date(Dateyymmd, format="%Y-%m-%d"),  YearMonth = floor_date(Date, "month")) %>%  # Convert the date column to Date type
  group_by(YearMonth, Continent) %>%
  summarise(Count = n()) %>%  # Count occurrences by date and place
  ungroup()

df_combined <- df %>% 
  mutate(Group = ifelse(Continent == "Pittsburgh", "Pittsburgh", "Others")) %>%
  group_by(YearMonth, Group) %>%
  summarise(TotalCount = sum(Count)) %>%
  ungroup()

ggplot(df_combined, aes(x = YearMonth, y = TotalCount, color = Group)) +
  geom_line(size = 1) +
  labs(title = "# of HMPV from Pittsburgh vs. Global",
       subtitle = "Aggregated by month",
       x = "Date",
       y = "Count",
       color = "Group") +
  theme_minimal() +
  theme(text = element_text(size = 18))
