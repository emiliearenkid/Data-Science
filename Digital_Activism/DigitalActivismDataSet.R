library(dplyr)
library(ggplot2)
library(tidytext)
library(tidyverse)


# Clean up the data
#   Replace 0 with "Unknown" and replace 55 with "Multiple" based on the codebook
#   Replace typos with correct country code
#   Remove whitespace from country code columns

da34625.0001 <- da34625.0001 %>% 
  mutate(CNTRY1 = str_replace(CNTRY1, "0", "Unknown")) %>% 
  mutate(CNTRY1 = str_replace(CNTRY1, "55", "Multiple")) %>% 
  mutate(CNTRY3 = str_replace(CNTRY3, "0", "Unknown")) %>% 
  mutate(CNTRY3 = str_replace(CNTRY3, "55", "Multiple")) %>% 
  mutate(CNTRY4 = str_replace(CNTRY4, "0", "Unknown")) %>% 
  mutate(CNTRY4 = str_replace(CNTRY4, "55", "Multiple")) %>% 
  mutate(ICNTRY1 = str_replace(ICNTRY1, "0", "Unknown")) %>% 
  mutate(ICNTRY1 = str_replace(ICNTRY1, "55", "Multiple")) %>% 
  mutate(ICNTRY1 = str_replace(ICNTRY1, "GRB", "GBR")) %>% 
  mutate(CNTRY1 = str_replace(CNTRY1, "GRB", "GBR")) %>% 
  mutate(CNTRY3 = str_replace(CNTRY3, "GRB", "GBR")) %>% 
  mutate(CNTRY4 = str_replace(CNTRY4, "GRB", "GBR")) %>% 
  mutate(ICNTRY1 = str_squish(ICNTRY1)) %>% 
  mutate(CNTRY1 = str_squish(CNTRY1)) %>% 
  mutate(CNTRY3 = str_squish(CNTRY3)) %>% 
  mutate(CNTRY4 = str_squish(CNTRY4))

# What countries have the most digital campaigns initiated by them? (ICNTRY1)
originating_countries <- da34625.0001 %>% 
  group_by(ICNTRY1) %>% 
  summarize(initiators = n()) %>% 
  slice_max(initiators, n = 10) 

# Replace Country Codes with Country Names and Remove Whitespace
originating_countries <- originating_countries %>% 
  mutate(ICNTRY1 = str_replace(ICNTRY1, "BRA", "Brazil")) %>% 
  mutate(ICNTRY1 = str_replace(ICNTRY1, "CAN", "Canada")) %>% 
  mutate(ICNTRY1 = str_replace(ICNTRY1, "CHN", "China")) %>% 
  mutate(ICNTRY1 = str_replace(ICNTRY1, "EGY", "Egypt")) %>% 
  mutate(ICNTRY1 = str_replace(ICNTRY1, "IND", "India")) %>% 
  mutate(ICNTRY1 = str_replace(ICNTRY1, "IRN", "Iran")) %>% 
  mutate(ICNTRY1 = str_replace(ICNTRY1, "RUS", "Russia")) %>% 
  mutate(ICNTRY1 = str_replace(ICNTRY1, "GBR", "UK"))

# Generate dot plot
ggplot(data = originating_countries) + 
  geom_point(mapping = aes(
    x = reorder(ICNTRY1, initiators), 
    y = initiators, 
    colour = ICNTRY1)
  ) + 
  scale_y_continuous(breaks = seq(0, 240, by = 20)) + 
  labs(
    title = "Countries Originating Campaigns", # plot title
    x = "Countries", # x-axis label
    y = "Total", # y-axis label
    colour = "Countries"
  ) + 
  theme(title = element_text(face="bold"), 
        axis.text.x = element_text(angle = 50, vjust=0.75), 
        axis.ticks = element_blank())


################################################################################


# What countries are the largest targets of digital activism? (CNTRY1, CNTRY3, CNNTRY4)
# Sum all instances of a country across CNTRY1, CNTRY3, and CNTRY4
target_countries1_mult <- da34625.0001 %>% 
  count(CNTRY1)
target_countries3_mult <- da34625.0001 %>% 
  count(CNTRY3)
target_countries4_mult <- da34625.0001 %>% 
  count(CNTRY4)

# Join the three sets, sum their values into 1 column, and get the top 10 countries
target_countries_mult <- target_countries1_mult %>% 
  full_join(target_countries3_mult, by = join_by(CNTRY1 == CNTRY3)) %>% 
  full_join(target_countries4_mult, by = join_by(CNTRY1 == CNTRY4)) 
  
target_countries <- target_countries_mult %>% 
  mutate(targets = rowSums(across(n.x:n), na.rm = TRUE)) %>% 
  mutate(targets = replace(targets, targets == 844, 61)) %>% 
  filter(CNTRY1 != "Multiple") %>% 
  slice_max(targets, n = 10)

# Format Country Names
target_countries <- target_countries %>% 
  mutate(CNTRY1 = str_replace(CNTRY1, "BRA", "Brazil")) %>% 
  mutate(CNTRY1 = str_replace(CNTRY1, "CHN", "China")) %>% 
  mutate(CNTRY1 = str_replace(CNTRY1, "EGY", "Egypt")) %>% 
  mutate(CNTRY1 = str_replace(CNTRY1, "IND", "India")) %>% 
  mutate(CNTRY1 = str_replace(CNTRY1, "IRN", "Iran")) %>% 
  mutate(CNTRY1 = str_replace(CNTRY1, "ISR", "Israel")) %>% 
  mutate(CNTRY1 = str_replace(CNTRY1, "RUS", "Russia")) %>% 
  mutate(CNTRY1 = str_replace(CNTRY1, "ZAF", "South Africa"))

# Generate dot plot
ggplot(data = target_countries) + 
  geom_point(mapping = aes(
    x = reorder(CNTRY1, targets), 
    y = targets, 
    colour = CNTRY1)
  ) + 
  scale_y_continuous(breaks = seq(0, 180, by = 20)) + 
  labs(
    title = "Countries Targeted by Campaigns", # plot title
    x = "Countries", # x-axis label
    y = "Total", # y-axis label
    colour = "Countries"
  ) + 
  theme(title = element_text(face="bold"), 
        axis.text.x = element_text(angle = 50, vjust=0.7), 
        axis.ticks = element_blank())


################################################################################


# What countries start campaigns against other countries?
#   (Compare ICNTRY with CNTRY1, CNTRY3, and CNTRY4)
diff_countries <- da34625.0001 %>% 
  filter(CNTRY1 != ICNTRY1 & 
           (is.na(CNTRY3) | CNTRY3 != ICNTRY1) & 
           (is.na(CNTRY4) | CNTRY4 != ICNTRY1)) %>% 
  group_by(ICNTRY1) %>% 
  filter(ICNTRY1 != "Unknown" & ICNTRY1 != "Multiple" & ICNTRY1 != 99) %>% 
  summarize(initiators = n()) %>% 
  slice_max(initiators, n = 8)

# Format Country Names
diff_countries <- diff_countries %>% 
  mutate(ICNTRY1 = str_squish(ICNTRY1)) %>% 
  mutate(ICNTRY1 = str_replace(ICNTRY1, "AUS", "Australia")) %>% 
  mutate(ICNTRY1 = str_replace(ICNTRY1, "CAN", "Canada")) %>% 
  mutate(ICNTRY1 = str_replace(ICNTRY1, "CHL", "Chile")) %>% 
  mutate(ICNTRY1 = str_replace(ICNTRY1, "CHN", "China")) %>%
  mutate(ICNTRY1 = str_replace(ICNTRY1, "COL", "Colombia")) %>% 
  mutate(ICNTRY1 = str_replace(ICNTRY1, "EGY", "Egypt")) %>%
  mutate(ICNTRY1 = str_replace(ICNTRY1, "FRA", "France")) %>%
  mutate(ICNTRY1 = str_replace(ICNTRY1, "GBR", "UK")) %>% 
  mutate(ICNTRY1 = str_replace(ICNTRY1, "HKG", "Hong Kong")) %>% 
  mutate(ICNTRY1 = str_replace(ICNTRY1, "IND", "India")) %>%
  mutate(ICNTRY1 = str_replace(ICNTRY1, "IRN", "Iran")) %>%
  mutate(ICNTRY1 = str_replace(ICNTRY1, "ITA", "Italy")) %>%
  mutate(ICNTRY1 = str_replace(ICNTRY1, "KWT", "Kuwait")) %>% 
  mutate(ICNTRY1 = str_replace(ICNTRY1, "NLD", "Netherlands")) %>% 
  mutate(ICNTRY1 = str_replace(ICNTRY1, "RUS", "Russia")) %>%
  mutate(ICNTRY1 = str_replace(ICNTRY1, "SYR", "Syria"))

# Generate dot plots
ggplot(data = diff_countries) + 
  geom_point(mapping = aes(
    x = reorder(ICNTRY1, initiators), 
    y = initiators, 
    colour = ICNTRY1)
  ) + 
  scale_y_continuous(breaks = seq(0, 80, by = 10)) + 
  labs(
    title = "Countries Starting Campaigns Against Other Countries", # plot title
    x = "Countries", # x-axis label
    y = "Total", # y-axis label
    colour = "Countries"
  ) + 
  theme(title = element_text(face="bold"), 
        axis.text.x = element_text(angle = 50, vjust=0.75), 
        axis.ticks = element_blank(), 
        legend.position = "none")
