##### Title: DataCamp Data Analyst Certification Project
##### Author: Alec Becker
##### Date Created: 2022-02-17
##### Last Updated: 2022-02-17

# Load libraries
library(tidyverse)


# Read dataset
df <- read_csv("boat_data.csv")

# Inspect data
str(df)
summary(df)
head(df)

# Clean data
df_clean <- df %>%
  separate(Price, c("Curr", "Price"), sep = " ") %>%
  mutate(Price = as.numeric(Price),
         `Year Built` = replace(`Year Built`, `Year Built` == 0, NA),
         type_diesel = str_detect(tolower(trimws(df$Type)), "diesel"),
         type_display = str_detect(tolower(trimws(df$Type)), "display"),
         type_electric = str_detect(tolower(trimws(df$Type)), "electric"),
         type_gas = str_detect(tolower(trimws(df$Type)), "gas"),
         type_unleaded = str_detect(tolower(trimws(df$Type)), "unleaded"),
         type_new_stock = str_detect(tolower(trimws(df$Type)), "new boat from stock"),
         type_new_order = str_detect(tolower(trimws(df$Type)), "new boat on order"),
         type_hybrid = str_detect(tolower(trimws(df$Type)), "hybrid"),
         type_used = str_detect(tolower(trimws(df$Type)), "used"),
         type_propane = str_detect(tolower(trimws(df$Type)), "propane"),
         boat_type_motor_yacht = str_detect(tolower(trimws(df$`Boat Type`)), "motor yacht"),
         boat_type_center_console = str_detect(tolower(trimws(df$`Boat Type`)), "center console boat"),
         boat_type_sport = str_detect(tolower(trimws(df$`Boat Type`)), "sport boat"),
         boat_type_fishing = str_detect(tolower(trimws(df$`Boat Type`)), "fishing boat"),
         boat_type_catamaran = str_detect(tolower(trimws(df$`Boat Type`)), "catamaran"),
         boat_type_pontoon = str_detect(tolower(trimws(df$`Boat Type`)), "pontoon boat"),
         boat_type_runabout = str_detect(tolower(trimws(df$`Boat Type`)), "runabout"),
         boat_type_deck = str_detect(tolower(trimws(df$`Boat Type`)), "deck boat"),
         boat_type_pilothouse = str_detect(tolower(trimws(df$`Boat Type`)), "pilothouse"),
         boat_type_cabin = str_detect(tolower(trimws(df$`Boat Type`)), "cabin boat"),
         boat_type_working = str_detect(tolower(trimws(df$`Boat Type`)), "working boat"),
         boat_type_classic = str_detect(tolower(trimws(df$`Boat Type`)), "classic"),
         boat_type_bowrider = str_detect(tolower(trimws(df$`Boat Type`)), "bowrider"),
         boat_type_trawler = str_detect(tolower(trimws(df$`Boat Type`)), "trawler"),
         boat_type_launch = str_detect(tolower(trimws(df$`Boat Type`)), "launch"),
         boat_type_flybridge = str_detect(tolower(trimws(df$`Boat Type`)), "flybridge"),
         boat_type_water_ski = str_detect(tolower(trimws(df$`Boat Type`)), "water ski"),
         boat_type_hardtop = str_detect(tolower(trimws(df$`Boat Type`)), "hardtop"),
         boat_type_offshore = str_detect(tolower(trimws(df$`Boat Type`)), "offshore boat"),
         boat_type_wakeboard = str_detect(tolower(trimws(df$`Boat Type`)), "wakeboard/wakesurf"),
         boat_type_passenger = str_detect(tolower(trimws(df$`Boat Type`)), "passenger boat"),
         boat_type_house = str_detect(tolower(trimws(df$`Boat Type`)), "house boat"),
         boat_type_ketch = str_detect(tolower(trimws(df$`Boat Type`)), "ketch"),
         boat_type_mega_yacht = str_detect(tolower(trimws(df$`Boat Type`)), "mega yacht"),
         boat_type_motorsailer = str_detect(tolower(trimws(df$`Boat Type`)), "motorsailer"),
         boat_type_rib = str_detect(tolower(trimws(df$`Boat Type`)), "rib"),
         country = fct_lump_min(as.factor(sapply(strsplit(Location, " "), head, 1)), min = 5, other_level = "Other"),
         Material = as.factor(Material),
         Manufacturer = as.factor(Manufacturer),
         price_EUR = case_when(
           Curr == "Â£" ~ Price * 1.2,
           Curr == "CHF" ~ Price * 0.96,
           Curr == "DKK" ~ Price * 0.13,
           Curr == "EUR" ~ Price
         )) %>% 
  mutate(views_tertile = ntile(`Number of views last 7 days`, 3)) %>% 
  select(!c(Location, Type, `Boat Type`, Price, Curr))
  

head(df_clean)


# Analysis

df_clean$`Views Group` <- as.factor(df_clean$views_tertile)


df_clean %>% 
  ggplot(aes(x = price_EUR, y = `Views Group`, color = `Views Group`)) +
  geom_boxplot(outlier.shape = NA, width = 0.25, mapping = aes(fill = `Views Group`), alpha = 0.3) +
  scale_x_continuous(limits = c(0, 600000)) +
  theme_bw() +
  labs(title = "Price (EUR)")
ggsave("price.png", device = "png")


df_clean %>% 
  ggplot(aes(x = `Year Built`, color = `Views Group`, fill = `Views Group`)) +
  geom_density(alpha = 0.4) +
  theme_bw() +
  labs(title = "Year Built")
ggsave("year.png", device = "png")

df_clean %>% 
  ggplot(aes(x = Length, color = `Views Group`, fill = `Views Group`)) +
  geom_density(alpha = 0.4) +
  theme_bw() +
  labs(title = "Length")
ggsave("length.png", device = "png")

df_clean %>% 
  ggplot(aes(x = Width, color = `Views Group`, fill = `Views Group`)) +
  geom_density(alpha = 0.4) +
  theme_bw() +
  labs(title = "Width")
ggsave("width.png", device = "png")


df_clean %>% 
  ggplot(aes(x = Material,  group = as.factor(views_tertile))) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat = "count") +
  labs(y = "Percent") +
  facet_grid(~as.factor(views_tertile)) +
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  labs(title = "Boat Material",
       y = "Percent",
       x = "") +
  coord_flip() +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.01),
                y= ..prop..), stat = "count", hjust = -.1) +
  theme_bw() +
  theme(legend.position = "none")
ggsave("material.png", device = "png")


df_clean %>% 
  ggplot(aes(x = country,  group = as.factor(views_tertile))) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat = "count") +
  labs(y = "Percent") +
  facet_grid(~as.factor(views_tertile)) +
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  labs(title = "Country",
       y = "Percent",
       x = "") +
  coord_flip() +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.01),
                y= ..prop..), stat = "count", hjust = -.1) +
  theme_bw() +
  theme(legend.position = "none")
ggsave("country.png", device = "png")
  
 

type_df_t1 <- data.frame(
  type = c("Diesel", "Display", "Electric", "Gas", "Unleaded", "New Stock", "New Order", "Hybrid", "Used", "Propane"),
  views_tertile = 1,
  pct = (colSums(df_clean %>% filter(views_tertile == 1) %>% select(type_diesel:type_propane), na.rm = TRUE) / sum(df_clean$views_tertile == 1)) * 100,
  row.names = NULL
)

type_df_t2 <- data.frame(
  type = c("Diesel", "Display", "Electric", "Gas", "Unleaded", "New Stock", "New Order", "Hybrid", "Used", "Propane"),
  views_tertile = 2,
  pct = (colSums(df_clean %>% filter(views_tertile == 2) %>% select(type_diesel:type_propane), na.rm = TRUE) / sum(df_clean$views_tertile == 2)) * 100,
  row.names = NULL
)

type_df_t3 <- data.frame(
  type = c("Diesel", "Display", "Electric", "Gas", "Unleaded", "New Stock", "New Order", "Hybrid", "Used", "Propane"),
  views_tertile = 3,
  pct = (colSums(df_clean %>% filter(views_tertile == 3) %>% select(type_diesel:type_propane), na.rm = TRUE) / sum(df_clean$views_tertile == 3)) * 100,
  row.names = NULL
)

type_df <- rbind(type_df_t1, type_df_t2, type_df_t3)

type_df %>% 
  ggplot(aes(y = type, x = pct, fill = type)) +
  geom_col(position = "dodge2") +
  facet_wrap(~as.factor(views_tertile)) +
  geom_text(label = paste0(round(type_df$pct, 2), "%"), hjust = -.1) +
  scale_x_continuous(limits = c(0, 100)) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(title = "Boat Condition / Engine Type",
       x = "Percent",
       y = "")
ggsave("condition_and_engine_type.png", device = "png")



boat_type_df_t1 <- data.frame(
  boat_type = c("Motor Yacht", "Center Console", "Sport", "Fishing", "Catamaran", "Pontoon", "Runabout", "Deck", "Pilothouse", "Cabin",
           "Working", "Classic", "Bowrider", "Trawler", "Launch", "Flybridge", "Water ski", "Hard top", "Offshore",
           "Wakeboard", "Passenger", "House", "Ketch", "Mega Yacht", "Motorsailer", "Rib"),
  views_tertile = 1,
  pct = (colSums(df_clean %>% filter(views_tertile == 1) %>% select(boat_type_motor_yacht:boat_type_rib), na.rm = TRUE) / sum(df_clean$views_tertile == 1)) * 100,
  row.names = NULL
)

boat_type_df_t2 <- data.frame(
  boat_type = c("Motor Yacht", "Center Console", "Sport", "Fishing", "Catamaran", "Pontoon", "Runabout", "Deck", "Pilothouse", "Cabin",
           "Working", "Classic", "Bowrider", "Trawler", "Launch", "Flybridge", "Water ski", "Hard top", "Offshore",
           "Wakeboard", "Passenger", "House", "Ketch", "Mega Yacht", "Motorsailer", "Rib"),
  views_tertile = 2,
  pct = (colSums(df_clean %>% filter(views_tertile == 2) %>% select(boat_type_motor_yacht:boat_type_rib), na.rm = TRUE) / sum(df_clean$views_tertile == 2)) * 100,
  row.names = NULL
)

boat_type_df_t3 <- data.frame(
  boat_type = c("Motor Yacht", "Center Console", "Sport", "Fishing", "Catamaran", "Pontoon", "Runabout", "Deck", "Pilothouse", "Cabin",
           "Working", "Classic", "Bowrider", "Trawler", "Launch", "Flybridge", "Water ski", "Hard top", "Offshore",
           "Wakeboard", "Passenger", "House", "Ketch", "Mega Yacht", "Motorsailer", "Rib"),
  views_tertile = 3,
  pct = (colSums(df_clean %>% filter(views_tertile == 3) %>% select(boat_type_motor_yacht:boat_type_rib), na.rm = TRUE) / sum(df_clean$views_tertile == 3)) * 100,
  row.names = NULL
)

boat_type_df <- rbind(boat_type_df_t1, boat_type_df_t2, boat_type_df_t3)


boat_type_df %>% 
  ggplot(aes(y = boat_type, x = pct, fill = boat_type)) +
  geom_col(position = "dodge2") +
  facet_wrap(~as.factor(views_tertile)) +
  geom_text(label = paste0(round(boat_type_df$pct, 2), "%"), hjust = -.1) +
  scale_x_continuous(limits = c(0, 100)) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(title = "Boat Type",
       x = "Percent",
       y = "")
ggsave("boat_type.png", device = "png")


df_clean %>%
  filter(!is.na(Manufacturer)) %>% 
  group_by(`Views Group`, Manufacturer) %>%
  summarize(count = n()) %>%
  arrange(`Views Group`, desc(count)) %>%
  filter(`Views Group` == 1) %>%
  top_n(10) %>%
  write_csv(file = "top_10_manu_views1.csv")

df_clean %>%
  filter(!is.na(Manufacturer)) %>% 
  group_by(`Views Group`, Manufacturer) %>%
  summarize(count = n()) %>%
  arrange(`Views Group`, desc(count)) %>%
  filter(`Views Group` == 2) %>%
  top_n(10) %>%
  write_csv(file = "top_10_manu_views2.csv")

df_clean %>%
  filter(!is.na(Manufacturer)) %>% 
  group_by(`Views Group`, Manufacturer) %>%
  summarize(count = n()) %>%
  arrange(`Views Group`, desc(count)) %>%
  filter(`Views Group` == 3) %>%
  top_n(10) %>%
  write_csv(file = "top_10_manu_views3.csv")



