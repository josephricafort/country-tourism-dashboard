library(tidyverse)

list.files('data/raw/')

# get_tidy <- function (path) {
#   tidy <- read.csv(path) %>% as_tibble() %>%
#     # Restructure data frame
#     select(-(1:3)) %>%
#     mutate(Indicator = paste0(X.1, X.2, X.3)) %>%
#     rename(Country = 'Basic.data.and.indicators',
#            Category = X) %>%
#     relocate(Indicator, .after = Category) %>%
#     select(-(X.1:X.3)) %>%
#     # Fill in missing cells
#     mutate(Country = if_else(Country == "", NA, Country),
#            Category = if_else(Category == "", NA, Category)) %>%
#     fill(Country, .direction = "down") %>%
#     fill(Category, .direction = "updown") %>%
#     filter(Indicator != "")
#   
#   return (tidy)
# }
# 
# get_clean <- function (tidy, value_colname) {
#   clean <- tidy %>%
#     # Remove unnecessary rows and columns
#     select(-Notes, -Series, -X.4) %>%
#     pivot_longer(cols = X1995:X2022, names_to = 'Year', values_to = value_colname) %>%
#     mutate(Year = str_replace(Year, "X", ""))
#   
#   return (clean)
# }



# Arrivals data

arrivals_tidy <- read.csv('data/raw/unwto-arrivals.csv') %>% as_tibble() %>%
  # Restructure data frame
  select(-(1:3)) %>%
  mutate(Indicator = paste0(X.1, X.2, X.3)) %>%
  rename(Country = 'Basic.data.and.indicators',
         Category = X) %>%
  relocate(Indicator, .after = Category) %>%
  select(-(X.1:X.3)) %>%
  # Fill in missing cells
  mutate(Country = if_else(Country == "", NA, Country),
         Category = if_else(Category == "", NA, Category)) %>%
  fill(Country, .direction = "down") %>%
  fill(Category, .direction = "updown") %>%
  filter(Indicator != "")

arrivals <- arrivals_tidy %>%
  # Remove unnecessary rows and columns
  select(-Notes, -Series, -X.4) %>%
  pivot_longer(cols = X1995:X2022, names_to = 'Year', values_to = 'Value') %>%
  mutate(Year = str_replace(Year, "X", ""),
         Value = as.numeric(Value))


# Regions data

regions_tidy <- read.csv('data/raw/unwto-regions.csv') %>% as_tibble() %>%
  # Restructure data frame
  select(-(1:3)) %>%
  mutate(Indicator = paste0(X.1, X.2, X.3)) %>%
  rename(Country = 'Basic.data.and.indicators',
         Category = X) %>%
  relocate(Indicator, .after = Category) %>%
  select(-(X.1:X.3)) %>%
  # Fill in missing cells
  mutate(Country = if_else(Country == "", NA, Country),
         Category = if_else(Category == "", NA, Category)) %>%
  fill(Country, .direction = "down") %>%
  fill(Category, .direction = "updown") %>%
  filter(Indicator != "")

regions <- regions_tidy %>%
  # Remove unnecessary rows and columns
  select(-Notes, -Series, -X.4) %>%
  pivot_longer(cols = X1995:X2022, names_to = 'Year', values_to = 'Value') %>%
  mutate(Year = str_replace(Year, "X", ""),
         Value = as.numeric(Value))


# Purpose data

purpose_tidy <- read.csv('data/raw/unwto-purpose.csv') %>% as_tibble() %>%
  # Restructure data frame
  select(-(1:3)) %>%
  mutate(Indicator = paste0(X.1, X.2)) %>%
  rename(Country = 'Basic.data.and.indicators',
         Category = X) %>%
  relocate(Indicator, .after = Category) %>%
  select(-(X.1:X.3)) %>%
  # Fill in missing cells
  mutate(Country = if_else(Country == "", NA, Country),
         Category = if_else(Category == "", NA, Category)) %>%
  fill(Country, .direction = "down") %>%
  fill(Category, .direction = "updown") %>%
  filter(Indicator != "")

purpose <- purpose_tidy %>%
  # Remove unnecessary rows and columns
  select(-Notes, -Series, -X.4) %>%
  pivot_longer(cols = X1995:X2022, names_to = 'Year', values_to = 'Value') %>%
  mutate(Year = str_replace(Year, "X", ""),
         Value = as.numeric(Value))


# Transport data

transport_tidy <- read.csv('data/raw/unwto-transport.csv') %>% as_tibble() %>%
  # Restructure data frame
  select(-(1:3)) %>%
  mutate(Indicator = paste0(X.1, X.2)) %>%
  rename(Country = 'Basic.data.and.indicators',
         Category = X) %>%
  relocate(Indicator, .after = Category) %>%
  select(-(X.1:X.3)) %>%
  # Fill in missing cells
  mutate(Country = if_else(Country == "", NA, Country),
         Category = if_else(Category == "", NA, Category)) %>%
  fill(Country, .direction = "down") %>%
  fill(Category, .direction = "updown") %>%
  filter(Indicator != "")

transport <- transport_tidy %>%
  # Remove unnecessary rows and columns
  select(-Notes, -Series, -X.4) %>%
  pivot_longer(cols = X1995:X2022, names_to = 'Year', values_to = 'Value') %>%
  mutate(Year = str_replace(Year, "X", ""),
         Value = as.numeric(Value))


# Expenditure data

expenditure_tidy <- read.csv('data/raw/unwto-expenditure.csv') %>% as_tibble() %>%
  # Restructure data frame
  select(-(1:3)) %>%
  mutate(Indicator = paste0(X.1)) %>%
  rename(Country = 'Basic.data.and.indicators',
         Category = X) %>%
  relocate(Indicator, .after = Category) %>%
  select(-(X.1:X.3)) %>%
  # Fill in missing cells
  mutate(Country = if_else(Country == "", NA, Country),
         Category = if_else(Category == "", NA, Category),
         Indicator = if_else(!is.na(Category), "Total", Indicator )) %>%
  fill(Country, .direction = "down") %>%
  fill(Category, .direction = "updown") %>%
  filter(Indicator != "")

expenditure <- expenditure_tidy %>%
  # Remove unnecessary rows and columns
  select(-Notes, -Series, -X.4) %>%
  pivot_longer(cols = X1995:X2022, names_to = 'Year', values_to = 'Value') %>%
  mutate(Year = str_replace(Year, "X", ""),
         Value = as.numeric(Value))


# Industries data

industries_tidy <- read.csv('data/raw/unwto-industries.csv') %>% as_tibble() %>%
  # Restructure data frame
  select(-(1:3)) %>%
  mutate(Indicator = paste0(X.1)) %>%
  rename(Country = 'Basic.data.and.indicators',
         Category = X) %>%
  relocate(Indicator, .after = Category) %>%
  select(-(X.1:X.3)) %>%
  # Fill in missing cells
  mutate(Country = if_else(Country == "", NA, Country),
         Category = if_else(Category == "", NA, Category),
         Indicator = if_else(!is.na(Category), "Total", Indicator )) %>%
  fill(Country, .direction = "down") %>%
  fill(Category, .direction = "updown") %>%
  filter(Indicator != "")

industries <- industries_tidy %>%
  # Remove unnecessary rows and columns
  select(-Notes, -X.4) %>%
  pivot_longer(cols = X1995:X2022, names_to = 'Year', values_to = 'Value') %>%
  mutate(Year = str_replace(Year, "X", ""),
         Value = as.numeric(Value))


# Combine into a single data and cleanup per individual cell

arrivals
regions
purpose
transport
expenditure
industries

categories <- c('Accommodation for visitors in hotels and similar establishments', 'Arrivals', 'Arrivals by main purpose',
                'Arrivals by mode of transport', 'Arrivals by region', 'Arrivals of non-resident tourists at national borders',
                'Tourism expenditure in the country')

tourism <- bind_rows(arrivals, regions, purpose, transport, expenditure, industries) %>%
  mutate(across(Country:Year, as.factor)) %>%
  select(Category, Indicator, Country, Year, Units, Value) %>%
  filter(Category %in% categories)
