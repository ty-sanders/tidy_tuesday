## Author: Tyler Sanders
## Date:   04/21/2021
## Project: Tidy Tuesday - Post Offices


## Load Key Packages 
library(tidytuesdayR)
library(tidyverse)
library(janitor)
library(geofacet)
## Load Data from tidytuesdayR
tuesdata <- tidytuesdayR::tt_load(2021, week = 16)

post_offices <- tuesdata$post_offices

## Data Descriptions 
tuesdata

## Data
glimpse(post_offices)

## Basic Cleaning. 
post_offices <- post_offices %>% 
  filter(! state %in% c("VAy", "MI/OH")) %>% # Remove non-state values. Could have worked to fix them but decided to drop
  mutate(state = state.name[match(state, state.abb)]) %>%  # use built in state.name and state.abb to change state to full state names
  filter(! is.na(state)) 

## Create an object with the count of new offices per state per year 
new_offices <- post_offices %>% 
  select(state, name, established) %>% 
  group_by(state, established) %>% 
  count()

## Create an object with the count of discontinued offices per state per year 
closed_offices <- post_offices %>% 
  select(state, name, discontinued) %>% 
  group_by(state, discontinued) %>% 
  count() %>% 
  select(state, discontinued, closed = n)
  
## Join new and closed offices. 
## Calculate the net gain in post offices per state per year
## Use cumsum() to get a cumulative total
## Create a label column for each row that is the "peak" year in post office counts 

join <- new_offices %>% 
  left_join(y = closed_offices, by = c("state", "established" = "discontinued")) %>% 
  mutate(closed = case_when(is.na(closed) ~ 0L, TRUE ~ closed)) %>% # change NA to 0 so I can calculate net 
  mutate(net = n - closed) %>% 
  group_by(state) %>% 
  mutate(total = cumsum(net), #cumsum is like a running total 
         label = case_when(total %in% max(total) ~ paste0("Peak: ", scales::comma(total, accuracy = 1), " (", established, ")"),
                                            TRUE ~ NA_character_)) #with case_when, TRUE basically means if row doesn't fit any case you specify

## US State Grid From geofacet package. To be used in ggplot with facet_geo()
grid <- geofacet::us_state_without_DC_grid2

grid <- grid %>% 
  filter(! code %in% c("HI", "AK"))  ## Decided to drop AK and HI, this needs to be done in the grid or else there will be blank squares there


## GeoFacet Plot of Post Offices By Year
join %>% 
  ggplot(aes(x = established, y = total, group = 1, color = (net > 0), label = label)) +
  geom_line() +
  scale_y_continuous(labels = scales::comma, breaks = c(250, 1000, 2500, 5000), limits =c(0, 7800)) +
  scale_x_continuous(breaks = c(1850, 1900, 1950), limits = c(1830, 1990)) + 
  scale_color_manual("Growth", values = c("red", "blue")) +
  theme_minimal() +
  geom_label(size = 3.5, nudge_x = 0, nudge_y = 1100, fill = "white", color = "black", fontface = "bold") + #geom_label picks up on the label = argument in my ggplot(aes(...))
  theme(legend.position = "none",                        #no legend, I'll explain the chart using text 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "linen"), #first time using linen, it looks nice. From(https://ggplot2-book.org/polishing.html)
        plot.background =  element_rect(fill = "linen"),
        strip.background = element_rect(fill = "grey20", color = "grey80", size = 3),
        strip.text = element_text(colour = "white", size = 12, face = "bold", family = "serif"),
        axis.text.y = element_blank(),
        #axis.title.y = element_text(size = 24, family = "serif"), #Decided to more or less cut y axis
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 24, family = "serif"),
        plot.title = element_text(hjust = .5, vjust = .8, size = 36, family = "serif"),
        plot.subtitle = element_text(hjust = .5, vjust = .8, size = 24, family = "serif")) +
  labs(title = "I Just Want to Mail a Letter", subtitle = "The United States' network of post offices has been in decline for more than a century.",
       x = "Tracking Total Post Offices Per State: 1850 - 1975", y = "Total Post Offices by Year", caption = "Chart by Tyler Sanders, @_TylerSanders // Tidy Tuesday Week 16, 2021 // Data from: Blevins, Cameron; Helbock, Richard W., 2021, 'US Post Offices'") + 
  facet_geo(~state, grid = grid, scales = "fixed") 
