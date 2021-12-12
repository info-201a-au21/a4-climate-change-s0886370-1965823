# Assignment 4
# server file
# Load libraries
library("ggplot2")
library("dplyr")
library("plotly")
library("tidyverse")


# Introduction Page
# Load data
co2_data <- read.csv("https://github.com/owid/co2-data/raw/master/owid-co2-data.csv")

# 5 Relevant Values of Interest

#1) Which countries have historically produced the most co2 emissions?
hist_most_co2 <- co2_data %>%
  group_by(country) %>%
  filter(country != "World") %>%
  filter(country != "Asia") %>%
  filter(country != "Europe") %>%
  filter(country != "North America") %>%
  filter(country != "EU-28") %>%
  filter(country != "EU-27") %>%
  filter(country != "Asia (excl. China & India)") %>%
  filter(country != "Europe (excl. EU-27)") %>%
  filter(country != "Europe (excl. EU-28)") %>%
  filter(country != "North America (excl. USA)") %>%
  filter(country != "South America") %>%
  filter(country != "International transport") %>%
  filter(country != "Oceania") %>%
  filter(country != "Micronesia") %>%
  summarise(co2_total = sum(co2, na.rm = T)) %>%
  arrange(-co2_total)

#2) As of the most recent year, which countries produce the most co2 emissions vs 
# per capita

recent_most_co2_percapita <- co2_data %>%
  group_by(country) %>%
  filter(year == last(year)) %>%
  filter(country != "World") %>%
  filter(country != "Asia") %>%
  filter(country != "Europe") %>%
  filter(country != "North America") %>%
  filter(country != "EU-28") %>%
  filter(country != "EU-27") %>%
  filter(country != "Asia (excl. China & India)") %>%
  filter(country != "Europe (excl. EU-27)") %>%
  filter(country != "Europe (excl. EU-28)") %>%
  filter(country != "North America (excl. USA)") %>%
  filter(country != "South America") %>%
  filter(country != "International transport") %>%
  filter(country != "Oceania") %>%
  filter(country != "Micronesia") %>%
  arrange(-co2_per_capita) %>%
  select(country, year, co2, co2_per_capita)

recent_most_co2_ <- co2_data %>%
  group_by(country) %>%
  filter(year == last(year)) %>%
  filter(country != "World") %>%
  filter(country != "Asia") %>%
  filter(country != "Europe") %>%
  filter(country != "North America") %>%
  filter(country != "EU-28") %>%
  filter(country != "EU-27") %>%
  filter(country != "Asia (excl. China & India)") %>%
  filter(country != "Europe (excl. EU-27)") %>%
  filter(country != "Europe (excl. EU-28)") %>%
  filter(country != "North America (excl. USA)") %>%
  filter(country != "South America") %>%
  filter(country != "International transport") %>%
  filter(country != "Oceania") %>%
  filter(country != "Micronesia") %>%
  arrange(-co2)

#3) For each year, what is the change in total co2 emitted in the US, 
# China, and Russia?
# I use this directly for the interactive app

co2_change_percap_US <- co2_data %>%
  filter(country == "United States") %>%
  group_by(year) %>%
  summarise(US_co2_percap = sum(co2_per_capita, na.rm = T)) %>%
  mutate(US_co2_change_percap = c(NA, diff(US_co2_percap)))

co2_change_US <- co2_data %>%
  filter(country == "United States") %>%
  group_by(year) %>%
  summarise(US_co2 = sum(co2, na.rm = T)) %>%
  mutate(US_co2_change = c(NA, diff(US_co2)))

co2_change_percap_China <- co2_data %>%
  filter(country == "China") %>%
  group_by(year) %>%
  summarise(China_co2_percap = sum(co2_per_capita, na.rm = T)) %>%
  mutate(China_co2_change_percap = c(NA, diff(China_co2_percap)))

co2_change_China <- co2_data %>%
  filter(country == "China") %>%
  group_by(year) %>%
  summarise(China_co2 = sum(co2, na.rm = T)) %>%
  mutate(China_co2_change = c(NA, diff(China_co2)))

co2_change_percap_Russia <- co2_data %>%
  filter(country == "Russia") %>%
  group_by(year) %>%
  summarise(Russia_co2_percap = sum(co2_per_capita, na.rm = T)) %>%
  mutate(Russia_co2_change_percap = c(NA, diff(Russia_co2_percap)))

co2_change_Russia <- co2_data %>%
  filter(country == "Russia") %>%
  group_by(year) %>%
  summarise(Russia_co2 = sum(co2, na.rm = T)) %>%
  mutate(Russia_co2_change = c(NA, diff(Russia_co2)))

co2_change_table <- inner_join(co2_change_US, co2_change_China, by = 'year') %>% inner_join(co2_change_Russia, by = 'year')
co2_change_percap_table <- inner_join(co2_change_percap_US, co2_change_percap_China, by = 'year') %>% inner_join(co2_change_percap_Russia, by = 'year')


#4) What was each country's average co2 emission in the past 10 years?

mean_co2_10_years <- co2_data %>%
  group_by(country) %>%
  filter(year > "2010") %>%
  filter(country != "World") %>%
  filter(country != "Asia") %>%
  filter(country != "Europe") %>%
  filter(country != "North America") %>%
  filter(country != "EU-28") %>%
  filter(country != "EU-27") %>%
  filter(country != "Asia (excl. China & India)") %>%
  filter(country != "Europe (excl. EU-27)") %>%
  filter(country != "Europe (excl. EU-28)") %>%
  filter(country != "North America (excl. USA)") %>%
  filter(country != "South America") %>%
  filter(country != "International transport") %>%
  filter(country != "Oceania") %>%
  filter(country != "Micronesia") %>%
  summarise(average_co2 = mean(co2, na.rm = T))

#5) In the most recent year for the United States, what percentage did each form
# of emission contribute to the overall co2 emission?

US_co2 <- co2_data %>% 
  filter(country == "United States") %>%
  filter(year == last(year)) %>%
  select(year, co2, coal_co2,
         flaring_co2, gas_co2,
         oil_co2, other_industry_co2,
         cement_co2) %>%
  mutate(coal_percent = (coal_co2 / co2) * 100,
         flaring_percent = (flaring_co2 / co2) * 100,
         oil_percent = (oil_co2 / co2) * 100,
         gas_percent = (gas_co2 / co2) * 100,
         cement_percent = (cement_co2 / co2) * 100,
         other_industry_percen = (other_industry_co2 / co2) * 100)

# Debugging
# df <- co2_change_table %>% filter(year == "2000") %>% select(US_co2, China_co2, Russia_co2)
# df <- data.frame(t(df))
# df <- cbind(c("US", "China", "Russia"), data.frame(df, row.names=NULL))
# names(df) <- data.frame("Country", "Amount")
# Plotting functions
# plot pie chart
plot_bar_chart <- function(c_year, change, var) {
  df <- co2_change_table
  df <- df %>% filter(year == c_year)
  y_axis <- 'Emmision (million tonnes)'
  
  
  if (change == 1) {
    df <- df %>% select(US_co2, China_co2, Russia_co2)
  } else {
    df <- df %>% select(US_co2_change, China_co2_change, Russia_co2_change)
  }
  
  # If other var, change dataset
  if (var == 2) {
    df <- co2_change_percap_table
    df <- df %>% filter(year == c_year)
    y_axis <- 'Emmision per Capita (tonnes per person)'
    
    if (change == 1) {
      df <- df %>% select(US_co2_percap, China_co2_percap, Russia_co2_percap)
    } else {
      df <- df %>% select(US_co2_change_percap, China_co2_change_percap, 
                          Russia_co2_change_percap)
    }
  }
  
  # filter chosen data set to display change or abs val
  
  df <- data.frame(t(df))
  df <- cbind(c("US", "China", "Russia"), data.frame(df, row.names=NULL))
  names(df) <- data.frame("Country", "Amount")
  
  figure <- plot_ly(df,
                    x = ~Country, y = ~Amount, type = "bar"
  )
  
  figure <- figure %>% layout(title = 'CO2 Emmisions per Country',
                              xaxis = list(title = 'Country',
                                           zeroline = TRUE),
                              yaxis = list(title = y_axis))
  
  return(figure)
}

# Setting up server
server <- function(input, output) {
  output$message <- renderText({
    msg <- paste0("CO2 Emissions in ", input$year)
    return(msg)
  })
  
  output$chart <- renderPlotly({plot_bar_chart(input$year, 
                                             input$change, input$var)})
}


