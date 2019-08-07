#Code for practicing animated plots
library(plotly)
library(crosstalk)
library(tidyverse)

summary(state_economic_data)

#Plot number 1: How has GDP been across US regions over the last 20 years?
#A linegraph of year (x) and gdp (y) colored by regions with a fill
stateEcon <- state_economic_data %>% 
  mutate(gdppercap  = (gdp/population*1000))

plotOne <- stateEcon %>% 
  group_by(state) 


p1 <- plotOne %>%
    plot_ly(x = ~year, y = ~gdppercap) %>%
    add_trace(text = ~state, hoverinfo = "text", color = ~region) %>%
    add_lines() %>%
    layout(xaxis = list(title = "Year (1997 - 2017)"),
           yaxis = list(title = "GDP Per Capita")) %>%
    highlight(on = "plotly_click")
#p1

#plot 2: how has home price changed with ownership over the years, regionally?
#scatterplot of homeownership respective to house price per region, animated by year
p2 <- stateEcon %>% group_by(state) %>%
  plot_ly(x = ~house_price, y = ~home_owners, color = ~region, size = ~population) %>%
  add_markers(frame = ~year,
              ids = ~year) %>%
  layout(xaxis = list(title = "House Price"),
         yaxis = list(title = "Home Ownership (%)")) %>%
  animation_opts(
    frame = 500,
    transition = 250,
    easing = "linear",
    redraw = TRUE
  )
#p2

#plot 3: How has the above changed BY region (linked, seperated by region)
#use ggplot2 to make a faceted graph, and then make it a plotly
p3 = ggplot(stateEcon, aes(x = house_price, y = home_owners, color = region)) +
  geom_point() +
  facet_grid(. ~region, scales = "free_x") +
  labs(x = "House Price", y = "Home Ownership (%)")

#ggplotly(p3) %>% add_trace(text = ~state, hoverinfo = "text") %>% layout(title = "Home Ownership by House Price")

#plot 4: Home Ownership by GDP, colored to region
temp <- stateEcon %>% filter(year == 2017)
sharedEcon <- SharedData$new(temp)

p4 <- sharedEcon %>% 
  plot_ly(x = ~gdp, y = ~home_owners, color = ~region) %>%
  layout(yaxis = list(title = "Home Ownership (%)"),
         xaxis = list(title = "State GDP")) %>%
  add_trace(text = ~state, hoverinfo = "text", color = ~region)
#p4
