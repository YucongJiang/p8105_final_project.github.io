library(flexdashboard)
library(tidyverse)
library(broom)
library(shiny)
library(plotly)

##############################################
#                                            #
#  Use shiny to build an interactive page    #
#                                            #
##############################################

# By Yucong Jiang

# This page permit users to choose the x-axis to generate scatterplot and linear model.
# The x-axis data are from 500-Cities and Wikipedia, data from 500-Cities are manipulated by Qi Lu, etc. 


# Read data from 500-Cities
mental = read.csv("./data/health data.csv")%>% 
  janitor::clean_names() %>% 
  as_tibble() %>% 
  filter(measure == "mental health" | measure == "obesity" | measure == "diabetes" | measure == "drink" | measure == "cancer" | measure == "sleeping") 

mental_data = mental %>%
  filter(measure == "mental health")  

total_data = mental %>% 
  pivot_wider(names_from = measure, city_name, values_from = data_value) %>% 
  unnest() %>% 
  drop_na() %>% 
  group_by(city_name) %>% 
  summarize(obesity = mean(obesity), drink = mean(drink), diabetes = mean(diabetes), mental_health = mean(`mental health`), cancer = mean(cancer), sleeping = mean(sleeping))

# We also want to show the results on the state scale

total_data_state <- mental %>%
  pivot_wider(names_from = measure, state_abbr, values_from = data_value) %>% 
  unnest() %>% 
  drop_na() %>% 
  group_by(state_abbr) %>% 
  summarize(obesity = mean(obesity), drink = mean(drink), diabetes = mean(diabetes), mental_health = mean(`mental health`), cancer = mean(cancer), sleeping = mean(sleeping)) %>%
  rename(state = state_abbr)

# Read the data from Wikipedia, this csv file contains mean prevalence of bad mental health.
# Some of work are done manually. 
# The raw data are in the subdictionery 'data/'

state_stat <- read_csv("data/state_stat.csv")

# It seems that function lm() and plot_ly() don't work inside render**, which leads to weird errors. 
# Therefore, we generate lm() and plot_ly() outside, which seems to be long and fool. 

drink_lm = lm(total_data$mental_health ~ total_data$drink)
diabetes_lm = lm(total_data$mental_health ~ total_data$diabetes)
cancer_lm = lm(total_data$mental_health ~ total_data$cancer)
obe_lm = lm(total_data$mental_health ~ total_data$obesity)
sleep_lm = lm(total_data$mental_health ~ total_data$sleeping)

drink_lm_state = lm(total_data_state$mental_health ~ total_data_state$drink)
diabetes_lm_state = lm(total_data_state$mental_health ~ total_data_state$diabetes)
cancer_lm_state = lm(total_data_state$mental_health ~ total_data_state$cancer)
obe_lm_state = lm(total_data_state$mental_health ~ total_data_state$obesity)
sleep_lm_state = lm(total_data_state$mental_health ~ total_data_state$sleeping)

sum_drink <- drink_lm %>% summary()
sum_diabetes <- diabetes_lm %>% summary()
sum_cancer <- cancer_lm %>% summary()
sum_obesity <- obe_lm %>% summary()
sum_sleeping <- sleep_lm %>% summary()

sum_drink_state <- drink_lm_state %>% summary()
sum_diabetes_state <- diabetes_lm_state %>% summary()
sum_cancer_state <- cancer_lm_state %>% summary()
sum_obesity_state <- obe_lm_state %>% summary()
sum_sleeping_state <- sleep_lm_state %>% summary()

plot_drink <- plot_ly(x = total_data$drink, y = total_data$mental_health, 
                      type = "scatter",
                      mode = "markers",alpha = .5, name = "point") %>% 
  add_lines(x = total_data$drink, y = fitted(drink_lm), name = "line") %>%
  layout(colorway = c('#F5B7B1', '#B03A2E'))

plot_diabetes <- plot_ly(x = total_data$diabetes, y = total_data$mental_health, 
                         type = "scatter",
                         mode = "markers",alpha = .5, name = "point") %>% 
  add_lines(x = total_data$diabetes, y = fitted(diabetes_lm), name = "line") %>%
  layout(colorway = c('#BB8FCE', '#7D3C98'))

plot_cancer <- plot_ly(x = total_data$cancer, y = total_data$mental_health, 
                       type = "scatter", 
                       mode = "markers",alpha = .5, name = "point") %>% 
  add_lines(x = total_data$cancer, y = fitted(cancer_lm), name = "line") %>%
  layout(colorway = c('#A3E4D7', '#148F77'))

plot_obesity <- plot_ly(x = total_data$obesity, y = total_data$mental_health, 
                        type = "scatter", 
                        mode = "markers",alpha = .5, name = "point") %>% 
  add_lines(x = total_data$obesity, y = fitted(obe_lm), name = "line") %>%
  layout(colorway = c('#F9E79F', '#D4AC0D'))

plot_sleeping <- plot_ly(x = total_data$sleeping, y = total_data$mental_health, 
                         type = "scatter", 
                         mode = "markers",alpha = .5, name = "point") %>% 
  add_lines(x = total_data$sleeping, y = fitted(sleep_lm), name = "line") %>%
  layout(colorway = c('#AED6F1', '#2E86C1'))

plot_drink_state <- plot_ly(x = total_data_state$drink, y = total_data_state$mental_health, 
                      type = "scatter",
                      mode = "markers",alpha = .5, name = "point") %>% 
  add_lines(x = total_data_state$drink, y = fitted(drink_lm_state), name = "line") %>%
  layout(colorway = c('#F5B7B1', '#B03A2E'))

plot_diabetes_state <- plot_ly(x = total_data_state$diabetes, y = total_data_state$mental_health, 
                         type = "scatter",
                         mode = "markers",alpha = .5, name = "point") %>% 
  add_lines(x = total_data_state$diabetes, y = fitted(diabetes_lm_state), name = "line") %>%
  layout(colorway = c('#BB8FCE', '#7D3C98'))

plot_cancer_state <- plot_ly(x = total_data_state$cancer, y = total_data_state$mental_health, 
                       type = "scatter", 
                       mode = "markers",alpha = .5, name = "point") %>% 
  add_lines(x = total_data_state$cancer, y = fitted(cancer_lm_state), name = "line") %>%
  layout(colorway = c('#A3E4D7', '#148F77'))

plot_obesity_state <- plot_ly(x = total_data_state$obesity, y = total_data_state$mental_health, 
                        type = "scatter", 
                        mode = "markers",alpha = .5, name = "point") %>% 
  add_lines(x = total_data_state$obesity, y = fitted(obe_lm_state), name = "line") %>%
  layout(colorway = c('#F9E79F', '#D4AC0D'))

plot_sleeping_state <- plot_ly(x = total_data_state$sleeping, y = total_data_state$mental_health, 
                         type = "scatter", 
                         mode = "markers",alpha = .5, name = "point") %>% 
  add_lines(x = total_data_state$sleeping, y = fitted(sleep_lm_state), name = "line") %>%
  layout(colorway = c('#AED6F1', '#2E86C1'))

income_lm <- lm(mean_crd ~ income, data = state_stat)
high_lm <- lm(mean_crd ~ high_school, data = state_stat)
bachelor_lm <- lm(mean_crd ~ bachelor, data = state_stat)
advanced_lm <- lm(mean_crd ~ advanced, data = state_stat)
white_lm <- lm(mean_crd ~ white, data = state_stat)
black_lm <- lm(mean_crd ~ black, data = state_stat)
indian_lm <- lm(mean_crd ~ indian, data = state_stat)
asian_lm <- lm(mean_crd ~ asian, data = state_stat)

sum_income <- income_lm %>% summary()
sum_high <- high_lm %>% summary()
sum_bachelor <- bachelor_lm %>% summary()
sum_advanced <- advanced_lm %>% summary()
sum_white <- white_lm %>% summary()
sum_black <- black_lm %>% summary()
sum_indian <- indian_lm %>% summary()
sum_asian <- asian_lm %>% summary()

plot_income <- plot_ly(x = state_stat$income, y = state_stat$mean_crd, 
                       type = "scatter", 
                       mode = "markers",alpha = .5, name = "point") %>% 
  add_lines(x = state_stat$income, y = fitted(income_lm), name = "line") %>%
  layout(colorway = c('#AED6F1', '#2E86C1'))

plot_high <- plot_ly(x = state_stat$high_school, y = state_stat$mean_crd, 
                       type = "scatter",
                       mode = "markers",alpha = .5, name = "point") %>% 
  add_lines(x = state_stat$high_school, y = fitted(high_lm), name = "line") %>%
  layout(colorway = c('#F5B7B1', '#B03A2E'))

plot_bachelor <- plot_ly(x = state_stat$bachelor, y = state_stat$mean_crd, 
                       type = "scatter",
                       mode = "markers",alpha = .5, name = "point") %>% 
  add_lines(x = state_stat$bachelor, y = fitted(bachelor_lm), name = "line") %>%
  layout(colorway = c('#A3E4D7', '#148F77'))

plot_advanced <- plot_ly(x = state_stat$advanced, y = state_stat$mean_crd, 
                       type = "scatter",
                       mode = "markers",alpha = .5, name = "point") %>% 
  add_lines(x = state_stat$advanced, y = fitted(advanced_lm), name = "line") %>%
  layout(colorway = c('#F9E79F', '#D4AC0D'))

plot_white <- plot_ly(x = state_stat$white, y = state_stat$mean_crd, 
                         type = "scatter",
                         mode = "markers",alpha = .5, name = "point") %>% 
  add_lines(x = state_stat$white, y = fitted(white_lm), name = "line") %>%
  layout(colorway = c('#A3E4D7', '#148F77'))

plot_black <- plot_ly(x = state_stat$black, y = state_stat$mean_crd, 
                         type = "scatter",
                         mode = "markers",alpha = .5, name = "point") %>% 
  add_lines(x = state_stat$black, y = fitted(black_lm), name = "line") %>%
  layout(colorway = c('#F9E79F', '#D4AC0D'))

plot_indian <- plot_ly(x = state_stat$indian, y = state_stat$mean_crd, 
                         type = "scatter",
                         mode = "markers",alpha = .5, name = "point") %>% 
  add_lines(x = state_stat$indian, y = fitted(indian_lm), name = "line") %>%
  layout(colorway = c('#AED6F1', '#2E86C1'))

plot_asian <- plot_ly(x = state_stat$asian, y = state_stat$mean_crd, 
                         type = "scatter",
                         mode = "markers",alpha = .5, name = "point") %>% 
  add_lines(x = state_stat$asian, y = fitted(asian_lm), name = "line") %>%
  layout(colorway = c('#F5B7B1', '#B03A2E'))


###### Start of shiny part ##################################

# Learned from Gallery in the offical website

ui <- fluidPage(
  
  titlePanel("Interactivity"),
  
  sidebarLayout(
    
    # sidebar panel
    sidebarPanel(
      # Choose the x-axis
      selectInput("x_axis", "X axis", 
                  c("income", "high_school", "bachelor", "advanced", "white", "black", "indian", "asian", "drink", "diabetes", "cancer", "obesity", "sleeping")),
      
      # Choose the scale, it is dynamic, depend on the x-axis.
      uiOutput("scale")
      
    ),
    
    # main panel
    mainPanel(
        # Print the plot
        plotlyOutput("scatter"),
        
        # Print the summary of linear model
        verbatimTextOutput("lm_summary")
      
    )
  )
)

server <- function(input, output) {
  output$scale <- renderUI({
    
    # The select Button depends on the x-axis
    switch_a <- selectInput("scale", "Scale", c("State"))
    switch_b <- selectInput("scale", "Scale", c("City", "State"))
    
    # It seem that dplyr doesn't work here, maybe conflict with html, so I use switch.
    switch(input$x_axis,
           "income" = switch_a,
           "high_school" = switch_a,
           "bachelor" = switch_a,
           "advanced" = switch_a,
           "white" = switch_a,
           "black" = switch_a,
           "indian" = switch_a,
           "asian" = switch_a,
           "drink" = switch_b, 
           "diabetes" = switch_b, 
           "cancer" = switch_b, 
           "obesity" = switch_b, 
           "sleeping" = switch_b)
  })
  
  output$scatter <- renderPlotly({
    
    # Use the nested switch code to control which plot to show
    switch(input$x_axis,
           "income" = plot_income,
           "high_school" = plot_high,
           "bachelor" = plot_bachelor,
           "advanced" = plot_advanced,
           "white" = plot_white,
           "black" = plot_black,
           "indian" = plot_indian,
           "asian" = plot_asian,
           "drink" = switch(input$scale, "City" = plot_drink, "State" = plot_drink_state),
           "diabetes" = switch(input$scale, "City" = plot_diabetes, "State" = plot_diabetes_state),
           "cancer" = switch(input$scale, "City" = plot_cancer, "State" = plot_cancer_state),
           "obesity" = switch(input$scale, "City" = plot_obesity, "State" = plot_obesity_state),
           "sleeping" = switch(input$scale, "City" = plot_sleeping, "State" = plot_sleeping_state))
  })
  
  output$lm_summary <- renderPrint({
    
    # almost the same as plot
    switch(input$x_axis,
           "income" = sum_income,
           "high_school" = sum_high,
           "bachelor" = sum_bachelor,
           "advanced" = sum_advanced,
           "white" = sum_white,
           "black" = sum_black,
           "indian" = sum_indian,
           "asian" = sum_asian,
           "drink" = switch(input$scale, "City" = sum_drink, "State" = sum_drink_state), 
           "diabetes" = switch(input$scale, "City" = sum_diabetes, "State" = sum_diabetes_state), 
           "cancer" = switch(input$scale, "City" = sum_cancer, "State" = sum_cancer_state), 
           "obesity" = switch(input$scale, "City" = sum_obesity, "State" = sum_obesity_state), 
           "sleeping" = switch(input$scale, "City" = sum_sleeping, "State" = sum_sleeping_state))
  })
}

# Build the webpage
shinyApp(ui, server)