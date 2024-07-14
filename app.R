# Field data
# things to add
# output table of fitted mean
# irradiance
# other data sets
# if you want confidence intervals, probably best to fit each spline with gradient shading Szocks

library(shiny)
library(dplyr)
library(tidyr)
library(jpeg)
library(png)
library(ggplot2)
library(shinythemes)
library(shinycssloaders)
library(bslib)

data1_comb <- read.table("data1_comb.txt", sep = "\t", header = TRUE)


options(scipen = 999)  # turn off scientific notation
#str(data1_comb)
data1_comb$deploy <- as.factor(as.character(data1_comb$deploy))



# Define UI for application
ui <- page_fillable(
  theme = bs_theme(version = 5, bootswatch = "cerulean"),
  tags$style(HTML("
    .card {
      height: 250px;  /* Adjust the height as needed */
    }
  ")),
  titlePanel("Underwater spectral profile"),
  fluidRow(
    column(4, card(
      title = "Select Deployment Year",
      selectInput("deploy_select", 
                  "Select Deployment Year:",
                  choices = unique(data1_comb$deploy))
    )),
    column(4, card(
      title = "NTU Percentiles (prob)",
      sliderInput("ntu_perc",
                  "NTU percentiles (prob)",
                  min = 0,
                  max = 1,
                  value = 0.5)
    )),
    column(4, card(
      title = "Decimal Time",
      sliderInput("time",
                  "Decimal time (0.5 * 24hr = midday)",
                  min = 0.2,
                  max = 0.8,
                  value = 0.5, step = 0.1)
    ))
  ),
  fluidRow(
    column(8,
           withSpinner(plotOutput("distPlot"))
    ),
    column(4,
           tableOutput("results")
    )
  ),
  fluidRow(
    column(12,
           "Please cite: Ricardo, G. (2024). Underwater spectral profiles in Cleveland Bay (Version 1.0.0) [Computer software]. https://github.com/gerard-ricardo/underwater-light-profile"
    )
  )
)


# # Define UI for application 
# ui <- fluidPage(
#   theme = shinytheme("cerulean"),
#   titlePanel("Underwater spectral profile"),
#   fluidRow("Underwater spectral profile for a reef site off Magnetic Island, Cleaveland Bay, Townsville. 
#   The 2017 deployment was in muddy seabed just off the reef at ~11m depth, whereas the 2018 deployment was at ~5 m on the reef.
#            Note: NTU can be converted to suspended sediment concentrations by using an approximate conversion factor of 1.1"),
#   sidebarLayout(
#     sidebarPanel(
#       selectInput("deploy_select", 
#                   "Select Deployment Year:",
#                   choices = unique(data1_comb$deploy)), 
#       sliderInput("ntu_perc",
#                   "NTU percentiles (prob)",
#                   min = 0,
#                   max = 1,
#                   value = 0.5)
#     ),
#     sidebarPanel(
#       sliderInput("time",
#                   "Decimal time (0.5 * 24hr = midday)",
#                   min = 0.2,
#                   max = 0.8,
#                   value = 0.5, step = 0.1)
#     )
#   ),
#   mainPanel(
#     withSpinner(plotOutput("distPlot", width = "1000px", height = "700px")),
#     "Please cite: Ricardo, G. (2024). Underwater spectral profiles in Cleveland Bay (Version 1.0.0) [Computer software]. https://github.com/gerard-ricardo/underwater-light-profile"
#   ),
#   fluidRow(
#     tableOutput("results")  # nm and irradiance
#   )
# )

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # Load data first
  #load("./2017_18_ntu_vs_par.RData")  # data1_comb
  #write.table(data1_comb, file = "data1_comb.txt", sep = "\t", row.names = F)

  
  # Get the current column names of the dataframe
  col_names <- names(data1_comb)
  
  # Identify numeric columns and prepend 'x' to their names
  names(data1_comb) <- ifelse(grepl("^[0-9]+$", col_names), paste0("x", col_names), col_names)
  
  # Update the select input with deployment years after data is loaded
  updateSelectInput(session, "deploy_select", choices = unique(data1_comb$deploy))
  
  output$distPlot <- renderPlot({
    req(input$ntu_perc, input$time, input$deploy_select)  # Ensure inputs are available
    
    data1_comb <- data1_comb[data1_comb$deploy == input$deploy_select, ]
    
    data1_comb$ntu <- round(data1_comb$ntu, 1)  # round to 1 dec for broader categories
    x_percentile <- input$ntu_perc  # in probs
    result_perc <- round(unname(quantile(data1_comb$ntu, probs = x_percentile)), 1)  # insert probs, return ntu
    subset_dc <- data1_comb[data1_comb$ntu == result_perc, ]  # numeric
    
    subset_dc$time <- round(subset_dc$time, 1)  # round to 1 dec for broader categories
    time_cat <- input$time  # values 
    subset_dc1 <- subset_dc[subset_dc$time == time_cat, ]  # numeric
    
    data_long <- subset_dc1 %>% pivot_longer(-c(date.time, ntu, SS, time, tot_par_a, deploy), names_to = "bin", values_to = "meas")
    nm <- c(425, 455, 485, 515, 555, 615, 660, 695)
    data_long$nm <- rep(nm, nrow(data_long) / length(nm))
    med_d3 <- data_long %>% group_by(bin) %>% summarise(med = median(meas))
    med_d3$nm <- nm
    
    img <- readPNG("spectrum.PNG")
    names_1 <- seq(425, 695, by = 1)
    spl_list <- spline(med_d3$nm, med_d3$med, n = length(names_1))
    spl_df <- data.frame(nm = spl_list$x, meas = spl_list$y)
    
    source("https://raw.githubusercontent.com/gerard-ricardo/data/master/theme_sleek3")  # set theme in code
    
    ggplot() +
      ggpubr::background_image(img) +
      geom_point(data_long, mapping = aes(x = nm, y = meas), alpha = 0.05, col = 'grey70') +
      geom_point(med_d3, mapping = aes(x = nm, y = med, alpha = 0.1), size = 3) +
      geom_line(spl_df, mapping = aes(x = nm, y = meas), size = 2, color = "steelblue4") +
      labs(x = expression(Wavelength~(nm)),
           y = expression(Irradiance~(mu~mol~m^{-2}~s^{-1}))) +
      scale_x_continuous(breaks = c(425, 455, 485, 515, 555, 615, 660, 695))
  })
  
  output$results <- renderTable({
    req(input$ntu_perc, input$time, input$deploy_select)  # Ensure inputs are available
    
    selected_data <- data1_comb[data1_comb$deploy == input$deploy_select, ]
    
    selected_data$ntu <- round(selected_data$ntu, 1)  # round to 1 dec for broader categories
    x_percentile <- input$ntu_perc  # in probs
    result_perc <- round(unname(quantile(selected_data$ntu, probs = x_percentile)), 1)  # insert probs, return ntu
    subset_dc <- selected_data[selected_data$ntu == result_perc, ]  # numeric
    
    subset_dc$time <- round(subset_dc$time, 1)  # round to 1 dec for broader categories
    time_cat <- input$time  # values 
    subset_dc1 <- subset_dc[subset_dc$time == time_cat, ]  # numeric
    
    data_long <- subset_dc1 %>% pivot_longer(-c(date.time, ntu, SS, time, tot_par_a, deploy), names_to = "bin", values_to = "meas")
    nm <- c(425, 455, 485, 515, 555, 615, 660, 695)
    data_long$nm <- rep(nm, nrow(data_long) / length(nm))
    
    df <- data.frame(median(data_long$tot_par_a))
    names(df) <- 'Total PAR'
    df
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
