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



# Define UI for application 
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("Underwater spectral profile"),
  fluidRow("Underwater spectral profile for a reef site at ~5 m depth off Magnetic Island, Cleaveland Bay, Townsville. Note: NTU can be converted to suspended sediment concentrations by using an approximate conversion factor of 1.1"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("ntu_perc",
                  "NTU percentiles (prob)",
                  min = 0,
                  max = 1,
                  value = 0.5)
    ),
    sidebarPanel(
      sliderInput("time",
                  "Decimal time (0.5 * 24hr = midday)",
                  min = 0.2,
                  max = 0.8,
                  value = 0.5, step = 0.1)
    )
  ),
  mainPanel(
    withSpinner(plotOutput("distPlot", width = "1000px", height = "700px")),
    "Please cite: Ricardo et al 2022 - XXX"
  ),
  fluidRow(
    tableOutput("results")  # nm and irradiance
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  data1_comb <- read.table(file="https://raw.githubusercontent.com/gerard-ricardo/data/master/2018%20ntu%20and%20par", header=TRUE, dec=",", na.strings=c("",".","NA"))
  options(scipen = 999)  # turn off scientific notation
  data1_comb[] <- lapply(data1_comb, function(x) as.numeric(as.character(x)))  # convert all to numeric
  
  output$distPlot <- renderPlot({
    req(input$ntu_perc, input$time)  # Ensure inputs are available
  
    data1_comb$ntu <- round(data1_comb$ntu, 1)  # round to 1 dec for broader categories
    x_percentile <- input$ntu_perc  # in probs
    result_perc <- round(unname(quantile(data1_comb$ntu, probs = x_percentile)), 1)  # insert probs, return ntu
    subset_dc <- data1_comb[data1_comb$ntu == result_perc, ]  # numeric
    
    subset_dc$time <- round(subset_dc$time, 1)  # round to 1 dec for broader categories
    time_cat <- input$time  # values 
    subset_dc1 <- subset_dc[subset_dc$time == time_cat, ]  # numeric
    
    data_long <- subset_dc1 %>% pivot_longer(-c(date.time, ntu, SS, time, tot.par.a), names_to = "bin", values_to = "meas")
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
    req(input$ntu_perc, input$time)  # Ensure inputs are available
    
    data1_comb$ntu <- round(data1_comb$ntu, 1)  # round to 1 dec for broader categories
    x_percentile <- input$ntu_perc  # in probs
    result_perc <- round(unname(quantile(data1_comb$ntu, probs = x_percentile)), 1)  # insert probs, return ntu
    subset_dc <- data1_comb[data1_comb$ntu == result_perc, ]  # numeric
    
    subset_dc$time <- round(subset_dc$time, 1)  # round to 1 dec for broader categories
    time_cat <- input$time  # values 
    subset_dc1 <- subset_dc[subset_dc$time == time_cat, ]  # numeric
    
    data_long <- subset_dc1 %>% pivot_longer(-c(date.time, ntu, SS, time, tot.par.a), names_to = "bin", values_to = "meas")
    nm <- c(425, 455, 485, 515, 555, 615, 660, 695)
    data_long$nm <- rep(nm, nrow(data_long) / length(nm))
    
    df <- data.frame(median(data_long$tot.par.a))
    names(df) <- 'Total PAR'
    df
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
