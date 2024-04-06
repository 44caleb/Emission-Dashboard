library(shiny)
library(shinydashboard)
library(bslib)
library(readxl)
library(ggplot2)
library(dplyr)
library(ggpattern)
library(hrbrthemes)
library(magick)
library(sf)
library(DT)


#loading data for the dashboard
co2_data = read_excel("data/co2_data.xlsx")
meth_data = read_excel("data/meth_data.xlsx")
map_data = map_data("world")

# processing the c02 data to remove fields with na values
co2_map = left_join(map_data, co2_data, by="region")
co2_map1 = co2_map %>% filter(!is.na(co2_map$a1990))

# processing the meth data to remove fields with na values
meth_map = left_join(map_data, meth_data, by="region")
meth_map1 = meth_map %>% filter(!is.na(meth_map$a1990))


############## choropleth map plotting function (used in the shiny app interface) #################################
plot_map = function(gas_type=co2_map1, year=co2_map1$a1990, color1="yellow") {
  
  
  map_plot = ggplot(gas_type, aes(x=long, y=lat, group=group)) +
    geom_polygon(aes(fill=year), color="grey") +
    scale_fill_gradient(low = color1, high="red", name="Gas Emissions (Kilotonnes)") +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.y=element_blank(),
          axis.title.x=element_blank(),
          rect = element_blank())
  
  map_plot
}






######## creating new data frame for use in area plot  #####################
#x -axis
recorded_years = seq(from=1990, to=2020, by=1)

# vector for total yearly emissions - y axis
total_co2_emission = c()
total_methane_emission = c()

# looping through each year (i.e the column names) to get the total emission
for (col_name in names(meth_map)[10:length(meth_map1)]) {
  
  # appends d the sum to the respective vectors
  total_co2_emission = c(total_co2_emission, sum(co2_map1[col_name]))
  total_methane_emission = c(total_methane_emission, sum(meth_map1[col_name]))
}




# data frames to be used for area plot depending on user input
methane_frame = data.frame(
  years = recorded_years,
  emissions = total_methane_emission
)

co2_frame = data.frame(
  years = recorded_years,
  emissions = total_co2_emission
)



####### function to create area plot (used in the shiny app interface)

plot_area = function(gas_type=co2_frame, color="blue") {
  #gas_type: dataframe to be used to make the plot
  
  
  ggplot(gas_type, aes(years, emissions)) + 
    geom_area_pattern(data = gas_type,
                      pattern = "gradient", 
                      fill = "#00000000",
                      pattern_fill  = "#00000000",
                      pattern_fill2 = color) + 
    geom_line(data = gas_type, colour = "black", linewidth = 0.8) +
    geom_point(shape = 16, size = 4.5, colour = "purple") +
    geom_point(shape = 16, size = 2.5, colour = "white") +
    theme_bw() +
    theme(plot.title = element_text(size = 14),
          panel.border       = element_blank(),
          axis.line.x        = element_line(),
          text               = element_text(size = 12),
          axis.ticks         = element_blank(),
          axis.text.y        = element_text(margin = margin(0,15,0,0, unit = "pt"))) +
    scale_alpha_identity() + labs(x="",y="") + 
    labs(x="Year", y="Overall Global Emission (Kilotonnes)")
  
}





#### function to create trend line plot (used in the shiny app interface) ############################

trend_plot = function(gas_type=co2_map1, country_name="USA", color1="blue") {
  ## args:
  # gas_type: represents the target dataframe to be used in the plot
  # country_name: country used to make the trend plot
  
  # data for x_axis,
  recorded_year = seq(from=1990, to=2020, by = 1)
  
  country_index = which(gas_type$region == country_name)[1]
  
  #data for y_axis. the emissions for that particular country from 1990 to 2020 (column 10 to the end)
  year_emissions = as.numeric(gas_type[country_index, 10:length(gas_type)])
  
  # creates new dataframe from target dataframe
  trend_data = data.frame(
    recorded_year = recorded_year,
    year_emissions = year_emissions
  )
  
  
  # makes the actual trend plot
  ggplot(trend_data,  aes(x=recorded_year, y=year_emissions)) +
    geom_line(color=color1) +
    theme_ipsum() +
    theme_classic() + 
    theme_bw() +
    theme(plot.title = element_text(size = 14),
          panel.border       = element_blank(),
          axis.line.x        = element_line(),
          text               = element_text(size = 12),
          axis.ticks         = element_blank(),
          axis.text.y        = element_text(margin = margin(0,15,0,0, unit = "pt"))) +
    scale_alpha_identity() + labs(x = "Year", y = "Total Gas Emissions (Kilotonnes)")
  
}





########################  MAIN APPLICATION LOGIC ############################

# UI logic for the app
ui = dashboardPage(
  
  dashboardHeader(title="EXPLORING GLOBAL EMISSION DATA", titleWidth = 650),
  
  dashboardSidebar(
    sidebarMenu(id = "sidebar",
                menuItem("Dataset", tabName = "data", icon = icon("database")),
                menuItem("Map Visualization", tabName="map_plot", icon=icon("map")),
                menuItem("Area Plot Visualization", tabName = "area_plot", icon=icon("chart-line")),
                menuItem("Trend Visualization", tabName = "trend_plot", icon=icon("chart-line"))
                
                
    )
  ),
  
  dashboardBody(
    
    tabItems(
      ## First tab item
      tabItem(tabName = "data", 
              tabBox(id="t1", width = 12, 
                     tabPanel("About", icon=icon("address-card"),
                              fluidRow(
                                column(width = 8, tags$img(src="greenhouse-gases-record-high.jpg", width =600 , height = 400),
                                       tags$br() , 
                                       tags$a("The Weisweiler coal-fired power plant in Weisweiler, Germany. Bernd Lauter / Getty Images"), align = "center"),
                                column(width = 4, tags$br() ,
                                       tags$p("Dataset obtained from the World Bank Group (https://data.worldbank.org/). Data consists of methane and co2 emissions of various countries from 1990 to 2020. The actual datasets used can also be viewed here ")
                                )
                          )
                      ),
                     
                     tabPanel("Co2 Data", DTOutput("co2_table"), icon=icon("table")),
                     
                     tabPanel("Methane Data", DTOutput("meth_table"), icon=icon("table"))
                     
              )),
      
      ## second tab item (for map)
      tabItem(
        tabName="map_plot",
        selectInput(
          "gas_type",
          label="Choose Greenhouse gas type",
          choices = c("CO2", "Methane"),
          selected="Methane"
        ),
        selectInput(
          "map_year",
          label="Select Year",
          choices=seq(from=1990, to=2020, by=1)
        ),
        card(
          card_header(h1("Overall Greenhouse Gas Emissions by Country"), 
                      plotOutput("map"))
        )
      ),
      
      ## 3rd tab item (for area plot visualisation)
      tabItem(tabName="area_plot",
              selectInput(
                "gas_type_area",
                label="Choose Greenhouse gas type",
                choices = c("CO2", "Methane"),
                selected="Methane"
              ),
              card(
                card_header(h1("Total Global Emissions (1990 to 2020)"), 
                            plotOutput("area_plot")
                            )
              )
        ),
      
      ## 4th tab item (for trend visualisation)
      tabItem(
        tabName="trend_plot",
        selectInput(
          "country_name",
          label = "Select Country",
          choices=unique(co2_map1$region)
        ),
        selectInput(
          "gas_type_trend",
          label="Choose Greenhouse gas type",
          choices = c("CO2", "Methane"),
          selected="Methane"
        ),
        card(
          card_header(h1("Greenhouse Gas Emission Trends Per Country (1990 to 2020)"), plotOutput("trend_plot"))
        ))
    
      )
    
      
    )
  )

  

###  server logic for the app
server = function(input, output) {
  output$co2_table = renderDT(
    co2_data[complete.cases(co2_data), ]
  )
  
  output$trend_plot = renderPlot({
    gas_type = switch(input$gas_type_trend,
                      "Methane" = meth_map1,
                      "CO2" = co2_map1)
    
    country_name = input$country_name
    
    color1 = switch(input$gas_type_trend,
                    "Methane" = "red",
                    "CO2" = "blue")
    
    trend_plot(gas_type, country_name, color1)
  })
  
  output$area_plot = renderPlot({
    
    gas_type = switch(input$gas_type_area,
                      "Methane" = methane_frame,
                      "CO2" = co2_frame)
    
    color1 = switch(input$gas_type_area,
                    "Methane" = "yellow",
                    "CO2" = "blue")
    
    plot_area(gas_type, color1)
  })
  
  output$meth_table = renderDT(
    meth_data[complete.cases(meth_data), ]
  )
  
  output$map = renderPlot({
    gas_type = switch(input$gas_type,
                      "Methane" = meth_map1,
                      "CO2" = co2_map1)
    
    color1 = switch(input$gas_type,
                    "Methane" = "yellow",
                    "CO2" = "blue")
    
    # I'm aware this kind of repetition is very bad code design
    #but at the moment I can't figure out an alternative. Open to suggestions from 
    # whoever sees this
    map_year = switch(input$map_year,
                      "1990" = gas_type$a1990,
                      "1991" = gas_type$a1991,
                      "1992" = gas_type$a1992,
                      "1993" = gas_type$a1993,
                      "1994" = gas_type$a1994,
                      "1995" = gas_type$a1995,
                      "1996" = gas_type$a1996,
                      "1997" = gas_type$a1997,
                      "1998" = gas_type$a1998,
                      "1999" = gas_type$a1999,
                      "2000" = gas_type$a2000,
                      "2001" = gas_type$a2001,
                      "2002" = gas_type$a2002,
                      "2003" = gas_type$a2003,
                      "2004" = gas_type$a2004,
                      "2005" = gas_type$a2005,
                      "2006" = gas_type$a2006,
                      "2007" = gas_type$a2007,
                      "2008" = gas_type$a2008,
                      "2009" = gas_type$a2009,
                      "2010" = gas_type$a2010,
                      "2011" = gas_type$a2011,
                      "2012" = gas_type$a2012,
                      "2013" = gas_type$a2013,
                      "2014" = gas_type$a2014,
                      "2015" = gas_type$a2015,
                      "2016" = gas_type$a2016,
                      "2017" = gas_type$a2017,
                      "2018" = gas_type$a2018,
                      "2019" = gas_type$a2019,
                      "2020" = gas_type$a2020
    )
    
    plot_map(gas_type, map_year, color1)
  })
}



# starts the app
shinyApp(ui = ui, server = server)
