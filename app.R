library(shiny)
library(dplyr)
library(maptools) # required for rgdal to work correctly
library(tigris)
library(stringr) # to pad FIPS codes
library(leaflet)
library(shinyjs)
library(sf)

gpclibPermit()
options(tigris_use_cache = TRUE)

# Load county and state data

# exclude_states <- c("AK", "AS", "MP", "GU", "HI", "PR", "VI")
all_states <- states()

all_counties <- counties() %>%
  .[which(.$STATEFP %in% all_states$STATEFP),] %>% 
  geo_join(.,all_states[,c("STATEFP", "STUSPS", "NAME")], 
           by =  c("STATEFP"="STATEFP"),
           how = "inner")

states = c('All', sort(c("AZ","AR","DE","GA","MN","CA","DC","FL","ID","IL",
                         "IA","KY","LA","ME","MD","MI","MO","MT","NY","OR","TN","TX","VA",
                         "WI","SD","UT","IN","MA","MS","NE","NM","NC","RI","OH","OK",
                         "SC","CO","KS","CT","NV","WA","WV","WY","AL","NH","NJ","ND","PA","VT")))

my_data <- read.csv("AppData_StateRisk.csv")
country_data <- read.csv("AppData_SectorChar_Country_Top5.csv")
# region_data <- read.csv("AppData_RegionRisk.csv")
county_data <- read.csv("AppData_CountyRisk.csv")

my_data <- my_data %>% 
  mutate(FIPS_State = str_pad(FIPS_State, 2, "left", pad = "0")) %>% 
  filter(State_Abbrev != "HI")

county_data <- county_data %>% 
  mutate(FIPS_County = str_pad(FIPS_County, 5, "left", pad = "0")) %>% 
  filter(State_Abbrev != "HI")

df_merged <- geo_join(all_states, my_data, "GEOID", "FIPS_State", how = "inner")

statesummary = my_data %>% select(State_Abbrev, State_Risk2)
stateshp = sf::as_Spatial(USAboundaries::us_states())
stateshp = merge(stateshp, statesummary, by.x='state_abbr', by.y='State_Abbrev')

pal <- colorNumeric(
  palette = "PuBu",
  domain = statesummary$State_Risk2
)

pal_county <- colorNumeric(
  palette = "Reds",
  domain = county_data$County_Risk
)

county_data$htmlcounty = sprintf('%s<br />Risk Score: %s<br />', 
                            county_data$County, 
                            county_data$County_Risk_Round)


ui <- navbarPage(("Inspecting Industry: Helping OSHA identify risky regions"), id='nav',
                tabPanel("Workplace Injury Risk",
                         useShinyjs(),
                         tags$head(
                           includeCSS("styles.css")
                         ),
                         div(class="outer",
                             leafletOutput("mymap", height=690),
                             
                             absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                           draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                           width = 330, height = 900,
                                           
                                           h3('State Focus'),
                                           selectInput('state','State', states, multiple = FALSE),
                                           #actionButton("recalc", "Return to US"),
                                           
                                           
                                           h3(textOutput("selected_var8")),
                                           h4(textOutput("selected_var6")),
                                           h4(textOutput("selected_var7")),
                                           h3(textOutput("selected_var9")),
                                           h4(textOutput("selected_var0")),
                                           h4(textOutput("selected_var")),
                                           h4(textOutput("selected_var1")),
                                           h4(textOutput("selected_var2")),
                                           h4(textOutput("selected_var3")),
                                           h4(textOutput("selected_var4")),
                                           h4(textOutput("selected_var5"))
                                           )
                             )
                ),
                tabPanel("About Project",
                         tags$head(
                           # Include our custom CSS
                           includeCSS("styles.css")
                         ),
                         div(h2('Project Information'), 
                             p(paste('Inspecting Industry was developed by Sarah Rustan as part of the Insight Data Science Boston program.',
                                     'The goal of the project was to help the Occupational Safety and Health Administration optimize personnel allocation and target inspections more efficiently by predicting the regions with the highest risk of severe workplace injuries.'),
                               p('Presentation slides are available via ', 
                                 a('Google Slides.', target='_blank', href="https://docs.google.com/presentation/d/e/2PACX-1vTpt2wKNSyXmUXYEra0ANHbrb8RFSSjzRvPZBu8AWEpOL0TbhrMpYRBcGUAVDJnhO4QKwDU-eEGsQei/pub?start=false&loop=false&delayms=3000")
                             ))

                         )
                )
  
)
  
  
  
  
  
  



server <- function(input, output, session) {

  #If chunk is removed, map and sidebar generate but aren't reactive  
  observeEvent(input$recalc, {
    updateSelectInput(session, "state", selected = 'All')
    
  }, ignoreNULL = FALSE)
  
  observe({
    message(input$map_shape_click)
    if(length(input$map_shape_click$id) > 0 && is.na(as.numeric(input$map_shape_click$id))){
      updateSelectInput(session, "state", selected = input$map_shape_click$id)
    }else if(length(input$map_shape_click$id) > 0 && !is.na(as.numeric(input$map_shape_click$id))){
    }
  })
  
  output$mymap <- renderLeaflet({  

    
    if(input$state == 'All'){
      shinyjs::disable("recalc")
      substate = subset(stateshp, state_abbr %in% states)
      
      output$selected_var9 <- renderText({
        paste0("National Characteristics")
      })
      
      output$selected_var <- renderText({
        paste0("Total Employees: ",
               as.character(country_data$Num_Employees[7]))
      })
      
      output$selected_var0 <- renderText({
        paste0("",
               as.character(subset(my_data, State_Abbrev==input$state)$State_Risk2_Round))
      })
      
      output$selected_var1 <- renderText({
        paste0("Construction: ",
               as.character(country_data$AppPct_Total_Industry[1]), "%")
      })

      output$selected_var2 <- renderText({
        paste0("Manufacturing: ",
               as.character(country_data$AppPct_Total_Industry[2]), "%")
      })
      
      output$selected_var3 <- renderText({
        paste0("Wholesale Trade: ",
               as.character(country_data$AppPct_Total_Industry[3]), "%")
      })
      
      output$selected_var4 <- renderText({
        paste0("Retail: ",
               as.character(country_data$AppPct_Total_Industry[4]), "%")
      })
      
      output$selected_var5 <- renderText({
        paste0("Waste Management: ",
               as.character(country_data$AppPct_Total_Industry[5]), "%")
      })
      
      output$selected_var6 <- renderText({
        paste0(" ")
      })
      
      output$selected_var7 <- renderText({
        paste0("")
      })
      
      output$selected_var8 <- renderText({
        paste0("")
      })     
      
      leaflet(substate) %>% 
        addPolygons(fillColor = ~pal(State_Risk2), 
                    color = "#b2aeae",
                    highlightOptions = highlightOptions(color = "white", weight = 2,
                                                        bringToFront = TRUE),
                    label=substate$state_name,
                    fillOpacity = 0.7, 
                    weight = 2, 
                    smoothFactor = 0.2) %>%
        addLegend("topleft", pal = pal, values = ~State_Risk2,
                  title = "State Risk Score",bins = 5,
                  opacity = 1) %>%
        addProviderTiles(providers$CartoDB.Positron)
    }
    
    else{
      
      output$selected_var0 <- renderText({
        paste0("Risk Score: ",
               as.character(subset(my_data, State_Abbrev==input$state)$State_Risk2_Round))
      })
      
      output$selected_var <- renderText({
        paste0("Total Employees: ",
               as.character(subset(my_data, State_Abbrev==input$state)$Num_Employees_Total))
      })
      
      output$selected_var1 <- renderText({
        paste0("Construction: ",
               as.character(subset(my_data, State_Abbrev==input$state)$Pct_Construction), "%")
      })
      
      output$selected_var2 <- renderText({
        paste0("Manufacturing: ",
               as.character(subset(my_data, State_Abbrev==input$state)$Pct_Manufacturing), "%")
      })
      
      output$selected_var3 <- renderText({
        paste0("Wholesale Trade: ",
               as.character(subset(my_data, State_Abbrev==input$state)$Pct_Wholesale), "%")
      })
      
      output$selected_var4 <- renderText({
        paste0("Retail: ",
               as.character(subset(my_data, State_Abbrev==input$state)$Percent_Retail), "%")
      })
      
      output$selected_var5 <- renderText({
        paste0("Waste Management: ",
               as.character(subset(my_data, State_Abbrev==input$state)$Pct_Waste), "%")
      })

      
      output$selected_var6 <- renderText({
        paste0("Risk Model: ",
               as.character(subset(my_data, State_Abbrev==input$state)$Personnel_Model_Round))
      })
      
      output$selected_var7 <- renderText({
        paste0("Baseline Model: ",
               as.character(subset(my_data, State_Abbrev==input$state)$Population_Model_Round))
      })
      
      output$selected_var8 <- renderText({
        paste0("Personnel Allocation ")
      })     
      
      output$selected_var9 <- renderText({
        paste0("State Characteristics")
      })
      
      df_county <- dplyr::filter(county_data, 
                    State_Abbrev == input$state) %>%
        geo_join(all_counties, ., "GEOID", "FIPS_County", how = "inner")
            
      leaflet(df_county) %>%
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(fillColor = ~pal_county(County_Risk), 
                    color = "#b2aeae", # you need to use hex colors
                    fillOpacity = 0.7, 
                    weight = 1, 
                    smoothFactor = 0.2,
                    popup=~htmlcounty, stroke = TRUE) %>%
        addLegend("topleft", pal = pal_county, values = ~County_Risk,
                  title = "County Risk Score",bins = 5,
                  opacity = 1
        )
    }

    })    

}


shinyApp(ui = ui, server = server)
