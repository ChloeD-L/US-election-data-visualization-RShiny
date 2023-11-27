# Check uninstalled libraries
list_packages <- c('shiny','dplyr','leaflet','plotly','purrr','sf', 'maps',
                   'shinybusy','shinythemes', 'sp', 'stringr', 'tidyverse')
uninstall_packages <- list_packages[!(list_packages %in% installed.packages()[,"Package"])]
if(length(uninstall_packages)) install.packages(uninstall_packages)

# Import library
library(shiny)
library(leaflet)
library(dplyr)
library(plotly)
library(purrr)
library(sf)
library(maps)
library(htmltools)
library(shinybusy)
library(shinythemes)
library(sp)
library(stringr)
library(tidyverse)

######################## Data Pre-processing ####################################
# Define color scale
color_scale <- function(party, percentage) {
  if (party == "DEM") {
    if (percentage < 50) return("#D1E6EF")
    if (percentage >= 50 & percentage < 60) return("#73A5C8")
    if (percentage >= 60 & percentage < 70) return("#0C68A9")
    return("#03416B")
  } else if (party == "REP") {
    if (percentage < 50) return("#F5C2BE")
    if (percentage >= 50 & percentage < 60) return("#F79187")
    if (percentage >= 60 & percentage < 70) return("#CF1A19")
    return("#760D1E")
  } else {  
    return("#90EE90") 
  }
}

# Define regions for states
regions <- list(
  new_england = c("CT", "MA", "ME", "NH", "RI", "VT"),
  mid_atlantic = c("NJ", "NY", "PA"),
  east_north_central = c("IL", "IN", "MI", "OH", "WI"),
  west_north_central = c("IA", "KS", "MN", "MO", "NE", "ND", "SD"),
  south_atlantic = c("DC", "DE", "FL", "GA", "MD", "NC", "SC", "VA", "WV"),
  east_south_central = c("AL", "KY", "MS", "TN"),
  west_south_central = c("AR", "LA", "OK", "TX"),
  mountain = c("AZ", "CO", "ID", "MT", "NV", "NM", "UT", "WY"),
  pacific = c("AK", "CA", "HI", "OR", "WA")
)

region_names <- c("New England", "Mid Atlantic", "East North Central",
                  "West North Central", "South Atlantic", "East South Central",
                  "West South Central", "Mountain", "Pacific")

# Read and preprocess data
data <- read.csv("data/countypres_2000-2020.csv", header = TRUE) 

data1 <- data %>%
  filter(!is.na(totalvotes), totalvotes != 0) %>%
  mutate(
    county_name = tolower(county_name),
    state = tolower(state),
    party = ifelse(party %in% c("DEM", "REP"), party, "OTHER")
  ) %>%
  filter(mode != 'total') %>%
  group_by(year, state, state_po, county_name, county_fips, party, candidate) %>%
  summarise(
    candidatevotes = sum(candidatevotes, na.rm = TRUE),
    totalvotes = first(totalvotes),
    .groups = 'drop'
  ) %>%
  mutate(percentage = (candidatevotes / totalvotes) * 100)

# Merge winning party and party percentage data
final_data <- data1 %>%
  group_by(year, state, county_name, county_fips) %>%
  summarise(
    winning_party = party[which.max(candidatevotes)],
    win_percentage = max(percentage, na.rm = TRUE),
    dem_candidate = first(candidate[party == "DEM"]),
    rep_candidate = first(candidate[party == "REP"]),
    dem_percentage = sum(percentage[party == "DEM"], na.rm = TRUE),
    rep_percentage = sum(percentage[party == "REP"], na.rm = TRUE),
    other_percentage = sum(percentage[party == "OTHER"], na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(color = purrr::map2_chr(winning_party, win_percentage, color_scale)) %>%
  left_join(data1 %>%
              group_by(year, state, state_po, party) %>%
              summarise(state_total_votes = sum(candidatevotes, na.rm = TRUE), .groups = 'drop') %>%
              pivot_wider(names_from = party, values_from = state_total_votes, values_fill = 0) %>%
              mutate(total_votes = DEM + REP + OTHER,
                     state_dem_percentage = (DEM / total_votes) * 100,
                     state_rep_percentage = (REP / total_votes) * 100,
                     state_other_percentage = (OTHER / total_votes) * 100) %>%
              select(year, state, state_po, state_dem_percentage, state_rep_percentage, state_other_percentage),
            by = c("year", "state"))

# Find out which party won in each state and its percentage
state_final_data <- data1 %>%
  group_by(year, state, state_po) %>%
  summarise(
    total_votes = sum(candidatevotes, na.rm = TRUE),
    dem_votes = sum(candidatevotes[party == "DEM"], na.rm = TRUE),
    rep_votes = sum(candidatevotes[party == "REP"], na.rm = TRUE),
    other_votes = total_votes - dem_votes - rep_votes,
    winning_party = ifelse(dem_votes > rep_votes, "DEM", ifelse(rep_votes > dem_votes, "REP", "OTHER")),
    win_percentage = case_when(
      winning_party == "DEM" ~ (dem_votes / total_votes) * 100,
      winning_party == "REP" ~ (rep_votes / total_votes) * 100,
      TRUE ~ (other_votes / total_votes) * 100
    ),
    dem_percentage = (dem_votes / total_votes) * 100,
    rep_percentage = (rep_votes / total_votes) * 100,
    other_percentage = (other_votes / total_votes) * 100,
    dem_candidate = first(candidate[party == "DEM"]),
    rep_candidate = first(candidate[party == "REP"]),
    .groups = 'drop'
  ) %>%
  mutate(color = purrr::map2_chr(winning_party, win_percentage, color_scale)) %>%
  mutate(region = case_when(
    state_po %in% regions$new_england ~ "New England",
    state_po %in% regions$mid_atlantic ~ "Mid Atlantic",
    state_po %in% regions$east_north_central ~ "East North Central",
    state_po %in% regions$west_north_central ~ "West North Central",
    state_po %in% regions$south_atlantic ~ "South Atlantic",
    state_po %in% regions$east_south_central ~ "East South Central",
    state_po %in% regions$west_south_central ~ "West South Central",
    state_po %in% regions$mountain ~ "Mountain",
    state_po %in% regions$pacific ~ "Pacific",
    TRUE ~ NA_character_
  ))


############################# Import map #######################################
# County map
county_map <- sf::st_read("data/cb_2022_us_county_5m/cb_2022_us_county_5m.shp") %>%
  st_transform(4326) %>%
  mutate(
    NAME = tolower(NAME),
    STATE_NAME = tolower(STATE_NAME),
    GEOID = as.integer(GEOID)
  )

# State map
state_map <- sf::st_read("data/cb_2022_us_state_5m/cb_2022_us_state_5m.shp") %>%
  st_transform(4326) %>%
  mutate(NAME = tolower(NAME))

###################### Plotting function definition ###########################
## Donut plot function for page 1 and the input x is county fips, y is selected year
plot_donut <- function(x, y) {

  county_filtered <- final_data %>%
    filter(county_fips == x, year == y) %>%
    select(winning_party, win_percentage, dem_percentage, rep_percentage, other_percentage,
           dem_candidate, rep_candidate, state_dem_percentage, state_rep_percentage, state) %>%
    mutate(winning_party = toupper(winning_party),
           state = tools::toTitleCase(state))

  donut_plot <- county_filtered %>%
    plot_ly(textinfo = "percent",
            textposition = "inside",
            textfont = list(size = 10, face = "bold"),
            hovertemplate = "%{percent}<br>%{value:,.0f} votes<extra></extra>") %>%
    add_pie(labels = c("DEM", "REP", "OTHER"), values = c(county_filtered$dem_percentage, county_filtered$rep_percentage, county_filtered$other_percentage), hole = 0.7, direction = "clockwise",
            rotation = 180, sort = FALSE, domain = list(x = c(0, 1), y = c(0, 1)),
            textinfo = "none",
            marker = list(colors =  c("#0B6BA6", "#CD191D", "#E6E6E6"),
                          line = list(color = "#FFFFFF", width = 1))) %>%
    add_pie(labels = c("DEM", "REP"), values = c(county_filtered$state_dem_percentage, county_filtered$state_rep_percentage), hole = 0.8, direction = "clockwise",
            rotation = 180, sort = FALSE, domain = list(x = c(0.15, 0.85), y = c(0.15, 0.85)),
            marker = list(colors = c("#9FC4DC", "#EBA4A6", "#F5F5F5"),
                          line = list(color = "#FFFFFF", width = 1))) %>%
    add_annotations(x = 0, y = 0, xref = "x", yref = "y", text = unique(county_filtered$state),
                    xanchor = "center", showarrow = FALSE,  font = list(size = 16)) %>%
    layout(showlegend = F,
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           margin = list(l = 10, r = 10, b = 0, t = 0, pad = 20)) %>%
    config(displayModeBar = FALSE)

  return(donut_plot)
}

## Trend plot function for page 1 
plot_trend <- function(x, y) {

  county_filtered <- final_data %>%
    filter(county_fips == x) %>%
    mutate(party = str_to_title(winning_party))

  plot_tick_format = ".0f"

  trend_plot <- county_filtered %>%
    plot_ly(hovertemplate = "%{x}:<br>%{y:.0f}% <extra></extra>") %>%
    add_trace(x = ~year, y = ~dem_percentage, name = "Democrat", type = "bar", marker = list(color = "#0B6BA6", line = list(color = "#FFFFFF", width = 0.7)), width = 1.5,showlegend = FALSE) %>%
    add_trace(x = ~year, y = ~rep_percentage, name = "Republican", type = "bar", marker = list(color = "#CD191D", line = list(color = "#FFFFFF", width = 0.7)), width = 1.5,showlegend = FALSE) %>%
    add_trace(x = ~year, y = ~other_percentage, name = "Other", type = "bar", marker = list(color = "#E6E6E6", line = list(color = "#FFFFFF", width = 0.7)), width = 1.5,showlegend = FALSE) %>%
    add_trace(x = ~year, y = ~state_dem_percentage, name = "State Democrat Rate", type = "scatter", mode = "lines",
              line = list(color = "#0B6BA6", width = 1.5, dash = "dash"),
              yaxis = "y2") %>%
    add_trace(x = ~year, y = ~state_rep_percentage, name = "State Republican Rate", type = "scatter", mode = "lines",
              line = list(color = "#CD191D", width = 1.5, dash = "dash"),
              yaxis = "y2") %>%
    layout(showlegend = TRUE,
           title = "County historical voting trend",
           xaxis = list(showgrid = FALSE, zeroline = FALSE,
                        dtick = 4, tick0 = 2000, tickmode = "linear", title = "",
                        tickfont = list(size = 10)),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE,
                        side = "right", title = ""),
           yaxis2 = list(showgrid = FALSE, zeroline = FALSE, overlaying = "y",
                         side = "left", title = "", tickformat = plot_tick_format,
                         tickfont = list(size = 10)),
           legend = list(orientation = "h", yanchor = "bottom", y = -1,
                         font = list(size = 10)),
           barmode = "relative",
           hoverdistance = 5,
           margin = m <- list(l = 50, r = 25, b = 0, t = 25, pad = 10)) %>%
    config(displayModeBar = FALSE)

}

## Generate showing table for each county
generate_label <- function(NAME, STATE_NAME, year_selector, dem_percentage, rep_percentage) {
  dem_percentage <- sprintf("%.2f", dem_percentage)
  rep_percentage <- sprintf("%.2f", rep_percentage)

  paste0(
    "<strong style=\"font-size:20px;\">", tools::toTitleCase(NAME), ", ", tools::toTitleCase(STATE_NAME), "</strong><br>",
    year_selector,
    "<table style=\"font-size:14px;\">",
    ifelse(dem_percentage > rep_percentage,
           paste0("<tr><td style=\"background-color:#0B6BA6; width:5px;\"></td><td> Democrat: </td><td>", dem_percentage, " %</td></tr>",
                  "<tr><td style=\"background-color:#CD191D; width:5px;\"></td><td> Republican: </td><td>", rep_percentage, " %</td></tr>"),
           paste0("<tr><td style=\"background-color:#CD191D; width:5px;\"></td><td> Republican: </td><td>", rep_percentage, " %</td></tr>",
                  "<tr><td style=\"background-color:#0B6BA6; width:5px;\"></td><td> Democrat: </td><td>", dem_percentage, " %</td></tr>")
    ),
    "</table>"
  )
}

###################################Shiny Web App################################

## UI design
ui <- navbarPage(
  theme = shinytheme("cosmo"), title = "US Election Results (2000 - 2020)", id = "main_page",

  ## Page one UI design
  tabPanel(
    "Map", icon = icon("map", lib = "font-awesome"),
    add_busy_spinner("scaling-squares", color = "#2C3E50",
                     timeout = 1000, position = "full-page", onstart = TRUE),

    div(
      class = "outer", tags$head(includeCSS("ui.css")),
      leafletOutput("map", height = "100%", width = "100%"),

      absolutePanel(
        id = "title_panel", class = "center", top = "10", right = "410",
        width = "300", height = "185", draggable = FALSE,
        tags$style(type = "text/css", ".irs-grid-pol.small {height: Opx;}"),

        "Election Year",
        div(id = "year_selector_space",
            sliderInput("year_selector", label = NULL, sep = "",
                        min = 2000, max = 2020, value = 2020, step = 4,
                        ticks = TRUE, round = TRUE)),
        div(id = "subtext_space",
            "Click county in map to show the result table")
      )
    ),

    absolutePanel(
      id = "data_panel", class = "center", right = "0", bottom = "0", top = "40",
      width = "400", draggable = FALSE,

      fluidRow(id = "county_title",
               column(12, div(textOutput("county_value")))),

      div(
        plotlyOutput("donut_plot", height = "200px"),
        ),

      div(
        fluidRow(
          column(6,
                 div(id = "democrat_candidate_float",
                     div(id = "democrat_percent_text", textOutput("democrat_percent", inline = TRUE), " %"),
                     div(id = "democrat_candidate_text", textOutput("democrat_candidate"))
                 )
          ),
          column(6,
                 div(id = "republican_candidate_float",
                     div(id = "republican_percent_text", textOutput("republican_percent", inline = TRUE), " %"),
                     div(id = "republican_candidate_text", textOutput("republican_candidate"))
                 )
          )
        )
      ),

      hr(),

      div(
        style = " height: 250px",
        plotlyOutput("trend_plot", height = "250px")),
      )),
  
  ## Page two UI design
  tabPanel(
    "Regional results", icon = icon("coins", lib = "font-awesome"),
    fluidRow(
      column(9, # Width of the sidebar (out of 12)
             sidebarPanel(
               selectInput("regionInput", "Select Region:", choices = region_names, selected = "East North Central")
             )
      ),
      column(12, # Width of the main content (out of 12)
             div(style = "display: flex; flex-direction: column; align-items: center; justify-content: center; height: 800px;",
                 span(class = "plot_description",
                      "State voting trends for US regions",
                      "(Each horizontal row represents a state)"), p(),
                 plotlyOutput("barPlot", height = "100%", width = "100%")
             )
      )
    )
  ),
  
  ## Page three UI design
  tabPanel(
    "About Data", icon = icon("chart-area", lib = "font-awesome"),
    #HTML element used
    a("Data Source Link",href="https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/VOQCHQ"),
    br(),
    dataTableOutput("rawdata")
  )
)

## Shiny server design
server <- function(input, output, session) {

  ################################# Map page####################################
  ## Filter data according to input variety Year 
  filtered_data_reactive <- reactive({
    selected_year <- if (is.null(input$year_selector)) {
      # If not set, use the default year, such as 2020
      2020
    } else {
      input$year_selector
    }
    # Filter according to year
    data_filtered <- final_data %>% filter(year == selected_year)
    return(data_filtered)
  })
  
  ## Choose map 
  selected_map <- reactive({
    return(county_map)
  })

  ## Combine filtered data and map to generate graphical map
  final_map_reactive <- reactive({
    inner_join(county_map, filtered_data_reactive(), by = c("GEOID" = "county_fips"))
  })

  ## Generate default map
  output$map <- renderLeaflet({
    leaflet(final_map_reactive()) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        data = final_map_reactive(),
        fillColor = ~color,  # 使用final_data中的color列
        color = "Grey", weight = 0.5, fillOpacity = 0.4, smoothFactor = 0,
        highlightOptions = highlightOptions(color = "Black", weight = 0.75, bringToFront = TRUE),
        label = lapply(1:nrow(final_map_reactive()), function(i) {
          # if(input$type_selector == "State") {
          #   STATE_NAME <- final_map_reactive()$NAME[i]
          # } else {
          #   STATE_NAME <- final_map_reactive()$STATE_NAME[i]
          # }
          htmltools::HTML(generate_label(
            NAME <- final_map_reactive()$NAME[i],
            STATE_NAME <- final_map_reactive()$STATE_NAME[i],
            input$year_selector,
            dem_percentage <- final_map_reactive()$dem_percentage[i],
            rep_percentage <- final_map_reactive()$rep_percentage[i]
          ))
        }),
        labelOptions = labelOptions(style = list("padding" = "5px")),
        group = "County",
        layerId = ~GEOID # Set GEOID as id
      ) %>%
      setView(lng = -80, lat = 40, zoom = 4) %>%
      addLegend("bottomleft",
                colors = c("#03416B", "#0C68A9", "#73A5C8", "#D1E6EF", "#760D1E","#CF1A19","#F79187", "#F5C2BE"),
                labels = c("DEM >70%", "DEM >60%", "DEM >50%", "DEM <50%", "REP >70%", "REP >60%", "REP >50%","REP <50%"),
                title = "Vote Percentage by Winning Party")
  })
  
  ## Observe slide bar for chosen year and apply corresponding map
  observeEvent(input$year_selector, ignoreInit = TRUE, {
    leafletProxy("map") %>%
      clearShapes() %>%
      addPolygons(data = final_map_reactive(),
                  fillColor = ~color,
                  color = "Grey", weight = 0.5, fillOpacity = 0.4, smoothFactor = 0,
                  highlightOptions = highlightOptions(color = "Black", weight = 0.75, bringToFront = TRUE),
                  label = lapply(1:nrow(final_map_reactive()), function(i) {
                    htmltools::HTML(generate_label(
                      NAME <- final_map_reactive()$NAME[i],
                      STATE_NAME <- final_map_reactive()$STATE_NAME[i],
                      input$year_selector,
                      dem_percentage <- final_map_reactive()$dem_percentage[i],
                      rep_percentage <- final_map_reactive()$rep_percentage[i]
                    ))
                  }),
                  labelOptions = labelOptions(style = list("padding" = "5px")),
                  layerId = ~GEOID)
  })

  ## Filter for county according to user click
  filtered_county <- reactiveValues(
    data = final_data %>%
      filter(county_fips =="36061")
  )

  ## Observe user click to change donut and trend plot
  observe(filtered_county$data <- final_data %>%
            filter(county_fips == county_geoid()))

############################## Input ##########################################
  ## User click id 
  county_geoid <- reactive({
    if(is.null(input$map_shape_click) == TRUE) {
      "36061"
    } else {
      input$map_shape_click$id
    }
  })

  ## User select year
  selected_year <- reactive({
    as.numeric(input$year_selector)
  })

############################## Output ##########################################
  ## County name
  output$county_value <- renderText({
    filtered_county$data %>%
      select(county_name) %>%
      mutate(county_name = tools::toTitleCase(county_name)) %>%
      head(1) %>%
      pull()
  })

  ## State name
  output$state_value <- renderText({
    filtered_county$data %>%
      select(state) %>%
      mutate(state = tools::toTitleCase(state)) %>%
      head(1) %>%
      pull()
  })

  ## Year chosen
  output$year_value <- renderText({
    selected_year()
  })

  ## County democrat percent
  output$democrat_percent <- renderText({
    filtered_county$data %>%
      filter(year == selected_year()) %>%
      select(dem_percentage) %>%
      pull() %>%
      round(digits = 2)
  })

  ## County republican percent
  output$republican_percent <- renderText({
    filtered_county$data %>%
      filter(year == selected_year()) %>%
      select(rep_percentage) %>%
      pull() %>%
      round(digits = 2)
  })

  ## State democrat percent
  output$democrat_percent_state <- renderText({
    filtered_county$data %>%
      filter(year == selected_year()) %>%
      select(state_dem_percentage) %>%
      pull() %>%
      round(digits = 2)
  })

  ## State republican percent
  output$republican_percent_state <- renderText({
    filtered_county$data %>%
      filter(year == selected_year()) %>%
      select(state_rep_percentage) %>%
      pull() %>%
      round(digits = 2)
  })

  ## Democrat candidate
  output$democrat_candidate <- renderText({
    filtered_county$data %>%
      filter(year == selected_year()) %>%
      select(dem_candidate) %>%
      pull()
  })

  ## Republican candidate
  output$republican_candidate <- renderText({
    filtered_county$data %>%
      filter(year == selected_year()) %>%
      select(rep_candidate) %>%
      pull()
  })

  ## Output donut plot
  output$donut_plot <- renderPlotly({
    plot_donut(county_geoid(), selected_year())
  })

  ## Output trend plot
  output$trend_plot <- renderPlotly({
    plot_trend(county_geoid(), selected_year())
  })
  
  ################################# Regional result Page ##########################
  ## Output state plot
  output$barPlot <- renderPlotly({
    filtered_state_percent <- subset(state_final_data, region == input$regionInput) # Exclude 1996 data
    
    plot_tick_format = ".2f"
    
    ## Regional state voting result plot
    plots <- lapply(unique(filtered_state_percent$state), function(state_name) {
      state_data <- filtered_state_percent[filtered_state_percent$state == state_name,]
      plot_ly(data = state_data, hovertemplate = paste(tools::toTitleCase(state_name), " :<br>%{y:.2f}% <extra></extra>")) %>%
        add_trace(x = ~year, y = ~dem_percentage, name = "Democrat", type = "bar", marker = list(color = "#0B6BA6", line = list(color = "#FFFFFF", width = 0.7)), width = 1.5, showlegend = FALSE, text = ~paste0(sprintf("%.2f", dem_percentage), "%"), textposition = "inside", insidetextfont = list(color = "white", family = "Arial Black", size = 10)) %>%
        add_trace(x = ~year, y = ~other_percentage, name = "Other", type = "bar", marker = list(color = "#E6E6E6", line = list(color = "#FFFFFF", width = 0.7)), width = 1.5, showlegend = FALSE) %>%
        add_trace(x = ~year, y = ~rep_percentage, name = "Republican", type = "bar", marker = list(color = "#CD191D", line = list(color = "#FFFFFF", width = 0.7)), width = 1.5, showlegend = FALSE, text = ~paste0(sprintf("%.2f", rep_percentage), "%"), textposition = "inside", insidetextfont = list(color = "white", family = "Arial Black", size = 10)) %>%
        layout(showlegend = FALSE,
               annotations = list(
                 list(x = 1998, y = max(state_data$dem_percentage, state_data$rep_percentage, state_data$other_percentage) / 2, text = tools::toTitleCase(state_name), showarrow = FALSE, font = list(size = 15), xanchor = "right") # Move state name to the left side of the plot and capitalize the first letter
               ),
               xaxis = list(showgrid = FALSE, zeroline = FALSE,
                            dtick = 4, tick0 = 2000, tickmode = "linear", title = "",
                            tickfont = list(size = 14), tickvals = c(2000, 2004, 2008, 2012, 2016, 2020)), # Exclude 1996 from tick values
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, # Hide Y-axis tick labels
                            side = "right", title = ""), # Move Y-axis to the right
               barmode = "stack",
               hoverdistance = 5,
               margin = m <- list(l = 50, r = 50, b = 25, t = 25, pad = 10)) # Adjust right margin to make space for state names
    })
    subplot(plots, nrows = length(plots), shareX = TRUE, shareY = TRUE, titleX = FALSE) # Set titleX to FALSE
  })
  
  ################################# Data Source Page ##########################
  output$rawdata <- renderDataTable({
    data
  })
  
}

# Run the Shiny app
shinyApp(ui = ui, server = server)

