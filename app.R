library(tidyverse)
library(magrittr)
library(sf)
library(DT)
library(leaflet)
library(shiny)
library(shinydashboard)
library(htmltools)
library(scales)

elections <- read_rds('output/elections_nest.rds') %>% 
  mutate(race_num = as.numeric(str_remove_all(race, 'house(0)*')))
state_2018_approx <- read_rds('output/approx_2018_shp.rds')
candidates_2018 <- read_rds('output/house_candidates_2018.rds') %>% 
  mutate(Party = if_else(Party == 'Democratic Party', 'Dem', 'GOP'))
candidates_2016 <- read_csv('input/house_candidates.csv')
house_full <- st_read(dsn = 'output/house_f', layer = 'house_map_shp') %>%
  mutate(DIST = paste0('house',str_pad(DISTRICT, 3, 'left', '0')))
aggregate_2016 <- read_csv('input/ky_house_agg.csv')   %>%
  rename(race_num = District) %>%
  mutate(race_num = map_dbl(race_num, ~as.numeric(paste(str_extract_all(., '\\d', T), collapse = ''))),
         District = paste0('house', map_chr(race_num, ~paste(str_extract_all(., '\\d', T), collapse = ''))),
         agg_d_pct = Dem / (Dem +GOP))
incumbents <- read_csv('input/house_incumbents.csv')

# elections <- read_rds('predict_2018/output/elections_nest.rds')
# state_2018_approx <- read_rds('predict_2018/output/approx_2018_shp.rds')
# candidates_2018 <- read_rds('predict_2018/output/house_candidates_2018.rds')
# candidates_2016 <- read_csv('predict_2018/input/house_candidates.csv')
# house_full <- st_read(dsn = 'predict_2018/shapefiles/house_f', layer = 'house_map_shp') %>%
#   mutate(DIST = paste0('house',str_pad(DISTRICT, 3, 'left', '0')))
# aggregate_2016 <- read_csv('predict_2018/input/ky_house_agg.csv')  %>%
#   rename(race_num = District) %>%
#   mutate(race_num = map_dbl(race_num, ~as.numeric(paste(str_extract_all(., '\\d', T), collapse = ''))),
#          District = paste0('house', map_chr(race_num, ~paste(str_extract_all(., '\\d', T), collapse = ''))),
#          agg_d_pct = Dem / (Dem +GOP))
# incumbents <- read_csv('predict_2018/input/house_incumbents.csv')

pol_pal <- function(x) case_when(x == 'GOP' ~ '#de2d26',
                                 x == 'Dem' ~ '#3182bd',
                                 T ~ 'white')

make_table_18 <- function(race_int, d_delta, r_delta){
  elections %>% 
    filter(race_num == race_int) %>% 
    select(data) %>% 
    unnest() %>% 
    mutate(`Margin 16` = Dem - GOP,
           `Winner 16` = case_when(`Margin 16` < 0 ~ 'GOP',
                                   `Margin 16` > 0 ~ 'Dem',
                                   T ~ 'Tie'),
           `Margin 16` = abs(`Margin 16`)) %>% 
    select(Precinct, Registered, `Dem 16` = Dem, `GOP 16` = GOP, `Winner 16`) %>% 
    mutate(`Dem 18` = round(`Dem 16` * (1 + d_delta), 0), 
           `GOP 18` = round(`GOP 16` * (1 + r_delta),0), 
           `Margin 18` = `Dem 18` - `GOP 18`,
           `Winner 18` =  case_when(`Margin 18` < 0 ~ 'GOP',
                                    `Margin 18` > 0 ~ 'Dem',
                                    T ~ 'Tie'))
}

make_leaflet_18 <- function(race_int, d_delta, r_delta){
  
  df <- make_table_18(race_int, d_delta, r_delta)
  df_l <- inner_join(state_2018_approx, df, by = "Precinct")
  df_centroid <- df_l %>% 
    mutate(lon = map_dbl(geometry, ~st_centroid(.x)[[1]]),
           lat = map_dbl(geometry, ~st_centroid(.x)[[2]])) %>% 
    select(-geometry)
  
  df_centroid <- tibble(Precinct = df_centroid$Precinct,
                        lon = df_centroid$lon,
                        lat = df_centroid$lat,
                        margin = df_centroid$`Margin 18`,
                        winner = df_centroid$`Winner 18`) %>% 
    mutate(lab = map2(Precinct, margin, 
                      function(p,m) htmltools::HTML(str_glue('<b>{str_sub(p, 6)}</b><br>
                                                             <b>Margin:</b> {scales::comma_format()(m)}'))))
  leaflet(df_l) %>% 
    addTiles() %>% 
    addPolygons(
      fillOpacity = 0,
      color = 'black',
      weight = 1
    ) %>% 
    addCircleMarkers(data = df_centroid, radius = ~(margin / 25),
                     color = ~pol_pal(winner),
                     label = ~lab,
                     fillOpacity = 0.8)
}

ui <-  dashboardPage(
  dashboardHeader(title = 'Kentucky Election, 2018', titleWidth = 300),
  dashboardSidebar(
    sidebarMenu(
      menuItem('Introduction', tabName = 'intro'),
      menuItem('Statewide', tabName = 'statewide'),
      menuItem('District', tabName = 'district')
    ),
    selectInput('district_select', 'Select District', choices = 1:100),
    sliderInput('dem_delta', 'Change in Democratic Turnout', min = -100, max = 10, value = -30),
    sliderInput('gop_delta', 'Change in GOP Turnout', min = -100, max = 10, value = -45)
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = 'intro', uiOutput('app_intro')),
      tabItem(tabName = 'district',
              fluidRow(
                box(width = 12, solidHeader = F,
                    uiOutput('inc_district'),
                    uiOutput('inc_name'),
                    uiOutput('inc_party'),
                    uiOutput('inc_counties'),
                    box(width = 12, title = '2018 Candidates', 
                        DT::dataTableOutput('candidates_2018'))
                    ),
                align = 'center'
              ),
              fluidRow(
                box(width = 6, solidHeader = F, leafletOutput('district_leaflet')),
                box(width = 6, title = 'Projected Results', tableOutput('district_results'))
              ),
              fluidRow(
                box(width = 12, solidHeader = F, title = 'Precinct Detail',
                    DT::dataTableOutput('precinct_dt'))
              )
      ),
      tabItem(tabName = 'statewide',
              fluidRow(
                box(width = 12, leafletOutput('state_leaflet')),
                box(width = 12, DT::dataTableOutput('state_table'))
              )
      )
    )
  )
)
  
server <- function(input, output){
  output$inc_district <- renderUI({
    h1(str_glue('Distrct {input$district_select}'))
  })
  output$inc_name <- renderUI({
    df <- incumbents %>% 
      filter(DISTRICT == reactive({input$district_select})())
    h2(str_glue('{df$First[1]} {df$Last[1]}'))
  })
  output$inc_party <- renderUI({
    df <- incumbents %>% 
      filter(DISTRICT == reactive({input$district_select})())
    h3(if_else(df$PARTY[1] == '(D)', 'Democratic', 'Republican'))
  })
  output$inc_counties <- renderUI({
    df <- incumbents %>% 
      filter(DISTRICT == reactive({input$district_select})())
    h4(df$COUNTIES[1])
  })
  output$candidates_2018 <- DT::renderDataTable({
    candidates_2018 %>% 
      filter(District == reactive({input$district_select})()) %>%
      select(-District) %>% 
      mutate_at(c('Website', 'Facebook', 'Twitter'), ~ifelse(is.na(.),'',str_glue('<a href = {.}>link</a>'))) %>% 
      datatable(options = list(dom = 't', ordering = F), rownames = F, escape = F)
  })
  output$district_leaflet <- renderLeaflet({
    shp <- filter(elections, race_num == reactive({input$district_select})())$shp[1] %>% if_else(is.na(.), F, .)
    if(shp == T){
      make_leaflet_18(reactive({input$district_select})(),
                      reactive({input$dem_delta})() / 100,
                      reactive({input$gop_delta})() / 100)
    }else{
      leaflet(house_full %>% filter(DISTRICT == reactive({input$district_select})())) %>% 
        addTiles() %>% 
        addPolygons(
          fillOpacity = 0,
          color = 'black',
          weight = 4
        )
    }
  })
  output$district_results <- renderTable({
    race_df <- aggregate_2016 %>% 
      filter(race_num == reactive({input$district_select})())
    d_16 <- race_df$Dem[1]
    d_18 <- race_df$Dem[1] * (1 + (reactive({input$dem_delta})() / 100))
    r_16 <- race_df$GOP[1]
    r_18 <- race_df$GOP[1] * (1 + (reactive({input$gop_delta})() / 100))
    turnout_delta <- ((r_18 + d_18) - (r_16 + d_16)) / (r_16 + d_16)
    win_16 <- case_when(d_16 > r_16 ~ 'Dem',
                        d_16 < r_16 ~ 'GOP',
                        T ~ 'Tie')
    win_18 <- case_when(d_18 > r_18 ~ 'Dem',
                        d_18 < r_18 ~ 'GOP',
                        T ~ 'Tie')
    tibble(Year = c('2016', '2018'), 
           `Democratic Votes` = c(str_glue('{comma_format()(d_16)}, ({percent_format()(race_df$agg_d_pct[1])})'),
                                  str_glue('{comma_format()(round(d_18,0))}, ({percent_format()(d_18 / (d_18 + r_18))})')),
           `Republican Votes` = c(str_glue('{comma_format()(r_16)}, ({percent_format()(1 - race_df$agg_d_pct)})'),
                                  str_glue('{comma_format()(round(r_18,0))}, ({percent_format()(r_18 / (d_18 + r_18))})')),
           Winner = c(win_16, win_18),
           `Turnout Change` = c('', percent_format()(turnout_delta))
    )

  })
  output$precinct_dt <- DT::renderDataTable({
    if(reactive({input$district_select})() %in% elections$race_num){
      precinct_df <- elections %>% 
        filter(race_num == reactive({input$district_select})()) %>% 
        select(data) %>% 
        unnest() %>% 
        mutate(Registered = comma_format()(Registered),
               `Dem 18` = Dem * (1+(reactive({input$dem_delta})() / 100)),
               `GOP 18` = GOP * (1+(reactive({input$gop_delta})() / 100)),
               `Turnout Change` = percent_format()(((`Dem 18` + `GOP 18`) - (Dem + GOP)) / (Dem + GOP)),
               Precinct = str_sub(Precinct, 6), 
               `Dem Margin 16` = str_glue('{round(Dem - GOP,)} ({percent_format()((Dem - GOP) / (Dem + GOP))})'),
               `Dem 16` = str_glue('{Dem} ({percent_format()(d_pct)})'),
               `GOP 16` = str_glue('{GOP} ({percent_format()(d_pct)})'),
               `Dem Margin 18` = str_glue('{round(`Dem 18` - `GOP 18`,0)} ({percent_format()((`Dem 18` - `GOP 18`) / (`Dem 18` + `GOP 18`))})'),
               Dem_18 = str_glue('{round(`Dem 18`,0)} ({percent_format()(`Dem 18` / (`Dem 18` + `GOP 18`))})'),
               `GOP 18` = str_glue('{round(`GOP 18`,0)} ({percent_format()(`Dem 18` / (`Dem 18` + `GOP 18`))})')) %>% 
        select(Precinct, Registered, `Dem 16`, `GOP 16`, `Dem Margin 16`, `Dem 18` = Dem_18, `GOP 18`, `Dem Margin 18`,
               `Turnout Change`) %>% 
        datatable(options = list(pageLength = 1000, dom = 't'), rownames = F)
    }else{
      datatable(tibble(`Precinct Data Unavailable` = ''), options = list(dom = 't'), rownames = F)
    }
  })
  output$state_leaflet <- renderLeaflet({
    df_centroid <- house_full %>% 
      left_join(aggregate_2016 %>%
                  mutate(Dem = Dem * (1 + (reactive({input$dem_delta})() / 100)),
                         GOP = GOP * (1 + (reactive({input$gop_delta})() / 100)),
                         margin = Dem - GOP, 
                         winner = case_when(margin > 0 ~ 'Dem',
                                            margin < 0 ~ 'GOP',
                                            T ~ 'Tie'),
                         margin = abs(margin),
                         race_num = as.character(race_num)),
                by = c('DISTRICT' = 'race_num')) %>% 
      mutate(lon = map_dbl(geometry, ~st_centroid(.x)[[1]]),
             lat = map_dbl(geometry, ~st_centroid(.x)[[2]])) %>% 
      select(-geometry)
    
    df_centroid <- tibble(District = df_centroid$DISTRICT,
                          lon = df_centroid$lon,
                          lat = df_centroid$lat,
                          margin = round(df_centroid$margin,0),
                          winner = df_centroid$winner) %>% 
      mutate(lab = map2(District, margin, 
                        function(p,m) htmltools::HTML(str_glue('<b>District: </b>{p}<br>
                                                           <b>Margin:</b> {scales::comma_format()(m)}'))))
    
    leaflet(house_full) %>% 
      addTiles() %>% 
      addPolygons(
        fillOpacity = 0,
        color = 'black',
        weight = 1
      ) %>% 
      addCircleMarkers(data = df_centroid, radius = ~(margin / 1000),
                       color = ~pol_pal(winner),
                       label = ~lab,
                       fillOpacity = 0.8)
  })
  output$state_table <- DT::renderDataTable({
    state_df <- aggregate_2016 %>%
      mutate(Dem18 = Dem * (1 + (reactive({input$dem_delta})() / 100)),
             GOP18 = GOP * (1 + (reactive({input$gop_delta})() / 100)),
             margin = Dem18 - GOP18, 
             winner = case_when(margin > 0 ~ 'Dem',
                                margin < 0 ~ 'GOP',
                                T ~ 'Tie'),
             margin = abs(margin),
             race_num = as.character(race_num))
    
    dem_seats <- state_df %>% mutate(d_seat = Dem18 > GOP18) %>% filter(d_seat == T) %>% nrow()
    gop_seats <- state_df %>% mutate(d_seat = Dem18 > GOP18) %>% filter(d_seat == F) %>% nrow()
    wi_k_d <- state_df %>% mutate(d_seat = Dem18 > GOP18) %>% filter(d_seat == T, margin < 1000) %>% nrow()
    wi_k_r <- state_df %>% mutate(d_seat = Dem18 > GOP18) %>% filter(d_seat == F, margin < 1000) %>% nrow()
    turnout_delta <- ((sum(state_df$Dem18) + sum(state_df$GOP18)) - 
                        (sum(state_df$Dem) + sum(state_df$GOP))) / (sum(state_df$Dem) + sum(state_df$GOP))
      
    
    tibble(`Democratic Seats` = dem_seats,
           `GOP Seats` = gop_seats,
           `Dem Seats, <1000 Vote Margin` = wi_k_d,
           `GOP Seats, <1000 Vote Margin` = wi_k_r,
           `Total Turnout Change` = percent_format()(turnout_delta)) %>% 
      datatable(options = list(dom = 't', ordering = F), rownames = F)
  })
  output$app_intro <- renderUI({
    tags$div(
      tags$h1('Predicting the 2018 Kentucky House of Representatives'),
      tags$p('This application uses election data from 2016 to assist in forming predictions about the 2018 election for the Kentucky House of Representatives.  Use the sliders in the sidebar to estimate how turnout will change from the 2016 election for Democrats and Republicans.  The defaults are set to a 30% decrease in turnout for Democrats and a 45% decrease in turnout for Republicans -- this results in a total decrease in turnout between 2016 and 2018 of 38.4%.  That number is in line with the 39% average decrease in turnout between Presidential elections and midterm elections which has occurred over the past 4 cycles.  I selected -30% for Democrats and -45% for Republicans very loosely on the special election in District 49 which occurred earlier this year, as well as a gut feeling, and dumb optimism.'),
      tags$h2('Elements'),
      tags$p('The first page of this application is a map of all the House districts in Kentucky, with a circle drawn on top of the center of the district.  The color of the circle indicates the candidate who the sliders in the sidebar predict will win the election, and the size of the circles dictates the margin of victory.  I’ve also included a table at the bottom which shows the number of races which fell within 1,000 votes.'),
      tags$p('The second page of the application is a district detail.  Each district has different elements.  For many districts, especially those entirely within Fayette, Jefferson, Boone, Kenton, Campbell, and Pendleton counties, there are detailed maps which show the impacts of your predicts on each individual precinct.  Unfortunately, not every district has a precinct map -- while I’ve managed to receive 2016 precinct maps from the counties listed above, the last statewide precinct map was created based on the 2015 election.  Any district which has seen their precinct change between 2015 and now will not have a precinct level map.'),
      tags$p('The second page also includes other information.  At the top, I’ve included the information I found in February of 2018 about the candidates running in each district (email me at rkahne@gmail.com if you would like to see an update).  I’ve also included an aggregate table showing the impact of your predictions on the race as a whole, and a detailed table of the impact of your predictions on each individual precinct.  However, the Secretary of State only has precinct level election results in tabluar format for 83 of the 100 districts. '),
      tags$h2('Caveats and Thanks'),
      tags$p('For districts that went uncontested in 2016, I used the US Senate data as a proxy.  That might make results in Lexington and surrounding areas a little wonky, as Jim Gray greatly outperformed many House candidates in those areas.'),
      tags$p('The idea for this app came from Troy Ransdell, who built a really great Excel tool that formed a lot of logic that went into creating this application.  Troy is the best!'),
      tags$p('This app was created by me, Robert Kahne.  Feel free to use any information you find in it anywhere you like, but please provide a citation.')
    )
  })
}

shinyApp(ui = ui, server = server)

