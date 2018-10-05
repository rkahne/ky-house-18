library(tidyverse)
library(magrittr)
library(sf)
library(DT)
library(leaflet)

state_2015 <- st_read(dsn = 'predict_2018/shapefiles/statewide_2015', layer = 'ky_voting_precinct_geographic_outlines_and_data') %>% 
  rename(Precinct = vtd) 

state_reduced <- state_2015 %>% 
  filter(!state_coun %in% c('21015', '21037', '21067', '21117', '21191', '21111')) %>% 
  select(Precinct, geometry)
boone <- st_read(dsn = 'predict_2018/shapefiles/boone', layer = 'BoonePrecints2') %>% 
  mutate(Precinct = paste0('21015', PRECINCTCD)) %>% 
  select(Precinct, geometry) %>% 
  st_set_crs(st_crs(state_reduced))
campbell <- st_read(dsn = 'predict_2018/shapefiles/campbell', layer = 'OGRGeoJSON') %>% 
  mutate(Precinct = paste0('21037', publishe_4)) %>% 
  select(Precinct, geometry)
fayette <- st_read(dsn = 'predict_2018/shapefiles/fayette', layer = 'VotingPrecinct') %>% 
  mutate(Precinct = paste0('21067', CODE)) %>% 
  select(Precinct, geometry)
kenton <- st_read(dsn = 'predict_2018/shapefiles/kenton', layer = 'OGRGeoJSON') %>% 
  mutate(Precinct = paste0('21117', publishe_3)) %>% 
  select(Precinct, geometry)
pendleton <- st_read(dsn = 'predict_2018/shapefiles/pendleton', layer = 'OGRGeoJSON') %>% 
  mutate(Precinct = paste0('21191', publishe_4)) %>% 
  select(Precinct, geometry)
jefferson <- st_read(dsn = 'predict_2018/shapefiles/jefferson', layer = 'precinct_ll') %>% 
  mutate(Precinct = paste0('21111', PRECINCT)) %>% 
  select(Precinct, geometry)
house_full <- st_read(dsn = 'predict_2018/shapefiles/house_f', layer = 'house_map_shp')

state_2018_approx <- rbind(boone, campbell, kenton, pendleton, fayette, jefferson, state_reduced)

# write_rds(state_2018_approx, 'predict_2018/output/approx_2018_shp.rds')


elections <- read_csv('predict_2018/output/elections_table.csv')

pol_pal <- function(x) case_when(x == 'GOP' ~ '#de2d26',
                                 x == 'Dem' ~ '#3182bd',
                                 T ~ 'white')

elections_nest <- elections %>% 
  mutate(county = str_sub(Precinct, 1, 5)) %>% 
  nest(-race) %>%
  mutate(shp = map_lgl(data, function(d){
    df <- d %>% 
      mutate(pres = case_when(county == '21015' ~ Precinct %in% boone$Precinct,
                              county == '21037' ~ Precinct %in% campbell$Precinct,
                              county == '21067' ~ Precinct %in% fayette$Precinct,
                              county == '21117' ~ Precinct %in% kenton$Precinct,
                              county == '21191' ~ Precinct %in% pendleton$Precinct,
                              county == '21111' ~ Precinct %in% jefferson$Precinct,
                              T ~ Precinct %in% state_reduced$Precinct))
    all(df$pres)
  }))

# write_rds(elections_nest, 'predict_2018/output/elections_nest.rds')

make_table_18 <- function(race_str, d_delta, r_delta){
  elections_nest %>% 
    filter(race == race_str) %>% 
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

make_leaflet_18 <- function(race_str, d_delta, r_delta){

  df <- make_table_18(race_str, d_delta, r_delta)
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

make_leaflet_18('house088', -.08, -.2)
make_table_18('house088', -.08, -.2)%>% 
  datatable(rownames = F, options = list(dom = 'pt'))

load('predict_2018/input/kyelect_data.rda')

candidates %>% 
  filter(Office == 'State Representative') %>% 
  select(-Office) %>% 
  write_rds('predict_2018/output/house_candidates_2018.rds')


df_centroid <- house_full %>% 
  left_join(aggregate_2016 %>%
              mutate(Dem = Dem * (1 + dem_delta),
                     GOP = GOP * (1 + gop_delta),
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
                    function(p,m) htmltools::HTML(str_glue('<b>{str_sub(p, 6)}</b><br>
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
