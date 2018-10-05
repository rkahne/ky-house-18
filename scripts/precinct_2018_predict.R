options(scipen = 999)

library(tidyverse)
library(magrittr)

candidates <- read_csv('predict_2018/input/house_candidates.csv') %>% select(-race)
precincts <- read_csv('predict_2018/input/precinct_registration_processed.csv')
aggregate <- read_csv('predict_2018/input/ky_house_agg.csv') %>% 
  mutate(District = paste0('house', map_chr(District, ~paste(str_extract_all(., '\\d', T), collapse = ''))),
         agg_d_pct = Dem / (Dem +GOP))
ussen <- read_csv('predict_2018/input/ussen.csv')

elections <- tibble(file_name = list.files('precinct-level-2017-results', 'house\\d{3}.csv')) %>% 
  mutate(df = map(file_name, function(x){
    df <- suppressMessages(read_csv(str_glue('precinct-level-2017-results/{x}'))) %>% 
      gather(candidate, vote, -Precinct, -race) %>% 
      left_join(candidates, by = 'candidate') %>% 
      mutate(candidate = map_chr(str_split(candidate, '\\s'), 
                                 function(y) paste0(str_sub(y[[1]][1], 1, 1), '_', str_to_title(tail(y,1)))),
             race = str_replace_all(x, '.csv', ''),
             Precinct = as.character(Precinct))
    if(x == 'house032.csv'){
      df$Precinct <- c('21111E139', '21111E141', '21111E142', '21111E143', '21111E144', '21111E145', '21111E148',
                       '21111E152', '21111E154', '21111E155', '21111E157', '21111E158', '21111E159', '21111E160',
                       '21111E161', '21111E162', '21111E163', '21111E164', '21111E165', '21111E166', '21111E167',
                       '21111E168', '21111E169', '21111E171', '21111E173', '21111E175', '21111E176', '21111E177',
                       '21111E178', '21111E179', '21111E180', '21111E181', '21111E182', '21111E183', '21111E184',
                       '21111E185')
    }
    df
  })) %>% 
  unnest() %>% 
  select(-file_name) %>% 
  mutate(Precinct_9 = str_sub(Precinct, 1, 9)) %>% 
  left_join(precincts, by = c('Precinct_9' = 'Precinct')) %>% 
  replace_na(list(Registered = 0)) %>% 
  distinct() %>% 
  filter(!str_detect(Precinct_9, 'AB')) %>% 
  select(Precinct = Precinct_9, race, party, vote, Registered) %>% 
  replace_na(list(party = 'Ind')) %>% 
  spread(party, vote) %>%
  replace_na(list(Dem = 0, GOP = 0, Ind = 0))

uncontested <- elections %>% 
  group_by(race) %>% 
  summarize(Dem = sum(Dem), GOP = sum(GOP)) %>% 
  filter(Dem == 0 | GOP == 0)

ussen_replace <- elections %>% 
  left_join(ussen, by = 'Precinct') %>% 
  select(Precinct, race, Registered, Dem = `Jim GRAY`, GOP = `Rand PAUL`) %>% 
  filter(race %in% uncontested$race) %>% 
  mutate(Ind = 0)

elections_table <- elections %>% 
  filter(!Precinct %in% ussen_replace$Precinct) %>% 
  bind_rows(ussen_replace) %>% 
  mutate(d_pct = Dem / (Dem + GOP + Ind)) %>% 
  left_join(aggregate %>% select(District, agg_d_pct), by = c('race' = 'District')) %>% 
  mutate(good_precinct = d_pct > agg_d_pct,
         turnout = (Dem + GOP + Ind) / Registered) %>% 
  filter(!is.na(good_precinct)) %>% 
  mutate(turnout = ifelse(is.infinite(turnout), NA, turnout))

# write_csv(elections_table, 'predict_2018/output/elections_table.csv')
  
