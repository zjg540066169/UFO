frequency\_map
================
J L
11/15/2019

## 

``` r
ufo_data = readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-25/ufo_sightings.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   date_time = col_character(),
    ##   city_area = col_character(),
    ##   state = col_character(),
    ##   country = col_character(),
    ##   ufo_shape = col_character(),
    ##   encounter_length = col_double(),
    ##   described_encounter_length = col_character(),
    ##   description = col_character(),
    ##   date_documented = col_character(),
    ##   latitude = col_double(),
    ##   longitude = col_double()
    ## )

``` r
ufo =
ufo_data %>%  
na.omit(ufo_data) %>% 
separate(date_time, into = c( "date","time"), sep = " " ) %>%
separate( date, into = c("month","day","year"), sep = "/")

plot_map_below100 = ufo %>% 
  filter(country == "us") %>% 
  select(state, city_area, longitude, latitude) %>% 
  group_by(state, city_area, longitude, latitude) %>% 
  summarize(n = n()) %>% 
  filter(n <= 100) %>% 
  plot_geo(locationmode = "USA-states", sizes = c(1, 250)) %>% 
  add_markers(
    x = ~longitude, y = ~latitude, group = ~city_area, color = ~n, hoverinfo = "text",
    text = ~ paste0(city_area, "<br />", state, ", <br />", n, " times seen UFO")
  ) %>% 
  colorbar(limits = c(0, 100))

plot_map_above100 = ufo %>% 
  filter(country == "us") %>% 
  select(state, city_area, longitude, latitude) %>% 
  group_by(state, city_area, longitude, latitude) %>% 
  summarize(n = n()) %>% 
  filter(n > 100) %>% 
  plot_geo(locationmode = "USA-states", sizes = c(1, 250)) %>% 
  add_markers(
    x = ~longitude, y = ~latitude, group = ~city_area, color = ~n, hoverinfo = "text",
    text = ~ paste0(city_area, "<br />", state, ", <br />", n, " times seen UFO")
  ) %>% 
  colorbar(limits = c(100, 550))
```
