data\_tidy
================
Jungang Zou
11/9/2019

## reading data

``` r
ufo_original_data = readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-25/ufo_sightings.csv")
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
ufo_original_data
```

    ## # A tibble: 80,332 x 11
    ##    date_time city_area state country ufo_shape encounter_length
    ##    <chr>     <chr>     <chr> <chr>   <chr>                <dbl>
    ##  1 10/10/19… san marc… tx    us      cylinder              2700
    ##  2 10/10/19… lackland… tx    <NA>    light                 7200
    ##  3 10/10/19… chester … <NA>  gb      circle                  20
    ##  4 10/10/19… edna      tx    us      circle                  20
    ##  5 10/10/19… kaneohe   hi    us      light                  900
    ##  6 10/10/19… bristol   tn    us      sphere                 300
    ##  7 10/10/19… penarth … <NA>  gb      circle                 180
    ##  8 10/10/19… norwalk   ct    us      disk                  1200
    ##  9 10/10/19… pell city al    us      disk                   180
    ## 10 10/10/19… live oak  fl    us      disk                   120
    ## # … with 80,322 more rows, and 5 more variables:
    ## #   described_encounter_length <chr>, description <chr>,
    ## #   date_documented <chr>, latitude <dbl>, longitude <dbl>

## Missing Value Detect

``` r
ufo_original_data %>% 
  map_df(.x = ., ~sum(is.na(.x))) %>% 
  knitr::kable()
```

| date\_time | city\_area | state | country | ufo\_shape | encounter\_length | described\_encounter\_length | description | date\_documented | latitude | longitude |
| ---------: | ---------: | ----: | ------: | ---------: | ----------------: | ---------------------------: | ----------: | ---------------: | -------: | --------: |
|          0 |          0 |  5797 |    9670 |       1932 |                 3 |                            0 |          15 |                0 |        1 |         0 |

We find the variable “state”, “country”, “ufo\_shape” has largest number
of missing values.

## Fill the missing value

According to the relationships of “city\_area”, “state” and “country”,
we can fill the missing value.

Since there are some abnormal data in “city\_area” like these, we first
clean the missing value:

    ## # A tibble: 12,464 x 2
    ##    city_area                               state
    ##    <chr>                                   <chr>
    ##  1 chester (uk/england)                    <NA> 
    ##  2 penarth (uk/wales)                      <NA> 
    ##  3 cardiff (uk/wales)                      <NA> 
    ##  4 stoke mandeville (uk/england)           <NA> 
    ##  5 saddle lake (canada)                    ab   
    ##  6 gisborne (new zealand)                  <NA> 
    ##  7 leeds (uk/england)                      <NA> 
    ##  8 seattle (ballard area)                  wa   
    ##  9 zlatoust (russia)                       <NA> 
    ## 10 toronto (greater toronto area) (canada) on   
    ## # … with 12,454 more rows
