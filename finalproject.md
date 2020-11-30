Final Project
================
Team 5
December 8, 2020

``` r
library(tidyverse) 
library(dsbox) 
```

``` r
covid <- read_csv("data.csv")
```

    ## Warning: Missing column names filled in: 'X1' [1]

    ## 
    ## ── Column specification ────────────────────────────────────────────────────────
    ## cols(
    ##   .default = col_double(),
    ##   geoid2 = col_character(),
    ##   date = col_date(format = ""),
    ##   date_rep = col_date(format = ""),
    ##   country = col_character(),
    ##   continent = col_character(),
    ##   region = col_character(),
    ##   distancing_bin = col_logical(),
    ##   lockdown_bin = col_logical(),
    ##   lockdown_n = col_logical(),
    ##   distancing_n = col_logical(),
    ##   days_rel_lockdown = col_logical(),
    ##   days_rel_distancing = col_logical(),
    ##   retail = col_logical(),
    ##   grocery = col_logical(),
    ##   parks = col_logical(),
    ##   transit = col_logical(),
    ##   work = col_logical(),
    ##   residential = col_logical(),
    ##   stringency = col_logical(),
    ##   C1_School.closing = col_logical()
    ##   # ... with 16 more columns
    ## )
    ## ℹ Use `spec()` for the full column specifications.

``` r
# glimpse(covid)
 names(covid)
```

    ##   [1] "X1"                                                        
    ##   [2] "geoid2"                                                    
    ##   [3] "date"                                                      
    ##   [4] "month"                                                     
    ##   [5] "day"                                                       
    ##   [6] "year"                                                      
    ##   [7] "elapsed"                                                   
    ##   [8] "date_rep"                                                  
    ##   [9] "cases"                                                     
    ##  [10] "deaths"                                                    
    ##  [11] "country"                                                   
    ##  [12] "population_2019"                                           
    ##  [13] "continent"                                                 
    ##  [14] "Cumulative_number_for_14_days_of_COVID.19_cases_per_100000"
    ##  [15] "cases_cum"                                                 
    ##  [16] "deaths_cum"                                                
    ##  [17] "deaths_cum_log"                                            
    ##  [18] "deaths_cum_l7"                                             
    ##  [19] "deaths_cum_g7"                                             
    ##  [20] "region"                                                    
    ##  [21] "gov_effect"                                                
    ##  [22] "trade"                                                     
    ##  [23] "ineq"                                                      
    ##  [24] "gdp_pc"                                                    
    ##  [25] "pop_tot"                                                   
    ##  [26] "older_m"                                                   
    ##  [27] "older_f"                                                   
    ##  [28] "air_travel"                                                
    ##  [29] "fdi"                                                       
    ##  [30] "pop_density"                                               
    ##  [31] "urban"                                                     
    ##  [32] "migration_share"                                           
    ##  [33] "oil"                                                       
    ##  [34] "soc_insur_cov"                                             
    ##  [35] "soc_contrib"                                               
    ##  [36] "soc_safety"                                                
    ##  [37] "pop_below14_2018"                                          
    ##  [38] "polity"                                                    
    ##  [39] "gini"                                                      
    ##  [40] "elf_epr"                                                   
    ##  [41] "rq_polarization"                                           
    ##  [42] "count_powerless"                                           
    ##  [43] "share_powerless"                                           
    ##  [44] "media_critical"                                            
    ##  [45] "journal_harass"                                            
    ##  [46] "health_equality"                                           
    ##  [47] "property_rights"                                           
    ##  [48] "transparent_law"                                           
    ##  [49] "bureaucracy_corrupt"                                       
    ##  [50] "polar_rile"                                                
    ##  [51] "trust_people"                                              
    ##  [52] "trust_gov"                                                 
    ##  [53] "electoral_pop"                                             
    ##  [54] "federal_ind"                                               
    ##  [55] "checks_veto"                                               
    ##  [56] "polariz_veto"                                              
    ##  [57] "dist_senate"                                               
    ##  [58] "dist_presid"                                               
    ##  [59] "dist_parlm"                                                
    ##  [60] "dist_anyelec"                                              
    ##  [61] "elect_pressure"                                            
    ##  [62] "pos_gov_lr"                                                
    ##  [63] "woman_leader"                                              
    ##  [64] "infections_mers"                                           
    ##  [65] "infections_sars"                                           
    ##  [66] "infections_ebola"                                          
    ##  [67] "infection"                                                 
    ##  [68] "med_age_2013"                                              
    ##  [69] "vdem_libdem"                                               
    ##  [70] "al_etfra"                                                  
    ##  [71] "al_religfra"                                               
    ##  [72] "fe_etfra"                                                  
    ##  [73] "vdem_mecorrpt"                                             
    ##  [74] "share_health_ins"                                          
    ##  [75] "pandemic_prep"                                             
    ##  [76] "pop_den_2018"                                              
    ##  [77] "life_exp_2017"                                             
    ##  [78] "resp_disease_prev"                                         
    ##  [79] "detect_index"                                              
    ##  [80] "doctors_pc"                                                
    ##  [81] "hosp_beds_pc"                                              
    ##  [82] "literacy_rate"                                             
    ##  [83] "healthcare_qual"                                           
    ##  [84] "acc_sanitation"                                            
    ##  [85] "health_exp_pc"                                             
    ##  [86] "hdi"                                                       
    ##  [87] "health_index"                                              
    ##  [88] "respond_index"                                             
    ##  [89] "state_fragility"                                           
    ##  [90] "pr"                                                        
    ##  [91] "share_older"                                               
    ##  [92] "pop_tot_log"                                               
    ##  [93] "pop_density_log"                                           
    ##  [94] "distancing_bin"                                            
    ##  [95] "lockdown_bin"                                              
    ##  [96] "lockdown_n"                                                
    ##  [97] "distancing_n"                                              
    ##  [98] "days_rel_lockdown"                                         
    ##  [99] "days_rel_distancing"                                       
    ## [100] "retail"                                                    
    ## [101] "grocery"                                                   
    ## [102] "parks"                                                     
    ## [103] "transit"                                                   
    ## [104] "work"                                                      
    ## [105] "residential"                                               
    ## [106] "mobility_index"                                            
    ## [107] "stringency"                                                
    ## [108] "C1_School.closing"                                         
    ## [109] "C2_Workplace.closing"                                      
    ## [110] "C3_Cancel.public.events"                                   
    ## [111] "C4_Restrictions.on.gatherings"                             
    ## [112] "C5_Close.public.transport"                                 
    ## [113] "C6_Stay.at.home.requirements"                              
    ## [114] "C7_Restrictions.on.internal.movement"                      
    ## [115] "C8_International.travel.controls"                          
    ## [116] "H1_Public.information.campaigns"                           
    ## [117] "temp_mean"                                                 
    ## [118] "precip"                                                    
    ## [119] "excess_deaths_weekly"                                      
    ## [120] "excess_deaths_source"                                      
    ## [121] "excess_deaths_last_obs"                                    
    ## [122] "excess_deaths_cum"                                         
    ## [123] "excess_deaths_cum_log"                                     
    ## [124] "deaths_cum_per_million"                                    
    ## [125] "deaths_cum_per_million_log"                                
    ## [126] "excess_deaths_cum_per_million"                             
    ## [127] "excess_deaths_cum_per_million_log"                         
    ## [128] "forcats..fct_explicit_na.geoid2."                          
    ## [129] "relative_start"                                            
    ## [130] "elapsed_rel"                                               
    ## [131] "relative_start_d"                                          
    ## [132] "elapsed_rel_d"

``` r
# Filter for the Americas 

  covid_america <- covid %>%
  filter(continent == "America")
  nrow(covid_america)
```

    ## [1] 48

``` r
covid_america
```

    ## # A tibble: 48 x 132
    ##       X1 geoid2 date       month   day  year elapsed date_rep   cases deaths
    ##    <dbl> <chr>  <date>     <dbl> <dbl> <dbl>   <dbl> <date>     <dbl>  <dbl>
    ##  1     1 ABW    2020-11-22    11    21  2020     326 2020-11-22    26      0
    ##  2     4 AIA    2020-11-22    11    21  2020     326 2020-11-22     0      0
    ##  3     8 ARG    2020-11-22    11    21  2020     326 2020-11-22  7143    112
    ##  4    10 ATG    2020-11-22    11    21  2020     326 2020-11-22     0      0
    ##  5    17 BES    2020-11-22    11    21  2020     326 2020-11-22     0      0
    ##  6    22 BHS    2020-11-22    11    21  2020     326 2020-11-22    28      0
    ##  7    25 BLZ    2020-11-22    11    21  2020     326 2020-11-22    54      5
    ##  8    26 BMU    2020-11-22    11    21  2020     326 2020-11-22     0      0
    ##  9    27 BOL    2020-11-22    11    21  2020     326 2020-11-22   167     15
    ## 10    28 BRA    2020-11-22    11    21  2020     326 2020-11-22 32622    376
    ## # … with 38 more rows, and 122 more variables: country <chr>,
    ## #   population_2019 <dbl>, continent <chr>,
    ## #   Cumulative_number_for_14_days_of_COVID.19_cases_per_100000 <dbl>,
    ## #   cases_cum <dbl>, deaths_cum <dbl>, deaths_cum_log <dbl>,
    ## #   deaths_cum_l7 <dbl>, deaths_cum_g7 <dbl>, region <chr>, gov_effect <dbl>,
    ## #   trade <dbl>, ineq <dbl>, gdp_pc <dbl>, pop_tot <dbl>, older_m <dbl>,
    ## #   older_f <dbl>, air_travel <dbl>, fdi <dbl>, pop_density <dbl>, urban <dbl>,
    ## #   migration_share <dbl>, oil <dbl>, soc_insur_cov <dbl>, soc_contrib <dbl>,
    ## #   soc_safety <dbl>, pop_below14_2018 <dbl>, polity <dbl>, gini <dbl>,
    ## #   elf_epr <dbl>, rq_polarization <dbl>, count_powerless <dbl>,
    ## #   share_powerless <dbl>, media_critical <dbl>, journal_harass <dbl>,
    ## #   health_equality <dbl>, property_rights <dbl>, transparent_law <dbl>,
    ## #   bureaucracy_corrupt <dbl>, polar_rile <dbl>, trust_people <dbl>,
    ## #   trust_gov <dbl>, electoral_pop <dbl>, federal_ind <dbl>, checks_veto <dbl>,
    ## #   polariz_veto <dbl>, dist_senate <dbl>, dist_presid <dbl>, dist_parlm <dbl>,
    ## #   dist_anyelec <dbl>, elect_pressure <dbl>, pos_gov_lr <dbl>,
    ## #   woman_leader <dbl>, infections_mers <dbl>, infections_sars <dbl>,
    ## #   infections_ebola <dbl>, infection <dbl>, med_age_2013 <dbl>,
    ## #   vdem_libdem <dbl>, al_etfra <dbl>, al_religfra <dbl>, fe_etfra <dbl>,
    ## #   vdem_mecorrpt <dbl>, share_health_ins <dbl>, pandemic_prep <dbl>,
    ## #   pop_den_2018 <dbl>, life_exp_2017 <dbl>, resp_disease_prev <dbl>,
    ## #   detect_index <dbl>, doctors_pc <dbl>, hosp_beds_pc <dbl>,
    ## #   literacy_rate <dbl>, healthcare_qual <dbl>, acc_sanitation <dbl>,
    ## #   health_exp_pc <dbl>, hdi <dbl>, health_index <dbl>, respond_index <dbl>,
    ## #   state_fragility <dbl>, pr <dbl>, share_older <dbl>, pop_tot_log <dbl>,
    ## #   pop_density_log <dbl>, distancing_bin <lgl>, lockdown_bin <lgl>,
    ## #   lockdown_n <lgl>, distancing_n <lgl>, days_rel_lockdown <lgl>,
    ## #   days_rel_distancing <lgl>, retail <lgl>, grocery <lgl>, parks <lgl>,
    ## #   transit <lgl>, work <lgl>, residential <lgl>, mobility_index <dbl>,
    ## #   stringency <lgl>, C1_School.closing <lgl>, C2_Workplace.closing <lgl>,
    ## #   C3_Cancel.public.events <lgl>, …

``` r
# Map initial glimpse of the data: cumulative deaths & cumulative cases

covid_america %>%
  ggplot(
    mapping = aes(x= deaths_cum,
                  y = cases_cum)) +
    geom_point()
```

![](finalproject_files/figure-gfm/deaths_cum,%20cases_cum-1.png)<!-- -->

``` r
# There are two obvious outliers - will remove them to enable a closer analysis. Remove by filtering cumulative cases < 2,500,000

new_covid_america <- covid_america %>% filter (cases_cum < 2500000) 
```

``` r
# Plot again cumulative cases and cumulative deaths with the new data set

new_covid_america %>%
  ggplot(
    mapping = aes(x= deaths_cum,
                  y = cases_cum)) +
    geom_point()
```

![](finalproject_files/figure-gfm/new_covid_america,%20first%20plot-1.png)<!-- -->

``` r
# Create a new variable for population over 65
# Create a new variable for mortality rate
# Now check for the variables that we are researching cumulative cases, cumulative deaths, population density, migration share

new_covid_america <- transform(new_covid_america, 
                               pop_older = older_m + older_f
                               )

new_covid_america <- transform(new_covid_america,
                              older_share = (pop_older / population_2019)*100
                              )

new_covid_america <- transform(new_covid_america, 
                              mortality = (deaths_cum / cases_cum)*100
                              )
```

``` r
  new_covid_america %>%
  ggplot(
    mapping = aes(x=pop_older,
                  y = mortality)) +
  geom_point()
```

    ## Warning: Removed 11 rows containing missing values (geom_point).

![](finalproject_files/figure-gfm/new_covid_america,%20second%20plot-1.png)<!-- -->

``` r
# Filter the different regions: Latin America & Caribbean, North America
  new_covid_america %>%
  ggplot(
    mapping = aes(x=pop_older,
                  y = mortality,
                  color = region,
                  fill = region)) +
  geom_point()  
```

    ## Warning: Removed 11 rows containing missing values (geom_point).

![](finalproject_files/figure-gfm/pop_older%20mortality%20point%20plot-1.png)<!-- -->

``` r
# geom point is not the best representation since a many points cover each other

  new_covid_america %>%
  ggplot(
    mapping = aes(x=older_share,
                  color = region,
                  fill = region)) +
  geom_density(alpha = 0.4)  
```

    ## Warning: Removed 11 rows containing non-finite values (stat_density).

    ## Warning: Groups with fewer than two data points have been dropped.

    ## Warning in max(ids, na.rm = TRUE): no non-missing arguments to max; returning -
    ## Inf

![](finalproject_files/figure-gfm/older_share%20mortality%20migration_share%20density%20plot-1.png)<!-- -->

``` r
  new_covid_america %>%
  ggplot(
    mapping = aes(x=mortality,
                  color = region,
                  fill = region)) +
  geom_density(alpha = 0.4)  
```

    ## Warning: Groups with fewer than two data points have been dropped.
    
    ## Warning: no non-missing arguments to max; returning -Inf

![](finalproject_files/figure-gfm/older_share%20mortality%20migration_share%20density%20plot-2.png)<!-- -->

``` r
  new_covid_america %>%
  ggplot(
    mapping = aes(x=migration_share,
                  color = region,
                  fill = region)) +
  geom_density(alpha = 0.4)  
```

    ## Warning: Removed 3 rows containing non-finite values (stat_density).

    ## Warning: Groups with fewer than two data points have been dropped.

    ## Warning in max(ids, na.rm = TRUE): no non-missing arguments to max; returning -
    ## Inf

![](finalproject_files/figure-gfm/older_share%20mortality%20migration_share%20density%20plot-3.png)<!-- -->
