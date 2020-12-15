How does the proportion of people aged 65+ affect Covid-19 mortality
across continents?
================
Team 5
December 8, 2020

``` r
library(tidyverse) 
library(dsbox) 
install.packages("magrittr") 
install.packages("dplyr")    
library(magrittr)
library(dplyr) 
install.packages("tufte")
library(ggplot2)
library(gridExtra)
```

The research done and data provided by WZB, Institutions and Political
Inequality Group stands out due to its detailed annotation. However,
even more commendable is its complete transparency with the data
collected and the way it has been manipulated. This empowers readers to,
firstly, gain a deeper understanding of the results and the workings of
statistical models, and, secondly, contribute with their own analysis.

This is exactly what my teammates and I did. The process is a lengthy
one and filled with trial and error. Firstly, one must go through all
the variables and understand what they entail. This resulted in 132
columns.

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
# names(covid)
```

Then the team ran through the data set to see its structure. Going
through the table produced, the team decided how to equally distribute
the workload. It made sense to distribute geographically. We divided the
world into three vertical strips:

  - North America, Central America and South America,

  - Europe and Africa,

  - Asia, Oceania and Australia.

I chose to analyze the first part: North America, Central America and
South America. To do this, the data set had to be filtered for these
three values and saved into a new variable. After skimming through the
data, I realized that countries belonging to all three parts are saved
under the same category for continent = America. The data set was
filtered for countries, where the continent value is set equal to
America and then saved into a new variable.

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

The new data set has 48 countries saved into rows and the same 132
variables saved into columns. For initial glimpse of the data at hand, I
plotted a simple point graph, which visualizes the relationship between
cumulative cases and cumulative deaths in the Americas to see if there
seems to be a pattern.

``` r
# Map initial glimpse of the data: cumulative deaths & cumulative cases

covid_america %>%
  ggplot(
    mapping = aes(x= deaths_cum,
                  y = cases_cum)) +
  theme_bw()+
  labs(y="Cumulative cases", x="Cumulative deaths")+
  ggtitle("Cumulative cases vs Cumulative deaths in the Americas")+
  scale_x_log10()+
    geom_point()
```

    ## Warning: Transformation introduced infinite values in continuous x-axis

![](finalproject_files/figure-gfm/deaths_cum,%20cases_cum-1.png)<!-- -->

Immediately after plotting this, it is obvious there are two extreme
outliers. This skews the plot, not allowing for careful examination of
the majority of values. These two values must be omitted. To do so a new
value is created, where only variables with cumulative cases below
2,500,000 are saved. Another variable is created that saves the two
outliers to see who these two countries are. Unsurprisingly, the country
with the most cumulative Covid-19 cases is the United States with
12,089,438 cumulative cases (at the time, when the data was collected).
The second country is Brazil with 6,052,786 cumulative cases (at the
time, when the data was collected).

``` r
# There are two obvious outliers - will remove them to enable a closer analysis. Remove by filtering cumulative cases < 2,500,000

new_covid_america <- covid_america %>% filter (cases_cum < 2500000) 
outliers <- covid_america %>% filter (cases_cum > 2500000)

outliers
```

    ## # A tibble: 2 x 132
    ##      X1 geoid2 date       month   day  year elapsed date_rep    cases deaths
    ##   <dbl> <chr>  <date>     <dbl> <dbl> <dbl>   <dbl> <date>      <dbl>  <dbl>
    ## 1    28 BRA    2020-11-22    11    21  2020     326 2020-11-22  32622    376
    ## 2   197 USA    2020-11-22    11    21  2020     326 2020-11-22 175494   1486
    ## # … with 122 more variables: country <chr>, population_2019 <dbl>,
    ## #   continent <chr>,
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

Now it is possible to delve into a more detailed analysis of the
remaining 46 countries spanning the Americas. However, the process was
not as smooth as one might think. When brainstorming, which variables
might be of interest for the team’s analysis, we decided to analyze
lockdown measures as this is something that acutely affects all of us
and frequents media headlines. After settling on the lockdown variables,
I started running through the data for the Americas. I quickly realized
that nearly all values for the lockdown variables were missing and
marked N/A. The data set had been created too early on in the pandemic
to incorporate data reflecting lockdown measures and their
effectiveness.

This idea was quickly scrapped and we had to move on to measuring
different variables. This time we immediately made sure that the
variables we are interested in had sufficient amounts of valid values.
As a team, we decided that we want to focus on variables measuring the
specificities in populations across the world as this might offer
interesting insights about human diversity across the globe. Thus, we
settled on measuring how the proportion of people aged 65+ affects
Covid-19 mortality across the globe.

To do this, I started with the initial step of measuring Covid-19
mortality. In this paper, Covid-19 mortality is measured as the
proportion of cumulative deaths when measured against cumulative cases.
Of course one must keep in mind the limitations, when measuring and
defining these two variables as different governments around the world
report data based on different definitions.

After plotting the two variables against each other, a clearer pattern
emerges. If mortality is measured as the slope of the graph, where the x
value is the number of cumulative cases and the y value is the number of
cumulative deaths, there seems to be a linear relationship until a
certain point (around 1,000,000 and 1,500,000 cumulative cases), where
mortality plateaus. There are definitely too few countries with high
cumulative death numbers to trust this pattern. Therefore, a similar
process has to be repeated, where the outlier has to be filtered out
(after creating a separate variable, we find the outlying country with
cumulative deaths greater than 7,500,000 is Mexico). Now plotting the
new variable that does not entail Mexico, the relationship is obviously
linear. We learn that, in the Americas, cumulative deaths are
proportionally related to cumulative cases. From here further research
discussing the implications and related policies from governments could
be done.

``` r
# Plot again cumulative cases and cumulative deaths with the new data set

new_covid_america %>%
  ggplot(
    mapping = aes(x= deaths_cum,
                  y = cases_cum)) +
  theme_bw()+
  
  labs(y="Cumulative cases", x="Cumulative deaths")+
  ggtitle("Cumulative cases vs Cumulative deaths in the Americas, excluding outliers")+
  
  geom_point()+
  stat_smooth(method="lm")
```

    ## `geom_smooth()` using formula 'y ~ x'

![](finalproject_files/figure-gfm/new_covid_america,%20first%20plot-1.png)<!-- -->

``` r
outlier_deaths <- new_covid_america %>% filter (deaths_cum > 75000)
outlier_deaths
```

    ## # A tibble: 1 x 132
    ##      X1 geoid2 date       month   day  year elapsed date_rep   cases deaths
    ##   <dbl> <chr>  <date>     <dbl> <dbl> <dbl>   <dbl> <date>     <dbl>  <dbl>
    ## 1   124 MEX    2020-11-22    11    21  2020     326 2020-11-22  6719    550
    ## # … with 122 more variables: country <chr>, population_2019 <dbl>,
    ## #   continent <chr>,
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
new_covid_america_without_outlier <- new_covid_america %>% filter (deaths_cum < 75000)
new_covid_america_without_outlier %>%
  ggplot(
    mapping = aes(x= deaths_cum,
                  y = cases_cum)) +
  theme_bw()+
  
  labs(y="Cumulative cases", x="Cumulative deaths")+
  ggtitle("Cumulative cases vs Cumulative deaths in the Americas, excluding outliers")+
  
  geom_point()+
  stat_smooth(method="lm")
```

    ## `geom_smooth()` using formula 'y ~ x'

![](finalproject_files/figure-gfm/new_covid_america,%20first%20plot-2.png)<!-- -->

Now after defining and analyzing mortality and its relationship to
cumulative cases and cumulative deaths, we can move on to the variables
analyzed in this paper. For this, three variables are added to the
dataset. Firstly, we must create a new variable that combines both the
female population that is 65+ and the male population that is 65+. Then,
we must look at the share of the population that is 65+ against the
total population of a country. This is saved into a new variable too.
Here we also define mortality as cumulative deaths divided by cumulative
cases.

``` r
# Create a new variable for population over 65
# Create a new variable for mortality rate
# Now check for the variables that we are researching cumulative cases, cumulative deaths, population density, migration share

new_covid_america_without_outlier <- transform(new_covid_america_without_outlier, 
                               pop_older = older_m + older_f
                               )

new_covid_america_without_outlier <- transform(new_covid_america_without_outlier,
                              older_share = (pop_older / population_2019)*100
                              )

new_covid_america_without_outlier <- transform(new_covid_america_without_outlier, 
                              mortality = (deaths_cum / cases_cum)*100
                              )



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

Now we can finally plot Covid-19 mortality against the share of
population that is 65 or older. While the uncertainty is quite high,
there seems to be a negative relationship between the two variables.
This is quite surprising as our initial hypothesis stated that countries
that have a larger proportion of citizens aged 65 or higher would see
higher Covid-19 mortality rates. The data suggests that as the share of
population aged 65 or older increases, Covid-19 mortality falls. This
might be due to the small dataset or due to powerful confounding
variables.

``` r
  new_covid_america_without_outlier %>%
  ggplot(
    mapping = aes(x=older_share,
                  y = mortality)) +
  theme_bw()+
  
  labs(y="Mortality", x="Share of population aged 65+")+
  ggtitle("Proportion of population aged 65+ vs Mortality")+
  
  geom_point()+
  stat_smooth(method="lm")
```

    ## `geom_smooth()` using formula 'y ~ x'

    ## Warning: Removed 11 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 11 rows containing missing values (geom_point).

![](finalproject_files/figure-gfm/new_covid_america,%20second%20plot-1.png)<!-- -->

To further explore the dataset, I went back to the beginning to see
further variables available for each country. The next step was to
filter the new dataset based on the region. This is where I ran into a
disadvantage of the dataset. It classifies the countries based on only
two regions:

  - North America (the US and Canada)

  - Latin America & Caribbean

  - Europe and Central Asia (Greenland)

  - N/A

There are two details that seemed quite shocking. Greenland is
classified as “Europe & Central Asia” as a region, but “America” as a
continent. After careful consideration, I decided to leave the data
point in my dataset since it would have little effect due to its small
population. Second, I was surprised that the entire continent of South
America fell under Latin America & Caribbean. After omitting the United
States as an outlier because of its extreme number of cumulative cases,
I ended up with only two countries in the North America category (Canada
and Bermuda (again, quite surprising)), one country in the Europe &
Central Asia category and the rest of the countries fell under the Latin
America & Caribbean category. Thus, further analysis based on region is
inconclusive.

``` r
# Filter the different regions: Latin America & Caribbean, North America
  new_covid_america_without_outlier %>%
  ggplot(
    mapping = aes(x=older_share,
                  y = mortality,
                  color = region,
                  fill = region)) +
 theme_bw()+
  
  labs(y="Mortality", x="Share of population aged 65+")+
  ggtitle("Proportion of population aged 65+ vs Mortality; categorized by region")+
  
  geom_point()+
  stat_smooth(method="lm")
```

    ## `geom_smooth()` using formula 'y ~ x'

    ## Warning: Removed 11 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 11 rows containing missing values (geom_point).

![](finalproject_files/figure-gfm/pop_older%20mortality%20point%20plot-1.png)<!-- -->

``` r
europe <- new_covid_america_without_outlier %>% filter (region == "Europe & Central Asia")
europe
```

    ##   X1 geoid2       date month day year elapsed   date_rep cases deaths   country
    ## 1 80    GRL 2020-11-22    11  21 2020     326 2020-11-22     0      0 Greenland
    ##   population_2019 continent
    ## 1           56660   America
    ##   Cumulative_number_for_14_days_of_COVID.19_cases_per_100000 cases_cum
    ## 1                                                      1.765        18
    ##   deaths_cum deaths_cum_log deaths_cum_l7 deaths_cum_g7                region
    ## 1          0              0             0            NA Europe & Central Asia
    ##   gov_effect  trade ineq gdp_pc pop_tot older_m older_f air_travel fdi
    ## 1      0.838 85.849   NA     NA   0.056      NA      NA         NA  NA
    ##   pop_density urban migration_share oil soc_insur_cov soc_contrib soc_safety
    ## 1       0.136    NA          10.695  NA            NA          NA         NA
    ##   pop_below14_2018 polity gini elf_epr rq_polarization count_powerless
    ## 1               NA     NA   NA      NA              NA              NA
    ##   share_powerless media_critical journal_harass health_equality property_rights
    ## 1              NA             NA             NA              NA              NA
    ##   transparent_law bureaucracy_corrupt polar_rile trust_people trust_gov
    ## 1              NA                  NA         NA           NA        NA
    ##   electoral_pop federal_ind checks_veto polariz_veto dist_senate dist_presid
    ## 1             0          NA          NA           NA          NA          NA
    ##   dist_parlm dist_anyelec elect_pressure pos_gov_lr woman_leader
    ## 1         NA           NA             NA         NA            0
    ##   infections_mers infections_sars infections_ebola infection med_age_2013
    ## 1              NA              NA               NA         0           NA
    ##   vdem_libdem al_etfra al_religfra fe_etfra vdem_mecorrpt share_health_ins
    ## 1          NA       NA          NA       NA            NA               NA
    ##   pandemic_prep pop_den_2018 life_exp_2017 resp_disease_prev detect_index
    ## 1            NA        0.136            NA             4.474           NA
    ##   doctors_pc hosp_beds_pc literacy_rate healthcare_qual acc_sanitation
    ## 1         NA           NA            NA              NA             NA
    ##   health_exp_pc hdi health_index respond_index state_fragility pr share_older
    ## 1            NA  NA           NA            NA              NA NA          NA
    ##   pop_tot_log pop_density_log distancing_bin lockdown_bin lockdown_n
    ## 1      -2.882          -1.991             NA           NA         NA
    ##   distancing_n days_rel_lockdown days_rel_distancing retail grocery parks
    ## 1           NA                NA                  NA     NA      NA    NA
    ##   transit work residential mobility_index stringency C1_School.closing
    ## 1      NA   NA          NA             NA         NA                NA
    ##   C2_Workplace.closing C3_Cancel.public.events C4_Restrictions.on.gatherings
    ## 1                   NA                      NA                            NA
    ##   C5_Close.public.transport C6_Stay.at.home.requirements
    ## 1                        NA                           NA
    ##   C7_Restrictions.on.internal.movement C8_International.travel.controls
    ## 1                                   NA                               NA
    ##   H1_Public.information.campaigns temp_mean precip excess_deaths_weekly
    ## 1                              NA     -10.7 55.867                   NA
    ##   excess_deaths_source excess_deaths_last_obs excess_deaths_cum
    ## 1                   NA                     NA                NA
    ##   excess_deaths_cum_log deaths_cum_per_million deaths_cum_per_million_log
    ## 1                    NA                      0                          0
    ##   excess_deaths_cum_per_million excess_deaths_cum_per_million_log
    ## 1                            NA                                NA
    ##   forcats..fct_explicit_na.geoid2. relative_start elapsed_rel relative_start_d
    ## 1                              GRL             88         238               NA
    ##   elapsed_rel_d pop_older older_share mortality
    ## 1            NA        NA          NA         0

``` r
north_america <- new_covid_america %>% filter (region == "North America")
north_america
```

    ##   X1 geoid2       date month day year elapsed   date_rep cases deaths country
    ## 1 26    BMU 2020-11-22    11  21 2020     326 2020-11-22     0      0 Bermuda
    ## 2 34    CAN 2020-11-22    11  21 2020     326 2020-11-22  4992     72  Canada
    ##   population_2019 continent
    ## 1           62508   America
    ## 2        37411038   America
    ##   Cumulative_number_for_14_days_of_COVID.19_cases_per_100000 cases_cum
    ## 1                                                     28.796       227
    ## 2                                                    175.499    325711
    ##   deaths_cum deaths_cum_log deaths_cum_l7 deaths_cum_g7        region
    ## 1          9          2.303             9         0.000 North America
    ## 2      11406          9.342         10891         0.047 North America
    ##   gov_effect  trade ineq gdp_pc pop_tot older_m older_f air_travel         fdi
    ## 1      1.388     NA   NA     NA   0.064      NA      NA         NA    95689122
    ## 2      1.716 66.224   NA 44.078  37.058 2927422 3458504     18.308 46542783394
    ##   pop_density  urban migration_share   oil soc_insur_cov soc_contrib soc_safety
    ## 1    1184.593     NA          30.846    NA            NA          NA         NA
    ## 2       4.075 45.933          21.802 0.894            NA      21.619         NA
    ##   pop_below14_2018 polity  gini elf_epr rq_polarization count_powerless
    ## 1               NA     NA    NA      NA              NA              NA
    ## 2            15.87     10 30.75   0.578           0.806               1
    ##   share_powerless media_critical journal_harass health_equality property_rights
    ## 1              NA             NA             NA              NA              NA
    ## 2           0.043          2.115          3.004           2.218           0.866
    ##   transparent_law bureaucracy_corrupt polar_rile trust_people trust_gov
    ## 1              NA                  NA         NA           NA        NA
    ## 2           2.306               0.034     51.245       41.111        NA
    ##   electoral_pop federal_ind checks_veto polariz_veto dist_senate dist_presid
    ## 1             0          NA          NA           NA          NA          NA
    ## 2             0       0.763           4            0          NA          NA
    ##   dist_parlm dist_anyelec elect_pressure pos_gov_lr woman_leader
    ## 1        859          859          0.001         NA            0
    ## 2       1314         1314          0.001      5.055            0
    ##   infections_mers infections_sars infections_ebola infection med_age_2013
    ## 1              NA              NA               NA         0           NA
    ## 2              NA             251               NA         1         40.1
    ##   vdem_libdem al_etfra al_religfra fe_etfra vdem_mecorrpt share_health_ins
    ## 1          NA       NA          NA       NA            NA               NA
    ## 2       0.785    0.712       0.696    0.596         3.821              100
    ##   pandemic_prep pop_den_2018 life_exp_2017 resp_disease_prev detect_index
    ## 1            NA     1184.593        81.442             3.095           NA
    ## 2          75.3        4.075        82.249             4.229         96.4
    ##   doctors_pc hosp_beds_pc literacy_rate healthcare_qual acc_sanitation
    ## 1         NA           NA            NA              NA             NA
    ## 2      253.9          270          99.9            93.8           98.5
    ##   health_exp_pc hdi health_index respond_index state_fragility pr share_older
    ## 1            NA  NA           NA            NA              NA NA          NA
    ## 2          3465 0.9         67.7          60.7               0  0      17.232
    ##   pop_tot_log pop_density_log distancing_bin lockdown_bin lockdown_n
    ## 1      -2.749           7.077             NA           NA         NA
    ## 2       3.612           1.405             NA           NA         NA
    ##   distancing_n days_rel_lockdown days_rel_distancing retail grocery parks
    ## 1           NA                NA                  NA     NA      NA    NA
    ## 2           NA                NA                  NA     NA      NA    NA
    ##   transit work residential mobility_index stringency C1_School.closing
    ## 1      NA   NA          NA             NA         NA                NA
    ## 2      NA   NA          NA          -27.4         NA                NA
    ##   C2_Workplace.closing C3_Cancel.public.events C4_Restrictions.on.gatherings
    ## 1                   NA                      NA                            NA
    ## 2                   NA                      NA                            NA
    ##   C5_Close.public.transport C6_Stay.at.home.requirements
    ## 1                        NA                           NA
    ## 2                        NA                           NA
    ##   C7_Restrictions.on.internal.movement C8_International.travel.controls
    ## 1                                   NA                               NA
    ## 2                                   NA                               NA
    ##   H1_Public.information.campaigns temp_mean precip excess_deaths_weekly
    ## 1                              NA    25.767 95.533                   NA
    ## 2                              NA     3.867 54.767                   NA
    ##   excess_deaths_source excess_deaths_last_obs excess_deaths_cum
    ## 1                   NA                     NA                NA
    ## 2                   NA                     NA                NA
    ##   excess_deaths_cum_log deaths_cum_per_million deaths_cum_per_million_log
    ## 1                    NA                140.684                      4.954
    ## 2                    NA                307.790                      5.733
    ##   excess_deaths_cum_per_million excess_deaths_cum_per_million_log
    ## 1                            NA                                NA
    ## 2                            NA                                NA
    ##   forcats..fct_explicit_na.geoid2. relative_start elapsed_rel relative_start_d
    ## 1                              BMU             86         240               NA
    ## 2                              CAN             55         271               79
    ##   elapsed_rel_d pop_older older_share mortality
    ## 1            NA        NA          NA  3.964758
    ## 2           247   6385926    17.06963  3.501877

It makes sense to further inspect mortality rates, when dividing the
dataset in three tiers:

  - Tier 1: older\_share \< 10

  - Tier 2: 10 \<= older\_share \>= 15

  - Tier 3: older\_share \> 15

We see that most countries in the Americas fit within the first tier,
which again proves that most of the countries in the Americas have young
populations. Tier 2 and tier 3 has too few data points to draw a
conclusion and tier 1 seems to counterintuitively state that countries
with older populations have lower mortality rates. This could have
multiple explanations, but most likely is affected by confounding
variables such as income levels.

``` r
first_new_covid_america_without_outlier <- new_covid_america_without_outlier %>% filter (older_share < 10)
second_new_covid_america_without_outlier <- new_covid_america_without_outlier %>% filter (older_share >= 10 & older_share <= 15)
third_new_covid_america_without_outlier <- new_covid_america_without_outlier %>% filter (older_share > 15)

first_new_covid_america_without_outlier %>%
  ggplot(
    mapping = aes(x=older_share,
                  y = mortality,
                  color = region,
                  fill = region)) +
 theme_bw()+
  
  labs(y="Mortality", x="Share of population aged 65+")+
  ggtitle("Tier 1: Proportion of population aged 65+ vs Mortality; categorized by region")+
  
  geom_point()+
  stat_smooth(method="lm")
```

    ## `geom_smooth()` using formula 'y ~ x'

![](finalproject_files/figure-gfm/older_share%20categorized%20in%20tiers-1.png)<!-- -->

``` r
second_new_covid_america_without_outlier %>%
  ggplot(
    mapping = aes(x=older_share,
                  y = mortality,
                  color = region,
                  fill = region)) +
 theme_bw()+
  
  labs(y="Mortality", x="Share of population aged 65+")+
  ggtitle("Tier 2: Proportion of population aged 65+ vs Mortality; categorized by region")+
  
  geom_point()+
  stat_smooth(method="lm")
```

    ## `geom_smooth()` using formula 'y ~ x'

![](finalproject_files/figure-gfm/older_share%20categorized%20in%20tiers-2.png)<!-- -->

``` r
third_new_covid_america_without_outlier %>%
  ggplot(
    mapping = aes(x=older_share,
                  y = mortality,
                  color = region,
                  fill = region)) +
 theme_bw()+
  
  labs(y="Mortality", x="Share of population aged 65+")+
  ggtitle("Tier 3: Proportion of population aged 65+ vs Mortality; categorized by region")+
  
  geom_point()+
  stat_smooth(method="lm")
```

    ## `geom_smooth()` using formula 'y ~ x'

![](finalproject_files/figure-gfm/older_share%20categorized%20in%20tiers-3.png)<!-- -->

A problem with the point plots is that many countries have similar
values, thus the points lie on top of each other, masking the magnitude
of the information in the data available. This is where density plots
make a great choice. Therefore, the same relationships are now revisited
through density plots in hope to discover deeper insights. Now the
overall distribution of values across the continents can be seen. Since
North America has only two values, it is not visible on the graph,
however, this shortcoming has already been discussed. Therefore, the
bulk of the analysis is about Central and South America. The graph
clearly depicts that Latin American & Caribbean countries are relatively
‘young’ countries with only a small share of population aged 65+. The
vast majority of countries are in the 5-10% range. In fact, only Puerto
Rico has more than 20% of its population aged 65+. This prompts the
question whether in the context of Latin America & Caribbean, there are
other variables that play a stronger role in the mortality of Covid-19
than the age of the population.

When inspecting the mortality density graph, it is surprising to see
that countries classified as North America, which are often considered
to be wealthier, with better health care systems, have much higher
mortality rates. Again, one must be careful to look for simple
explanations as the dataset consists of only two countries. In the case
of North America, a study inspecting Covid-19 mortality rates state by
state would prove to be more conclusive.

Inspecting Latin America & Caribbean shows that the vast majority of
countries have mortality rates below 3.5%. However, Bolivia and Ecuador
are extreme outliers that have mortality rates higher than 6%.

However, superimposing the mortality plot on top of the plot
representing the share of population aged 65+ proves inconclusive. There
seems to be little to no relationship between the share of the
population aged 65+ and its respective Covid-19 mortality rates. This
was also observed in the previous point plot, where the share of the
population aged 65+ was plotted against mortality rates and the plot
resulted in an almost horizontal plot with the slope of around 0.

``` r
# geom point is not the best representation since a many points cover each other
 new_covid_america_without_outlier %>%
 ggplot(
    mapping = aes(x=older_share,
                  color = region,
                  fill = region)) +

 theme_bw()+
  
  labs(y="Frequency", x="Share of population aged 65+")+
  ggtitle("Density plot of countries by the share of their populations aged 65+")+
  
  geom_density(alpha=0.4)
```

    ## Warning: Removed 11 rows containing non-finite values (stat_density).

    ## Warning: Groups with fewer than two data points have been dropped.

    ## Warning in max(ids, na.rm = TRUE): no non-missing arguments to max; returning -
    ## Inf

![](finalproject_files/figure-gfm/older_share%20mortality%20migration_share%20density%20plot-1.png)<!-- -->

``` r
  new_covid_america_without_outlier %>%
  ggplot(
    mapping = aes(x=mortality,
                  color = region,
                  fill = region)) +
 theme_bw()+
  
  labs(y="Frequency", x="Mortality")+
  ggtitle("Density plot of countries by their mortality rates of Covid-19")+
  
  geom_density(alpha=0.4)
```

    ## Warning: Groups with fewer than two data points have been dropped.
    
    ## Warning: no non-missing arguments to max; returning -Inf

![](finalproject_files/figure-gfm/older_share%20mortality%20migration_share%20density%20plot-2.png)<!-- -->

``` r
older <- new_covid_america_without_outlier %>% filter (older_share > 20)
older
```

    ##    X1 geoid2       date month day year elapsed   date_rep cases deaths
    ## 1 154    PRI 2020-11-22    11  21 2020     326 2020-11-22  1295     21
    ##       country population_2019 continent
    ## 1 Puerto Rico         2933404   America
    ##   Cumulative_number_for_14_days_of_COVID.19_cases_per_100000 cases_cum
    ## 1                                                    398.377     83141
    ##   deaths_cum deaths_cum_log deaths_cum_l7 deaths_cum_g7
    ## 1       1012          6.921           921         0.084
    ##                      region gov_effect   trade ineq gdp_pc pop_tot older_m
    ## 1 Latin America & Caribbean     -0.157 105.839   NA 35.085   3.195  248175
    ##   older_f air_travel fdi pop_density  urban migration_share oil soc_insur_cov
    ## 1  348577         NA  NA      360.22 76.814           7.465  NA            NA
    ##   soc_contrib soc_safety pop_below14_2018 polity   gini elf_epr rq_polarization
    ## 1          NA         NA           16.836     NA 51.057      NA              NA
    ##   count_powerless share_powerless media_critical journal_harass health_equality
    ## 1              NA              NA             NA             NA              NA
    ##   property_rights transparent_law bureaucracy_corrupt polar_rile trust_people
    ## 1              NA              NA                  NA         NA           NA
    ##   trust_gov electoral_pop federal_ind checks_veto polariz_veto dist_senate
    ## 1        NA             0          NA          NA           NA         237
    ##   dist_presid dist_parlm dist_anyelec elect_pressure pos_gov_lr woman_leader
    ## 1         237        237          237          0.004         NA            0
    ##   infections_mers infections_sars infections_ebola infection med_age_2013
    ## 1              NA              NA               NA         0           NA
    ##   vdem_libdem al_etfra al_religfra fe_etfra vdem_mecorrpt share_health_ins
    ## 1          NA       NA          NA       NA            NA               NA
    ##   pandemic_prep pop_den_2018 life_exp_2017 resp_disease_prev detect_index
    ## 1            NA       360.22        79.635             3.145           NA
    ##   doctors_pc hosp_beds_pc literacy_rate healthcare_qual acc_sanitation
    ## 1         NA           NA            NA              NA             NA
    ##   health_exp_pc hdi health_index respond_index state_fragility pr share_older
    ## 1            NA  NA           NA            NA              NA NA      18.677
    ##   pop_tot_log pop_density_log distancing_bin lockdown_bin lockdown_n
    ## 1       1.162           5.887             NA           NA         NA
    ##   distancing_n days_rel_lockdown days_rel_distancing retail grocery parks
    ## 1           NA                NA                  NA     NA      NA    NA
    ##   transit work residential mobility_index stringency C1_School.closing
    ## 1      NA   NA          NA          -45.2         NA                NA
    ##   C2_Workplace.closing C3_Cancel.public.events C4_Restrictions.on.gatherings
    ## 1                   NA                      NA                            NA
    ##   C5_Close.public.transport C6_Stay.at.home.requirements
    ## 1                        NA                           NA
    ##   C7_Restrictions.on.internal.movement C8_International.travel.controls
    ## 1                                   NA                               NA
    ##   H1_Public.information.campaigns temp_mean precip excess_deaths_weekly
    ## 1                              NA      27.1  197.4                   NA
    ##   excess_deaths_source excess_deaths_last_obs excess_deaths_cum
    ## 1                   NA                     NA                NA
    ##   excess_deaths_cum_log deaths_cum_per_million deaths_cum_per_million_log
    ## 1                    NA                 316.73                      5.761
    ##   excess_deaths_cum_per_million excess_deaths_cum_per_million_log
    ## 1                            NA                                NA
    ##   forcats..fct_explicit_na.geoid2. relative_start elapsed_rel relative_start_d
    ## 1                              PRI             87         239               92
    ##   elapsed_rel_d pop_older older_share mortality
    ## 1           234    596752    20.34333  1.217209

``` r
north_america
```

    ##   X1 geoid2       date month day year elapsed   date_rep cases deaths country
    ## 1 26    BMU 2020-11-22    11  21 2020     326 2020-11-22     0      0 Bermuda
    ## 2 34    CAN 2020-11-22    11  21 2020     326 2020-11-22  4992     72  Canada
    ##   population_2019 continent
    ## 1           62508   America
    ## 2        37411038   America
    ##   Cumulative_number_for_14_days_of_COVID.19_cases_per_100000 cases_cum
    ## 1                                                     28.796       227
    ## 2                                                    175.499    325711
    ##   deaths_cum deaths_cum_log deaths_cum_l7 deaths_cum_g7        region
    ## 1          9          2.303             9         0.000 North America
    ## 2      11406          9.342         10891         0.047 North America
    ##   gov_effect  trade ineq gdp_pc pop_tot older_m older_f air_travel         fdi
    ## 1      1.388     NA   NA     NA   0.064      NA      NA         NA    95689122
    ## 2      1.716 66.224   NA 44.078  37.058 2927422 3458504     18.308 46542783394
    ##   pop_density  urban migration_share   oil soc_insur_cov soc_contrib soc_safety
    ## 1    1184.593     NA          30.846    NA            NA          NA         NA
    ## 2       4.075 45.933          21.802 0.894            NA      21.619         NA
    ##   pop_below14_2018 polity  gini elf_epr rq_polarization count_powerless
    ## 1               NA     NA    NA      NA              NA              NA
    ## 2            15.87     10 30.75   0.578           0.806               1
    ##   share_powerless media_critical journal_harass health_equality property_rights
    ## 1              NA             NA             NA              NA              NA
    ## 2           0.043          2.115          3.004           2.218           0.866
    ##   transparent_law bureaucracy_corrupt polar_rile trust_people trust_gov
    ## 1              NA                  NA         NA           NA        NA
    ## 2           2.306               0.034     51.245       41.111        NA
    ##   electoral_pop federal_ind checks_veto polariz_veto dist_senate dist_presid
    ## 1             0          NA          NA           NA          NA          NA
    ## 2             0       0.763           4            0          NA          NA
    ##   dist_parlm dist_anyelec elect_pressure pos_gov_lr woman_leader
    ## 1        859          859          0.001         NA            0
    ## 2       1314         1314          0.001      5.055            0
    ##   infections_mers infections_sars infections_ebola infection med_age_2013
    ## 1              NA              NA               NA         0           NA
    ## 2              NA             251               NA         1         40.1
    ##   vdem_libdem al_etfra al_religfra fe_etfra vdem_mecorrpt share_health_ins
    ## 1          NA       NA          NA       NA            NA               NA
    ## 2       0.785    0.712       0.696    0.596         3.821              100
    ##   pandemic_prep pop_den_2018 life_exp_2017 resp_disease_prev detect_index
    ## 1            NA     1184.593        81.442             3.095           NA
    ## 2          75.3        4.075        82.249             4.229         96.4
    ##   doctors_pc hosp_beds_pc literacy_rate healthcare_qual acc_sanitation
    ## 1         NA           NA            NA              NA             NA
    ## 2      253.9          270          99.9            93.8           98.5
    ##   health_exp_pc hdi health_index respond_index state_fragility pr share_older
    ## 1            NA  NA           NA            NA              NA NA          NA
    ## 2          3465 0.9         67.7          60.7               0  0      17.232
    ##   pop_tot_log pop_density_log distancing_bin lockdown_bin lockdown_n
    ## 1      -2.749           7.077             NA           NA         NA
    ## 2       3.612           1.405             NA           NA         NA
    ##   distancing_n days_rel_lockdown days_rel_distancing retail grocery parks
    ## 1           NA                NA                  NA     NA      NA    NA
    ## 2           NA                NA                  NA     NA      NA    NA
    ##   transit work residential mobility_index stringency C1_School.closing
    ## 1      NA   NA          NA             NA         NA                NA
    ## 2      NA   NA          NA          -27.4         NA                NA
    ##   C2_Workplace.closing C3_Cancel.public.events C4_Restrictions.on.gatherings
    ## 1                   NA                      NA                            NA
    ## 2                   NA                      NA                            NA
    ##   C5_Close.public.transport C6_Stay.at.home.requirements
    ## 1                        NA                           NA
    ## 2                        NA                           NA
    ##   C7_Restrictions.on.internal.movement C8_International.travel.controls
    ## 1                                   NA                               NA
    ## 2                                   NA                               NA
    ##   H1_Public.information.campaigns temp_mean precip excess_deaths_weekly
    ## 1                              NA    25.767 95.533                   NA
    ## 2                              NA     3.867 54.767                   NA
    ##   excess_deaths_source excess_deaths_last_obs excess_deaths_cum
    ## 1                   NA                     NA                NA
    ## 2                   NA                     NA                NA
    ##   excess_deaths_cum_log deaths_cum_per_million deaths_cum_per_million_log
    ## 1                    NA                140.684                      4.954
    ## 2                    NA                307.790                      5.733
    ##   excess_deaths_cum_per_million excess_deaths_cum_per_million_log
    ## 1                            NA                                NA
    ## 2                            NA                                NA
    ##   forcats..fct_explicit_na.geoid2. relative_start elapsed_rel relative_start_d
    ## 1                              BMU             86         240               NA
    ## 2                              CAN             55         271               79
    ##   elapsed_rel_d pop_older older_share mortality
    ## 1            NA        NA          NA  3.964758
    ## 2           247   6385926    17.06963  3.501877

``` r
deadly <-new_covid_america_without_outlier %>% filter (mortality > 6)
deadly
```

    ##   X1 geoid2       date month day year elapsed   date_rep cases deaths country
    ## 1 27    BOL 2020-11-22    11  21 2020     326 2020-11-22   167     15 Bolivia
    ## 2 57    ECU 2020-11-22    11  21 2020     326 2020-11-22  1036     44 Ecuador
    ##   population_2019 continent
    ## 1        11513102   America
    ## 2        17373657   America
    ##   Cumulative_number_for_14_days_of_COVID.19_cases_per_100000 cases_cum
    ## 1                                                     12.985    143922
    ## 2                                                     65.559    184876
    ##   deaths_cum deaths_cum_log deaths_cum_l7 deaths_cum_g7
    ## 1       8904          9.094          8841         0.006
    ## 2      13139          9.483         12997         0.009
    ##                      region gov_effect  trade ineq gdp_pc pop_tot older_m
    ## 1 Latin America & Caribbean     -0.322 57.110 30.4  6.986  11.353  377907
    ## 2 Latin America & Caribbean     -0.261 45.867 34.4 10.412  17.084  565818
    ##   older_f air_travel        fdi pop_density  urban migration_share   oil
    ## 1  438604     15.232  255082886      10.480 41.336           1.333 1.320
    ## 2  656958     15.495 1410435145      68.789 27.635           2.400 4.957
    ##   soc_insur_cov soc_contrib soc_safety pop_below14_2018 polity   gini elf_epr
    ## 1         7.771          NA      76.88           31.074      7 43.587   0.672
    ## 2        10.930          NA      67.05           28.029      5 42.737   0.327
    ##   rq_polarization count_powerless share_powerless media_critical journal_harass
    ## 1           0.853               1            0.03          0.269          0.082
    ## 2           0.560               3            0.19          0.518          1.128
    ##   health_equality property_rights transparent_law bureaucracy_corrupt
    ## 1          -0.709           0.648           0.642               0.465
    ## 2           0.649           0.837           1.075               0.365
    ##   polar_rile trust_people trust_gov electoral_pop federal_ind checks_veto
    ## 1         NA       15.713    33.474             0       0.061           2
    ## 2         NA       14.154    26.163             0      -0.375           2
    ##   polariz_veto dist_senate dist_presid dist_parlm dist_anyelec elect_pressure
    ## 1            0          53          53         53           53          0.019
    ## 2            0          NA         345        345          345          0.003
    ##   pos_gov_lr woman_leader infections_mers infections_sars infections_ebola
    ## 1         NA            1              NA              NA               NA
    ## 2         NA            0              NA              NA               NA
    ##   infection med_age_2013 vdem_libdem al_etfra al_religfra fe_etfra
    ## 1         0         22.4       0.420    0.740       0.208    0.743
    ## 2         0         26.1       0.295    0.655       0.142    0.655
    ##   vdem_mecorrpt share_health_ins pandemic_prep pop_den_2018 life_exp_2017
    ## 1         3.180             42.7          35.8       10.480        70.945
    ## 2         2.675             22.8          50.1       68.789        76.584
    ##   resp_disease_prev detect_index doctors_pc hosp_beds_pc literacy_rate
    ## 1             3.445         33.1       47.3          110          92.5
    ## 2             3.772         71.2      166.5          150          94.4
    ##   healthcare_qual acc_sanitation health_exp_pc hdi health_index respond_index
    ## 1            48.8           52.6           326 0.7         14.9          29.2
    ## 2            62.2           86.1           483 0.8         35.2          39.5
    ##   state_fragility pr share_older pop_tot_log pop_density_log distancing_bin
    ## 1              11  1       7.192       2.429           2.349             NA
    ## 2               7  1       7.157       2.838           4.231             NA
    ##   lockdown_bin lockdown_n distancing_n days_rel_lockdown days_rel_distancing
    ## 1           NA         NA           NA                NA                  NA
    ## 2           NA         NA           NA                NA                  NA
    ##   retail grocery parks transit work residential mobility_index stringency
    ## 1     NA      NA    NA      NA   NA          NA          -72.2         NA
    ## 2     NA      NA    NA      NA   NA          NA          -63.0         NA
    ##   C1_School.closing C2_Workplace.closing C3_Cancel.public.events
    ## 1                NA                   NA                      NA
    ## 2                NA                   NA                      NA
    ##   C4_Restrictions.on.gatherings C5_Close.public.transport
    ## 1                            NA                        NA
    ## 2                            NA                        NA
    ##   C6_Stay.at.home.requirements C7_Restrictions.on.internal.movement
    ## 1                           NA                                   NA
    ## 2                           NA                                   NA
    ##   C8_International.travel.controls H1_Public.information.campaigns temp_mean
    ## 1                               NA                              NA    21.433
    ## 2                               NA                              NA    22.067
    ##    precip excess_deaths_weekly excess_deaths_source excess_deaths_last_obs
    ## 1  62.633                   NA                   NA                     NA
    ## 2 101.867                   NA                   NA                     NA
    ##   excess_deaths_cum excess_deaths_cum_log deaths_cum_per_million
    ## 1                NA                    NA                784.276
    ## 2                NA                    NA                769.066
    ##   deaths_cum_per_million_log excess_deaths_cum_per_million
    ## 1                      6.666                            NA
    ## 2                      6.646                            NA
    ##   excess_deaths_cum_per_million_log forcats..fct_explicit_na.geoid2.
    ## 1                                NA                              BOL
    ## 2                                NA                              ECU
    ##   relative_start elapsed_rel relative_start_d elapsed_rel_d pop_older
    ## 1             73         253               94           232    816511
    ## 2             64         262               82           244   1222776
    ##   older_share mortality
    ## 1    7.092016  6.186684
    ## 2    7.038104  7.106926

Although this is not the main focus of the research paper, we wanted to
explore a possible confounding variable, since we could not find a
strong relationship between our chosen variables. Reports of Covid-19
case explosions in migrant communities beg the question whether there is
a correlation between mortality rates and the migrant population as a
share of the total population. When it comes to the Americas, the most
affected are Venezuelan migrants. “As of 30 October 2020, more than
136,000 Venezuelan migrants and refugees had returned to Venezuela from
other countries in the region (IOM and UN OCHA, 2020). At its peak, 600
Venezuelans returned from Colombia daily and an average of 88
Venezuelans returned from Brazil daily via the border at Pacaraima
(Coordination Platform for Refugees and Migrants from Venezuela, 2020).”
(Migration Data Portal)

The hypothesis would suggest that countries with a higher share of their
population consisting of migrants would have respectively higher
Covid-19 mortality rates. Superimposing the two density plots onto each
other, we see a strong correlation in North America and a weaker (but
not non-existing) correlation in Latin America & Caribbean. This could
be explored in further research papers.

``` r
  new_covid_america_without_outlier %>%
  ggplot(
    mapping = aes(x=migration_share,
                  color = region,
                  fill = region)) +
  theme_bw()+
  
  labs(y="Frequency", x="Migrant share of the population")+
  ggtitle("Density plot of countries by the share of migrants")+
  
  geom_density(alpha=0.4)
```

    ## Warning: Removed 3 rows containing non-finite values (stat_density).

    ## Warning: Groups with fewer than two data points have been dropped.

    ## Warning in max(ids, na.rm = TRUE): no non-missing arguments to max; returning -
    ## Inf

![](finalproject_files/figure-gfm/older_share%20migration_share-1.png)<!-- -->

``` r
#install.packages(c("coda","mvtnorm","devtools","loo","dagitty","remotes"))
#remotes::install_github("rmcelreath/rethinking")
```

To further discuss the shortcomings of the dataset and analysis itself,
it is useful to look at the statistical values of the dataset at hand.
These tools show us once again that most countries in the Americas have
a relatively low number of cumulative cases. However, the large
difference between the mean and median values mirrors that the dataset
has a few extreme outliers.

``` r
mean(new_covid_america$cases_cum, na.rm = TRUE)
```

    ## [1] 147759.5

``` r
median(new_covid_america$cases_cum, na.rm = TRUE)
```

    ## [1] 5510

``` r
sd(new_covid_america$cases_cum, na.rm = TRUE)
```

    ## [1] 330865.1

``` r
CumulativeCases <- new_covid_america$cases_cum
  hist(CumulativeCases)
```

![](finalproject_files/figure-gfm/cumulative%20cases%20statistics-1.png)<!-- -->

``` r
  plot(CumulativeCases, type = 'b', main = "Cumulative cases of Covid-19 in the Americas")
```

![](finalproject_files/figure-gfm/cumulative%20cases%20statistics-2.png)<!-- -->

``` r
summary(new_covid_america$cases_cum)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##       3     234    5510  147760  114922 1366169

However, when looking at mortality, the outliers are not as obvious as
with cumulative cases. Some interesting further research exploring this
relationship could also be done. The difference between the median and
the mean is a lot smaller. The histogram shows that there is a more
balanced distribution between 2% mortality and 4% mortality.

``` r
mean(new_covid_america$mortality)
```

    ## [1] 2.163083

``` r
median(new_covid_america$mortality)
```

    ## [1] 2.048019

``` r
sd(new_covid_america$mortality)
```

    ## [1] 1.892234

``` r
Mortality <- new_covid_america$mortality
  hist(Mortality)
```

![](finalproject_files/figure-gfm/mortality%20statistics-1.png)<!-- -->

``` r
  plot(Mortality, type = 'b', main = "Covid-19 mortality in the Americas")
```

![](finalproject_files/figure-gfm/mortality%20statistics-2.png)<!-- -->

``` r
summary(new_covid_america$mortality)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.0000  0.9595  2.0480  2.1631  2.7851  9.8164

When looking at the share of migrants, we again see a large difference
between the mean and the median values, suggesting there are a few
extreme outliers. The histogram shows us that most countries in the
Americas have migrants as a share of the population of 10% or less.

``` r
mean(new_covid_america$migration_share, na.rm = TRUE)
```

    ## [1] 12.89577

``` r
median(new_covid_america$migration_share, na.rm = TRUE)
```

    ## [1] 4.805

``` r
sd(new_covid_america$migration_share, na.rm = TRUE)
```

    ## [1] 17.08245

``` r
MigrationShare <- new_covid_america$migration_share
  hist(MigrationShare)
```

![](finalproject_files/figure-gfm/migration%20statistics-1.png)<!-- -->

``` r
  plot(MigrationShare, type = 'b', main = "Migrant share of the population in the Americas")
```

![](finalproject_files/figure-gfm/migration%20statistics-2.png)<!-- -->

``` r
summary(new_covid_america$migration_share)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##   0.117   1.669   4.805  12.896  15.137  70.448       3

-----

``` r
covid <- read_csv("data.csv")
```

    ## Warning: Missing column names filled in: 'X1' [1]

``` r
covid_2 <- read_csv("data2.csv")
```

I’ll be analyzing the third part: Asia, Oceania and Australia. First,
I’ll be filtering the data set to include only the countries that are
present in this specific region, which will be stored in a new variable
called ‘covid\_asia’. I noticed that Australia was already included in
Oceania region in the dataset. Therefore, I filtered only for Asia and
Oceania. By using the ‘nrow’ function, I got to know the total number of
rows i.e. countries in this region, which is equal to 52 countries.

``` r
# Filter for countries in Asia-Pacific

covid_asia <- covid %>%
  filter(continent == "Asia" | continent == "Oceania")
nrow(covid_asia)
```

    ## [1] 52

**General situation or background of this region**

Asia-Pacific is the hardest hit by COVID-19 among all the other
continents. About 80 percent of the global total of people affected by
disasters and COVID-19 in 2020 were in the Asia-Pacific.

“Asia surpassed 10 million infections of the coronavirus previous month,
as cases continue to mount. Behind only Latin America, Asia accounts for
about one-fourth of the global caseload of 42.1 million of the virus.
With over 163,000 deaths, the region accounts for some 14% of the global
COVID-19 toll” (Source: News18 India, Asia becomes second region to
exceed 10 million coronavirus cases, 24 Oct 2020, Reuters).

The Coronavirus pandemic has challenged healthcare systems across the
world in a way not seen in modern times. Older people are
disproportionately affected by the COVID-19 pandemic, which has had a
profound impact on research as well as clinical service delivery. They
are bearing the consequences of the pandemic as a group at the highest
risk of hospitalization and death from COVID-19 illness. In this
research paper, we’ll take a closer look at how exactly the old
population and population densities of specific countries in
Asia-Pacific region affect their mortality rate and are they even
associated or not.

As proved by studies, the risk for severe illness with COVID-19
increases with age, with older adults at highest risk. Although all age
groups are at risk of contracting COVID-19, older people face
significant risk of developing severe illness if they contract the
disease due to physiological changes that come with ageing and potential
underlying health conditions.

For example, people in their 50s are at higher risk for severe illness
than people in their 40s. Similarly, people in their 60s or 70s are, in
general, at higher risk for severe illness than people in their 50s. The
greatest risk for severe illness from COVID-19 is among those aged 85 or
older.

The table below represents the age intervals which have higher risk of
getting infected (hospitalization) and death.

![](old.jpg) (Source: Centers for Disease Control and Prevention CDC
24/7)

In our research, we’ll be considering only the population aged 65 and
above i.e. old adults including male as well as female.

-----

COVID-19 mortality is the number of deaths out of the total number of
infected cases. Studies have shown that COVID-19 mortality can be
explained by age, obesity, and underlying diseases, such as
hypertension, diabetes, and coronary heart disease, as well as clinical
symptoms, complications, hospital care, previous immunity and virus
mutations.

Countries vary widely in terms of capacities to prevent, detect and
respond to disease outbreaks. In this paper, I aim to explore these
factors associated with COVID-19 mortalities at the country level,
specifically in Asia-Pacific region.

-----

**VARIABLES that I’ll be using from the data set:**

  - **cases\_cum** - Variable for cumulative cases. For instance, the
    number of people who have ever tested positive for coronavirus in a
    given country, regardless of whether they have recovered.

  - **deaths\_cum** - Variable for cumulative deaths. It is the total
    number of people who have died due to coronavirus in a given
    country.

  - **mortality** - Variable that holds the total number of deaths out
    of total COVID-19 positive infected cases.

  - **old\_perc** - Variable that refers to the percentage of old aged
    population among the total population of the country.

  - **pop\_older** - It is the total old aged population in a country,
    including male and female.

-----

To begin with the analysis, we’ll first plot a basic scatter plot
**(cumulative cases vs cumulative deaths)** showing the initial glimpse
or pattern in this region or continent. I’m making use of the two
variables here i.e. cases\_cum (X-axis) and deaths\_cum (Y-axis).

``` r
# Map initial glimpse of the data: cumulative cases & cumulative deaths

covid_asia %>%
  ggplot(mapping = aes(x = cases_cum, 
                       y = deaths_cum)) + 
  theme_bw() +
  labs(y="Cumulative Deaths", x = "Cumulative Cases") +
  ggtitle("Cumulative Cases vs Cumulative Deaths") +
  geom_point()
```

![](finalproject_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

Since there is one outlier, so removing it will enable a more closer and
accurate analysis. Hence, I’ll be filtering the data set again and
storing in a new variable called ‘new\_covid\_asia’ to include only the
countries that have a total number of cases less than 2,500,000.

``` r
new_covid_asia <- covid_asia %>% filter(cases_cum < 2500000)

outlier <- covid_asia %>% filter (cases_cum > 2500000)
outlier
```

    ## # A tibble: 1 x 132
    ##      X1 geoid2 date       month   day  year elapsed date_rep   cases deaths
    ##   <dbl> <chr>  <date>     <dbl> <dbl> <dbl>   <dbl> <date>     <dbl>  <dbl>
    ## 1    90 IND    2020-11-22    11    21  2020     326 2020-11-22 45209    501
    ## # … with 122 more variables: country <chr>, population_2019 <dbl>,
    ## #   continent <chr>,
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

As expected, the outlier is India, the second most populated and second
most COVID-19 affected country in the world with over 9.7 million
coronavirus cases as of December 10, 2020.

Now, we can plot the **cumulative cases vs cumulative deaths** again
with the new data set (where the two outliers were removed) and
interpret the plot better.

``` r
new_covid_asia %>%
  ggplot(mapping = aes(x = cases_cum,
                       y = deaths_cum)) + 
  theme_bw() +
  labs(y="Cumulative Deaths", x = "Cumulative Cases") +
  ggtitle("Cumulative Cases vs Cumulative Deaths (better representation)") +
  geom_point() + geom_smooth()
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](finalproject_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

This scatter plot has an **exponential curve**. Hence, it has a
**positive correlation** between the two variables i.e. cases\_cum and
deaths\_cum. This means that as the number of positive COVID-19 cases
rise, the death cases also rise, and the increase is exponential
i.e. with small increase in cases, there is large increase in death
count.

-----

Similarly, I’ll be analyzing the relationship between COVID-19 mortality
(in percentage) and cumulative cases.

``` r
covid_2 %>% filter(cases_cum < 600000) %>%
  ggplot(mapping = aes(x = cases_cum,
                       y = (deaths_cum/cases_cum)*100)) + 
  geom_point() + 
  geom_smooth(method = lm, se=F) + 
  theme_bw() + 
  labs(y="COVID-19 Mortality %", x = "Cumulative Cases")
```

    ## `geom_smooth()` using formula 'y ~ x'

![](finalproject_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

All the countries have mortality rates below 10% except one country
i.e. Yemen having a 29% mortality rate.

``` r
outlier <- covid_2 %>% filter((deaths_cum/cases_cum)*100 > 20)
outlier
```

    ## # A tibble: 1 x 8
    ##   country pop_density cases_cum deaths_cum old_perc continent region      income
    ##   <chr>         <dbl>     <dbl>      <dbl>    <dbl> <chr>     <chr>       <chr> 
    ## 1 Yemen            56      2093        608     2.90 Asia      Middle Eas… Low i…

Now, after removing Yemen from the plot:

``` r
covid_2 %>% filter(cases_cum < 600000 & (deaths_cum/cases_cum)*100 < 20) %>%
  ggplot(mapping = aes(x = cases_cum,
                       y = (deaths_cum/cases_cum)*100)) + 
  geom_point() + 
  geom_smooth(method = lm, se=F) + 
  theme_bw() + 
  labs(y="COVID-19 Mortality %", x = "Cumulative Cases")
```

    ## `geom_smooth()` using formula 'y ~ x'

![](finalproject_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
lm(formula = (deaths_cum/cases_cum)*100 ~ cases_cum, data = covid_2)
```

    ## 
    ## Call:
    ## lm(formula = (deaths_cum/cases_cum) * 100 ~ cases_cum, data = covid_2)
    ## 
    ## Coefficients:
    ## (Intercept)    cases_cum  
    ##   2.179e+00   -7.657e-08

The scatter plot indicates that there is a slightly positive correlation
between COVID-19 mortality and cumulative cases overall (including all
countries without the outliers i.e. India and Yemen). To verify, the
slope intercept shows a positive value +2.179.

A more closer analysis of different countries is done based on
percentage of old population.

Now, we’ll divide the proportions of old aged population into

  - **low % old population:** Countries that have less than 4.718% of
    old population among the total population of that country.
  - **moderate % old population:** Countries that have between 4.718% to
    5.652% of old population among the total population of that country.
  - **high % old population:** Countries that have more than 7.652% of
    old population among the total population of that country.

In order to see what kind of relationships exist between the two
variables in these categorized countries, we’ll be:

**Filtering countries that have old aged population % below 4.718 (LOW
%).**

  - **Central Asia**
      - Tajikistan
      - Uzbekistan
  - **Middle East and North Africa**
      - United Arab Emirates
      - Qatar
      - Oman
      - Bahrain
      - Kuwait
      - Yemen
      - Palestinian Territories
      - Iraq
      - Saudi Arabia
      - Jordan
  - **South Asia**
      - Afghanistan
      - Pakistan
  - **East Asia and Pacific**
      - Vanuatu
      - Papua New Guinea
      - Solomon Islands
      - Timor-Leste
      - Laos
      - Mongolia

<!-- end list -->

``` r
sp1 <- covid_2 %>% 
  filter(old_perc < 4.718) %>%
  ggplot(mapping = aes(x = cases_cum,
                       y = (deaths_cum/cases_cum)*100)) + 
  geom_point() + 
  geom_smooth(method = lm, se=F) + 
  theme_bw(base_size = 10) + 
  labs(y="COVID-19 Mortality %", x = "Cumulative Cases") +
  ggtitle("Countries with low % old population")
```

**Filtering countries that have old aged population % between 4.718 and
7.652 (MODERATE %).**

  - **Central Asia**
      - Kyrgyzstan
      - Kazakhstan
      - Vietnam
  - **Middle East and North Africa**
      - Syria
      - Bhutan
      - Lebanon
  - **South Asia**
      - Bangladesh
      - Nepal
      - India
      - Iran
  - **East Asia and Pacific**
      - Cambodia
      - Brunei
      - Philippines
      - Fiji
      - Myanmar (Burma)
      - Indonesia
      - Malaysia

<!-- end list -->

``` r
sp2 <- covid_2 %>% 
  filter(old_perc >= 4.718 & old_perc <= 7.652) %>%
  ggplot(mapping = aes(x = cases_cum,
                       y = (deaths_cum/cases_cum)*100)) + 
  geom_point() + 
  geom_smooth(method = lm, se=F) + 
  theme_bw(base_size = 10) + 
  labs(y="COVID-19 Mortality %", x = "Cumulative Cases") +
  ggtitle("Countries with moderate % old population")
```

**Filtering countries that have old aged population % greater than 8
(HIGH %).**

  - **Middle East and North Africa**
      - Israel
  - **South Asia**
      - Sri Lanka
  - **East Asia and Pacific**
      - China
      - Singapore
      - Thailand
      - South Korea
      - Australia
      - New Zealand
      - Japan
      - New Caledonia

<!-- end list -->

``` r
sp3 <- covid_2 %>% 
  filter(old_perc > 8) %>%
  ggplot(mapping = aes(x = cases_cum,
                       y = (deaths_cum/cases_cum)*100)) + 
  geom_point() + 
  geom_smooth(method = lm, se=F) + 
  theme_bw(base_size = 10) + 
  labs(y="COVID-19 Mortality %", x = "Cumulative Cases") +
  ggtitle("Countries with high % old population")
```

``` r
grid.arrange(sp1, sp2, sp3, ncol = 2)
```

    ## `geom_smooth()` using formula 'y ~ x'
    ## `geom_smooth()` using formula 'y ~ x'
    ## `geom_smooth()` using formula 'y ~ x'

![](finalproject_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

According to the plots, the **negative correlation** was significant for
countries with moderate and low percentage of old population,
respectively. I had tried to use logarithmic scale in these plots
earlier but plots without using log seemed more clearer in pattern. The
overall negative correlation in all three plots might be due to the fact
that countries, with time adopted several preventive measures, including
better healthcare facilities, professionals to lessen the impact of the
virus on its people.

The survival rates seems to have improved, but rising case numbers are
causing the total number of deaths to increase. The average age of
people who developed COVID-19 and those visiting emergency rooms due to
the disease dropped as more young people came down with the illness.
Thus, there was an increase in younger people hospitalized with
COVID-19.

Moreover, Many people at risk are also taking more steps to reduce the
chances of being exposed to the virus. People who are older and have
more underlying medical conditions are more consistently doing social
distancing, frequent handwashing, and other measures to protect
themselves.

As we compare the above three plots, we see that the countries that had
comparatively lower population of old aged, showed a steeper negative
slope (or comparatively more negative correlation than other two). It
indicates that due to old population already being low, there was more
decrease in mortality rate with rising cases.

While the countries that have moderate percentage of old population,
showed not much decrease in mortality rate or less steeper negative
slope than the previous. This might be because there is more old
population proportion in these countries comparatively.

Based on surveying the countries in Asia-Pacific, the countries that
have high percentage of old people, are advanced in terms of technology,
government and have better healthcare system. Hence, we see a very
little or no correlation in high % old population scatter plot.

-----

**Descriptive Analysis**

``` r
covid_2 %>% 
  summary(covid_2)
```

    ##    country           pop_density       cases_cum         deaths_cum      
    ##  Length:47          Min.   :   2.0   Min.   :      1   Min.   :     0.0  
    ##  Class :character   1st Qu.:  32.5   1st Qu.:   1489   1st Qu.:    26.5  
    ##  Mode  :character   Median :  93.0   Median :  69581   Median :   603.0  
    ##                     Mean   : 398.0   Mean   : 319030   Mean   :  5567.3  
    ##                     3rd Qu.: 248.0   3rd Qu.: 163312   3rd Qu.:  2073.0  
    ##                     Max.   :8358.0   Max.   :9095806   Max.   :133227.0  
    ##     old_perc       continent            region             income         
    ##  Min.   : 1.157   Length:47          Length:47          Length:47         
    ##  1st Qu.: 3.554   Class :character   Class :character   Class :character  
    ##  Median : 5.180   Mode  :character   Mode  :character   Mode  :character  
    ##  Mean   : 6.593                                                           
    ##  3rd Qu.: 7.413                                                           
    ##  Max.   :28.002

![](descanalysis.PNG)

The table above summarizes the mortality rate, cumulative cases, deaths
and % old population of different countries in Asia-Pacific region.

**Descriptive statistics**

For the 51 countries, **the mean COVID-19 mortality rate was 2.15%**,
the mean COVID-19 cumulative cases was 319030 and deaths was 5567.
Moreover, the mean percentage of old population considering all
countries in Asia-Pacific region was 6.59%.

-----

I feel that the residents living in areas with high population density,
such as big or metropolitan cities have a higher probability to come
into close contact with others and consequently any contagious disease
is expected to spread rapidly in dense areas. Now, I’ll analyze and
conclude what kind of relationship exists between these.

``` r
covid_2 %>% filter(pop_density < 1000) %>%
  ggplot(mapping = aes(x = log(pop_density),
                       y = log((deaths_cum/cases_cum)*100))) + 
  theme_bw(base_size = 10) +
  labs(y="Mortality Rate % (log)", x = "Population Density in sq. kms (log)") +
  ggtitle("Mortality vs Population Density") +
  geom_point() + geom_smooth(method=lm, se=F)
```

    ## `geom_smooth()` using formula 'y ~ x'

    ## Warning: Removed 8 rows containing non-finite values (stat_smooth).

![](finalproject_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

Population density refers to the number of people living in an area per
square kilometer.

I’ve used logarithmic scale here so that the points appear more spread
out to enable better analysis. Surprisingly, the mortality rate seems to
be decreasing with increasing population density. After looking at the
countries, I found out that countries with higher densities have
significantly lower virus-related death rates than do counties with
lower densities, possibly due to superior healthcare systems.
High-density cities and countries may offer more opportunities for
crowding. But in Asia, proper public health precautions have spared many
countries from the worst.

The plot shows that higher-density countries were actually associated
with lower mortality rates, possibly because residents were more
strictly following social-distancing guidelines or had better access to
health care. Their superior health and educational systems could help
mitigate the full impact of the disease for those who are infected,
leading to higher rates of recovery and lower rates of mortality. Dense
areas may be more likely to put in place policies that foster social
distancing, thus reducing actual rates of infection or simply leading to
greater social distancing due to greater public awareness of the threat.
In addition, it is possible that denser environments make it easier for
people to stay somewhat connected with neighbors, families, and friends
while they are sheltering in place.

On the other hand, the lesser-density countries, even if they have less
number of contacts but due to not having services to support patients,
might result in higher mortality which is verified by the plot above.

-----

``` r
MigrationShare <- covid_asia$migration_share
  hist(MigrationShare)
```

![](finalproject_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

Now I’ll be exploring our confounding variable i.e. migration\_share.
Reports of COVID-19 case explosions in migrant communities beg the
question whether there is a correlation between mortality rates and the
migrant population as a share of the total population. As is the case
with America, the histogram shows us that most countries in Asia-Pacific
have migrants as a share of the population of 10% or less.

``` r
ggplot(data = covid_asia, mapping = aes(x = (deaths_cum/cases_cum)*100), fill = region, color = region) +
  geom_histogram(color="black", fill="light gray") +
  theme_bw() + labs(x = "Mortality (%)")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](finalproject_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

This histogram shows that most of the countries have less than 10%
mortality rate, with Yemen being an outlier having 29% mortality %.

``` r
outlier_mortality <- covid_asia %>%
  filter((deaths_cum/cases_cum)*100 > 20)
outlier_mortality
```

    ## # A tibble: 1 x 132
    ##      X1 geoid2 date       month   day  year elapsed date_rep   cases deaths
    ##   <dbl> <chr>  <date>     <dbl> <dbl> <dbl>   <dbl> <date>     <dbl>  <dbl>
    ## 1   206 YEM    2020-11-22    11    21  2020     326 2020-11-22     3      1
    ## # … with 122 more variables: country <chr>, population_2019 <dbl>,
    ## #   continent <chr>,
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
covid_asia_without_outlier <- covid_asia %>%
  filter((deaths_cum/cases_cum)*100 < 10)
ggplot(data = covid_asia_without_outlier, mapping = aes(x = (deaths_cum/cases_cum)*100), fill = region, color = region) +
  geom_histogram(color="black", fill="light gray") +
  theme_bw() + labs(x = "Mortality (%)") + ggtitle("Histogram without Yemen")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](finalproject_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->
