# Analyze NIH funding on an RCDC category

## Introduction
This repo documents the processes we typically use to analyze NIH funding data from RePORTER. It provides the actual R code we use, explanations of what the code is doing at each step, and a little about why we do things this way. We hope this repo also acts as a springboard for performing other analyses of NIH award data from RePORTER, and as a window onto how we might approach similar analyses using data from internal NIH systems. 

In this example, we’ll analyze all NIH awards on the RCDC spending category of “Preterm, Low Birth Weight, and Health of the Newborn” that were funded from 2014-2023. But this code could be re-used for other RCDC categories and other fiscal years by simply changing the search criteria saved to the `mq` object in line 6 and adjusting the plotting parameters (if necessary). 

## Vignette
We begin by loading the packages, custom functions, and color palette we’re going to use. Here, we’re using the `tidyverse` package, a set of custom functions for getting data via the NIH RePORTER API (the `get_nih_reporter.r` file, available from the [nih_reporter_api]( https://github.com/christopherBelter/nih_reporter_api) repo), and a custom color palette for NICHD.
```r
library(tidyverse)
mcols <- scan("nichd_palette.txt", what = "varchar", sep = "\n")
source("get_nih_reporter.r")
```

Next, we create the RePORTER query we’re going to use as `mq` (shorthand for my query) and then run the `get_nih_reporter()` function to retrieve the data via the API. The API unfortunately expects spending category codes (i.e. “686”) instead of the names of the categories, so you’ll need to do some digging to find the right category code for the category you want to search for.
```r
mq <- create_query(FY = as.character(2014:2023), spending_cats = "686", exclude_subprojects = TRUE)
grants <- get_nih_reporter(mq, "pretermBirth_fy14to23.txt")
```

Now that we have the data in our R session, we begin to analyze it. First, we calculate the total number of awards and total funding per Administering IC. We use the `group_by()`, `summarise()`, `mutate()` function chain to group the data by Admin IC, calculate the awards and funding per group, and then add a column for the percent of total funding per group. We then sort the resulting data frame by the total funding for plotting later on.
```r
ic_sum <- grants %>% 
  group_by(agency_ic_admin_abbreviation) %>% 
  summarise(
    awards = length(appl_id),
    funding = sum(award_amount) / 1000000,
    .groups = "drop"
  ) %>% 
  mutate(perc_total_funding = funding / sum(funding)) %>% 
  arrange(funding)
```

We then generate a bar chart of funding by IC for the top 10 ICs by total funding. We convert the agency_ic_admin_abbreviation column to a factor to ensure the ICs are sorted the way we want in the plot, generate the base plot, and then set the specifics of how we want that plot to be displayed. If you want each bar to be labeled in the plot, uncomment the `geom_text()` line to ensure it runs.
```r
top_ics <- tail(ic_sum$agency_ic_admin_abbreviation, 10)
ic_sum$agency_ic_admin_abbreviation <- factor(ic_sum$agency_ic_admin_abbreviation, levels = ic_sum$agency_ic_admin_abbreviation)
p1 <- ggplot(ic_sum[ic_sum$agency_ic_admin_abbreviation %in% top_ics,], aes(funding, agency_ic_admin_abbreviation))
p1 + geom_col(fill = mcols[3]) + 
  scale_x_continuous(labels = scales::label_dollar(suffix = "M")) + 
  #geom_text(aes(label = scales::dollar(round(funding), suffix = "M")), hjust = "left", nudge_x = 5, size = 5) + 
  labs(x = "Administered Funding", y = "Institute, Center, or Office") + 
  theme_gray(base_size = 18)
```

Next, we use almost the same function chain and plotting code to calculate the awards and funding by funding mechanism and create a bar plot of the results. The only thing that’s different is the column name we pass to the `group_by()` function.
```r
mech_sum <- grants %>% 
  group_by(funding_mechanism) %>% 
  summarise(
    awards = n(),
    funding = sum(award_amount, na.rm = TRUE) / 1000000,
    .groups = "drop"
  ) %>% 
  mutate(percent_funding = funding / sum(funding)) %>% 
  arrange(funding)
mech_sum$funding_mechanism <- factor(mech_sum$funding_mechanism, levels = mech_sum$funding_mechanism)
p2 <- ggplot(mech_sum, aes(percent_funding, funding_mechanism))
p2 + geom_col(fill = mcols[3]) + 
  scale_x_continuous(breaks = seq(0,1,0.1), labels = scales::label_percent(accuracy = 1)) + 
  labs(x = "Percent of Funding", y = "Funding Mechanism") + 
  theme_gray(base_size = 18)
```

So far we’ve been looking at awards and funding over the whole time period, but what if we’re interested in looking at changes over time? We can use the same code again and group by the fiscal_year column to summarize the total across all ICs.
```r
year_sum <- grants %>% 
  group_by(fiscal_year) %>% 
  summarise(
    awards = length(appl_id),
    funding = sum(award_amount) / 1000000,
    .groups = "drop"
  )
p3 <- ggplot(year_sum, aes(fiscal_year, funding))
p3 + geom_col(fill = mcols[1]) + 
  scale_x_continuous(breaks = seq(2014,2023,2)) + 
  scale_y_continuous(labels = scales::label_dollar(suffix = "M")) + 
  labs(x = "Fiscal Year", y = "Total Funding") + 
  theme_gray(base_size = 18)
```

Or we can group by both fiscal_year and agency_ic_admin_abbreviation to look at the total per IC per year. The summarization code is almost the same, we just group by two columns instead of just one. And we visualize the results with a line chart instead of a bar chart so we can see trends in funding per year for each IC.
```r
ic_year <- grants %>% 
  group_by(agency_ic_admin_abbreviation, fiscal_year) %>% 
  summarise(
    awards = length(appl_id),
    funding = sum(award_amount) / 1000000,
    .groups = "drop"
  )
p4 <- ggplot(ic_year[ic_year$agency_ic_admin_abbreviation %in% top_ics,], aes(fiscal_year, funding, color = agency_ic_admin_abbreviation))
p4 + geom_line(size = 4) + scale_color_manual(values = mcols) + 
  scale_y_continuous(labels = scales::label_dollar(suffix = "M")) + 
  labs(x = "Fiscal Year", y = "Administered Funding", color = "ICO") + 
  theme_gray(base_size = 18)
```

Another thing we can do is add a `filter()` function to this chain to calculate awards and funding for a select set of rows in the data set. Say, for example, that we wanted to look at awards and funding by IC and fiscal year for just research project grants, instead of all grant mechanisms. We simply add a `filter()` specification to the top of the pipe chain and then run all the rest of the code as above. 
```r
ic_year_rpg <- grants %>% 
  filter(mechanism_code_dc == "RP") %>% 
  group_by(agency_ic_admin_abbreviation, fiscal_year) %>% 
  summarise(
    awards = length(appl_id),
    funding = sum(award_amount) / 1000000,
    .groups = "drop"
  )
p5 <- ggplot(ic_year_rpg[ic_year_rpg$agency_ic_admin_abbreviation %in% top_ics,], aes(fiscal_year, funding, color = agency_ic_admin_abbreviation))
p5 + geom_line(size = 4) + scale_color_manual(values = mcols) + 
  scale_y_continuous(labels = scales::label_dollar(suffix = "M")) + 
  labs(x = "Fiscal Year", y = "Administered Funding", color = "ICO") + 
  theme_gray(base_size = 18)
```

Finally, we can use R’s built-in mapping capabilities, especially its “simple features” object class, to create a color-coded map of funding by US state. First, we create a state summary data frame as we’ve been doing, just grouping by state name. 
```r
state_sum <- grants %>%  
  group_by(organization_org_state) %>% 
  summarise(
    awards = n(),
    funding = sum(award_amount, na.rm = TRUE) / 1000000,
    .groups = "drop"
  )
```

Then we load a base map of the US using the “sf” or “special features” object class. This vignette uses the `rnaturalearth` package, but other sf mapping packages are also available.
```r
us_base <- rnaturalearth::ne_states("united states of america", returnclass = "sf")
```

This map is stored in R as a data frame, so we can manipulate it using the same methods we would use on any other data frame. So, we then merge the state summary with the base map to get all of the variables we want in a single data frame. 

```r
us_base <- us_base %>% 
  left_join(state_sum, by = c("postal" = "organization_org_state"))
```

Finally, we plot the map using the `geom_sf()` function, set each state’s fill color to the funding amount we just generated, use the Viridis color palette for the fill color, and use `coord_sf()` to zoom in on the continental US.
```r
p2 <- ggplot(us_base, aes(fill = funding)) + geom_sf(color = "grey75") + coord_sf(xlim = c(-123, -69), ylim = c(25,50)) 
p2 + scale_fill_viridis_c(labels = scales::label_dollar(suffix = "M")) + labs(fill = "Funding") + theme_void(base_size = 18)
```

There are obviously many more things we could do with this data set, but as we’ve seen, most of them involve only minor tweaks to the function chains that we’ve already been using. And as already noted, we could also re-use this code to analyze grant data on other topics, or even by entire ICs, simply by changing the search parameters in line 6.
