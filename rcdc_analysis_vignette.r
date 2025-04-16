library(tidyverse)
mcols <- scan("branding/nichd_palette.txt", what = "varchar", sep = "\n")
source("r/functions/get_nih_reporter.r")


mq <- create_query(FY = as.character(2014:2023), spending_cats = "686", exclude_subprojects = TRUE)
grants <- get_nih_reporter(mq, "projects/ospra replication/pretermBirth_fy14to23.txt")

## Total IC funding
ic_sum <- grants %>% 
  group_by(agency_ic_admin_abbreviation) %>% 
  summarise(
    awards = length(appl_id),
    funding = sum(award_amount) / 1000000,
    .groups = "drop"
  ) %>% 
  mutate(perc_total_funding = funding / sum(funding)) %>% 
  arrange(funding)
  
top_ics <- tail(ic_sum$agency_ic_admin_abbreviation, 10)
ic_sum$agency_ic_admin_abbreviation <- factor(ic_sum$agency_ic_admin_abbreviation, levels = ic_sum$agency_ic_admin_abbreviation)
p1 <- ggplot(ic_sum[ic_sum$funding > 20,], aes(funding, agency_ic_admin_abbreviation))
p1 + geom_col(fill = mcols[3]) + 
  scale_x_continuous(labels = scales::label_dollar(suffix = "M")) + 
  #geom_text(aes(label = scales::dollar(round(funding), suffix = "M")), hjust = "left", nudge_x = 5, size = 5) + 
  labs(x = "Administered Funding", y = "Institute, Center, or Office") + 
  theme_gray(base_size = 18)
  
## total mechanism distribution
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

## Funding per year
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

## IC awards/funding by fiscal year
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

## map states
state_sum <- grants %>%  
  group_by(organization_org_state) %>% 
  summarise(
    awards = n(),
    funding = sum(award_amount, na.rm = TRUE) / 1000000,
    .groups = "drop"
  )
us_base <- rnaturalearth::ne_states("united states of america", returnclass = "sf")
us_base <- us_base %>% 
  left_join(state_sum, by = c("postal" = "organization_org_state"))
p2 <- ggplot(us_base, aes(fill = funding)) + geom_sf(color = "grey75") + coord_sf(xlim = c(-123, -69), ylim = c(25,50)) 
p2 + scale_fill_viridis_c(labels = scales::label_dollar(suffix = "M")) + labs(fill = "Funding") + theme_void(base_size = 18)

