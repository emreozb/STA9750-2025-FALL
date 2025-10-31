if(!require("tidyverse")) install.packages("tidyverse")
library(readr)
library(dplyr)

# Make sure data/mp01 folder exists
if(!dir.exists(file.path("data", "mp01"))){
  dir.create(file.path("data", "mp01"), showWarnings = FALSE, recursive = TRUE)
}

# Define filenames
GLOBAL_TOP_10_FILENAME <- file.path("data", "mp01", "global_top10_alltime.csv")
COUNTRY_TOP_10_FILENAME <- file.path("data", "mp01", "country_top10_alltime.csv")

GLOBAL_TOP_10 <- read_tsv(GLOBAL_TOP_10_FILENAME)
str(GLOBAL_TOP_10)
glimpse(GLOBAL_TOP_10)

GLOBAL_TOP_10 <- GLOBAL_TOP_10 |>
  mutate(season_title = if_else(season_title == "N/A", 
                                NA_character_, 
                                season_title))
# confirm fix
glimpse(GLOBAL_TOP_10)

COUNTRY_TOP_10 <- read_tsv(COUNTRY_TOP_10_FILENAME)
str(COUNTRY_TOP_10)
glimpse(COUNTRY_TOP_10)

COUNTRY_TOP_10 <- COUNTRY_TOP_10 |>
  mutate(season_title = if_else(season_title == "N/A", 
                                NA_character_, 
                                season_title))
# confirm fix
glimpse(COUNTRY_TOP_10)

library(DT)
GLOBAL_TOP_10 |> 
  head(n=20) |>
  datatable(options=list(searching=FALSE, info=FALSE))

library(stringr)
format_titles <- function(df){
  colnames(df) <- str_replace_all(colnames(df), "_", " ") |> str_to_title()
  df
}

GLOBAL_TOP_10 |> 
  format_titles() |>
  head(n=20) |>
  datatable(options=list(searching=FALSE, info=FALSE)) |>
  formatRound(c('Weekly Hours Viewed', 'Weekly Views'))

GLOBAL_TOP_10 |> 
  select(-season_title) |>
  format_titles() |>
  head(n=20) |>
  datatable(options=list(searching=FALSE, info=FALSE)) |>
  formatRound(c('Weekly Hours Viewed', 'Weekly Views'))

GLOBAL_TOP_10 |> 
  mutate(`runtime_(minutes)` = round(60 * runtime)) |>
  select(-season_title, 
         -runtime) |>
  format_titles() |>
  head(n=20) |>
  datatable(options=list(searching=FALSE, info=FALSE)) |>
  formatRound(c('Weekly Hours Viewed', 'Weekly Views'))

# install if needed
install.packages("openxlsx")

library(openxlsx)

# write.xlsx(GLOBAL_TOP_10, "GLOBAL_TOP_10.xlsx")



library(dplyr)
library(DT)
library(htmltools)
COUNTRY_TOP_10 |> 
  head(n=20) |>
  datatable(options=list(searching=FALSE, info=FALSE))

COUNTRY_TOP_10 |> 
  select(-season_title) |>
  format_titles() |>
  head(n=20) |>
  datatable(options=list(searching=FALSE, info=FALSE))

# write.xlsx(COUNTRY_TOP_10, "COUNTRY_TOP_10.xlsx")

# How many different countries does Netflix operate in? 
# Count distinct countries
n_countries <- COUNTRY_TOP_10 |>
  distinct(country_name) |>
  nrow()
n_countries

# Optional: show all country names as a publication-quality table
COUNTRY_TOP_10 |>
  distinct(country_name, country_iso2) |>
  arrange(country_name) |>
  datatable(
    options = list(pageLength = 10, searching = FALSE, info = FALSE),
    caption = htmltools::tags$caption(
      style = "caption-side: top; text-align: left;",
      "List of countries represented in Netflix Top 10 data"
    )
  )

# Which non-English-language film has spent the most cumulative weeks in the global top 10? How many weeks did it spend?
GLOBAL_TOP_10 |>
  filter(category == "Films (Non-English)") |>
  group_by(show_title) |>
  summarise(max_weeks = max(cumulative_weeks_in_top_10, na.rm = TRUE)) |>
  arrange(desc(max_weeks)) |>
  slice(1)
# Compute once and reuse
top_non_english_film <- GLOBAL_TOP_10 |>
  filter(category == "Films (Non-English)") |>
  group_by(show_title) |>
  summarise(max_weeks = max(cumulative_weeks_in_top_10, na.rm = TRUE), .groups = "drop") |>
  arrange(desc(max_weeks)) |>
  slice(1)

# Pretty datatable object
top_non_english_film_dt <- top_non_english_film |>
  rename(`Title` = show_title, `Max weeks in Top 10` = max_weeks) |>
  datatable(
    rownames = FALSE,
    options = list(dom = "t", paging = FALSE),
    caption = htmltools::tags$caption(
      style = "caption-side: top; text-align: left;",
      "Top Non-English Film by Weeks in Netflix Top 10"
    )
  ) |>
  formatStyle("Max weeks in Top 10", `text-align` = "right")
top_non_english_film_dt

# Show the table
top_non_english_film_dt
# What is the longest film (English or non-English) to have ever appeared in the Netflix global Top 10? How long is it in minutes?
GLOBAL_TOP_10 |>
  filter(grepl("Films", category)) |>        # keep only films (English + Non-English)
  mutate(runtime_minutes = round(runtime * 60)) |>  # convert hours → minutes
  group_by(show_title) |>
  summarise(max_runtime = max(runtime_minutes, na.rm = TRUE)) |>
  arrange(desc(max_runtime)) |>
  slice(1)

longest_film <- GLOBAL_TOP_10 |>
  filter(grepl("Films", category)) |>        # keep only films (English + Non-English)
  mutate(runtime_minutes = round(runtime * 60)) |>  # convert hours → minutes
  group_by(show_title) |>
  summarise(max_runtime = max(runtime_minutes, na.rm = TRUE), .groups = "drop") |>
  arrange(desc(max_runtime)) |>
  slice(1) |>
  pull(show_title)

longest_film_runtime <- GLOBAL_TOP_10 |>
  filter(grepl("Films", category)) |>        # keep only films (English + Non-English)
  mutate(runtime_minutes = round(runtime * 60)) |>  # convert hours → minutes
  group_by(show_title) |>
  summarise(max_runtime = max(runtime_minutes, na.rm = TRUE), .groups = "drop") |>
  arrange(desc(max_runtime)) |>
  slice(1) |>
  pull(max_runtime) 


# Top 5 longest films
GLOBAL_TOP_10 |>
  filter(grepl("Films", category)) |>
  mutate(runtime_minutes = round(runtime * 60)) |>
  group_by(show_title) |>
  summarise(max_runtime = max(runtime_minutes, na.rm = TRUE)) |>
  arrange(desc(max_runtime)) |>
  slice(1:5) |>
  DT::datatable(
    options = list(pageLength = 5, searching = FALSE, info = FALSE),
    caption = htmltools::tags$caption(
      style = "caption-side: top; text-align: left;",
      "Top 5 Longest Films in Netflix Global Top 10"
    )
  )

# For each of the four categories, what program has the most total hours of global viewership?
GLOBAL_TOP_10 |>
  group_by(category, show_title) |>
  summarise(total_hours = sum(weekly_hours_viewed, na.rm = TRUE)) |>
  slice_max(total_hours, n = 1) |>
  ungroup() |>
  arrange(desc(total_hours))

top_shows_by_category <- GLOBAL_TOP_10 |>
  group_by(category, show_title) |>
  summarise(total_hours = sum(weekly_hours_viewed, na.rm = TRUE), .groups = "drop_last") |>
  slice_max(total_hours, n = 1, with_ties = FALSE) |>
  ungroup() |>
  arrange(desc(total_hours))

datatable(
  top_shows_by_category |>
    rename(`Category` = category,
           `Title` = show_title,
           `Total Hours Viewed` = total_hours),
  rownames = FALSE,
  options = list(dom = "t", paging = FALSE),
  caption = htmltools::tags$caption(
    style = "caption-side: top; text-align: left;",
    "Top Netflix Shows by Category (Total Hours Viewed)"
  )
)

# Which TV show had the longest run in a country’s Top 10? How long was this run and in what country did it occur?
COUNTRY_TOP_10 |>
  filter(category == "TV") |>                                    # only TV shows
  group_by(country_name, show_title) |>                          # group by show & country
  summarise(longest_run = max(cumulative_weeks_in_top_10, na.rm = TRUE)) |>
  ungroup() |>
  arrange(desc(longest_run)) |>
  slice(1)

longest_tv_run <- COUNTRY_TOP_10 |>
  filter(category == "TV") |>                                    # only TV shows
  group_by(country_name, show_title) |>                          # group by show & country
  summarise(longest_run = max(cumulative_weeks_in_top_10, na.rm = TRUE)) |>
  ungroup() |>
  arrange(desc(longest_run)) |>
  slice(1)                                                       # keep only top row

# Extract values into separate objects
longest_country <- longest_tv_run$country_name
longest_show    <- longest_tv_run$show_title
longest_weeks   <- longest_tv_run$longest_run

# Netflix provides over 200 weeks of service history for all but one country in our data set. Which country is this and when did Netflix cease operations in that country?

COUNTRY_TOP_10 |>
  group_by(country_name) |>
  summarise(total_weeks = n_distinct(week),
            last_week = max(week)) |>
  filter(total_weeks < 200)

country_summary <- COUNTRY_TOP_10 |>
  group_by(country_name) |>
  summarise(
    total_weeks = n_distinct(week),
    last_week   = max(week),
    .groups = "drop"
  ) |>
  filter(total_weeks < 200)

datatable(
  country_summary |>
    rename(
      `Country` = country_name,
      `Total Weeks in Top 10` = total_weeks,
      `Most Recent Week` = last_week
    ),
  rownames = FALSE,
  options = list(pageLength = 10, searching = FALSE, info = FALSE),
  caption = htmltools::tags$caption(
    style = "caption-side: top; text-align: left;",
    "Countries with < 200 Total Weeks in Netflix Top 10"
  )
)

# What is the total viewership of the TV show Squid Game? Note that there are three seasons total and we are looking for the total number of hours watched across all seasons.
squid_game_total <- GLOBAL_TOP_10 |>
  filter(grepl("Squid Game", show_title, ignore.case = TRUE),
         category == "TV (Non-English)" | category == "TV (English)") |>
  summarise(total_hours = sum(weekly_hours_viewed, na.rm = TRUE))

squid_game_total <- GLOBAL_TOP_10 |>
  filter(
    grepl("Squid Game", show_title, ignore.case = TRUE),
    category %in% c("TV (Non-English)", "TV (English)")
  ) |>
  summarise(total_hours = sum(weekly_hours_viewed, na.rm = TRUE)) |>
  pull(total_hours)

# Print without scientific notation
options(scipen = 999)   # turn off scientific notation globally

squid_game_total

datatable(
  GLOBAL_TOP_10 |> head(20),
  options = list(searching = FALSE, info = FALSE),
  class = "display nowrap cell-border stripe hover" # force light style
)

# The movie Red Notice has a runtime of 1 hour and 58 minutes. Approximately how many views did it receive in 2021? Note that Netflix does not provide the weekly_views values that far back in the past, but you can compute it yourself using the total view time and the runtime.
# runtime of Red Notice in hours (1h 58m = 118 minutes)
runtime_hours <- 118 / 60  

red_notice_views <- GLOBAL_TOP_10 |>
  filter(show_title == "Red Notice", year(week) == 2021) |>   # pick 2021 only
  summarise(total_hours = sum(weekly_hours_viewed, na.rm = TRUE)) |>
  mutate(approx_views = round(total_hours / runtime_hours))
red_notice_views

datatable(
  red_notice_views |>
    rename(
      `Total Hours Viewed` = total_hours,
      `Approx. Views` = approx_views
    ),
  rownames = FALSE,
  options = list(dom = "t", paging = FALSE),
  caption = htmltools::tags$caption(
    style = "caption-side: top; text-align: left;",
    "Red Notice Views Estimate (2021)"
  )
)

library(dplyr)
library(lubridate)
library(ggplot2)

# runtime in hours (1h 58m = 118 minutes)
runtime_hours <- 118 / 60  

# filter for Red Notice in 2021
red_notice_weekly <- GLOBAL_TOP_10 |>
  filter(show_title == "Red Notice", year(week) == 2021) |>
  mutate(approx_views = weekly_hours_viewed / runtime_hours)

datatable(
  red_notice_weekly |>
    select(week, weekly_rank, weekly_hours_viewed) |>
    rename(
      `Week` = week,
      `Weekly Rank` = weekly_rank,
      `Weekly Hours Viewed` = weekly_hours_viewed
    ),
  rownames = FALSE,
  options = list(pageLength = 7, searching = FALSE, info = FALSE),
  caption = htmltools::tags$caption(
    style = "caption-side: top; text-align: left;",
    "Red Notice Weekly Hours Viewed (2021)"
  )
) |>
  formatCurrency("Weekly Hours Viewed", currency = "", interval = 3, mark = ",")


# line plot of weekly hours viewed
ggplot(red_notice_weekly, aes(x = week, y = weekly_hours_viewed)) +
  geom_line(color = "steelblue", linewidth = 1.2) +
  geom_point(color = "darkred", size = 2) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Weekly Viewing Hours of *Red Notice* in 2021",
    x = "Week",
    y = "Weekly Hours Viewed",
    caption = "Source: Netflix Top 10 dataset"
  ) +
  theme_minimal(base_size = 13)


library(dplyr)
library(lubridate)
library(DT)

# runtime of Red Notice in hours (1h 58m = 118 minutes)
runtime_hours <- 118 / 60  

# compute weekly approximate views
red_notice_weekly <- GLOBAL_TOP_10 |>
  filter(show_title == "Red Notice", year(week) == 2021) |>
  mutate(approx_views = round(weekly_hours_viewed / runtime_hours)) |>
  select(Week = week,
         `Weekly Hours Viewed` = weekly_hours_viewed,
         `Approximate Views` = approx_views)

# interactive publication-quality table
datatable(
  red_notice_weekly,
  options = list(pageLength = 10, searching = FALSE, info = FALSE),
  caption = htmltools::tags$caption(
    style = "caption-side: top; text-align: left;",
    "Weekly Hours Viewed and Estimated Weekly Views for Red Notice (2021)"
  )
) |>
  formatRound(c("Weekly Hours Viewed", "Approximate Views"), digits = 0)


# How many Films reached Number 1 in the US but did not originally debut there? That is, find films that first appeared on the Top 10 chart at, e.g., Number 4 but then became more popular and eventually hit Number 1? What is the most recent film to pull this off?

films_us_rank <- COUNTRY_TOP_10 |>
  filter(category == "Films", country_name == "United States") |>
  group_by(show_title) |>
  summarise(
    debut_rank = min(weekly_rank, na.rm = TRUE),          # first appearance rank
    ever_number1 = any(weekly_rank == 1, na.rm = TRUE),   # TRUE if it ever hit #1
    most_recent_week = max(week, na.rm = TRUE)            # last time it appeared
  ) |>
  ungroup() |>
  filter(ever_number1, debut_rank != 1)                   # must have hit #1, but not on debut

# Count of such films
num_films <- nrow(films_us_rank)

# Most recent film to pull this off
recent_film <- films_us_rank |>
  arrange(desc(most_recent_week)) |>
  slice(1)



# Which TV show/season hit the top 10 in the most countries in its debut week? In how many countries did it chart?
tv_debut_countries <- COUNTRY_TOP_10 |>
  filter(category == "TV") |>
  group_by(show_title, season_title, country_name) |>
  summarise(first_week = min(week), .groups = "drop") |>     # debut week per country
  group_by(show_title, season_title, first_week) |>
  summarise(num_countries = n_distinct(country_name), .groups = "drop") |>
  arrange(desc(num_countries))

# top result
tv_debut_countries |> slice(1)

# top 5
tv_debut_countries |>
  slice(1:5) |>
  datatable(
    options = list(pageLength = 5, searching = FALSE, info = FALSE),
    caption = htmltools::tags$caption(
      style = "caption-side: top; text-align: left;",
      "Top 5 TV Shows/Seasons by Number of Countries in Debut Week"
    )
  )

# Press Release 1: Upcoming Season of Stranger Things 
stranger_global <- GLOBAL_TOP_10 |>
  filter(grepl("Stranger Things", show_title, ignore.case = TRUE)) |>
  summarise(
    total_hours = sum(weekly_hours_viewed, na.rm = TRUE),
    total_weeks = max(cumulative_weeks_in_top_10, na.rm = TRUE)
  )

stranger_global

stranger_countries <- COUNTRY_TOP_10 |>
  filter(grepl("Stranger Things", show_title, ignore.case = TRUE)) |>
  summarise(n_countries = n_distinct(country_name))

stranger_countries

GLOBAL_TOP_10 |>
  filter(show_title %in% c("Stranger Things", "Wednesday", "Bridgerton"),
         grepl("TV", category)) |>
  group_by(show_title) |>
  summarise(total_hours = sum(weekly_hours_viewed, na.rm = TRUE),
            total_weeks = max(cumulative_weeks_in_top_10, na.rm = TRUE)) |>
  arrange(desc(total_hours))

# Press Release 2: Commercial Success in India
GLOBAL_TOP_10 |>
  filter(category %in% c("Films (Non-English)", "TV (Non-English)"),
         grepl("Hindi", show_title, ignore.case = TRUE)) |>
  summarise(total_hours = sum(weekly_hours_viewed, na.rm = TRUE))

COUNTRY_TOP_10 |>
  filter(country_name == "India", category %in% c("Films", "TV")) |>
  anti_join(
    COUNTRY_TOP_10 |> filter(country_name == "United States") |> distinct(show_title),
    by = "show_title"
  ) |>
  count(show_title, wt = cumulative_weeks_in_top_10, sort = TRUE)

COUNTRY_TOP_10 |>
  filter(country_name == "India") |>
  mutate(year = year(week)) |>
  group_by(year) |>
  summarise(total_entries = n())

COUNTRY_TOP_10 |>
  filter(country_name == "India") |>
  mutate(year = year(week)) |>
  group_by(year) |>
  summarise(total_weeks = sum(cumulative_weeks_in_top_10, na.rm = TRUE))


library(dplyr)
library(ggplot2)
library(DT)

# Top 5 Non-English Films by total hours
top5_nonenglish <- GLOBAL_TOP_10 |>
  filter(category == "Films (Non-English)") |>
  group_by(show_title) |>
  summarise(total_hours = sum(weekly_hours_viewed, na.rm = TRUE)) |>
  arrange(desc(total_hours)) |>
  slice(1:5)

# Print as a table
datatable(
  top5_nonenglish,
  options = list(pageLength = 5, searching = FALSE, info = FALSE),
  caption = htmltools::tags$caption(
    style = "caption-side: top; text-align: left;",
    "Top 5 Non-English Films on Netflix by Total Hours Viewed"
  )
) |>
  formatRound("total_hours", digits = 0)

# Chart
ggplot(top5_nonenglish, aes(x = reorder(show_title, total_hours), y = total_hours)) +
  geom_col(fill = "darkred") +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Top 5 Non-English Films on Netflix (Global Total Hours)",
    x = "Film",
    y = "Total Hours Viewed"
  ) +
  theme_minimal(base_size = 13)

total_nonenglish_hours <- GLOBAL_TOP_10 |>
  filter(category == "Films (Non-English)") |>
  summarise(total_hours = sum(weekly_hours_viewed, na.rm = TRUE))

total_nonenglish_hours


