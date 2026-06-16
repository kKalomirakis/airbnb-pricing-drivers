###################################################################
#                                                                 #
#  Pricing drivers (individual + neighborhood) and revenue loss:  #
#                Stockholm, Oslo, Athens, Rome                    # 
#                                                                 #
###################################################################

# is Airbnb a sharing-economy thing run by locals renting a spare room, or a commercial operation run by professional managers
# and does that differ between over-touristed Southern Europe and the wealthier, less leisure-driven Nordics?

## Phase 1: Importing & Transforming data ##


# Creating a reusable pipeline to be used for each city
library(tidyverse)
run_airbnb_pipeline <- function(url, city_name) {

# This downloads and reads the compressed CSV directly from "insideairbnb.com"
# And suppresses the output to avoid cluttering
  listings_raw <- read_csv(url, show_col_types = FALSE) 

# Defining a dataframe from listings_raw with only the relevant factors involved
  df <- listings_raw |>
    select(
      id, price, neighbourhood_cleansed, room_type,
      accommodates, bedrooms, bathrooms,
      host_is_superhost, host_total_listings_count, host_id
    ) |>
    mutate(
      price_num = parse_number(as.character(price)),
      log_price = log(price_num),
      is_professional_host = host_total_listings_count >= 5,
      city = city_name
    ) |>
    filter(
      !is.na(price_num), price_num > 0,
      !is.na(bedrooms), !is.na(bathrooms),
      !is.na(host_is_superhost),
      bedrooms <= 6
    )
  
  # Remove price outliers at 98th percentile
  p98 <- quantile(df$price_num, 0.98, na.rm = TRUE)
  df <- df |> filter(price_num <= p98)
  
  return(df)
}

athens    <- run_airbnb_pipeline("https://data.insideairbnb.com/greece/attica/athens/2025-09-26/data/listings.csv.gz", "Athens")
rome <- run_airbnb_pipeline("https://data.insideairbnb.com/italy/lazio/rome/2025-09-14/data/listings.csv.gz", "Rome")
stockholm <- run_airbnb_pipeline("https://data.insideairbnb.com/sweden/stockholms-l%C3%A4n/stockholm/2025-09-29/data/listings.csv.gz", "Stockholm")
oslo      <- run_airbnb_pipeline("https://data.insideairbnb.com/norway/oslo/oslo/2025-09-29/data/listings.csv.gz", "Oslo")


# Confirming that the pipeline is clean and consistent
# 1 — How many listings did each city produce after cleaning?
map_dfr(list(athens, rome, stockholm, oslo), 
        ~ tibble(city = unique(.x$city), n = nrow(.x)))

# 2 — Any NAs remaining in any city?
all_cities <- bind_rows(athens, rome, stockholm, oslo)
all_cities |>
  group_by(city) |>
  summarise(across(everything(), ~ sum(is.na(.))))

# 3 — Price ranges look sensible across cities?
all_cities |>
  group_by(city) |>
  summarise(
    min_price    = min(price_num),
    median_price = median(price_num),
    max_price    = max(price_num),
    n            = n()
  )


## Descriptive statistics & conversions ##


# Converting Oslo and Stockholm price to Euros
# Rates from European Central Bank at the date when data was gathered

df <- bind_rows(athens, rome, oslo, stockholm)
class(df)

rates <- c(NOK = 11.6775, SEK = 11.03, EUR = 1) 
df <- df %>%
  mutate(
    currency_code = case_when(
      city %in% c("Athens", "Rome") ~ "EUR",
      city == "Stockholm"           ~ "SEK",
      city == "Oslo"                ~ "NOK"
    ),
    price_eur = price_num / rates[currency_code]
  )

# Checking that all countries are in the same currency
df %>% filter(is.na(price_eur)) %>% nrow() # should be 0


df %>%
  group_by(city) %>%
  summarise(
    n = n(),
    pct_professional = mean(is_professional_host, na.rm = TRUE) * 100,
    pct_superhost = mean(host_is_superhost, na.rm = TRUE) * 100,
    median_eur = median(price_eur)
  )

# Airbnb is sold as a "sharing economy," but across these four cities it's actually four structurally distinct markets 
# from Oslo's amateur cottage industry to Athens's hyper-commercialized operator market — and professionalization doesn't even imply a single pricing logic. 
# That's a thesis with stakes, and it's defensible from your own data.

# Do prof hosts charge more or less than amateurs, within each city?
df %>%
  group_by(city, is_professional_host) %>%
  summarise(median_eur = median(price_eur), n = n(), .groups = "drop") %>%
  arrange(city, is_professional_host)

# Athens: amateur €68 → professional €84 (+24%)
# Rome: amateur €120 → professional €149 (+24%)
# Oslo: amateur €107 → professional €92.8 (−13%)
# Stockholm: amateur €113 → professional €98.6 (−13%)

# Interpretation: 
# In the tourist-saturated South, professional operators run the premium segment (optimized, well-located tourist units) and command a markup. 
# In the Nordics, the few professionals appear to operate at the budget end (standardized and competed-down units) 
# While individual residents renting their own (nicer, larger) homes sit above them. Same structural feature, opposite pricing logic.


# Doing the same but holding room-type constant:
df %>%
  filter(!is.na(is_professional_host), room_type == "Entire home/apt") %>%
  group_by(city, is_professional_host) %>%
  summarise(median_eur = median(price_eur), n = n(), .groups = "drop")

# Oslo discount was conditional on room-type, meaning that they have pricier entire homes (+10%), while more professionals list cheaper rooms
# Stockholm discount is maintained, meaning that professionals undercut amatuer hosts across all room types
# Athens & Rome are maintained and unchanged as well. 



library(lme4)
m <- lmer(log(price_eur) ~ is_professional_host * city + room_type + accommodates + bedrooms
          + (1 | city:neighbourhood_cleansed),
          data = filter(df, !is.na(is_professional_host)))
summary(m)




library(emmeans)
# 1. Professional − amateur effect within each city (log scale), with SEs
emm  <- emmeans(m, ~ is_professional_host | city, lmer.df = "asymptotic")
prof <- contrast(emm, method = "revpairwise", by = "city") %>%   # TRUE − FALSE
  as.data.frame() %>%
  mutate(
    lo     = estimate - 1.96 * SE,
    hi     = estimate + 1.96 * SE,
    pct    = (exp(estimate) - 1) * 100,  # log effect -> % change
    pct_lo = (exp(lo) - 1) * 100,
    pct_hi = (exp(hi) - 1) * 100,
    region = if_else(city %in% c("Athens", "Rome"), "Southern Europe", "Nordics")
  )

# 2. Coefficient / forest plot
ggplot(prof, aes(x = pct, y = reorder(city, pct), colour = region)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_errorbarh(aes(xmin = pct_lo, xmax = pct_hi), height = 0.18, linewidth = 0.7) +
  geom_point(size = 3.5) +
  geom_text(aes(label = sprintf("%+.0f%%", pct)), vjust = -1.1, size = 3.5,
            show.legend = FALSE) +
  scale_colour_manual(values = c("Southern Europe" = "#D55E00",
                                 "Nordics" = "#0072B2")) +
  labs(
    title = "The professional-host price effect flips between regions",
    subtitle = "Professional vs. amateur price difference, net of room type, size and neighbourhood",
    x = "Price difference for professional hosts (%)",
    y = NULL, colour = "Region"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.y = element_blank())



library(lme4); library(emmeans); library(tidyverse)

prof_effects <- function(model, label) {
  emmeans(model, ~ is_professional_host | city, lmer.df = "asymptotic") |>
    contrast("revpairwise", by = "city") |>          # professional − amateur
    as_tibble() |>
    transmute(spec = label, city,
              pct = (exp(estimate) - 1) * 100,
              sig = abs(estimate / SE) > 1.96)
}



# Robustness check 1: accounting for both neighbourhood and multicollinear listings by the same host
m_host <- lmer(log(price_eur) ~ is_professional_host * city + room_type + accommodates + bedrooms +
                 (1 | city:neighbourhood_cleansed) + (1 | host_id),
               data = filter(df, !is.na(is_professional_host)))
prof_effects(m_host, "+ host random effect")


# Robustness check 2: whether the results are sensitive to the prof-threshold
fit_threshold <- function(k) {
  d <- df |> mutate(is_professional_host = host_total_listings_count >= k) |>
    filter(!is.na(is_professional_host))
  lmer(log(price_eur) ~ is_professional_host * city + room_type + accommodates + bedrooms +
         (1 | city:neighbourhood_cleansed), data = d)
}
map2_dfr(c(2, 5, 10), c("≥2", "≥5 (base)", "≥10"),
         ~ prof_effects(fit_threshold(.x), .y))


# Robustness check 3: restricting toward only entire-home listings
m_entire <- lmer(log(price_eur) ~ is_professional_host * city + accommodates + bedrooms +
                   (1 | city:neighbourhood_cleansed),
                 data = filter(df, !is.na(is_professional_host), room_type == "Entire home/apt"))
prof_effects(m_entire, "entire homes only")

# interpretation: The regional reversal was robust to room-type restriction for three of four cities. 
# Oslo's professional discount attenuated to near-zero among entire homes (−2.6%, p > 0.05), 
# suggesting it partly reflects professionals' tendency to list more private rooms rather than a pricing strategy per se.



