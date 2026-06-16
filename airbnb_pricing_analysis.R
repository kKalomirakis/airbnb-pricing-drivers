###################################################################
#                                                                 #
#  Pricing drivers (individual + neighborhood) and revenue loss:  #
#        Stockholm, Oslo, Athens, Rome, Vienna, Budapest          #
#                                                                 #
###################################################################


## Phase 1: Importing & Transforming data ##


# Creating a reusable pipeline to be used for each city
library(tidyverse)

run_airbnb_pipeline <- function(url, city_name) {
  listings_raw <- read_csv(url, show_col_types = FALSE)
  
  df <- listings_raw |>
    select(
      id, host_id, price, neighbourhood_cleansed, room_type,
      accommodates, bedrooms, bathrooms,
      host_is_superhost, host_total_listings_count
    ) |>
    mutate(
      price_num            = parse_number(as.character(price)),
      log_price            = log(price_num),
      is_professional_host = host_total_listings_count >= 5,
      city                 = city_name
    ) |>
    filter(
      !is.na(price_num), price_num > 0,
      !is.na(bedrooms), !is.na(bathrooms),
      !is.na(host_is_superhost),
      bedrooms <= 6
    )
  
  # Remove price outliers at 98th percentile (per city, before combining)
  p98 <- quantile(df$price_num, 0.98, na.rm = TRUE)
  df  <- df |> filter(price_num <= p98)
  
  return(df)
}

# ── City pipelines ────────────────────────────────────────────────────────────
# Southern Europe
athens    <- run_airbnb_pipeline("https://data.insideairbnb.com/greece/attica/athens/2025-09-26/data/listings.csv.gz", "Athens")
rome      <- run_airbnb_pipeline("https://data.insideairbnb.com/italy/lazio/rome/2025-09-14/data/listings.csv.gz", "Rome")

# Nordics
stockholm <- run_airbnb_pipeline("https://data.insideairbnb.com/sweden/stockholms-l%C3%A4n/stockholm/2025-09-29/data/listings.csv.gz", "Stockholm")
oslo      <- run_airbnb_pipeline("https://data.insideairbnb.com/norway/oslo/oslo/2025-09-29/data/listings.csv.gz", "Oslo")

# Central / Eastern Europe  ← new
vienna    <- run_airbnb_pipeline("https://data.insideairbnb.com/austria/vienna/vienna/2025-09-14/data/listings.csv.gz", "Vienna")
budapest  <- run_airbnb_pipeline("https://data.insideairbnb.com/hungary/k%C3%B6z%C3%A9p-magyarorsz%C3%A1g/budapest/2025-09-25/data/listings.csv.gz", "Budapest")


## Phase 2: Pipeline sanity checks ──────────────────────────────────────────

# 1 — How many listings did each city produce after cleaning?
map_dfr(
  list(athens, rome, stockholm, oslo, vienna, budapest),
  ~ tibble(city = unique(.x$city), n = nrow(.x))
)

# 2 — Any NAs remaining in key columns?
all_cities <- bind_rows(athens, rome, stockholm, oslo, vienna, budapest)
all_cities |>
  group_by(city) |>
  summarise(across(everything(), ~ sum(is.na(.))))

# 3 — Price ranges look sensible across cities? (still in local currency)
all_cities |>
  group_by(city) |>
  summarise(
    min_price    = min(price_num),
    median_price = median(price_num),
    max_price    = max(price_num),
    n            = n()
  )


## Phase 3: Currency conversion to EUR ──────────────────────────────────────

# Exchange rates from the European Central Bank, day of September 2025
# Source: ECB reference rates (EUR-Lex, OJ C/2025/4506, published 01.10.2025)
# Athens (EUR) and Rome (EUR) and Vienna (EUR) need no conversion.
# Stockholm (SEK): 1 EUR = 11.03 SEK
# Oslo (NOK):      1 EUR = 11.6775 NOK
# Budapest (HUF):  1 EUR = 390.73 HUF

rates <- c(EUR = 1, SEK = 11.03, NOK = 11.6775, HUF = 390.73)

df <- bind_rows(athens, rome, oslo, stockholm, vienna, budapest) |>
  mutate(
    currency_code = case_when(
      city %in% c("Athens", "Rome", "Vienna") ~ "EUR",
      city == "Stockholm"                      ~ "SEK",
      city == "Oslo"                           ~ "NOK",
      city == "Budapest"                       ~ "HUF"
    ),
    price_eur = price_num / rates[currency_code],
    
    region = case_when(
      city %in% c("Athens", "Rome")      ~ "Southern Europe",
      city %in% c("Oslo", "Stockholm")   ~ "Nordics",
      city %in% c("Vienna", "Budapest")  ~ "Central/Eastern Europe"
    )
  )

# Sanity check: zero NA prices after conversion
df |> filter(is.na(price_eur)) |> nrow()   # should be 0
df |> count(city, currency_code, region)   # every city maps to one currency


## Phase 4: Descriptive statistics ──────────────────────────────────────────

df |>
  group_by(city, region) |>
  summarise(
    n                = n(),
    pct_professional = mean(is_professional_host, na.rm = TRUE) * 100,
    pct_superhost    = mean(host_is_superhost,    na.rm = TRUE) * 100,
    median_eur       = median(price_eur),
    .groups          = "drop"
  ) |>
  arrange(region, city)

# Professionalization gradient across all cities (all room types)
df |>
  group_by(city, is_professional_host) |>
  summarise(median_eur = median(price_eur), n = n(), .groups = "drop") |>
  arrange(city, is_professional_host)

# Robustness: holding room type constant (entire homes only)
df |>
  filter(!is.na(is_professional_host), room_type == "Entire home/apt") |>
  group_by(city, is_professional_host) |>
  summarise(median_eur = median(price_eur), n = n(), .groups = "drop")

# Listings-per-host ratio: proxy for market concentration
df |>
  group_by(city, region) |>
  summarise(
    listings     = n(),
    unique_hosts = n_distinct(host_id),
    ratio        = listings / unique_hosts,
    .groups      = "drop"
  ) |>
  arrange(desc(ratio))


## Phase 5: Mixed-effects model ─────────────────────────────────────────────

# Key modeling decisions:
#   • city = FIXED effect  — only 6 named groups; we want per-city estimates
#   • neighbourhood = RANDOM intercept — 90+ groups, ICC ≈ 0.25 in the
#     6-city model; ignoring clustering would deflate SEs on interactions
#   • log(price_eur) as outcome — prices are right-skewed; log scale gives
#     interpretable % effects via exp(coef) − 1

library(lme4)
library(emmeans)

m <- lmer(
  log(price_eur) ~ is_professional_host * city +
    room_type + accommodates + bedrooms +
    (1 | city:neighbourhood_cleansed),
  data = filter(df, !is.na(is_professional_host))
)
summary(m)

# Helper: extract per-city professional premium as % with significance flag
prof_effects <- function(model, label) {
  emmeans(model, ~ is_professional_host | city, lmer.df = "asymptotic") |>
    contrast("revpairwise", by = "city") |>
    as_tibble() |>
    transmute(
      spec = label,
      city,
      pct  = (exp(estimate) - 1) * 100,
      sig  = abs(estimate / SE) > 1.96
    )
}

prof_effects(m, "Base model")


## Phase 6: Coefficient plot ─────────────────────────────────────────────────

emm  <- emmeans(m, ~ is_professional_host | city, lmer.df = "asymptotic")
prof <- contrast(emm, method = "revpairwise", by = "city") |>
  as.data.frame() |>
  mutate(
    lo     = estimate - 1.96 * SE,
    hi     = estimate + 1.96 * SE,
    pct    = (exp(estimate) - 1) * 100,
    pct_lo = (exp(lo)       - 1) * 100,
    pct_hi = (exp(hi)       - 1) * 100,
    region = case_when(
      city %in% c("Athens", "Rome")     ~ "Southern Europe",
      city %in% c("Oslo", "Stockholm")  ~ "Nordics",
      city %in% c("Vienna", "Budapest") ~ "Central/Eastern Europe"
    )
  )

ggplot(prof, aes(x = pct, y = reorder(city, pct), colour = region)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_errorbarh(aes(xmin = pct_lo, xmax = pct_hi), height = 0.18, linewidth = 0.7) +
  geom_point(size = 3.5) +
  geom_text(aes(label = sprintf("%+.0f%%", pct)), vjust = -1.1, size = 3.5,
            show.legend = FALSE) +
  scale_colour_manual(values = c(
    "Southern Europe"        = "#D55E00",
    "Nordics"                = "#0072B2",
    "Central/Eastern Europe" = "#009E73"
  )) +
  labs(
    title    = "The professional-host price effect varies across European regions",
    subtitle = "Professional vs. amateur price difference, net of room type, size and neighbourhood",
    x        = "Price difference for professional hosts (%)",
    y        = NULL,
    colour   = "Region"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.y = element_blank())


## Phase 7: Robustness checks ────────────────────────────────────────────────

# Check 1: Add host-level random intercept to account for multi-listing hosts contributing correlated observations
m_host <- lmer(
  log(price_eur) ~ is_professional_host * city +
    room_type + accommodates + bedrooms +
    (1 | city:neighbourhood_cleansed) + (1 | host_id),
  data = filter(df, !is.na(is_professional_host))
)
prof_effects(m_host, "+ host random effect")

# Check 2: Threshold sensitivity — professional cutoff at ≥2, ≥5, ≥10 listings
fit_threshold <- function(k) {
  d <- df |>
    mutate(is_professional_host = host_total_listings_count >= k) |>
    filter(!is.na(is_professional_host))
  lmer(
    log(price_eur) ~ is_professional_host * city +
      room_type + accommodates + bedrooms +
      (1 | city:neighbourhood_cleansed),
    data = d
  )
}
map2_dfr(
  c(2,    5,           10),
  c("≥2", "≥5 (base)", "≥10"),
  ~ prof_effects(fit_threshold(.x), .y)
)

# Check 3: Entire homes only — removes room-type composition confound
m_entire <- lmer(
  log(price_eur) ~ is_professional_host * city +
    accommodates + bedrooms +
    (1 | city:neighbourhood_cleansed),
  data = filter(df, !is.na(is_professional_host), room_type == "Entire home/apt")
)
prof_effects(m_entire, "Entire homes only")

# Assemble full robustness table (rows = specs, columns = cities)
robustness_table <- bind_rows(
  prof_effects(m,       "Base model"),
  prof_effects(m_host,  "+ host random effect"),
  map2_dfr(c(2, 5, 10), c("≥2", "≥5 (base)", "≥10"),
           ~ prof_effects(fit_threshold(.x), .y)),
  prof_effects(m_entire, "Entire homes only")
) |>
  mutate(
    pct  = round(pct, 1),
    cell = paste0(pct, "%", if_else(sig, "*", ""))
  ) |>
  select(spec, city, cell) |>
  pivot_wider(names_from = city, values_from = cell)

robustness_table


