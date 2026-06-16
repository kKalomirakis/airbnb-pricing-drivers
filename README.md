# Is Airbnb Still a Sharing Economy? A Six-City European Study

> **The short answer:** It depends entirely on where you look —
> and the pattern is not what you'd expect.

---

## The Question That Started This

Airbnb was founded on a simple idea: ordinary people renting out a spare room to travellers passing through. A sharing economy. Neighbours helping neighbours, with a small platform fee in between.

That story has always been contested. Critics argue Airbnb long ago stopped being a network of individuals and became a marketplace dominated by commercial operators running dozens of properties like a hotel business — removing homes from the housing market, driving up rents, and gutting the very neighbourhoods tourists come to experience. Sparking large protests in cities that are heavily affected by this, for instance Athens and Rome. 

But who is right? And does the answer change depending on which city — or which part of Europe — you look at?

That is the question this project set out to answer.

---

## What I Did

I pulled listing data for six European cities from Inside Airbnb — a public-interest project that scrapes Airbnb's platform — covering roughly **70,000 listings** across late September 2025:

| Region | Cities |
|---|---|
| Southern Europe | Athens, Rome |
| Nordics | Oslo, Stockholm |
| Central/Eastern Europe | Vienna, Budapest |

For each listing I could see the price per night, the type of space, the size, the neighbourhood — and crucially, **how many properties that host controls in total**. A host managing one listing looks like the original sharing-economy vision. A host managing thirty looks like a property management company.

I defined anyone with five or more listings as a "professional operator" and asked: **do these operators charge more or less than ordinary hosts — and does that differ by region?**

---

## What I Found

The results split cleanly — and surprisingly — along regional lines.

**In Southern Europe, professional operators charge a premium.** In Athens and Rome, a listing run by a commercial operator costs roughly 10–13% more than a comparable listing from an individual host, after accounting for the size of the property, the type of room, and the neighbourhood. These cities are dominated by tourist demand, and professional operators appear to have positioned themselves at the top end of that market — optimised, well-located, premium-priced.

**In the Nordics, the opposite is true.** In Oslo and Stockholm, professional operators charge *less* than individual hosts — roughly 7–14% less. The Nordics have stricter short-term rental regulation and less tourism pressure. Here, commercial operators appear to compete on volume and price rather than premium positioning. Individual residents renting their own homes — nicer, larger, more personal — sit *above* them.

**Central and Eastern Europe sit in between**, with Vienna and Budapest showing their own distinct pattern that reflects their different regulatory environments and tourism profiles.

The key insight is not just that the numbers differ — it is that **the same business model produces opposite outcomes depending on the market it operates in**. Professionalization is not inherently premium or budget. It is a strategy, and it adapts.

![Coefficient plot showing professional host price effect by city](airbnb-pricing-drivers/coefficient_plot.png)

---

## The Moment That Threatened The Regional Differentiation

My first pass at the data showed a clean, satisfying pattern: professional hosts charged more in the South and less in the North, and the difference looked symmetrical and neat.

To be certain that the results were robust, I ran a robustness check — restricting the comparison to like-for-like listings (entire homes only, removing the possibility that professionals simply list different *types* of property). Oslo's discount disappeared almost entirely. The clean story had a crack in it.

Digging further revealed why: in Oslo, professional operators list a higher proportion of cheaper room types. When you compare only entire homes against entire homes, their apparent discount was mostly a composition effect, not a pricing strategy. The discount in Stockholm, however, survived — professionals there genuinely undercut individual hosts even on identical unit types.

This kind of self-correction is invisible in a final result but it is where most analytical errors live. The final model accounts for unit type, property size, and neighbourhood simultaneously, which is why the conclusions are trustworthy rather than just interesting.

---

## Why This Matters Beyond the Numbers

The professional-host share varies enormously: **59% of Athens listings** are run by multi-property operators, versus just **10% in Oslo**. Athens has effectively ceased to be a sharing economy. Oslo still resembles one.

That gap has consequences. Cities with high professional operator density have less housing available for long-term residents, higher rental prices in surrounding streets, and a different character in their tourist districts. The question of *who controls Airbnb listings* is not an academic one — it is the central question in housing policy debates from Barcelona to Berlin.

This project does not answer the policy question. But it shows that the structure of these markets varies dramatically across cities that are only a short flight apart, and that a single regulatory approach will land differently depending on which type of market a city already has.

---

## How It Was Built

The analysis is written entirely in R. A single reusable pipeline function downloads, cleans, and standardises data from each city — meaning every city was processed identically, with no ad hoc adjustments. Prices were converted to EUR using European Central Bank reference rates from the day the data was collected.

The core model is a mixed-effects regression: fixed effects for city and host type, random intercepts for neighbourhood (to account for the fact that listings on the same street are not independent observations). The finding was tested six ways — different professional thresholds, different room-type restrictions, different model specifications — and the regional pattern held in five of six checks, with the one exception (Oslo, entire homes only) explained and documented rather than quietly dropped.

Full code, model output, and robustness tables are in [`airbnb_pricing_analysis.R`](airbnb_pricing_analysis.R).

To reproduce: install R, run `renv::restore()` to install the exact package versions used, then source the script top to bottom. All data downloads live from Inside Airbnb — no local files required.

---

## What I Would Do Next

Three extensions would meaningfully strengthen this work:

**Market concentration.** The professional-host flag is binary. A richer measure would be the share of each city's listings controlled by the largest fifty hosts — a Gini coefficient for Airbnb supply. Cities where ten operators control half the listings have a different problem than cities where the same share is spread across thousands of small landlords.

**A natural experiment.** Several European cities introduced or tightened short-term rental regulation in 2023–2024. Comparing a city before and after a policy change — using a neighbouring unaffected city as a control — would move this from description to causal identification. That is the analysis that would actually inform policy.

**Seasonality.** A September snapshot captures one moment. The professionalization premium likely grows in peak summer season and shrinks in winter, which would tell us whether commercial operators are specifically exploiting tourist surges or operating year-round.

---

*Data: [Inside Airbnb](https://insideairbnb.com), September 2025.
Analysis: R (tidyverse, lme4, emmeans). Author: [Konstantin Kalomirakis].*
