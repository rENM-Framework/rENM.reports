# rENM.reports

![rENM](https://img.shields.io/badge/rENM-framework-blue) ![module](https://img.shields.io/badge/module-reports-informational)

**Reporting and synthesis outputs for the rENM Framework**

## Overview

`rENM.reports` assembles the final, publication-ready outputs of the rENM Framework. It integrates modeled results and analytical metrics into structured summaries, maps, tables, and reports.

This package focuses on **presentation, synthesis, and communication of results**.

## Role in the rENM Framework

Within the modular rENM ecosystem, `rENM.reports`: - Aggregates outputs from modeling and analysis stages - Builds **maps, tables, and time series summaries** - Assembles **multi-page, structured reports** - Produces consistent, reproducible reporting artifacts

It is the final step in translating analytical results into human-readable products.

## Key Functions

-   `assemble_final_report()` — Build complete multi-section report
-   `assemble_suitability_trends_page()` — Summarize suitability trends
-   `assemble_centroid_trends_page()` — Summarize centroid dynamics
-   `assemble_variable_trends_page()` — Summarize variable effects
-   `assemble_state_trends_page()` — Regional summaries
-   `assemble_range_timeseries_page()` — Range dynamics over time
-   `assemble_suitability_timeseries_page()` — Suitability trajectories
-   `create_*_summary_table()` — Generate standardized tables
-   `gather_*()` — Collect maps, statistics, and intermediate outputs

## Installation

``` r
devtools::install_local("rENM.reports")
```

## Example

``` r
library(rENM.reports)

# assemble final report
assemble_final_report("CASP")
```

## Relationship to Other Packages

`rENM.reports` integrates outputs from all upstream components into coherent deliverables.

## License

See `LICENSE` for details.

------------------------------------------------------------------------

**rENM Framework**\
A modular system for reconstructing and analyzing long-term ecological niche dynamics.
