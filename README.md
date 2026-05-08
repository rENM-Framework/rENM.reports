# rENM.reports

![rENM](https://img.shields.io/badge/rENM-framework-blue)
![module](https://img.shields.io/badge/module-reports-informational)

**Reporting and synthesis outputs for the rENM Framework**

## Overview

`rENM.reports` assembles the final, publication-ready outputs of the rENM
Framework. It integrates modeled results and analytical metrics into
structured summaries, maps, tables, and reports.

This package depends on `rENM.core` for project-directory resolution and
species metadata access. All functions accept an optional `project_dir`
argument; see `?rENM_project_dir` for configuration options.

## Key functions

| Function | Description |
|---|---|
| `gather_suitability_maps()` | Assemble 3×3 suitability map contact sheet (PNG, PDF, DOCX) |
| `gather_range_maps()` | Assemble 3×3 range map contact sheet (PNG, PDF, DOCX) |
| `gather_suitability_trend_stats()` | Merge state-level suitability and hot-spot statistics |
| `gather_top_variable_trend_maps()` | Assemble side-by-side variable trend map composites |
| `create_suitability_trend_summary_table()` | State-level GAP range / hot-spot summary table |
| `create_centroid_trend_summary_table()` | Centroid shift and regression summary table |
| `create_variable_trend_summary_table()` | Variable trend statistics summary table |
| `assemble_suitability_trends_page()` | Single-page PDF: suitability trend + change-trend maps |
| `assemble_centroid_trends_page()` | Single-page PDF: centroid trend map + table |
| `assemble_state_trends_page()` | Single-page PDF: state trend map + hot-spot map + table |
| `assemble_variable_trends_page()` | Single-page PDF: variable contributions plot + table |
| `assemble_variable_trend_maps_page()` | Stage variable trend map pages (caption + maps) |
| `assemble_suitability_timeseries_page()` | Single-page PDF: suitability time-series contact sheet |
| `assemble_range_timeseries_page()` | Single-page PDF: range time-series contact sheet |
| `assemble_final_report()` | Combine all pages into a single paginated PDF report |

## Installation

```r
# From GitHub
devtools::install_github("rENM-Framework/rENM.reports")

# From a local source directory
devtools::install_local("rENM.reports")
```

## Getting started

Analytical outputs from `rENM.analysis` must be present before running
reporting functions. The full reporting workflow runs in two stages:
first gather and table functions produce intermediate summaries, then
assemble functions compose those summaries into report pages.

```r
library(rENM.reports)

proj <- "/path/to/your/rENM/project"

# 1. Gather map contact sheets
gather_suitability_maps("CASP")
gather_range_maps("CASP")

# 2. Build summary tables
gather_suitability_trend_stats("CASP")
create_suitability_trend_summary_table("CASP")
create_centroid_trend_summary_table("CASP")
create_variable_trend_summary_table("CASP")
gather_top_variable_trend_maps("CASP")

# 3. Assemble report pages
assemble_suitability_trends_page("CASP")
assemble_centroid_trends_page("CASP")
assemble_state_trends_page("CASP")
assemble_variable_trends_page("CASP")
assemble_variable_trend_maps_page("CASP")
assemble_suitability_timeseries_page("CASP")
assemble_range_timeseries_page("CASP")

# 4. Combine into final report
assemble_final_report("CASP")
```

For interactive work, configure the project directory once per session to
avoid passing it to every function:

```r
options(rENM.project_dir = "/path/to/your/rENM/project")

gather_suitability_maps("CASP")
assemble_final_report("CASP")
```

## Reporting pipeline

```
gather_suitability_maps()          gather_range_maps()
gather_suitability_trend_stats()
        ↓
create_suitability_trend_summary_table()
create_centroid_trend_summary_table()
create_variable_trend_summary_table()
gather_top_variable_trend_maps()
        ↓
assemble_suitability_trends_page()
assemble_centroid_trends_page()
assemble_state_trends_page()
assemble_variable_trends_page()
assemble_variable_trend_maps_page()
assemble_suitability_timeseries_page()
assemble_range_timeseries_page()
        ↓
assemble_final_report()
```

Map contact sheets are written to `<run_dir>/Summaries/maps/`. Tables are
written to `<run_dir>/Summaries/tables/`. Assembled pages are written to
`<run_dir>/Summaries/pages/`. The final report is written to
`<run_dir>/Summaries/`. All functions append a structured summary block to
`<run_dir>/_log.txt`.

## Role in the rENM framework

`rENM.reports` is the final stage in the pipeline:

```
rENM.core → rENM.data → rENM.model → rENM.analysis → rENM.ai → rENM.reports
```

It consumes the quantitative trends and spatial metrics produced by
`rENM.analysis` and assembles them into structured, publication-ready
reporting artifacts.

## License

See `LICENSE` for details.

---

**rENM Framework** — A modular system for reconstructing and analyzing
long-term ecological niche dynamics.
