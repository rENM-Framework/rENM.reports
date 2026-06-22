# rENM.reports 0.1.0

* Initial release.
* Added `gather_suitability_maps()` to assemble a 3×3 suitability map contact
  sheet (PNG, PDF, DOCX).
* Added `gather_range_maps()` to assemble a 3×3 range map contact sheet
  (PNG, PDF, DOCX).
* Added `gather_suitability_trend_stats()` to merge state-level suitability
  and hot-spot statistics.
* Added `gather_top_variable_trend_maps()` to assemble side-by-side variable
  trend map composites.
* Added `create_suitability_trend_summary_table()` to produce a state-level
  GAP range and hot-spot summary table (XLSX, PNG, PDF).
* Added `create_centroid_trend_summary_table()` to produce a centroid shift and
  regression summary table (XLSX, PNG, PDF).
* Added `create_variable_trend_summary_table()` to produce a variable trend
  statistics summary table (XLSX, PNG, PDF).
* Added `assemble_suitability_trends_page()` to compose a single-page PDF
  of the suitability trend and change-trend maps.
* Added `assemble_centroid_trends_page()` to compose a single-page PDF of the
  centroid trend map and summary table.
* Added `assemble_state_trends_page()` to compose a single-page PDF of the
  state trend map, hot-spot map, and summary table.
* Added `assemble_variable_trends_page()` to compose a single-page PDF of the
  variable contributions plot and summary table.
* Added `assemble_variable_trend_maps_page()` to stage variable trend map pages
  combining caption and map panels.
* Added `assemble_suitability_timeseries_page()` to compose a single-page PDF
  of the suitability time-series contact sheet.
* Added `assemble_range_timeseries_page()` to compose a single-page PDF of the
  range time-series contact sheet.
* Added `assemble_final_report()` to combine all assembled pages into a single
  paginated PDF species report.
