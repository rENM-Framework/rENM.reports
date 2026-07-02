# rENM.reports 0.2.0

* `assemble_final_report()` — complete rewrite of the final report assembly
  pipeline:
  * Replaced rasterize-to-add-page-numbers approach with `cpdf` (Coherent PDF
    Tools), which stamps page numbers as native PDF text in a single pass.
    Output is fully vector; no quality loss, no bloated file size.
  * Added page normalization via `cpdf -scale-to-fit` to ensure all pages are
    uniform letter size (612 × 792 pt) in the output, regardless of the
    coordinate space of the source PDFs (e.g. raster-embedded pages at 300 dpi
    have a point-space size of 2550 × 3300 pt and previously caused varying
    apparent page sizes in PDF viewers).
  * Added `docx` parameter (default `FALSE`). When `TRUE`, a `.docx` version
    of the report is also written alongside the PDF. Rasterization is
    unavoidable for Word embedding and only runs when explicitly requested.
  * Added `dpi` parameter (default `150`) controlling rasterization resolution
    for the `.docx` path only.
  * `page_numbers` parameter now controls `cpdf` stamping instead of an
    in-process raster overlay; the `dpi` parameter has no effect on the PDF
    path.
  * Return value changed from a single path (character scalar) to a character
    vector: PDF path first, `.docx` path second when `docx = TRUE`.
  * `cpdf` is a new system dependency; install via `brew install cpdf` on
    macOS. The function checks for it at startup and stops with a clear
    install message if not found.
* Removed `graphics` from `Imports` (no longer used).
* Added `SystemRequirements: cpdf` to `DESCRIPTION`.

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
