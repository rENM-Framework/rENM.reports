# rENM.reports 0.2.0.9000
* `assemble_final_report()` — the `cpdf` check now runs on every call rather
  than only when `page_numbers = TRUE`. Page normalization invokes `cpdf`
  unconditionally, so `page_numbers = FALSE` never avoided the dependency;
  it only skipped the check, leaving the normalization call to fail with
  `cpdf`'s exit status instead of the message explaining what to install.
  The help text implied the same thing and has been corrected.
* `inst/resources/variables.pdf` — removed a trailing blank page. The file
  had always been two pages with an empty second one, which put a blank page
  at the end of every assembled report. The `.docx` it is generated from ends
  with an empty paragraph that spills past the first page; that paragraph is
  still there, so the blank page returns if the PDF is regenerated from Word
  without deleting it.
* Added `inst/resources/methods.docx`/`.pdf`, a methods note appended to
  every report. The report previously carried no methods text at all, so a
  reader had no way to know what defined the modeled extent. It states that
  the extent is the GAP range polygon buffered outward by 250 km in
  EPSG:5070, derives the 250 km figure from Huang, Sauer & Dubayah (2017),
  explains that range-based statistics are still computed against the
  unbuffered polygon, describes the buffer-ring statistic, and records the
  limitation that the figure is drawn from permanent resident species and
  may not suit strongly migratory ones.
* `inst/resources/variables.docx`/`.pdf` — dropped the version number from
  the title, which now reads "MERRA-2 and MERRAclim-2 variables in the
  extended dataset". It had read v0.1.0 and would have gone stale at every
  release.
* `assemble_final_report()` — `appendix` now accepts a character vector and
  defaults to `c("methods.pdf", "variables.pdf")`, so the methods note
  precedes the MERRA variable reference at the back of the report.
* `assemble_final_report()` — added an `optional_pages` argument, defaulting
  to the AI narrative page. Pages named there are skipped with a warning
  when their PDF is absent, rather than aborting assembly. Previously any
  missing page was fatal, so a transient failure of the external narrative
  service cost the entire report even though every other page had been
  produced. Missing pages not named in `optional_pages` still raise an
  error, since that normally means something upstream broke and an
  incomplete report should not ship quietly.
* `assemble_state_trends_page()` — the hotspot summary caption
  (`inst/captions/hotspot_summary_caption.docx`/`.pdf`) now describes the
  boundary block, noting that those two rows are range-wide rather than
  states, what comparing them shows, and that they are not to be summed with
  the state rows above.
* `create_suitability_trend_summary_table()` — appends a boundary block below
  the state rows, separated by a rule: `Range interior` and
  `Buffer ring (250 km)`, read from the CSV written by
  `rENM.analysis::find_boundary_trend_statistics()`. These are range-wide
  figures rather than states, and the comparison between them is the point —
  a ring more positive than the interior points to conditions improving where
  the species would expand into. They are never summed with the state rows.
  Extent Area and Range % are blank for them, Range % being a state's share
  of the species total range. The block is skipped when the file is absent,
  so the table still builds for runs predating that function.
* `gather_suitability_trend_stats()` — now checks that no row reports a hot
  spot area exceeding its range area, or a range area exceeding its extent
  area. Offending rows are named in a warning and recorded in the run log
  under an "Area checks" line; the table is still written, so one suspect row
  does not cost an unattended run. The extent comparison allows 1% because a
  cell-summed area and a vector area measure the same region on different
  bases. The hot-spot comparison cannot currently fail, since both areas are
  coverage-weighted sums over the same cells, and is kept as a regression
  guard: that invariant has broken twice before.
* `gather_suitability_trend_stats()` — the Range Area column and the Hot Spot %
  denominator now come from `range_area_km2` in the hotspot-stats file rather
  than `GAP.RANGE.AREA`. Both figures then share a measurement basis with the
  hot-spot numerator, so Hot Spot % cannot exceed 100. Reported Range Area
  shifts by well under a percent: it is the same range measured on the model
  grid instead of from the source polygon. This matters more once results are
  reported as intervals across seeds, since an interval's upper bound can
  cross 100 where a point estimate would not.
* `gather_suitability_trend_stats()` — Hot Spot % is now computed as hotspot
  area over range area, matching the table's own documented definition,
  instead of being passed through from the upstream hotspot-stats file
  (which only tracks hotspot area as a percent of state area). Also renamed
  the `state_area` field to `extent_area_state` to reflect what it actually
  measures: the portion of the state's area within the species' modeled
  extent, not the state's true area (which varied across species reports
  for the same state, since it depended on each species' extent).
* `create_suitability_trend_summary_table()` — renamed the "State Area"
  column to "Extent Area" to match the corrected field name
  above.
* `assemble_state_trends_page()` — updated the hotspot summary table caption
  (`inst/captions/hotspot_summary_caption.docx`/`.pdf`) to describe the
  renamed "Extent Area" column accurately; it previously
  called this column "total state area," which was already inaccurate
  before the rename.
* `create_suitability_trend_summary_table()` — added `top_states` parameter.
  When set to a positive integer, the table (Excel, PNG, and PDF) is
  restricted to the states with the highest Range % (gap range as a
  percentage of state area), sorted descending. Default `NULL` preserves
  prior behavior of including all states.
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
* Internal: PNG/PDF export via `gt::gtsave()` now temporarily raises
  `options(chromote.timeout)` to 60s (was the chromote default of 10s) to
  avoid spurious `"Chrome debugging port not open after 10 seconds"`
  failures when the headless Chrome subprocess is slow to start under
  system load. Affects `create_suitability_trend_summary_table()`,
  `create_centroid_trend_summary_table()`, and
  `create_variable_trend_summary_table()`.

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
