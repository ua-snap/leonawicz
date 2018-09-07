# snapprep 0.2.8 (Release date: 2018-01-23)

* Updated to work with changes in `snapgrid` package data.

# snapprep 0.2.7 (Release date: 2017-12-13)

* Added functions `clim_locs_prep`, `clim_locs`, and `clim_locs_dec_all` for extracting and curating point location climate data.

# snapprep 0.2.6 (Release date: 2017-10-20)

* Added `split_monthly_files` function for alternative file structure for outputs.
* Added `region_group` argument to `aws_upload` for file filtering by region group.
* Minor bug fixes.
* Updated documentation.

# snapprep 0.2.5 (Release date: 2017-10-06)

* Added `aws_upload` for uploading curated SNAP data to AWS S3 buckets for use by Shiny apps and other applications and projects.
* Added a vignette providing example context and usage of `aws_upload` for uploading SNAP data to AWS S3 buckets as a final step in typical SNAP data prep operations.

# snapprep 0.2.0 (Release date: 2017-10-06)

* Rearranged arguments in `clim_dist_seasonal`.
* Added `region_group` argument to `clim_stats_ar5` for file filtering to allow smaller batch processing jobs.

# snapprep 0.1.9 (Release date: 2017-10-05)

* Fixed issue with output directory paths for seasonal climate distributions.
* Updated example code.
* Removed unneeded, unused function argument from `clim_dist_seasonal`. The function only takes `files` for input. Use absolute paths.
* Added `vairable` and `rcp` arguments to `clim_dist_seasonal` for splitting processing into smaller file batches.
* Refactored `clim_stats_ar5`. This function uses monthly or seasonal outputs from `clim_dist_monthly` and `clim_dist_seasonal` to compute statistics.

# snapprep 0.1.0

* Added initial climate variable distribution estimation content to vignette.
* Updated functions and documentation for CMIP5 data extraction and density estimation functions.
* Updated defaults in `snapdef`.
* Add defaults unit tests.
* Added better handling of near-zero precipitation.

# snapprep 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
