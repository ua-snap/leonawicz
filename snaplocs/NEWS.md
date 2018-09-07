# snaplocs 0.2.3 (Release date: 2017-01-14)

* Fixed bug where vector of multiple locations could possibly return state/province, country or coordinates in incorrect order with respect to vector.


# snaplocs 0.2.2 (Release date: 2017-12-15)

* Breaking changes: Changed `region` argument in metadata helper functions to `group`. Change `loc` and `region` columns names in `locs` data frame to `Location` and `Group`. These changes bring more consistency with packages like `snapclim` and `snapfire`.
* Migrate `regions` data frame to `snaplocs` from `snapclim`. `regions` data frame will support multiple downsteam packages.
* Update introduction vignette, help documentation and unit tests.

# snaplocs 0.2.1 (Release date: 2017-10-05)

* Added `region` argument to metadata helper functions to subset `locs` data frame and limit possible duplicate results when a point location name is unique within a region but not between regions.
* Update introduction vignette.
* Added unit tests.

# snaplocs 0.2.0

* Added function for conversion to NAD83 Alaska Albers equal area conic projection.
* Added unit tests and documentation.
* Vectorized helper functions.
* Added initial introduction vignette.

# snaplocs 0.1.0

* Added point location metadata helper functions.
* Added point locations data set.

# snaplocs 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
