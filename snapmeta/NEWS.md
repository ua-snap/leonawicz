# snapmeta 0.1.8 (Release date: 2018-04-01)

* Updated to reflect inclusion of new `jfsp` package.
* Added `use_hex` function for providing package hex icon generating script to new packages.

# snapmeta 0.1.7 (Release date: 2018-01-09)

* Updated to reflect inclusion of new `snapplot` package.
* Other minor changes.

# snapmeta 0.1.6 (Release date: 2017-12-04)

* When calling `use_these`, `snapmeta` is now included as an Import in `DESCRIPTION` file for new package when creating package scaffolding only if the new package is listed as a SNAPverse sector package. `snapmeta` is never added as a suggested package.

# snapmeta 0.1.5 (Release date: 2017-10-11)

* Added `use_appsdesc` for adding template `DESCRIPTION` files to apps; to support the `snapapps` package.
    * The default arguments are specific to the SNAPverse author/maintainer since `snapmeta` is a developer package.
    * Available arguments include `title`, `author`, `url`, `license`, `mode` and `tags`.
* Added `use_appsreadme` for adding template `Readme.md` files to apps; to support the `snapapps` package.
    * The template is essentially empty.
* Added vignette content with a new section mentioning helper functions for including a existing Shiny app in a new R package.
* Generalized `use_apps` so that despite SNAPverse-themed default arguments, it can easily be used with arbitrary external apps and packages in which include them.
* Updated documentation.

# snapmeta 0.1.1

* Added `use_apps` to support the `snapapps` package.
* Updated documentation.
* Ignore `rsconnect/` with `use_apps`. It will be part of the bulk copy but then will be deleted following copy.

# snapmeta 0.1.0

* Added new functions and documentation.
