# snaputils 0.2.6 (Release date: 2017-10-27)

* Ported SNAP-related image files to package.
* Added `snap_res` function for obtaining resource path so that images can be easily accessed by other packages, apps and general R code.
* Updated unit tests.

# snaputils 0.2.5 (Release date: 2017-10-25)

* Ported several functions from `apputils` and refactored code.
* Added package documentation.

# snaputils 0.2.1 (Release date: 2017-10-09)

* Update metadata for SNAP apps.
* Removed suggested package, `snapmeta`.

# snaputils 0.2.0 (Release date: 2017-09-28)

* Added `faq` override function. This function overrides rather than wraps around `apputils::faq`. It uses a SNAP FAQ dictionary inside `snaputils` so it does not take an external `faqlist` argument. It also allows for passing a `drop` argument on to `snapp_showcase`.
* Tidied code and updated documentation.
* Updated contact info template, now using FontAwesome icons.
* Logos are now optional in any `contactinfo` template.
* Added new template in `contactinfo`.

# snaputils 0.1.0

* Added `contactinfo` that wraps around that from the `apputils` parent package.
* Added metadata content related to SNAP Shiny apps
* Added related helper functions.
* Added `snapp_showcase` wrapper function around `apputils::app_showcase`.
