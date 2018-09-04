# snapapps 0.2.6 (Release date: 2017-10-27)

* Refactored package code and resources, moving SNAP-related image files and accessor function to `snaputils` package. This frees up packages in `snapapps` that are used directly for hosting canonical apps on shinyapps.io from having to load `snapapps` to access a logo. By loading `snaputils` instead, shinyapps.io does not need to install all the "supporting" packages imported by `snapapps` (required for all apps in combination).
* Updated documentation and added minimal vignette content.
* Added an Amazon Web Services data source indicator column in the `snapps` metadata table.
* Minor updates to package apps.

# snapapps 0.2.5 (Release date: 2017-10-26)

* Added three more early Shiny apps (2013). These and were all part of the same marine coastal climate downscaling project.
    * Alaska temperature and extreme wind events
    * Alaska sea ice coverage totals and spatial extent
    * Alaska sea ice and extreme wind events
* Updated metadata and documentation.

# snapapps 0.2.2 (Release date: 2017-10-12)

* Added a column `rating` to the metadata table returned by `snapps`, providing an app complexity rating with levels: `Beginner`, `Intermediate`, `Advanced` and `Developer`.
* Updated readme.
* Added unit test for remote app launch.

# snapapps 0.2.1 (Release date: 2017-10-11)

* Added a revised version of the simplistic 2013 `tree_rings` example app.

# snapapps 0.2.0 (Release date: 2017-10-10)

* Added `snapp_resources` for assisting packaged apps with resource paths and shared resources.
* Added five initial Shiny apps to the collection.
    * Updated any issues and minor bugs with the legacy apps that were included (RV distributions 1 - 4).
    * Included a new version of RV distributions example app.
* Added support for Shiny showcase display mode with app metadata, descriptions and reactive code highlighting.
* Added resource files for images shared between package apps.
* Updated documentation.
* Added unit tests.
* Minor bug fixes.

# snapapps 0.1.0

* Added `snapp` for launching apps.
* Added `snapps` for apps metadata.
* Added documentation and readme content.
