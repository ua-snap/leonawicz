#' New R package reminders
#'
#' Print out a list of steps to take upon creating a new R package in RStudio.
#'
#' Adding the repo on GitHub (step 3) can be done any time prior to step 4.
#' The suggested steps are mostly universal except for the final two, which cater to SNAPverse defaults.
#' See \code{use_these} for details on defaults.
#'
#' @param pkg defaults to working directory.
#' @param account GitHub account name, defaults to "leonawicz".
#'
#' @return \code{cat} a list of steps to the console.
#' @export
#'
#' @examples
#' reminders()
reminders <- function(pkg=basename(getwd()), account="leonawicz"){
  x <- paste0(
    "1. Create a new R package via RStudio > New Project > R Package\n  with 'Create a git repository' checked.\n",
    "2. Make initial commit by adding the initial .Rbuildignore, .gitignore and [pkgname].Rproj files.\n",
    "3. Add the repo on GitHub:\n  ",
    "Use default settings (Do not create README.md).\n  ",
    "Set docs/ directory for hosting project website (Must first push docs/ to GitHub).\n",
    "4. In git shell, enter:\n  ",
    paste0("git remote add origin git@github.com:", account, "/", pkg, ".git\n  "),
    "git push -u origin master\n",
    "5. Then return to R console and run:\n  ",
    "snapmeta::use_these()\n    ",
    "NOTE: Run Rstudio session as Administator in Windows so usethese() can create lintr symbolic link.\n",
    "6. Add Travis CI, Appveyor and code coverage badges to README.Rmd. Add projects on respective sites.\n  ",
    "Badges are in console output. Remember to add the `after_success` segment to .travis.yml as well.\n",
    "7. Check the following:\n  ",
    "Delete absolute path to `docs` created by pkgdown in .Rbuildignore.\n  ",
    "Make initial updates to DESCRIPTION and other files, e.g., README.Rmd, vignette Rmd file, LICENSE.md.\n  ",
    "Delete NAMESPACE so it can be auto-generated via devtools.\n  ",
    "Delete any Hello World files.\n  ",
    "At least one inital unit test is required to pass build. Lintr test will suffice.\n  ",
    "Commit changes, but hold off on cran-comments.md and revdep until meaningful.\n"
  )
  cat(x)
}

repo <- function(){
  r <- git2r::repository(".", discover = TRUE)
  remote <- git2r::remotes(r)
  r <- git2r::remote_url(r, remote)
  r <- strsplit(r, ":")[[1]][2]
  list(account = dirname(r), repo=gsub("\\.git", "", basename(r)))
}

#' Package author and copyright holder/funder
#'
#' Specify the package author and the copyright holder/funder.
#'
#' This function is specific to the SNAPverse and is used as a default argument to \code{use_authors}.
#'
#' @param first author first name.
#' @param last author last name.
#' @param email author email address
#' @param cph_fnd copyright holder and funder.
#'
#' @return a list to be used by \code{use_authors}.
#' @export
#'
#' @examples
#' \dontrun{pkg_authors()}
pkg_authors <- function(first="Matthew", last="Leonawicz", email="mfleonawicz@alaska.edu",
                        cph_fnd=pkg_cph()){
  list(
    "Authors@R"=paste0(
      "c(",
      "person(\"", first, "\", \"", last, "\", email = \"", email, "\", role = c(\"aut\", \"cre\")),",
      "\n    person(\"", cph_fnd, "\", role = c(\"cph\", \"fnd\"))",
      "\n    )")
  )
}

#' Copyright holder
#'
#' Specify the copyright holder.
#'
#' This function simply returns its input, the copyright holder string. It is used as a default argument to \code{use_authors}.
#'
#' @param cph copyright holder.
#'
#' @return copyright holder.
#' @export
#'
#' @examples
#' pkg_cph()
pkg_cph <- function(cph="Scenarios Network for Alaska and Arctic Planning"){
  cph
}

#' Add author and license information to R package
#'
#' Add author and MIT license to DESCRIPTION and add license files to R package.
#' The defaults for this function cater to the SNAPverse.
#'
#' @param authors list as returned by \code{pkg_authors}.
#' @param cph copyright holder as returned by \code{pkg_cph}.
#'
#' @return side effect of updating DESCRIPTION and adding MIT license.
#' @export
#'
#' @examples
#' \dontrun{use_authors()}
use_authors <- function(authors=pkg_authors(), cph=pkg_cph()){
  usethis::use_description(authors)
  usethis::use_mit_license(cph)
}

#' Add \code{lintr} to R package
#'
#' Add scaffolding for \code{lintr} package usage to R package.
#'
#' This function creates a template \code{.lintr} file for SNAPverse packages inside \code{inst} and
#' adds a symbolic link to this file in the package root directory.
#' It also adds both files to \code{.Rbuildignore} and creates a unit test for lint-free package code.
#'
#' @param base_path package root directory.
#' @param use_testthat logical, if \code{TRUE} (default), adds \code{tests/testthat/test-lintr.R} to package.
#'
#' @return side effect of adding \code{lintr} scaffolding to package.
#' @export
#'
#' @examples
#' \dontrun{use_lintr()}
use_lintr <- function(base_path = ".", use_testthat = TRUE){
  dir.create(file.path(base_path, "inst"), showWarnings = FALSE)
  sink("inst/.lintr")
  cat(paste0("linters: with_defaults(\n  ",
             "line_length_linter(120),\n  ",
             "infix_spaces_linter=NULL,\n  ",
             "camel_case_linter = NULL,\n  ",
             "snake_case_linter = NULL,\n  ",
             "spaces_left_parentheses_linter=NULL)\n"))
  sink()
  if(!file.exists(".lintr")) file.symlink("inst/.lintr", ".lintr")
  usethis::use_build_ignore(c(".lintr", "inst/.lintr"))
  sink("tests/testthat/test-lintr.R")
  cat(paste0("if (requireNamespace(\"lintr\", quietly = TRUE)) {\n  ",
    "context(\"lints\")\n  ",
    "test_that(\"Package Style\", {\n    ",
    "lintr::expect_lint_free()\n  ",
    "})\n", "}\n"))
  sink()
  usethis::use_package("lintr", "suggests")
}

#' Add \code{clone_notes.md} to R package
#'
#' Add \code{clone_notes.md} to a SNAPverse R package.
#'
#' The created file is for tracking information unique to a package and helpful to know upon cloning
#' the repository, e.g., external locations of large data sets, available cached files, or other required files that are not tracked with git.
#'
#' @return side effect of creating file.
#' @export
#'
#' @examples
#' \dontrun{use_clone_notes}
use_clone_notes <- function(){
  sink("clone_notes.md")
  cat(paste0(
    "# Clone this repository\n\n",
    "Cloning this repository may require you to remove the top-level .lintr file and regenerate a symbolic link to the actual inst/.lintr file.\n\n", # nolint
    "There are no other notes for this respository.\n"))
  sink()
  usethis::use_git_ignore("docs/clone_notes.html")
}

#' Add \code{hex.R} to R package
#'
#' Add \code{hex.R} to a SNAPverse R package \code{data-raw} directory.
#'
#' This script provides a template for package hex sticker icons, a large icon in \code{data-raw} and a small icon in \code{inst}.
#' The script is not run by \code{snapmeta} since it will likely require customization after it is generated.
#'
#' @return side effect of creating file.
#' @export
#'
#' @examples
#' \dontrun{use_hex}
use_hex <- function(){
  file.copy(system.file(package = "snapmeta", "resources/hex.R"), "data-raw/hex.R")
}

#' Wrapper function for package setup
#'
#' Wrapper function around several package creation/setup functions from the \code{usethis} and \code{snapmeta} packages.
#'
#' \code{use_these} calls the following functions: \code{use_authors}, \code{use_github_links}, \code{use_clone_notes},
#' \code{use_cran_comments},\code{use_data_raw}, \code{use_news_md}, \code{use_testthat}, \code{use_vignette},
#' \code{use_readme_rmd}, \code{use_revdep}, \code{use_lintr}, \code{use_appveyor}, \code{use_travis} and \code{use_coverage}.
#'
#' Authors, clone notes and \code{lintr} use are by \code{snapmeta} functions. The others are \code{usethis} package functions,
#' except for \code{use_vignettes}, which still relies on \code{devtools}.
#' \code{pkgdown} for R package website building is also initialized, using a \code{pkgdown} directory in the package root
#' directory containing template \code{_pkgdown.yml} and \code{extra.css} files relevant to the SNAPverse.
#' The \code{docs} directory is used for website files and should be specified likewise in the repository settings on GitHub.
#'
#' \code{pkgdown::init_site} is also called.
#' \code{.Rbuildignore} is also updated. Make sure to remove the absolute path created by \code{pkgdown::init_site}.
#' Only retain the relative path to the \code{docs} directory.
#' Finally, it adds \code{snapmeta} under the \code{DESCRIPTION} file \code{Imports} field for SNAPverse sector packages.
#' If needed for any other packages, it must be added manually.
#'
#' @param pkg defaults to the name of the working directory.
#' @param authors list as generated by \code{pkg_authors}.
#' @param cph copyright holder as generated by \code{pkg_cph}.
#'
#' @return side effect of setting up various package files and configurations.
#' @export
#'
#' @examples
#' \dontrun{use_these()}
use_these <- function(pkg = basename(getwd()), authors = pkg_authors(), cph=pkg_cph()){
  snapmeta::use_authors(authors, cph)
  usethis::use_github_links()
  snapmeta::use_clone_notes()
  usethis::use_cran_comments()
  usethis::use_data_raw()
  usethis::use_news_md()
  usethis::use_testthat()
  if(!file.exists(paste0("vignettes/", pkg, ".Rmd"))) devtools::use_vignette(pkg)
  usethis::use_readme_rmd()
  usethis::use_revdep()
  snapmeta::use_lintr()
  pkgdown::init_site()
  use_hex()
  pdfiles <- list.files(file.path(system.file(package = "snapmeta"), "resources/pkgdown"),
                        full.names = TRUE)
  dir.create("pkgdown", showWarnings = FALSE)
  file.copy(pdfiles[2], file.path("pkgdown", basename(pdfiles[2])), overwrite = TRUE)
  usethis::use_build_ignore(c("docs", "pkgdown", "clone_notes.md"))
  r <- repo()
  file <- "pkgdown/_pkgdown.yml"
  x <- paste(readLines(pdfiles[1]), collapse = "\n")
  x <- gsub("_ACCOUNT_", r$account, x)
  x <- gsub("_PACKAGE_", r$repo, x)
  if(r$account == "leonawicz") x <- gsub("_ENTER_CODE_HERE_", "46129458-3", x)
  sink(file)
  cat(paste0(x, "\n"))
  sink()

  usethis::use_appveyor()
  usethis::use_travis()
  usethis::use_coverage()

  sector_pkgs <- dplyr::filter(sv_pkgs(), (!! as.name("type")) == "sector")$pkg
  if(pkg %in% sector_pkgs) usethis::use_package("snapmeta")

  badges <- paste0("[![Travis-CI Build Status](https://travis-ci.org/", r$account, # nolint start
                   "/", r$repo, ".svg?branch=master)](https://travis-ci.org/",
                   r$account, "/", r$repo, ")\n\n  ",
  "[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/", r$account,
  "/", r$repo, "?branch=master&svg=true)](https://ci.appveyor.com/project/", r$account,
  "/", r$repo, ")\n\n  ",
  "[![Coverage Status](https://img.shields.io/codecov/c/github/", r$account, "/", r$repo,
  "/master.svg)](https://codecov.io/github/", r$account, "/", r$repo, "?branch=master)\n\n") # nolint end
  code_cov <- paste0("after_success:\n    ", "- Rscript -e 'covr::codecov()'\n")
  cat(paste0("Add the following badges to README.Rmd:\n  ", badges,
            "Add the following to .travis.yml:\n  ", code_cov))
}
