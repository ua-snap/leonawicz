# @knitr create_project
source("C:/github/ProjectManagement/code/rpm.R") # eventually load a package instead of source script
proj.name <- "Flammability" # Project name
proj.location <- matt.proj.path # Use default file location

docDir <- c("Rmd/include", "md", "html", "Rnw", "pdf", "timeline")
newProject(proj.name, proj.location, docs.dirs=docDir, overwrite=T) # create a new project

appcode.frp.files <- list.files("C:/github/shiny-apps/alfoutdev", pattern="\\.R$", full=TRUE, recursive=TRUE)
appcode.alf.files <- list.files("C:/github/shiny-apps/run_alfresco", pattern="\\.R$", full=TRUE, recursive=TRUE)
rfile.path <- file.path(proj.location, proj.name, "code") # path to R scripts
file.copy(appcode.frp.files, paste0(rfile.path, "/appcode/appcode_frp_", basename(appcode.frp.files)), overwrite=TRUE)
file.copy(appcode.alf.files, paste0(rfile.path, "/appcode/appcode_alf_", basename(appcode.alf.files)), overwrite=TRUE)
docs.path <- file.path(proj.location, proj.name, "docs")
rmd.path <- file.path(docs.path, "Rmd")

# generate Rmd files from existing R scripts using default yaml front-matter
genRmd(path=rfile.path) # specify header.args list argument if necessary

# @knitr update_project
# update yaml front-matter only
genRmd(path=rfile.path, update.header=TRUE)

# obtain knitr code chunk names in existing R scripts
chunkNames(path=file.path(proj.location, proj.name, "code"))

# append new knitr code chunk names found in existing R scripts to any Rmd files which are outdated
chunkNames(path=file.path(proj.location, proj.name, "code"), append.new=TRUE)

# @knitr website
# Setup for generating a project website
user <- "leonawicz"
proj.github <- file.path("https://github.com", user, proj.name)
index.url <- "index.html"
#file.copy(index.url, "index.html")

proj.title <- "Flammability"
proj.menu <- c("Overview", "Data Prep", "GBM Flammability", "ALFRESCO", "Statistics", "Apps", "All Projects")

proj.submenu <- list(
	c("empty"),
	c("Climate data prep", "clim_resample_2km_1km.R", "clim_1km_clip2ak.R", "tpByVeg_CRU32.R", "tpByVeg_CMIP5.R", "tpByVeg_plots.R"),
	c("GBM Modeling", "baByVeg_historical.R", "gbm_modeling_flammability.R", "gbm_modeling_lightning.R", "gbm_lightning_coefficients.R", "divider", "Flammability maps", "gbm_flam_prep.R", "gbm_flam_maps.R", "gbm_flam_maps2.R", "gbm_flam_comparisons.R", "flam_dist.R", "flam_trunc.R", "FlammabilityMapMultipliers.R", "divider", "ALFRESCO prep", "duplicate_flam_maps.R", "alf_gcmRuns_inputPrep.R"),
	c("Main scripts", "AlfrescoCalibration.R", "AlfrescoFRP.R", "fsByVeg.R", "divider", "Supporting scripts", "obs_fire_setup.R", "divider", "Functions", "histPrep.R", "fireSizePlot.R", "AByearPlot.R", "CABvsFSPlot.R", "CABvsTimePlot.R"),
	c("Fire size distributions", "EDA: fire samples", "EDA: Noatak shrub fire", "EDA: statewide forest fire", "divider", "MLE: setup", "MLE: Noatak shrub fire", "MLE: statewide forest fire"),
	c("ALFRESCO launcher", "global.R", "ui.R", "server.R", "sidebar.R", "reactives.R", "about.R", "divider",
		"Results app", "ui.R", "server.R", "sidebar.R", "app.R", "reactives.R", "plotFunctions.R", "about.R"),
	c("empty")
)

proj.files <- list(
	c("index.html"),
	c("clim_prep.html", "clim_resample_2km_1km.html", "clim_1km_clip2ak.html", "tpByVeg_CRU32.html", "tpByVeg_CMIP5.html", "tpByVeg_plots.html"),
	c("header", "baByVeg_historical.html", "gbm_modeling_flammability.html", "gbm_modeling_lightning.html", "gbm_lightning_coefficients.html", "divider", "header", "gbm_flam_prep.html", "gbm_flam_maps.html", "gbm_flam_maps2.html", "gbm_flam_comparisons.html", "flam_dist.html", "flam_trunc.html", "FlammabilityMapMultipliers.html", "divider", "header", "duplicate_flam_maps.html", "alf_gcmRuns_inputPrep.html"),
	c("header", "AlfrescoCalibration.html", "AlfrescoFRP.html", "fsByVeg.html", "divider", "header", "obs_fire_setup.html", "divider", "header", "histPrep.html", "fireSizePlot.html", "AByearPlot.html", "CABvsFSPlot.html", "CABvsTimePlot.html"),
	c("header", "fs_eda1.html", "fs_eda2.html", "fs_eda3.html", "divider", "fs_mle1.html", "fs_mle2.html", "fs_mle3.html"),
	c("header", "appcode_alf_global.html", "appcode_alf_ui.html", "appcode_alf_server.html", "appcode_alf_sidebar.html", "appcode_alf_reactives.html", "appcode_alf_about.html", "divider",
		"header", "appcode_frp_ui.html", "appcode_frp_server.html", "appcode_frp_sidebar.html", "appcode_frp_app.html", "appcode_frp_reactives.html", "appcode_frp_plotFunctions.html", "appcode_frp_about.html"),
	c("https://leonawicz.github.io")
)

# generate navigation bar html file common to all pages
genNavbar(htmlfile=file.path(rmd.path, "include/navbar.html"), title=proj.title, menu=proj.menu, submenus=proj.submenu, files=proj.files, site.url=proj.github, include.home=FALSE)

# generate _output.yaml file
# Note that external libraries are expected, stored in the "libs" below
yaml.out <- file.path(proj.location, proj.name, "docs/Rmd/_output.yaml")
libs <- "libs"
common.header <- "include/in_header.html"
genOutyaml(file=yaml.out, lib=libs, header=common.header, before_body="include/navbar.html", after_body="include/after_body.html")

# @knitr knit_setup
library(rmarkdown)
library(knitr)
setwd(rmd.path)

# Rmd files
files.Rmd <- list.files(pattern=".Rmd$", full=T)

# @knitr save
# write all yaml front-matter-specified outputs to Rmd directory for all Rmd files
for(i in 1:length(files.Rmd)) render(files.Rmd[i], output_format="all")
insert_gatc(list.files(pattern=".html$"))
moveDocs(path.docs=docs.path)

# if also making PDFs for a project, speed up the Rmd to Rnw file conversion/duplication
rnw.path <- file.path(docs.path, "Rnw")
setwd(rnw.path)
#themes <- knit_theme$get()
highlight <- "solarized-dark"
convertDocs(path=rmd.path, emphasis="replace", overwrite=TRUE, highlight=highlight) # Be careful
lapply(list.files(pattern=".Rnw$"), knit2pdf)
moveDocs(path.docs=docs.path, type="pdf", remove.latex=FALSE)
