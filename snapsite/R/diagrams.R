#' Create a SNAPverse flowchart diagram
#'
#' Save a SNAPverse flowchart diagram to disk. The default \code{context = "complete"} is the only option at this time.
#' Other arguments are passed to \code{DiagrammeR::export_graph}.
#'
#' @param file_name output file name.
#' @param context character, type of verse flowchart diagram. Defaults to \code{"complete"}.
#' @param title character, flowchart title.
#' @param file_type the output file type.
#' @param width numeric.
#' @param height numeric.
#'
#' @return side effect of saving image file to disk.
#' @export
#'
#' @examples
#' \dontrun{
#' x <- "Simplified overview of the SNAPverse"
#' chart_verse(file_name = "sv_all.svg", title = x)
#' chart_verse(file_name = "sv_all.png", title = x, width = 800)
#' }
chart_verse <- function(file_name, context = "complete", title = NULL,
                        file_type = NULL, width = 350, height = NULL){
  pkgs <- snapmeta::sv_pkgs()
  types <- unique(pkgs$type)
  clrs <- c("Chartreuse3", "DarkOrchid", "Orange", "DodgerBlue", "#555555")[match(pkgs$type, types)]
  hide_edge_idx <- c(rep(1, 17), 2, 1, 1, 2, 1)

  ndf <- DiagrammeR::create_node_df(
    n = nrow(pkgs), type = "a", label = pkgs$pkg, fillcolor = clrs, style = "filled", color = "#333333",
    fontcolor = "white", shape = "rectangle", fontname = "arial", fixedsize = TRUE, width = 1)

  edf <- DiagrammeR::create_edge_df(
    from = c(rep(1, 3), 2, 5:8, 3, 10:13, 4, 15:17, 19:23),
    to   = c(2:4, 5:14, 15:18, 20:24),
    arrowhead = c("normal", "none")[c(rep(1, 4), rep(2, 4), 1, rep(2, 4), 1, rep(2, 4), rep(1, 3), 1)],
    rel = "a",
    color = paste0("#333333", c("", "00")[hide_edge_idx]),
    penwidth = c(1, 0)[hide_edge_idx])

  g <- DiagrammeR::create_graph(nodes_df=ndf, edges_df=edf, attr_theme = NULL)
  DiagrammeR::export_graph(g, file_name = file_name, file_type = file_type, title = title,
                           width = width, height = height)
  invisible()
}

#' Create a SNAPverse satellites flowchart diagram
#'
#' Save a SNAPverse satellite packages flowchart diagram to disk.
#' The default \code{context = "all"} creates a flowchart of all satellite packages.
#' Other options include \code{"utils"} and \code{"all"}, which create flowcharts for
#' the three utils (app, map, and snap) packages and the SNAPverse development packages (meta and site), respectively.
#' If \code{selected} is not \code{NULL}, selected packages are given a fixed color and any unselected are gray.
#' If \code{NULL}, each context group is colored uniquely.
#' Other arguments are passed to \code{DiagrammeR::export_graph}.
#'
#' @param file_name output file name, defaults to \code{"sv_td_all.svg"} for a topdown chart of the full verse.
#' @param context character, all statellites, utils packages only, or dev packages. See details.
#' @param selected character, vector of any selected packages intended to be highlighted vs. others. See details.
#' @param title character, flowchart title.
#' @param file_type the output file type.
#' @param width numeric.
#' @param height numeric.
#'
#' @return side effect of saving image file to disk.
#' @export
#'
#' @examples
#' \dontrun{
#' chart_satellites(file_name = "sv_satellites_all.svg",
#'   context = "all", title = "SNAPverse satellite packages")
#' chart_satellites(file_name = "sv_satellites_utils.svg",
#'   context = "utils", title = "SNAPverse utils satellites")
#' chart_satellites(file_name = "sv_satellites_utils_app.svg",
#'   context = "utils", selected = "apputils",
#'   title = "SNAPverse utils satellites")
#' chart_satellites(file_name = "sv_satellites_dev.svg",
#'   context = "dev", title = "SNAPverse development satellites")
#' }
chart_satellites <- function(file_name, context = "all", selected = NULL,
                             file_type = NULL, title = NULL, width = 350, height = NULL){
  if(!context %in% c("all", "utils", "dev"))
    stop("`context` must be 'all', 'utils' or 'dev'.")
  pkgs <- snapmeta::sv_pkgs() %>% dplyr::filter(.data[["type"]] == "satellite") # nolint
  if(context != "all"){
    if(context == "utils"){
      x <- c("apputils", "maputils", "snaputils")
      pkgs <- dplyr::filter(pkgs, .data[["pkg"]] %in% x)
      clrs <- c("DarkOrchid", "Orange", "DodgerBlue")
      if(inherits(selected, "character") && all(selected %in% x)){
        clrs <- rep("#555555", length(x))
        clrs[x %in% selected] <- "Orange"
      }
      hide_edge_idx <- c(1, 1)
      from <- c(1, 2)
      to <- c(2, 3)
    } else if(context == "dev"){
      pkgs <- dplyr::filter(pkgs, .data[["pkg"]] %in% c("snapsite", "snapmeta"))
      clrs <- rep("Chartreuse3", 2)
      if(inherits(selected, "character") && all(selected %in% x)){
        clrs <- rep("#555555", length(x))
        clrs[x %in% selected] <- "Orange"
      }
      hide_edge_idx <- 1
      from <- 1
      to <- 2
    }
  } else {
    clrs <- c("DarkOrchid", rep("Orange", 3), rep("DodgerBlue", 2))
    if(inherits(selected, "character") && all(selected %in% x)){
      clrs <- rep("#555555", length(clrs))
      clrs[x %in% selected] <- "Orange"
    }
    hide_edge_idx <- rep(1, 3)
    from <- c(2, 3, 5)
    to <- c(3, 4, 6)
  }

  ndf <- DiagrammeR::create_node_df(
    n = nrow(pkgs), type = "a", label = pkgs$pkg, fillcolor = clrs, style = "filled", color = "#333333",
    fontcolor = "white", shape = "rectangle", fontname = "arial", fixedsize = TRUE, width = 1)

  edf <- DiagrammeR::create_edge_df(
    from = from, to = to, arrowhead = "normal", rel = "a",
    color = paste0("#333333", c("", "00")[hide_edge_idx]),
    penwidth = c(1, 0)[hide_edge_idx])

  g <- DiagrammeR::create_graph(nodes_df = ndf, edges_df = edf, attr_theme = NULL) %>%
    DiagrammeR::add_global_graph_attrs(attr = "rankdir", value =  "LR", attr_type =  "graph")
  DiagrammeR::export_graph(g, file_name = file_name, file_type = file_type, title = title,
                           width = width, height = height)
  invisible()
}

#' Make package icons
#'
#' Make package icon svg thumbail files.
#'
#' @param base_path output directory.
#' @param width numeric, icon width.
#' @param height numeric, icon height.
#'
#' @return side effect of writing files.
#' @export
#'
#' @examples
#' \dontrun{make_pkg_icons()}
make_pkg_icons <- function(base_path = ".", width = 400, height = 200){
  pkgs <- snapmeta::sv_pkgs()
  types <- unique(pkgs$type)
  clrs <- c("Chartreuse3", "DarkOrchid", "Orange", "DodgerBlue", "#555555")[match(pkgs$type, types)]
  purrr::walk2(pkgs$pkg, clrs,
  ~({
    file_name <- paste0(base_path, "/pkg_icon_", .x, ".svg") # nolint
    DiagrammeR::create_node_df(
      n = 1, type = "a", label = .x, fillcolor = .y, style = "filled", color = .y,
      fontcolor = "white", shape = "rectangle", fontname = "arial", fixedsize = TRUE, width = 1) %>%
      DiagrammeR::create_graph(attr_theme = NULL) %>%
      DiagrammeR::export_graph(file_name = file_name, width = width, height = height)
  })
  )
}
