###
### Purpose : Download ATLID Level-2a granules (EBD extinction + TC target
###           classification) for a given date via the ESA OADS downloader.
###           https://earth.esa.int/eogateway/tools/oads-download
### Example : Dependency for start.R, no need to source directly.
###

#' Download all ATLID granules of a product for one calendar day.
#'
#' @param date Character or Date, e.g. "2026-03-07".
#' @param product_key "EBD" or "TC" (see config.R `atlid_products`).
#' @return Invisibly, the directory the granules are expected in.
download_atlid_product <- function(date, product_key) {
  product <- atlid_products[[product_key]]
  date    <- as.Date(date)
  date_next <- date + 1

  st <- format(date,      "%Y%m%d")
  et <- format(date_next,  "%Y%m%d")

  out_dir <- file.path(path_data, product$dir_code, format(date, "%Y"), format(date, "%m"), format(date, "%d"))
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  cmd <- paste0(
    shQuote(path_oads_python), " ", shQuote(path_oads_script), " ",
    product$code, ":", atlid_version_suffix,
    " -st ", st, " -et ", et,
    " --no_unzip -o ", shQuote(out_dir)
  )
  message("### Downloading ATLID ", product_key, " for ", format(date, "%Y-%m-%d"))
  status <- tryCatch(system(cmd), error = function(e) { message(conditionMessage(e)); 1 })
  if (status != 0) {
    message("### WARNING: download command returned a non-zero status for ", product_key,
            " on ", format(date, "%Y-%m-%d"), ". Continuing with any files already on disk.")
  }
  invisible(out_dir)
}

#' List locally available granule files (zipped or unzipped) for a product/date.
#'
#' @param date Character or Date.
#' @param product_key "EBD" or "TC".
#' @return Character vector of full file paths, sorted.
list_atlid_files <- function(date, product_key) {
  product <- atlid_products[[product_key]]
  date <- as.Date(date)
  in_dir <- file.path(path_data, product$dir_code, format(date, "%Y"), format(date, "%m"), format(date, "%d"))
  if (!dir.exists(in_dir)) return(character(0))
  pattern <- paste0("^ECA_", atlid_version, "_ATL_", product$dir_code, "_2A_.*\\.(ZIP|zip|h5|H5)$")
  sort(list.files(in_dir, pattern = pattern, full.names = TRUE))
}

#' Extract the shared orbit+frame identifier from an ATLID filename, e.g.
#' "ECA_EXBA_ATL_EBD_2A_20260307T001004Z_20260307T014544Z_01234E.ZIP" -> "01234E".
#' This identifier is common between products (EBD/TC) for the same orbit/frame.
atlid_orbit_frame_id <- function(filename) {
  base <- basename(filename)
  sub("^.*_([0-9]{5}[A-H])\\.[A-Za-z0-9]+$", "\\1", base)
}
