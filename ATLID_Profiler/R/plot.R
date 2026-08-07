###
### Purpose : Render and save ATLID profile plots (aerosol extinction and
###           target classification) as PNG files.
### Example : Dependency for start.R, no need to source directly.
###

#' Build the color palette (breaks + colors) for the extinction profile.
extinction_palette <- function() {
  breaks <- seq(-0.0002, 0.1, 0.0002)
  oce::colormap(
    col = colorRampPalette(c("#301934", "purple", "blue", "green", "yellow", "orange", "red", "darkred"))(length(breaks) - 1),
    breaks = breaks,
    missingColor = "#FFFFFF00"
  )
}

#' Build the color palette (breaks + colors) for the target classification profile.
classification_palette <- function() {
  breaks <- c(1000.5:1025.5)
  oce::colormap(
    col = colorRampPalette(c(
      "black", "#a2653e", "#d8dcd7", "cyan", "#ffff84", "#95fa7b", "#96d0fc",
      "#e2ca76", "#ffbacd", "#b2996e", "#937e94", "#856798", "#ac85a8",
      "#d0fefe", "#0fff0e", "#2afeb7", "#59656d", "#76434e", "#363737",
      "#ffbacd", "#dfc5fe", "#84597e", "#3b638c", "#cfff04", "#4efd54"
    ))(length(breaks) - 1),
    breaks = breaks,
    missingColor = "#FFFFFF00"
  )
}

#' Shared curtain-plot renderer: along-track x altitude, colored by z, with a
#' colorbar, a marker at the closest orbit point and a lon/lat axis.
#'
#' @param profile List with x, y, z, lon, lat (see process.R).
#' @param palette Result of oce::colormap().
#' @param title Character, plot title.
#' @param out_file Character, PNG path to write.
#' @param closest_index Integer index (within the window) of the closest orbit point.
render_profile_png <- function(profile, palette, title, out_file, closest_index) {
  grDevices::png(filename = out_file, width = 2400, height = 1200, res = dpi)
  on.exit(grDevices::dev.off(), add = TRUE)

  graphics::layout(matrix(c(1, 2), 1, 2), widths = c(6, 0.6))

  graphics::par(mar = c(5, 5, 3, 1))
  fields::poly.image(
    x = profile$x, y = profile$y, z = profile$z,
    col = palette$col, breaks = palette$breaks,
    xlab = "Along-track sample (centered on closest orbit point)",
    ylab = "Height (km)",
    main = title,
    ylim = c(0, max_height_km),
    las = 1
  )
  graphics::abline(v = profile$x[closest_index], col = "black", lty = 2, lwd = 2)

  graphics::par(mar = c(5, 1, 3, 4))
  fields::image.plot(
    zlim = range(palette$breaks), col = palette$col, breaks = palette$breaks,
    legend.only = TRUE, legend.width = 3, smallplot = c(0.25, 0.55, 0.1, 0.9)
  )
}

#' Plot and save the aerosol extinction profile.
#'
#' @param profile Output of process_extinction_profile().
#' @param meta List with target_lon, target_lat, date, orbit_frame_id, distance_km, closest_index.
#' @return Path to the saved PNG (invisibly).
plot_extinction_profile <- function(profile, meta) {
  out_file <- file.path(
    path_output,
    sprintf(
      "ATLID_Extinction_%s_lon%.2f_lat%.2f_%s_%s.png",
      atlid_version, meta$target_lon, meta$target_lat, meta$date, meta$orbit_frame_id
    )
  )
  title <- sprintf(
    "ATLID Aerosol Extinction 355nm (Medium Res.) | %s | Target (%.2f, %.2f) | Closest orbit point %.1f km @ %s UTC",
    meta$date, meta$target_lon, meta$target_lat, meta$distance_km, format(profile$tim[meta$closest_index], "%H:%M:%S")
  )
  render_profile_png(profile, extinction_palette(), title, out_file, meta$closest_index)
  message("### Saved: ", out_file)
  invisible(out_file)
}

#' Plot and save the target classification profile.
#'
#' @param profile Output of process_classification_profile().
#' @param meta List with target_lon, target_lat, date, orbit_frame_id, distance_km, closest_index.
#' @return Path to the saved PNG (invisibly).
plot_classification_profile <- function(profile, meta) {
  out_file <- file.path(
    path_output,
    sprintf(
      "ATLID_TargetClassification_%s_lon%.2f_lat%.2f_%s_%s.png",
      atlid_version, meta$target_lon, meta$target_lat, meta$date, meta$orbit_frame_id
    )
  )
  title <- sprintf(
    "ATLID Target Classification | %s | Target (%.2f, %.2f) | Closest orbit point %.1f km @ %s UTC",
    meta$date, meta$target_lon, meta$target_lat, meta$distance_km, format(profile$tim[meta$closest_index], "%H:%M:%S")
  )
  render_profile_png(profile, classification_palette(), title, out_file, meta$closest_index)
  message("### Saved: ", out_file)
  invisible(out_file)
}
