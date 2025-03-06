print_progress <- function(i, N, time_start=NA, time_units="mins") {
  ### IF time_start is given
  if (!is.na(time_start)) { 
    time_passed <- round(difftime(Sys.time(),time_start, units=time_units),0)
    message(paste0("### ",round(100 * i / N, 0), "% (", i, "/", N, ") ### ( ",time_passed," ",time_units," )"), appendLF = FALSE)
  }
  if (is.na(time_start)) { 
    message(paste0("### ",round(100 * i / N, 0), "% (", i, "/", N, ") ###"), appendLF = FALSE)
  }
  flush.console()
  cat("\r")
}