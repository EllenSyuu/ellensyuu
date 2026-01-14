library(copula)

# Example station list (replace with your actual data objects)
# stations <- list(d1$z, d2$z, d3$z, d4$z, d5$z, d6$z)
# station_names <- c("Drummondville", "L'Assomption", "Les Cèdres",
#                    "St-Charles-de-Mandeville", "Lennoxville", "Nicolet")

run_evTestK_pairs <- function(stations, station_names,
                              method = c("fsample", "asymptotic", "jackknife"),
                              ties = NA,
                              N = 100) {
  method <- match.arg(method)

  if (length(stations) != length(station_names)) {
    stop("stations and station_names must have the same length.")
  }

  results <- list()
  idx <- 1

  for (i in seq_len(length(stations) - 1)) {
    for (j in (i + 1):length(stations)) {
      cat(sprintf("\nProcessing pair: %s - %s\n",
                  station_names[i], station_names[j]))

      z1 <- stations[[i]]
      z2 <- stations[[j]]

      if (length(z1) != length(z2)) {
        stop(sprintf("Length mismatch for %s vs %s.",
                     station_names[i], station_names[j]))
      }

      # Convert to pseudo-observations in [0, 1]
      u <- pobs(cbind(z1, z2))

      test_result <- evTestK(u, method = method, ties = ties, N = N)

      results[[idx]] <- data.frame(
        pair = paste(station_names[i], "-", station_names[j]),
        method = method,
        statistic = unname(test_result$statistic),
        p_value = unname(test_result$p.value),
        stringsAsFactors = FALSE
      )
      idx <- idx + 1
    }
  }

  do.call(rbind, results)
}

# Example usage:
# results_df <- run_evTestK_pairs(stations, station_names, method = "fsample")
# print(results_df)
