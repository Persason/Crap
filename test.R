library(ggplot2)

x <- analysis_mace_old

d <- do.call(rbind, lapply(
  names(x$cumulative_intervention_probs), function(trt) {

    p <- x$cumulative_intervention_probs[[trt]]
    m <- x$intervention_match[[trt]]

    do.call(rbind, lapply(seq_len(ncol(m)), function(k) {

      node <- paste0("censor_", k)

      # Still at risk
      at_risk <- x$followup$last_interval >= (k - 1)

      # Not censored at this interval
      uncensored <- as.character(
        x$prepared_data[[node]]
      ) == "0"

      # Adherent through this interval
      adherent <- rowSums(
        m[, 1:k, drop = FALSE] == 1,
        na.rm = TRUE
      ) == k

      keep <- which(at_risk & uncensored & adherent)

      data.frame(
        Treatment = trt,
        Time = k - 1,
        Probability = p[keep, node]
      )
    }))
  }
))

ggplot(d, aes(x = factor(Time), y = Probability)) +
  geom_boxplot(outlier.alpha = 0.3) +
  facet_wrap(~Treatment) +
  theme_bw() +
  labs(
    x = "Time",
    y = "Cumulative intervention probability"
  )
