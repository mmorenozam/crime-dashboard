## utils_helpers.R ----------------------------------------------------------
## Small, pure, reusable helper functions shared by both analytical pages.
## Keeping these here (instead of duplicating logic inside each module)
## is what keeps the Homicides / Arrests pipelines DRY even though they
## operate on different datasets.

#' Build sorted unique choice vectors for a select input, with an optional
#' "All" sentinel prepended.
#'
#' @param x vector of raw values
#' @param include_all whether to prepend an "All" choice
choices_from <- function(x, include_all = TRUE) {
  vals <- sort(unique(as.character(x[!is.na(x) & x != ""])))
  if (include_all) vals <- c("Todos", vals)
  vals
}

#' Expand a multi-select input that may contain "Todos" into the concrete set
#' of values that should be used to filter a column. If "Todos" is selected
#' (or nothing is selected), returns NULL meaning "no filter".
expand_all_selection <- function(selected, all_values) {
  if (is.null(selected) || length(selected) == 0) return(NULL)
  if ("Todos" %in% selected) return(NULL)
  selected
}

#' Generic row-filter applied to a data.frame given a list of
#' column -> selected_values pairs. NULL entries are skipped (no filter).
#'
#' @param df data.frame to filter
#' @param filters named list; each element is either NULL (skip) or a
#'   character vector of allowed values for that column
apply_filters <- function(df, filters) {
  for (col in names(filters)) {
    vals <- filters[[col]]
    if (!is.null(vals) && col %in% names(df)) {
      df <- df[df[[col]] %in% vals, , drop = FALSE]
    }
  }
  df
}

#' Filter a data.frame to a year range (inclusive) on a given year column.
filter_year_range <- function(df, year_col, from_year, to_year) {
  if (is.null(from_year) || is.null(to_year)) return(df)
  if (!(year_col %in% names(df))) return(df)
  df[!is.na(df[[year_col]]) & df[[year_col]] >= from_year & df[[year_col]] <= to_year, , drop = FALSE]
}

#' Ensure a "From <= To" year relationship, returning a corrected pair.
#' Used server-side to prevent invalid year combinations from propagating.
sanitize_year_range <- function(from_year, to_year) {
  from_year <- suppressWarnings(as.integer(from_year))
  to_year   <- suppressWarnings(as.integer(to_year))
  if (is.na(from_year) || is.na(to_year)) return(list(from = from_year, to = to_year))
  if (from_year > to_year) {
    tmp <- from_year
    from_year <- to_year
    to_year <- tmp
  }
  list(from = from_year, to = to_year)
}

#' Compute annual incidence rate (events per 100,000 population) by parish
#' and year, using a long population table.
#'
#' @param events data.frame with parroquia_key and year columns (one row per event)
#' @param pob population table with parroquia_key, year, poblacion
#' @param per numeric scale, default 100,000
compute_incidence <- function(events, pob, year_col = "year",
                               parish_col = "parroquia_key", per = 1e5) {
  counts <- stats::aggregate(
    list(n = rep(1, nrow(events))),
    by = list(parroquia_key = events[[parish_col]], year = events[[year_col]]),
    FUN = sum
  )

  merged <- merge(
    counts, pob[, c("parroquia_key", "year", "poblacion")],
    by = c("parroquia_key", "year"), all.x = TRUE
  )
  merged$incidencia <- ifelse(
    is.na(merged$poblacion) | merged$poblacion == 0,
    NA_real_,
    merged$n / merged$poblacion * per
  )
  merged
}

#' Aggregate incidence across the full filtered range (sum counts / avg pop)
#' by parish only -- used for the choropleth incidence map.
compute_incidence_by_parish <- function(events, pob, from_year, to_year,
                                         parish_col = "parroquia_key", per = 1e5) {
  counts <- stats::aggregate(
    list(n = rep(1, nrow(events))),
    by = list(parroquia_key = events[[parish_col]]),
    FUN = sum
  )

  pob_range <- pob[pob$year >= from_year & pob$year <= to_year, ]
  pob_avg <- stats::aggregate(
    list(poblacion = pob_range$poblacion),
    by = list(parroquia_key = pob_range$parroquia_key),
    FUN = mean, na.rm = TRUE
  )

  merged <- merge(counts, pob_avg, by = "parroquia_key", all.x = TRUE)
  merged$incidencia <- ifelse(
    is.na(merged$poblacion) | merged$poblacion == 0,
    NA_real_,
    merged$n / merged$poblacion * per
  )
  merged
}

#' Standard weekday order (Mon-Sun) starting on Monday, used by the
#' hour x weekday heatmap so labels are consistent across pages.
weekday_levels_es <- c("lunes", "martes", "mi\u00e9rcoles", "jueves",
                        "viernes", "s\u00e1bado", "domingo")

#' Derive weekday (Spanish, Monday-first) and hour-of-day integer from a
#' Date/time and an "HH:MM[:SS]" character hour field.
derive_time_fields <- function(df, date_col, hour_col) {
  if (!(date_col %in% names(df))) return(df)

  wd_num <- as.integer(format(df[[date_col]], "%u")) # 1 = Monday ... 7 = Sunday
  df$weekday <- factor(weekday_levels_es[wd_num], levels = weekday_levels_es)

  if (hour_col %in% names(df)) {
    hour_chr <- as.character(df[[hour_col]])
    hour_num <- suppressWarnings(as.integer(substr(hour_chr, 1, 2)))
    df$hour_of_day <- hour_num
  } else {
    df$hour_of_day <- NA_integer_
  }

  df
}

#' Safe wrapper around scales::comma-style formatting without adding a
#' hard dependency if scales is unavailable at runtime.
fmt_number <- function(x, digits = 1) {
  format(round(x, digits), big.mark = ",", scientific = FALSE)
}
