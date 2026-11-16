#' Acceptance Criteria
#'
#' Default values follow ICH M10 (2022) for ligand binding assays: standards
#' and QC within +-20 % of the nominal concentration (+-25 % at the lowest and
#' highest level), precision (CV) <= 20 % (25 % at the ends of the range), at
#' least 75 % and at least 6 standard levels accepted, at least 2/3 of all QC
#' and 50 % of each QC level within limits, dilution linearity within +-20 %
#' with CV <= 20 %.
#'
#' @param std_bias,std_bias_edge Allowed bias of back-calculated standards (%),
#'   inside the range and at its ends.
#' @param cv,cv_edge Allowed CV of replicates (%), inside the range and near its ends.
#' @param min_fraction,min_levels Minimum fraction and number of accepted standard levels.
#' @param qc_bias,qc_bias_edge Allowed bias of QC (%), inside the range and near its ends.
#' @param qc_fraction,qc_level_fraction Minimum fraction of all QC wells and of
#'   the wells of each QC level within limits.
#' @param dilution_bias,dilution_cv Allowed deviation (%) of a dilution from
#'   the mean of all dilutions of a sample, and CV (%) across dilutions.
#' @param range Quantification range: `"accepted"` (from the lowest to the
#'   highest accepted standard level) or `"all"` (all standards used in the fit).
#' @param edge Concentrations within this factor of LLOQ or ULOQ use the
#'   `_edge` limits.
#'
#' @return A list of class `lin_criteria`.
#' @export
#' @examples
#' qc_criteria()
#' qc_criteria(std_bias = 15, std_bias_edge = 20, cv = 15, cv_edge = 20)  # chromatography
qc_criteria <- function(std_bias = 20, std_bias_edge = 25, cv = 20, cv_edge = 25,
                        min_fraction = 0.75, min_levels = 6,
                        qc_bias = 20, qc_bias_edge = 25, qc_fraction = 2 / 3, qc_level_fraction = 0.5,
                        dilution_bias = 20, dilution_cv = 20, range = 'accepted', edge = 1.25) {
    out <- list(std_bias = std_bias, std_bias_edge = std_bias_edge, cv = cv, cv_edge = cv_edge,
                min_fraction = min_fraction, min_levels = min_levels,
                qc_bias = qc_bias, qc_bias_edge = qc_bias_edge, qc_fraction = qc_fraction,
                qc_level_fraction = qc_level_fraction, dilution_bias = dilution_bias,
                dilution_cv = dilution_cv, range = range, edge = edge)
    numeric <- setdiff(names(out), 'range')
    bad <- numeric[!vapply(out[numeric], function(v) is.numeric(v) && length(v) == 1 && !is.na(v) && v >= 0,
                           logical(1))]
    if (length(bad) > 0) lin_abort('error.criteria', names = bad)
    if (!range %in% c('accepted', 'all')) lin_abort('error.criteria', names = 'range')
    structure(out, class = 'lin_criteria')
}


#' Coefficient of Variation in Percent
#' @keywords internal
cv_percent <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) < 2 || mean(x) == 0) return(NA_real_)
    100 * stats::sd(x) / abs(mean(x))
}


#' Is a Concentration Near the Ends of the Range?
#' @keywords internal
near_edge <- function(conc, range, edge) {
    !is.na(conc) & (conc <= range[1] * edge | conc >= range[2] / edge)
}


#' Assess Standard Levels
#'
#' Back-calculates the standards used in the fit and checks, for each level,
#' the bias of the mean back-calculated concentration and the CV of the ODs.
#' The lowest and highest levels use the `_edge` limits.
#'
#' @param fit A `lin_fit` object.
#' @param criteria See [qc_criteria()].
#'
#' @return A data.frame with one row per level: `conc`, `n`, `od.mean`,
#'   `od.cv`, `calc.mean`, `bias` (%), `bias.limit`, `cv.limit`, `pass`.
#' @export
assess_standards <- function(fit, criteria = qc_criteria()) {
    bc <- back_calculate(fit)
    levels <- sort(unique(bc$conc))
    ends <- range(levels)
    rows <- lapply(levels, function(lv) {
        b <- bc[bc$conc == lv, ]
        edge <- lv %in% ends
        calc_mean <- if (all(is.na(b$calc))) NA_real_ else mean(b$calc, na.rm = TRUE)
        bias <- 100 * (calc_mean - lv) / lv
        bias_limit <- if (edge) criteria$std_bias_edge else criteria$std_bias
        cv_limit <- if (edge) criteria$cv_edge else criteria$cv
        od_cv <- cv_percent(b$OD)
        pass <- !is.na(bias) && abs(bias) <= bias_limit && (is.na(od_cv) || od_cv <= cv_limit)
        data.frame(conc = lv, n = nrow(b), od.mean = mean(b$OD), od.cv = od_cv,
                   calc.mean = calc_mean, bias = bias, bias.limit = bias_limit,
                   cv.limit = cv_limit, pass = pass)
    })
    do.call(rbind, rows)
}


#' Acceptance of the Calibration
#'
#' @param standards Output of [assess_standards()].
#' @param criteria See [qc_criteria()].
#'
#' @return A list with `n_levels`, `n_pass`, `accepted`, `lloq` and `uloq`
#'   (lowest and highest accepted level, `NA` if none).
#' @keywords internal
calibration_acceptance <- function(standards, criteria = qc_criteria()) {
    n_levels <- nrow(standards)
    n_pass <- sum(standards$pass)
    needed <- max(ceiling(criteria$min_fraction * n_levels), min(criteria$min_levels, n_levels))
    passed <- standards$conc[standards$pass]
    list(n_levels = n_levels, n_pass = n_pass, n_needed = needed,
         accepted = n_pass >= needed,
         lloq = if (length(passed) > 0) min(passed) else NA_real_,
         uloq = if (length(passed) > 0) max(passed) else NA_real_)
}


#' Replicate Groups of Samples
#'
#' Wells with the same `id` (plate format), or with the same `sample.name`,
#' `dilution` and `sample.date` (table format), are replicates.
#'
#' @param result Well-level results.
#'
#' @return Character vector of group labels.
#' @keywords internal
replicate_groups <- function(result) {
    if ('id' %in% names(result) && !anyNA(result$id)) return(as.character(result$id))
    cols <- intersect(c('sample.name', 'dilution', 'sample.date'), names(result))
    do.call(paste, c(lapply(result[cols], as.character), sep = ' | '))
}


#' Summary of Sample Replicates
#'
#' @param result Well-level sample results from [calculate()].
#' @param range Quantification range.
#' @param criteria See [qc_criteria()].
#'
#' @return A data.frame with one row per replicate group: identification
#'   columns, `n`, `n.quantified`, `mean`, `sd`, `cv` (%), `cv.limit` and
#'   `flag` (codes from [FLAGS] separated by `"; "`).
#' @keywords internal
summarise_samples <- function(result, range, criteria = qc_criteria()) {
    if (nrow(result) == 0) return(NULL)
    group <- replicate_groups(result)
    id_cols <- intersect(c('id', 'sample.name', 'dilution', 'sample.date'), names(result))
    rows <- lapply(unique(group), function(g) {
        r <- result[group == g, , drop = FALSE]
        conc <- r$conc[!is.na(r$conc)]
        m <- if (length(conc) > 0) mean(conc) else NA_real_
        cv <- cv_percent(conc)
        undiluted <- m / r$dilution[1]
        cv_limit <- if (near_edge(undiluted, range, criteria$edge)) criteria$cv_edge else criteria$cv
        flags <- unique(r$flag[r$flag != FLAGS$ok])
        if (!is.na(cv) && cv > cv_limit) flags <- c(FLAGS$high_cv, flags)
        out <- r[1, id_cols, drop = FALSE]
        out$n <- nrow(r)
        out$n.quantified <- length(conc)
        out$mean <- m
        out$sd <- if (length(conc) >= 2) stats::sd(conc) else NA_real_
        out$cv <- cv
        out$cv.limit <- cv_limit
        out$flag <- if (length(flags) == 0) FLAGS$ok else paste(flags, collapse = '; ')
        out
    })
    out <- do.call(rbind, rows)
    rownames(out) <- NULL
    out
}


#' Dilution Linearity
#'
#' For each sample measured at two or more dilutions, compares the
#' dilution-corrected mean concentration at each dilution with the mean over
#' all dilutions and computes the CV across dilutions.
#'
#' @param samples Output of [summarise_samples()].
#' @param criteria See [qc_criteria()].
#'
#' @return A data.frame with `sample.name` (and `sample.date` if present),
#'   `dilution`, `mean`, `deviation` (%), `cv` (% across dilutions) and
#'   `pass`; `NULL` if no sample has several dilutions.
#' @export
dilution_linearity <- function(samples, criteria = qc_criteria()) {
    if (is.null(samples) || !all(c('sample.name', 'dilution') %in% names(samples))) return(NULL)
    key_cols <- intersect(c('sample.name', 'sample.date'), names(samples))
    key <- do.call(paste, c(lapply(samples[key_cols], as.character), sep = ' | '))
    rows <- lapply(unique(key), function(k) {
        s <- samples[key == k & !is.na(samples$mean), , drop = FALSE]
        if (length(unique(s$dilution)) < 2) return(NULL)
        by_dil <- stats::aggregate(s$mean, list(dilution = s$dilution), mean)
        if (nrow(by_dil) < 2) return(NULL)
        overall <- mean(by_dil$x)
        dev <- 100 * (by_dil$x - overall) / overall
        cv <- cv_percent(by_dil$x)
        out <- s[rep(1, nrow(by_dil)), key_cols, drop = FALSE]
        out$dilution <- by_dil$dilution
        out$mean <- by_dil$x
        out$deviation <- dev
        out$cv <- cv
        out$pass <- all(abs(dev) <= criteria$dilution_bias) && (is.na(cv) || cv <= criteria$dilution_cv)
        out
    })
    out <- do.call(rbind, rows)
    if (is.null(out)) return(NULL)
    rownames(out) <- NULL
    out
}


#' Assess Quality Controls
#'
#' @param qc Well-level QC results from [calculate()].
#' @param range Quantification range.
#' @param criteria See [qc_criteria()].
#'
#' @return A list with `levels` (one row per QC: `id`, `nominal`, `n`,
#'   `n.pass`, `mean`, `cv`, `bias`, `bias.limit`, `pass`), `wells` (the QC
#'   wells with `bias` and `pass`) and `accepted` (the QC rule).
#' @keywords internal
assess_qc <- function(qc, range, criteria = qc_criteria()) {
    if (is.null(qc) || nrow(qc) == 0) return(NULL)
    # QC wells without a code are grouped by their nominal concentration
    by_nominal <- paste('QC', qc$nominal)
    if (!'id' %in% names(qc)) qc$id <- by_nominal
    qc$id[is.na(qc$id)] <- by_nominal[is.na(qc$id)]
    qc$bias.limit <- ifelse(near_edge(qc$nominal, range, criteria$edge),
                            criteria$qc_bias_edge, criteria$qc_bias)
    qc$bias <- 100 * (qc$conc - qc$nominal) / qc$nominal
    qc$pass <- !is.na(qc$bias) & abs(qc$bias) <= qc$bias.limit
    levels <- do.call(rbind, lapply(unique(qc$id), function(i) {
        q <- qc[qc$id == i, ]
        m <- if (all(is.na(q$conc))) NA_real_ else mean(q$conc, na.rm = TRUE)
        data.frame(id = i, nominal = q$nominal[1], n = nrow(q), n.pass = sum(q$pass),
                   mean = m, cv = cv_percent(q$conc), bias = 100 * (m - q$nominal[1]) / q$nominal[1],
                   bias.limit = q$bias.limit[1],
                   pass = sum(q$pass) / nrow(q) >= criteria$qc_level_fraction)
    }))
    accepted <- mean(qc$pass) >= criteria$qc_fraction - 1e-9 && all(levels$pass)
    list(levels = levels, wells = qc, accepted = accepted)
}


#' Quality Assessment of a Run
#'
#' Combines the calibration acceptance, the QC rule, replicate precision,
#' dilution linearity and the quality of the curve into a verdict.
#'
#' @param fit A `lin_fit` object.
#' @param standards,calibration Outputs of [assess_standards()] and
#'   `calibration_acceptance()`.
#' @param result,qc Well-level sample and QC results.
#' @param range Quantification range.
#' @param lack_of_fit Output of [lack_of_fit()].
#' @param criteria See [qc_criteria()].
#'
#' @return A list with `standards`, `calibration`, `samples`, `dilution`,
#'   `qc`, `curve` (R2, residual SE, lack-of-fit p) , `accepted` (logical) and
#'   `reasons` (list of `lin_message` explaining a rejection or warnings).
#' @keywords internal
assess_run <- function(fit, standards, calibration, result, qc, range, lack_of_fit,
                       criteria = qc_criteria()) {
    samples <- summarise_samples(result, range, criteria)
    dilution <- dilution_linearity(samples, criteria)
    if (!is.null(dilution) && !is.null(samples)) {
        bad <- dilution[!dilution$pass, intersect(c('sample.name', 'sample.date'), names(dilution)),
                        drop = FALSE]
        if (nrow(bad) > 0) {
            key_cols <- names(bad)
            k_s <- do.call(paste, c(lapply(samples[key_cols], as.character), sep = ' | '))
            k_b <- do.call(paste, c(lapply(bad[key_cols], as.character), sep = ' | '))
            hit <- k_s %in% k_b
            samples$flag[hit] <- ifelse(samples$flag[hit] == FLAGS$ok, FLAGS$nonlinear,
                                        paste(FLAGS$nonlinear, samples$flag[hit], sep = '; '))
        }
    }
    qc_assessment <- assess_qc(qc, range, criteria)

    res <- fit$y - curve_fun(fit, fit$x)
    curve <- list(r2 = 1 - sum(res^2) / sum((fit$y - mean(fit$y))^2),
                  sigma = fit$sigma, lof_p = lack_of_fit$p)

    reasons <- list()
    if (!calibration$accepted) {
        reasons <- c(reasons, list(lin_message('quality.reason_calibration', n = calibration$n_pass,
                                               total = calibration$n_levels, needed = calibration$n_needed)))
    }
    if (!is.null(qc_assessment) && !qc_assessment$accepted) {
        reasons <- c(reasons, list(lin_message('quality.reason_qc')))
    }
    accepted <- length(reasons) == 0

    warnings <- list()
    if (!is.null(samples) && any(grepl(FLAGS$high_cv, samples$flag, fixed = TRUE))) {
        warnings <- c(warnings, list(lin_message('quality.warn_cv',
                                                 n = sum(grepl(FLAGS$high_cv, samples$flag, fixed = TRUE)))))
    }
    if (!is.null(dilution) && any(!dilution$pass)) {
        n_bad <- nrow(unique(dilution[!dilution$pass, intersect(c('sample.name', 'sample.date'),
                                                                  names(dilution)), drop = FALSE]))
        warnings <- c(warnings, list(lin_message('quality.warn_dilution', n = n_bad)))
    }
    if (!is.na(curve$r2) && curve$r2 < 0.99) {
        warnings <- c(warnings, list(lin_message('quality.warn_r2', r2 = round(curve$r2, 4))))
    }

    list(standards = standards, calibration = calibration, samples = samples, dilution = dilution,
         qc = qc_assessment, curve = curve, accepted = accepted, reasons = reasons,
         warnings = warnings, criteria = criteria)
}
