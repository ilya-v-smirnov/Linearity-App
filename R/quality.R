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
#'   inside the range and at the boundary calibrators (lowest and highest level).
#' @param cv,cv_edge Allowed CV of the back-calculated concentrations of a
#'   standard level (%), inside the range and at the boundary calibrators.
#' @param sample_cv,sample_cv_edge Allowed CV of the concentrations of sample
#'   replicates (%), inside the range and near its ends; by default the same as
#'   `cv` and `cv_edge`.
#' @param min_fraction,min_levels Minimum fraction and number of accepted standard levels.
#' @param qc_bias,qc_bias_edge Allowed bias of QC (%), inside the range and near its ends.
#' @param qc_fraction,qc_level_fraction Minimum fraction of all QC wells and of
#'   the wells of each QC level within limits.
#' @param dilution_bias,dilution_cv Allowed deviation (%) of a dilution from
#'   the mean of all dilutions of a sample, and CV (%) across dilutions.
#' @param range Quantification range: `"accepted"` (from the lowest to the
#'   highest accepted standard level) or `"all"` (all standards used in the fit).
#' @param edge Concentrations within this factor of the boundary calibrators
#'   (the lowest and highest accepted calibrator of the plate) use the
#'   `_edge` limits.
#' @param singlets Are standard levels planned as single wells accepted (by
#'   their bias, the CV cannot be assessed)? Only the plan counts: a level
#'   planned in replicate that keeps one result follows `replicate_loss`.
#' @param replicate_loss A standard level planned in replicate with a single
#'   back-calculated result: `"documented"` accepts it (by its bias, the CV
#'   cannot be assessed) only if every well lost before the fit was excluded
#'   by the user, who gives a reason; `"reject"` requires at least two
#'   back-calculated results whatever the reason of the loss. A well without
#'   an OD or left out automatically (OD <= blank) is not a documented loss
#'   (see [assess_standards()]).
#'
#' @return A list of class `lin_criteria`.
#' @export
#' @examples
#' qc_criteria()
#' qc_criteria(std_bias = 15, std_bias_edge = 20, cv = 15, cv_edge = 20)  # chromatography
qc_criteria <- function(std_bias = 20, std_bias_edge = 25, cv = 20, cv_edge = 25,
                        min_fraction = 0.75, min_levels = 6, sample_cv = cv, sample_cv_edge = cv_edge,
                        qc_bias = 20, qc_bias_edge = 25, qc_fraction = 2 / 3, qc_level_fraction = 0.5,
                        dilution_bias = 20, dilution_cv = 20, range = 'accepted', edge = 1.25,
                        singlets = TRUE, replicate_loss = 'documented') {
    out <- list(std_bias = std_bias, std_bias_edge = std_bias_edge, cv = cv, cv_edge = cv_edge,
                min_fraction = min_fraction, min_levels = min_levels,
                sample_cv = sample_cv, sample_cv_edge = sample_cv_edge,
                qc_bias = qc_bias, qc_bias_edge = qc_bias_edge, qc_fraction = qc_fraction,
                qc_level_fraction = qc_level_fraction, dilution_bias = dilution_bias,
                dilution_cv = dilution_cv, range = range, edge = edge, singlets = singlets,
                replicate_loss = replicate_loss)
    if (!(is.logical(singlets) && length(singlets) == 1 && !is.na(singlets))) {
        lin_abort('error.criteria', names = 'singlets')
    }
    if (!(length(replicate_loss) == 1 && replicate_loss %in% replicate_loss_rules())) {
        lin_abort('error.criteria', names = 'replicate_loss')
    }
    numeric <- setdiff(names(out), c('range', 'singlets', 'replicate_loss'))
    bad <- numeric[!vapply(out[numeric], function(v) is.numeric(v) && length(v) == 1 && !is.na(v) && v >= 0,
                           logical(1))]
    if (length(bad) > 0) lin_abort('error.criteria', names = bad)
    if (!range %in% c('accepted', 'all')) lin_abort('error.criteria', names = 'range')
    structure(out, class = 'lin_criteria')
}


#' Rules for a Standard Level That Lost a Replicate
#'
#' See `replicate_loss` of [qc_criteria()].
#'
#' @return Character vector of rule codes.
#' @export
replicate_loss_rules <- function() c('documented', 'reject')


#' Presets of the Acceptance Criteria
#'
#' * `ich_m10`: ICH M10 (2022) for ligand binding assays, the defaults of
#'   [qc_criteria()];
#' * `colorimetric`: tighter limits for colorimetric assays (BCA, Griess);
#' * `custom`: editable values, starting from those of ICH M10.
#'
#' @param part `"standards"` (the criteria of the standard curve step) or
#'   `"samples"` (the replicates, quality controls and dilutions of the result
#'   summary step).
#'
#' @return A named list (by preset code) of lists: `std_bias`,
#'   `std_bias_edge`, `cv`, `cv_edge`, `min_fraction` and `min_levels` for
#'   the standards; `sample_cv`, `sample_cv_edge`, `qc_bias`, `qc_bias_edge`,
#'   `qc_fraction`, `qc_level_fraction`, `dilution_bias` and `dilution_cv` for
#'   the samples.
#' @export
criteria_presets <- function(part = c('standards', 'samples')) {
    part <- match.arg(part)
    if (part == 'samples') {
        ich_m10 <- list(sample_cv = 20, sample_cv_edge = 25, qc_bias = 20, qc_bias_edge = 25,
                        qc_fraction = 2 / 3, qc_level_fraction = 0.5, dilution_bias = 20, dilution_cv = 20)
        colorimetric <- list(sample_cv = 15, sample_cv_edge = 20, qc_bias = 15, qc_bias_edge = 20,
                             qc_fraction = 2 / 3, qc_level_fraction = 0.5, dilution_bias = 15, dilution_cv = 15)
    } else {
        ich_m10 <- list(std_bias = 20, std_bias_edge = 25, cv = 20, cv_edge = 25, min_fraction = 0.75,
                        min_levels = 6)
        colorimetric <- list(std_bias = 15, std_bias_edge = 20, cv = 15, cv_edge = 20, min_fraction = 0.80,
                             min_levels = 5)
    }
    list(ich_m10 = ich_m10, colorimetric = colorimetric, custom = ich_m10)
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
#' the bias of the mean back-calculated concentration and the CV of the
#' back-calculated concentrations of the replicates. The lowest and highest
#' levels of the fit (boundary calibrators) use the `_edge` limits.
#'
#' Each level counts its wells: planned (in the layout), measured (with an
#' OD), fitted (used in the fit) and back-calculated (with a concentration
#' from the inverse of the curve). The bias and the CV use the back-calculated
#' concentrations only; the CV of the OD is kept apart (`od.cv`). The status
#' of a level (`status`):
#'
#' * `pass`, `fail`: the bias and the CV against their limits;
#' * `incomplete`: some fitted wells have no back-calculated concentration
#'   (e.g. the OD lies outside the asymptotes); not accepted;
#' * `pass_no_cv`: planned in replicate, one result is left and every well
#'   lost before the fit was excluded by the user (with a reason), as the rule
#'   `replicate_loss = "documented"` allows; accepted by its bias, the CV is
#'   unavailable;
#' * `replicate_lost`: planned in replicate, one result is left, and the rule
#'   requires two results (`replicate_loss = "reject"`) or a well was lost
#'   without a documented reason (no OD, OD <= blank, ...); not accepted;
#' * `pass_singlet`, `singlet`: planned as a single well; accepted by its bias
#'   if the criteria allow singlets (`singlets`), otherwise not accepted;
#' * `no_points`: no well of the level in the fit; not accepted;
#' * `outside_interval`: the level lies outside the monotonic interval set
#'   for a quadratic curve (`fit$interval`); not accepted.
#'
#' @param fit A `lin_fit` object.
#' @param criteria See [qc_criteria()].
#' @param levels Concentrations of all standard wells of the run (one value
#'   per well, so that the planned replicates are counted); by default the
#'   wells of the fit. Not used with `wells`.
#' @param wells All standard wells of the run with `conc` and `OD` (`NA`
#'   where the OD is missing); optionally `key` (well ID), `lost` (why a
#'   well is not in the fit: `"excluded"` by the user, `"missing"` OD or
#'   `"od_le_blank"`; `NA` for the wells of the fit) and `comment` (the reason
#'   given by the user). Without `lost`, a well without an OD is `"missing"`
#'   and the other wells left out of the fit have no known reason.
#'
#' @return A data.frame with one row per level: `conc`, `n` (wells in the
#'   fit), `n.planned`, `n.measured`, `n.calc` (back-calculated), `od.mean`,
#'   `od.cv`, `calc.mean`, `calc.cv` (%), `bias` (%), `bias.limit`,
#'   `cv.limit`, `status`, `pass`, `failed` (wells of the fit without a
#'   back-calculated concentration: a data.frame with `key` and `reason` for
#'   each level, see [inverse_reason()]) and `lost` (all wells of the level
#'   without a back-calculated concentration, lost before the fit or by the
#'   inverse: a data.frame with `key`, `reason` and `comment` for each level).
#' @export
assess_standards <- function(fit, criteria = qc_criteria(), levels = NULL, wells = NULL) {
    bc <- back_calculate(fit)
    ends <- range(bc$conc)
    rule <- if (is.null(criteria$replicate_loss)) 'documented' else criteria$replicate_loss
    if (!is.null(wells)) {
        wells <- wells[!is.na(wells$conc), , drop = FALSE]
        if (is.null(wells$lost)) wells$lost <- ifelse(is.na(wells$OD), 'missing', NA_character_)
        if (is.null(wells$key)) wells$key <- rep(NA_character_, nrow(wells))
        if (is.null(wells$comment)) wells$comment <- rep(NA_character_, nrow(wells))
    }
    planned <- if (!is.null(wells)) wells$conc else levels
    planned <- planned[!is.na(planned)]
    measured <- if (!is.null(wells)) wells$conc[!is.na(wells$OD)] else planned
    all_levels <- sort(unique(c(bc$conc, planned)))
    rows <- lapply(all_levels, function(lv) {
        b <- bc[bc$conc == lv, ]
        n_fit <- nrow(b)
        n_planned <- max(sum(planned == lv), n_fit)
        n_measured <- max(sum(measured == lv), n_fit)
        # Wells lost before the fit, with their reasons; the loss is documented when every lost well
        # was excluded by the user (a reason is required)
        before <- if (is.null(wells)) data.frame(key = character(0), reason = character(0), comment = character(0)) else {
            w <- wells[wells$conc == lv & !is.na(wells$lost), , drop = FALSE]
            data.frame(key = as.character(w$key), reason = w$lost, comment = as.character(w$comment))
        }
        n_lost <- n_planned - n_fit
        documented <- n_lost > 0 && nrow(before) == n_lost && all(before$reason == 'excluded')
        calc <- b$calc[!is.na(b$calc)]
        n_calc <- length(calc)
        edge <- lv <= ends[1] || lv >= ends[2]
        bias_limit <- if (edge) criteria$std_bias_edge else criteria$std_bias
        cv_limit <- if (edge) criteria$cv_edge else criteria$cv
        calc_mean <- if (n_calc > 0) mean(calc) else NA_real_
        bias <- 100 * (calc_mean - lv) / lv
        calc_cv <- cv_percent(calc)
        within <- !is.na(bias) && abs(bias) <= bias_limit && (is.na(calc_cv) || calc_cv <= cv_limit)
        outside <- !is.null(fit$interval) &&
            (lv < fit$interval[1] * (1 - 1e-9) || lv > fit$interval[2] * (1 + 1e-9))
        status <- if (n_fit == 0) {
            'no_points'
        } else if (outside) {
            'outside_interval'
        } else if (n_calc < n_fit) {
            'incomplete'
        } else if (n_calc == 1 && n_planned >= 2 && !(rule == 'documented' && documented)) {
            # A replicate left alone: by the plan, not as a singlet
            'replicate_lost'
        } else if (!within) {
            'fail'
        } else if (n_calc >= 2) {
            'pass'
        } else if (n_planned >= 2) {
            'pass_no_cv'
        } else if (isFALSE(criteria$singlets)) {
            'singlet'
        } else {
            'pass_singlet'
        }
        out <- data.frame(conc = lv, n = n_fit, n.planned = n_planned, n.measured = n_measured, n.calc = n_calc,
                          od.mean = if (n_fit > 0) mean(b$OD) else NA_real_,
                          od.cv = if (n_fit > 0) cv_percent(b$OD) else NA_real_,
                          calc.mean = calc_mean, calc.cv = calc_cv, bias = bias, bias.limit = bias_limit,
                          cv.limit = cv_limit, status = status,
                          pass = status %in% c('pass', 'pass_no_cv', 'pass_singlet'))
        lost <- is.na(b$calc)
        failed <- data.frame(key = b$key[lost], reason = b$reason[lost])
        list(row = out, failed = failed,
             lost = rbind(before, data.frame(key = as.character(failed$key), reason = failed$reason,
                                             comment = rep(NA_character_, nrow(failed)))))
    })
    out <- do.call(rbind, lapply(rows, `[[`, 'row'))
    out$failed <- lapply(rows, `[[`, 'failed')
    out$lost <- lapply(rows, `[[`, 'lost')
    out
}


#' Acceptance of the Calibration
#'
#' The calibration is accepted if at least `min_fraction` of all standard
#' levels of the run, and at least `min_levels` levels, are accepted. The CV
#' of a level with a single result (a singlet, or a replicate left alone)
#' cannot be assessed: when fewer than
#' `min_levels` accepted levels have an assessed CV, the calibration is
#' accepted with this exception (`status = "accepted_no_cv"`).
#'
#' @param standards Output of [assess_standards()] with all levels of the run.
#' @param criteria See [qc_criteria()].
#'
#' @return A list with `n_levels`, `n_pass`, `n_needed`, `accepted`,
#'   `n_no_cv` (accepted levels whose CV cannot be assessed), `n_cv`
#'   (accepted levels with an assessed CV), `n_cv_needed` (`min_levels`),
#'   `status`
#'   (`"accepted"`, `"accepted_no_cv"` or `"rejected"`), `lloq` and `uloq`
#'   (lowest and highest accepted level, `NA` if none).
#' @keywords internal
calibration_acceptance <- function(standards, criteria = qc_criteria()) {
    n_levels <- nrow(standards)
    n_pass <- sum(standards$pass)
    needed <- max(ceiling(criteria$min_fraction * n_levels), criteria$min_levels)
    passed <- standards$conc[standards$pass]
    accepted <- n_pass >= needed
    n_no_cv <- sum(standards$pass & is.na(standards$calc.cv))
    n_cv <- n_pass - n_no_cv
    status <- if (!accepted) 'rejected' else if (n_cv < criteria$min_levels) 'accepted_no_cv' else 'accepted'
    list(n_levels = n_levels, n_pass = n_pass, n_needed = needed,
         accepted = accepted, n_no_cv = n_no_cv, n_cv = n_cv, n_cv_needed = criteria$min_levels,
         status = status,
         lloq = if (length(passed) > 0) min(passed) else NA_real_,
         uloq = if (length(passed) > 0) max(passed) else NA_real_)
}


#' Replicate Groups of Samples
#'
#' Wells with the same values of `group_vars` are replicates. Without
#' grouping columns, wells with the same `id` (plate format), or with the same
#' `sample.name`, `dilution` and `sample.date` (table format), are replicates.
#'
#' @param result Well-level results.
#' @param group_vars Grouping columns, or `NULL` for the default grouping.
#'
#' @return Character vector of group labels.
#' @keywords internal
replicate_groups <- function(result, group_vars = NULL) {
    if (length(group_vars) == 0) {
        if ('id' %in% names(result) && !anyNA(result$id)) return(as.character(result$id))
        group_vars <- intersect(c('sample.name', 'dilution', 'sample.date'), names(result))
    }
    do.call(paste, c(lapply(result[group_vars], as.character), sep = ' | '))
}


#' Summary of Sample Replicates
#'
#' @param result Well-level sample results from [calculate()].
#' @param range Quantification range; `NULL` uses the limit inside the range
#'   (`sample_cv`) for every group.
#' @param criteria See [qc_criteria()].
#' @param group_vars Columns defining a replicate group (see
#'   [replicate_groups()]); `NULL` for the default grouping.
#'
#' @return A data.frame with one row per replicate group (in the order of the
#'   wells; with `group_vars`, ordered by the columns, each in the order of
#'   its first appearance): identification columns (`group_vars`, or `id`, `sample.name`,
#'   `dilution`, `sample.date` for the default grouping), `n`, `n.quantified`,
#'   `mean`, `sd`, `cv` (%), `min`, `max`, `cv.limit` and `flag` (codes from
#'   [FLAGS] separated by `"; "`); `NULL` without wells.
#' @keywords internal
summarise_samples <- function(result, range = NULL, criteria = qc_criteria(), group_vars = NULL) {
    if (is.null(result) || nrow(result) == 0) return(NULL)
    if (length(group_vars) > 0) {
        # Groups ordered column by column, each column in the order of its first appearance
        ranks <- lapply(group_vars, function(v) {
            x <- as.character(result[[v]])
            match(x, unique(x))
        })
        result <- result[do.call(order, ranks), , drop = FALSE]
    }
    group <- replicate_groups(result, group_vars)
    id_cols <- if (length(group_vars) > 0) group_vars else
        intersect(c('id', 'sample.name', 'dilution', 'sample.date'), names(result))
    dilution <- if ('dilution' %in% names(result)) result$dilution else rep(1, nrow(result))
    flag <- if ('flag' %in% names(result)) result$flag else rep(FLAGS$ok, nrow(result))
    rows <- lapply(split(seq_len(nrow(result)), factor(group, levels = unique(group))), function(i) {
        quantified <- i[!is.na(result$conc[i])]
        conc <- result$conc[quantified]
        m <- if (length(conc) > 0) mean(conc) else NA_real_
        cv <- cv_percent(conc)
        # The limit near the ends of the range follows the concentration in the well
        measured <- if (length(conc) > 0) mean(conc / dilution[quantified]) else NA_real_
        edge <- !is.null(range) && near_edge(measured, range, criteria$edge)
        cv_limit <- if (edge) criteria$sample_cv_edge else criteria$sample_cv
        flags <- unique(flag[i][!is.na(flag[i]) & flag[i] != FLAGS$ok])
        if (!is.na(cv) && cv > cv_limit) flags <- c(FLAGS$high_cv, flags)
        out <- result[i[1], id_cols, drop = FALSE]
        out$n <- length(i)
        out$n.quantified <- length(conc)
        out$mean <- m
        out$sd <- if (length(conc) >= 2) stats::sd(conc) else NA_real_
        out$cv <- cv
        out$min <- if (length(conc) > 0) min(conc) else NA_real_
        out$max <- if (length(conc) > 0) max(conc) else NA_real_
        out$cv.limit <- cv_limit
        out$flag <- if (length(flags) == 0) FLAGS$ok else paste(flags, collapse = '; ')
        out
    })
    out <- do.call(rbind, rows)
    rownames(out) <- NULL
    out
}


#' Value Columns of the Dilution Linearity
#'
#' The columns of [dilution_linearity()] that are not the keys of a sample.
#'
#' @return Character vector.
#' @keywords internal
dilution_value_columns <- function() c('dilution', 'n', 'n.quantified', 'mean', 'deviation', 'cv', 'pass')


#' Dilution Linearity
#'
#' For each sample measured at two or more dilutions, compares the
#' dilution-corrected mean concentration at each dilution with the mean over
#' all dilutions and computes the CV across dilutions.
#'
#' @param samples Output of [summarise_samples()] with a `dilution` column.
#' @param criteria See [qc_criteria()].
#' @param key_cols Columns identifying a sample; by default `sample.name` and
#'   `sample.date`.
#'
#' @return A data.frame with the `key_cols`, `dilution`, `n` and
#'   `n.quantified` (wells of the sample at the dilution, summed over the
#'   rows of `samples`), `mean`, `deviation` (%), `cv` (% across dilutions)
#'   and `pass`; `NULL` if no sample has several dilutions.
#' @export
dilution_linearity <- function(samples, criteria = qc_criteria(), key_cols = NULL) {
    if (is.null(samples) || !'dilution' %in% names(samples)) return(NULL)
    if (is.null(key_cols)) key_cols <- intersect(c('sample.name', 'sample.date'), names(samples))
    if (length(key_cols) == 0 || !all(key_cols %in% names(samples))) return(NULL)
    key <- do.call(paste, c(lapply(samples[key_cols], as.character), sep = ' | '))
    rows <- lapply(unique(key), function(k) {
        all_rows <- samples[key == k, , drop = FALSE]
        s <- all_rows[!is.na(all_rows$mean), , drop = FALSE]
        if (length(unique(s$dilution)) < 2) return(NULL)
        by_dil <- stats::aggregate(s$mean, list(dilution = s$dilution), mean)
        overall <- mean(by_dil$x)
        dev <- 100 * (by_dil$x - overall) / overall
        cv <- cv_percent(by_dil$x)
        # Wells behind each mean, including the rows without a quantified well
        count <- function(col) {
            if (!col %in% names(all_rows)) return(rep(NA_integer_, nrow(by_dil)))
            vapply(by_dil$dilution, function(x) as.integer(sum(all_rows[[col]][all_rows$dilution %in% x])), 0L)
        }
        out <- s[rep(1, nrow(by_dil)), key_cols, drop = FALSE]
        out$dilution <- by_dil$dilution
        out$n <- count('n')
        out$n.quantified <- count('n.quantified')
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


#' Quality Assessment of Samples
#'
#' Precision of the replicates and dilution linearity of the samples. The
#' replicate groups are the rows of the result summary: with `group_vars`
#' given, the dilutions are compared within the groups of `group_vars`
#' without `dilution`.
#'
#' @param result Well-level sample results from [calculate()].
#' @param range Quantification range.
#' @param criteria See [qc_criteria()].
#' @param group_vars Columns defining a replicate group; `NULL` for the
#'   default grouping (see [replicate_groups()]).
#'
#' @return A list with `samples` (see [summarise_samples()], with the
#'   `"Dilution non-linear"` flag added), `dilution` (see
#'   [dilution_linearity()]) and `warnings` (list of messages).
#' @export
assess_samples <- function(result, range = NULL, criteria = qc_criteria(), group_vars = NULL) {
    samples <- summarise_samples(result, range, criteria, group_vars)
    dilution <- NULL
    if (!is.null(samples) && 'dilution' %in% names(result)) {
        if (length(group_vars) == 0) {
            key_cols <- intersect(c('sample.name', 'sample.date'), names(samples))
            by_dilution <- samples
        } else {
            key_cols <- setdiff(group_vars, 'dilution')
            by_dilution <- if ('dilution' %in% group_vars) samples else
                summarise_samples(result, range, criteria, c(key_cols, 'dilution'))
        }
        if (length(key_cols) > 0) dilution <- dilution_linearity(by_dilution, criteria, key_cols)
        if (!is.null(dilution) && any(!dilution$pass)) {
            key <- function(x) do.call(paste, c(lapply(x[key_cols], as.character), sep = ' | '))
            hit <- key(samples) %in% key(dilution[!dilution$pass, , drop = FALSE])
            samples$flag[hit] <- ifelse(samples$flag[hit] == FLAGS$ok, FLAGS$nonlinear,
                                        paste(FLAGS$nonlinear, samples$flag[hit], sep = '; '))
        }
    }

    warnings <- list()
    n_cv <- if (is.null(samples)) 0 else sum(grepl(FLAGS$high_cv, samples$flag, fixed = TRUE))
    if (n_cv > 0) warnings <- c(warnings, list(lin_message('quality.warn_cv', n = n_cv)))
    if (!is.null(dilution) && any(!dilution$pass)) {
        cols <- setdiff(names(dilution), dilution_value_columns())
        n_bad <- nrow(unique(dilution[!dilution$pass, cols, drop = FALSE]))
        warnings <- c(warnings, list(lin_message('quality.warn_dilution', n = n_bad)))
    }
    list(samples = samples, dilution = dilution, warnings = warnings)
}


#' Message Keys of the Sample Assessment
#'
#' Warnings that depend on the grouping of the replicates; the app shows them
#' in the result summary, where the grouping is chosen.
#'
#' @return Character vector of dictionary keys.
#' @keywords internal
sample_warning_keys <- function() c('quality.warn_cv', 'quality.warn_dilution')


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
#' @param background Status of the background (see [background_summary()]):
#'   `"unavailable"` suspends the verdict, the only reason given is then the
#'   missing background (the other criteria are assessed on the OD as
#'   measured and would mislead).
#'
#' @return A list with `standards`, `calibration`, `samples`, `dilution`,
#'   `qc`, `curve` (R2, residual SE, lack-of-fit p) , `accepted` (logical),
#'   `background` (the status) and `reasons` (list of `lin_message`
#'   explaining a rejection or warnings).
#' @keywords internal
assess_run <- function(fit, standards, calibration, result, qc, range, lack_of_fit,
                       criteria = qc_criteria(), background = 'ok') {
    sample_assessment <- assess_samples(result, range, criteria)
    samples <- sample_assessment$samples
    dilution <- sample_assessment$dilution
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
    if (identical(background, 'unavailable')) reasons <- list(lin_message('quality.reason_background'))
    accepted <- length(reasons) == 0

    warnings <- sample_assessment$warnings
    if (identical(calibration$status, 'accepted_no_cv')) {
        warnings <- c(warnings, list(lin_message('quality.warn_cv_not_assessed', n_cv = calibration$n_cv,
                                                 needed = calibration$n_cv_needed, n = calibration$n_no_cv)))
    }
    # R2 is descriptive: no criterion of the methods is set on it, so it gives no warning
    if (identical(background, 'unavailable')) warnings <- list()

    list(standards = standards, calibration = calibration, samples = samples, dilution = dilution,
         qc = qc_assessment, curve = curve, accepted = accepted, background = background, reasons = reasons,
         warnings = warnings, criteria = criteria)
}
