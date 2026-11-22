#' Verdict of the Calibration
#'
#' A green, orange or red box with the acceptance of the calibration (the
#' standards only; the quality controls are assessed in the result summary).
#' The title gives the decision and the number of levels passed (orange: the
#' levels whose CV is unavailable instead). Below: the accepted range (or the
#' rule of acceptance that is not met) and one line for each level that needs
#' attention (see [verdict_levels()]): the criterion failed, its value and
#' limit, and where the level is. Each line opens on the wells of its level
#' (see [level_wells_ui()]). A rejected calibration ends with the range
#' applied to the samples, whose concentrations are not reportable. With an
#' unavailable background (see [background_summary()]) the verdict is
#' suspended: a red box says so instead.
#'
#' @param result Output of [calculate()].
#' @param lang Language code.
#' @param unit Concentration unit appended to the range (e.g. `"nM"`); empty
#'   for none.
#'
#' @return A shiny tag.
#' @keywords internal
verdict_ui <- function(result, lang, unit = '') {
    if (identical(result$background$status, 'unavailable')) return(background_unavailable_ui(lang))
    cal <- result$quality$calibration
    st <- result$quality$standards
    accepted <- isTRUE(cal$accepted)
    no_cv <- accepted && identical(cal$status, 'accepted_no_cv')
    unit <- if (nzchar(unit)) paste0(' ', unit) else ''
    used <- list(lloq = signif(result$calibration_range[1], 3), uloq = signif(result$calibration_range[2], 3),
                 unit = unit)
    ids <- level_ids(result)
    counts <- list(n = cal$n_pass, total = cal$n_levels, needed = cal$n_needed)
    title <- if (no_cv) {
        no_cv_rows <- which(st$pass & is.na(st$calc.cv))
        tr('verdict.title_no_cv', lang, levels = ids[no_cv_rows[order(-st$conc[no_cv_rows])]])
    } else if (accepted) {
        do.call(tr, c(list('verdict.title_accepted', lang), counts))
    } else {
        do.call(tr, c(list('verdict.title_rejected', lang), counts))
    }
    range <- do.call(tr, c(list('quality.calibration_range', lang), used))
    head <- if (no_cv) {
        list(paste0(range, '. ', do.call(tr, c(list('verdict.passed_cv', lang, n_cv = cal$n_cv), counts))))
    } else if (accepted) {
        list(paste0(range, '.'))
    } else {
        list(do.call(tr, c(list('verdict.rule', lang), counts)))
    }
    items <- verdict_levels(result, lang)
    tail <- if (!accepted) {
        list(tr('quality.range_not_established', lang), do.call(tr, c(list('quality.range_used', lang), used)))
    }
    style <- if (no_cv) 'alert-warning' else if (accepted) 'alert-success' else 'alert-danger'
    icon <- if (no_cv) 'triangle-exclamation' else if (accepted) 'circle-check' else 'circle-xmark'
    shiny::div(class = 'quality-verdict',
               shiny::div(class = paste('alert', style),
                          shiny::h4(shiny::icon(icon), ' ', title),
                          lapply(head, shiny::p),
                          items,
                          lapply(tail, shiny::p)))
}


#' IDs of the Standard Levels of an Assessment
#'
#' @param result Output of [calculate()].
#'
#' @return The IDs (`S1`, ...) of the rows of `result$quality$standards`, as
#'   in every table of the app (see [standard_level_map()]).
#' @keywords internal
level_ids <- function(result) {
    st <- result$quality$standards
    map <- if (is.null(result$standard_levels)) st$conc else result$standard_levels
    standard_names(st$conc, map)
}


#' Name of a Standard Level in a Message
#'
#' @param id Level ID (`S3`).
#' @param conc Nominal concentration.
#'
#' @return `"S3 (25)"`.
#' @keywords internal
level_label <- function(id, conc) paste0(id, ' (', signif(conc, 3), ')')


#' Signed Percentage Against Its Limit
#'
#' @inheritParams pct_limit
#'
#' @return The values of [pct_limit()] with a `+` before the positive ones.
#' @keywords internal
signed_pct <- function(x, limit) {
    out <- pct_limit(x, limit)
    ifelse(!is.na(x) & x > 0 & nzchar(out), paste0('+', out), out)
}


#' Wells of a Standard Level
#'
#' The wells of the fit, with their back-calculated concentration and bias,
#' then the wells lost before the fit (excluded by the user, without an OD,
#' left out as OD <= blank).
#'
#' @param result Output of [calculate()].
#' @param i Row of `result$quality$standards`.
#'
#' @return A data.frame with `key`, `OD` (background-subtracted), `calc`,
#'   `bias` (%), `fit` (`TRUE` for the wells of the fit), `reason` and
#'   `comment` (why a well has no back-calculated concentration: see
#'   `lost` of [assess_standards()]).
#' @keywords internal
level_wells <- function(result, i) {
    st <- result$quality$standards
    lv <- st$conc[i]
    std <- result$standard_data[result$standard_data$conc == lv, , drop = FALSE]
    calc <- if (nrow(std) > 0) inverse_fun(result$fit, std$OD) else numeric(0)
    lost <- st$lost[[i]]
    lost$key <- as.character(lost$key)
    fit <- data.frame(key = as.character(std$.key), OD = std$OD, calc = calc, bias = 100 * (calc - lv) / lv,
                      fit = rep(TRUE, nrow(std)))
    fit$reason <- lost$reason[match(fit$key, lost$key)]
    fit$comment <- rep(NA_character_, nrow(fit))
    before <- lost[!lost$key %in% fit$key, , drop = FALSE]
    ex <- result$exclusions
    od <- if (is.null(ex) || nrow(before) == 0) rep(NA_real_, nrow(before)) else ex$OD[match(before$key, ex$key)]
    rbind(fit, data.frame(key = before$key, OD = od, calc = rep(NA_real_, nrow(before)),
                          bias = rep(NA_real_, nrow(before)), fit = rep(FALSE, nrow(before)),
                          reason = before$reason, comment = as.character(before$comment)))
}


#' Wells of a Standard Level as a Table
#'
#' @param result Output of [calculate()].
#' @param i Row of `result$quality$standards`.
#' @param lang Language code.
#'
#' @return A shiny tag: one row per well (see [level_wells()]) with the
#'   corrected OD, the back-calculated concentration, the bias and whether
#'   the well is in the fit, or why not.
#' @keywords internal
level_wells_ui <- function(result, i, lang) {
    w <- level_wells(result, i)
    limit <- result$quality$standards$bias.limit[i]
    num <- function(x) ifelse(is.na(x), '', trimws(formatC(x, digits = 3, format = 'fg')))
    state <- vapply(seq_len(nrow(w)), function(k) {
        if (w$fit[k] && !is.na(w$calc[k])) return(tr('verdict.state_fit', lang))
        lost_wells_text(data.frame(key = NA_character_, reason = w$reason[k], comment = w$comment[k]), lang)
    }, '')
    head <- c(tr('exclusion.key', lang), tr('col.od_corrected', lang), tr('col.calc', lang), tr('col.bias', lang),
              tr('verdict.col_state', lang))
    rows <- lapply(seq_len(nrow(w)), function(k) {
        shiny::tags$tr(shiny::tags$td(w$key[k]), shiny::tags$td(ifelse(is.na(w$OD[k]), '', sprintf('%.4f', w$OD[k]))),
                       shiny::tags$td(num(w$calc[k])), shiny::tags$td(signed_pct(w$bias[k], limit)),
                       shiny::tags$td(state[k]))
    })
    shiny::tags$table(class = 'table table-sm verdict-wells',
                      shiny::tags$thead(shiny::tags$tr(lapply(head, shiny::tags$th))),
                      shiny::tags$tbody(rows))
}


#' Wells as Text
#'
#' @param keys Well IDs.
#' @param lang Language code.
#'
#' @return `"C7"`, `"C7 and C8"` or `"C7, C8 and C9"`.
#' @keywords internal
wells_text <- function(keys, lang) {
    n <- length(keys)
    if (n <= 1) return(paste(keys, collapse = ''))
    paste0(paste(keys[-n], collapse = ', '), ' ', tr('verdict.and', lang), ' ', keys[n])
}


#' Status of a Standard Level as Text
#'
#' What the criteria found at one level: the criterion failed (bias, CV or
#' both) with the value and the limit, a lost replicate, an incomplete
#' back-calculation, a level outside the monotonic interval, without wells in
#' the fit, a single well not accepted, a level accepted by its bias alone
#' after a documented exclusion, or the values of a level passed.
#'
#' @param result Output of [calculate()].
#' @param i Row of `result$quality$standards`.
#' @param lang Language code.
#'
#' @return One string, beginning with the name of the level.
#' @keywords internal
level_status_text <- function(result, i, lang) {
    st <- result$quality$standards
    s <- st[i, ]
    level <- level_label(level_ids(result)[i], s$conc)
    criteria <- if (is.null(result$quality$criteria)) qc_criteria() else result$quality$criteria
    w <- level_wells(result, i)
    kept <- w$key[w$fit & !is.na(w$calc)]
    before <- w[!w$fit, , drop = FALSE]
    lost <- lost_wells_text(before, lang)
    bias <- signed_pct(s$bias, s$bias.limit)
    cv <- pct_limit(s$calc.cv, s$cv.limit)
    switch(
        s$status,
        pass = tr('verdict.level_pass', lang, level = level, bias = bias, bias_limit = s$bias.limit, cv = cv,
                  cv_limit = s$cv.limit),
        pass_singlet = tr('verdict.level_pass_singlet', lang, level = level, bias = bias, limit = s$bias.limit),
        pass_no_cv = {
            excluded <- paste0(before$key, ifelse(is.na(before$comment) | !nzchar(before$comment), '',
                                                  paste0(' (', before$comment, ')')))
            tr('verdict.bias_alone', lang, level = level, excluded = paste(excluded, collapse = ', '),
               kept = kept, bias = bias)
        },
        fail = {
            bias_bad <- exceeds_limit(s$bias, s$bias.limit)
            cv_bad <- exceeds_limit(s$calc.cv, s$cv.limit)
            if (bias_bad && cv_bad) {
                tr('verdict.fail_both', lang, level = level, bias = bias, bias_limit = s$bias.limit, cv = cv,
                   cv_limit = s$cv.limit)
            } else if (cv_bad) {
                tr('verdict.fail_cv', lang, level = level, cv = cv, limit = s$cv.limit)
            } else {
                tr('verdict.fail_bias', lang, level = level, bias = bias, limit = s$bias.limit)
            }
        },
        replicate_lost = {
            key <- if (identical(criteria$replicate_loss, 'reject')) 'verdict.replicate_lost_rule' else
                'verdict.replicate_lost_undocumented'
            tr(key, lang, level = level, kept = kept, lost = lost)
        },
        incomplete = {
            failed <- w[w$fit & is.na(w$calc), , drop = FALSE]
            tr('verdict.incomplete', lang, level = level, n_calc = s$n.calc, n = s$n,
               lost = lost_wells_text(failed, lang))
        },
        outside_interval = tr('verdict.outside_interval', lang, level = level,
                              lo = signif(result$fit$interval[1], 3), hi = signif(result$fit$interval[2], 3)),
        no_points = if (nzchar(lost)) tr('verdict.no_points', lang, level = level, lost = lost) else
            tr('verdict.no_points_plain', lang, level = level),
        singlet = tr('verdict.singlet', lang, level = level),
        level
    )
}


#' Where a Standard Level Not Accepted Lies
#'
#' For an accepted calibration: a level inside the accepted range says
#' whether its wells were in the fit (the range is not narrowed by it); a
#' level beyond it says where the range ends.
#'
#' @param result Output of [calculate()].
#' @param i Row of `result$quality$standards`.
#' @param lang Language code.
#'
#' @return One sentence, or `NULL`.
#' @keywords internal
level_position_text <- function(result, i, lang) {
    cal <- result$quality$calibration
    st <- result$quality$standards
    lv <- st$conc[i]
    if (!isTRUE(cal$accepted) || is.na(cal$lloq)) return(NULL)
    ids <- level_ids(result)
    edge <- function(conc) level_label(ids[match(conc, st$conc)], conc)
    if (lv > cal$uloq) return(tr('verdict.range_ends', lang, level = edge(cal$uloq)))
    if (lv < cal$lloq) return(tr('verdict.range_starts', lang, level = edge(cal$lloq)))
    w <- level_wells(result, i)
    fit <- w$key[w$fit]
    if (length(fit) == 0) return(NULL)
    if (all(w$fit)) {
        key <- if (length(fit) == 1) 'verdict.fit_one' else if (length(fit) == 2) 'verdict.fit_both' else
            'verdict.fit_all'
        return(tr(key, lang, wells = wells_text(fit, lang)))
    }
    tr('verdict.fit_partial', lang, wells = wells_text(fit, lang), lost = lost_wells_text(w[!w$fit, ], lang))
}


#' Standard Levels That Need Attention
#'
#' One line for each level not accepted and each level accepted by its bias
#' alone after a documented exclusion; these first, then the others from
#' the highest level (`S1`). When the CV is unavailable at too many levels
#' (orange verdict), the levels planned as single wells are named in one more
#' line. Each line opens on the wells of its level.
#'
#' @param result Output of [calculate()].
#' @param lang Language code.
#'
#' @return A list of shiny tags (possibly empty).
#' @keywords internal
verdict_levels <- function(result, lang) {
    st <- result$quality$standards
    if (is.null(st) || nrow(st) == 0) return(list())
    cal <- result$quality$calibration
    order <- order(st$status != 'pass_no_cv', -st$conc)
    show <- order[!st$pass[order] | st$status[order] == 'pass_no_cv']
    items <- lapply(show, function(i) {
        text <- paste(c(level_status_text(result, i, lang),
                        if (!st$pass[i]) level_position_text(result, i, lang)), collapse = ' ')
        shiny::tags$details(class = 'verdict-level', shiny::tags$summary(text), level_wells_ui(result, i, lang))
    })
    singlets <- which(st$status == 'pass_singlet')
    if (identical(cal$status, 'accepted_no_cv') && length(singlets) > 0) {
        ids <- level_ids(result)[singlets[order(-st$conc[singlets])]]
        items <- c(items, list(shiny::p(tr('verdict.singlets', lang, levels = ids))))
    }
    items
}


#' A Standard Point in the Dialog of Exclusion
#'
#' What the dialog of a standard point says besides the point: the
#' back-calculated concentration and the bias of the well against the limit
#' of its level (the criterion of a well), the status of its level (the
#' criteria of a level, see [level_status_text()]) and, before an exclusion,
#' what it leaves to the level: no result, or a single result judged by its
#' bias alone or not accepted, by the rule for a lost replicate.
#'
#' @param result Output of [calculate()].
#' @param key Well ID.
#' @param excluded `TRUE` if the well is excluded (the dialog restores it).
#' @param lang Language code.
#'
#' @return A list of shiny tags (possibly empty).
#' @keywords internal
point_context_ui <- function(result, key, excluded, lang) {
    st <- result$quality$standards
    if (is.null(st) || nrow(st) == 0) return(list())
    pt <- rbind(data.frame(key = as.character(result$standard_data$.key), conc = result$standard_data$conc),
                if (!is.null(result$exclusions)) {
                    data.frame(key = as.character(result$exclusions$key), conc = result$exclusions$conc)
                })
    i <- match(pt$conc[match(key, pt$key)], st$conc)
    if (is.na(i)) return(list())
    s <- st[i, ]
    level <- level_label(level_ids(result)[i], s$conc)
    w <- level_wells(result, i)
    k <- match(key, w$key)
    well <- if (!excluded && !is.na(k) && w$fit[k]) {
        if (is.na(w$calc[k])) {
            reason <- data.frame(key = NA_character_, reason = w$reason[k], comment = NA_character_)
            tr('verdict.point_no_calc', lang, reason = lost_wells_text(reason, lang))
        } else {
            tr('verdict.point_well', lang, calc = signif(w$calc[k], 3), bias = signed_pct(w$bias[k], s$bias.limit),
               limit = s$bias.limit)
        }
    }
    after <- if (!excluded && !is.na(k) && w$fit[k]) {
        criteria <- if (is.null(result$quality$criteria)) qc_criteria() else result$quality$criteria
        left <- sum(w$fit & !is.na(w$calc) & w$key != key)
        others <- w[!w$fit, , drop = FALSE]
        if (left == 0) {
            tr('verdict.after_none', lang, level = level)
        } else if (left == 1 && s$n.planned >= 2) {
            if (identical(criteria$replicate_loss, 'reject')) {
                tr('verdict.after_reject', lang, level = level)
            } else if (all(others$reason == 'excluded')) {
                tr('verdict.after_bias_alone', lang, level = level)
            } else {
                tr('verdict.after_undocumented', lang, level = level,
                   lost = lost_wells_text(others[others$reason != 'excluded', ], lang))
            }
        }
    }
    Filter(Negate(is.null), list(
        if (!is.null(well)) shiny::p(well),
        shiny::p(level_status_text(result, i, lang)),
        if (!is.null(after)) shiny::p(class = 'text-muted', after)
    ))
}


#' Acceptance of the Calibration by Each Model
#'
#' @param results A named list (by model code) of outputs of [calculate()]
#'   with the same data, background, exclusions and criteria; `NULL` for a
#'   model that cannot be calculated.
#'
#' @return A data.frame with `model`, `n.pass`, `n.levels` and `status`
#'   (see [calibration_acceptance()]; `NA` without a calibration).
#' @keywords internal
model_acceptance <- function(results) {
    one <- function(r) {
        cal <- if (is.null(r) || identical(r$background$status, 'unavailable')) NULL else r$quality$calibration
        if (is.null(cal)) return(data.frame(n.pass = NA_integer_, n.levels = NA_integer_, status = NA_character_))
        data.frame(n.pass = as.integer(cal$n_pass), n.levels = as.integer(cal$n_levels), status = cal$status)
    }
    cbind(model = names(results), do.call(rbind, lapply(results, one)), row.names = NULL)
}


#' Acceptance of the Calibration by a Model as Text
#'
#' @param acceptance Output of [model_acceptance()], or `NULL`.
#' @param models Model codes, in the order of the rows.
#'
#' @return `"6/7 ✓"` (accepted), `"6/7 ⚠"` (accepted, the CV unavailable at
#'   too many levels) or `"5/7 ✗"` (rejected); empty where there is no
#'   calibration or no `acceptance`.
#' @keywords internal
acceptance_text <- function(acceptance, models) {
    if (is.null(acceptance)) return(rep('', length(models)))
    a <- acceptance[match(models, acceptance$model), , drop = FALSE]
    mark <- c(accepted = '✓', accepted_no_cv = '⚠', rejected = '✗')[a$status]
    ifelse(is.na(a$status), '', paste0(a$n.pass, '/', a$n.levels, ' ', mark))
}
