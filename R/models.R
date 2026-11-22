#' Calibration Models
#'
#' Polynomial models (`C` is the concentration, `OD` the background-subtracted
#' optical density):
#'
#' * `LIN`: linear OD = a + b*C;
#' * `QUAD`: quadratic OD = a + b*C + c*C^2;
#' * `LRM`: log-log linear log10(OD) = a + b*log10(C);
#' * `QRM`: log-log quadratic log10(OD) = a + b*log10(C) + c*log10(C)^2.
#'
#' `LRM` and `QRM` are fitted on the original scale (as `LIN` and `QUAD`)
#' when called with `linear = TRUE`.
#'
#' Logistic models on the original OD scale:
#'
#' * `4PL`: four-parameter logistic OD = d + (a - d) / (1 + (C/c)^b);
#' * `5PL`: five-parameter logistic OD = d + (a - d) / (1 + (C/c)^b)^g.
#'
#' In the logistic models `a` is the response at zero concentration, `d` at
#' infinite concentration, `b` the slope factor and `g` the asymmetry. In the
#' 4PL `c` is the concentration at the midpoint of the response, (a + d) / 2
#' (the EC50, and the inflection point on the log scale of the concentration).
#' In the 5PL `c` is a scale parameter: the response at C = c is
#' d + (a - d) / 2^g, the midpoint only when g = 1; the concentration at the
#' midpoint of the response is C50 = c (2^(1/g) - 1)^(1/b), given as a row of
#' its own in the coefficients (see [get_coef_ci()]).
#'
#' @return Character vector of model codes.
#' @export
calibration_models <- function() c('LIN', 'QUAD', 'LRM', 'QRM', '4PL', '5PL')


#' Degree of a Polynomial Model
#'
#' @param model Model code (see [calibration_models()]).
#'
#' @return 1 or 2 for the polynomial models, 0 for the logistic ones.
#' @keywords internal
poly_degree <- function(model) {
    if (model %in% c('LIN', 'LRM')) return(1)
    if (model %in% c('QUAD', 'QRM')) return(2)
    0
}


#' Is a Model Fitted on log10 Scales?
#'
#' @param model Model code (see [calibration_models()]).
#' @param linear For LRM and QRM: fit on untransformed values.
#'
#' @return `TRUE` for the log-log models (`LRM`, `QRM` unless `linear`).
#' @keywords internal
model_log_scale <- function(model, linear = FALSE) model %in% c('LRM', 'QRM') && !isTRUE(linear)


#' Weighting Schemes
#'
#' * `none`: unweighted least squares;
#' * `1/y`, `1/y2`: weights 1/OD and 1/OD^2 (mean OD of the level);
#' * `1/x`, `1/x2`: weights 1/conc and 1/conc^2;
#' * `power`: 1/OD^theta, where theta is estimated from the variances of
#'   replicates on the scale of the residuals of the model (variance function
#'   var = k * mean^theta, the mean on the OD scale): the variance of OD for
#'   the models on the OD scale, of log10(OD) for the log-log models, so that
#'   a constant CV gives theta near 2 on the OD scale and near 0 on log scales.
#'   Equal weights (theta = 0) are used, with a note, when fewer than three
#'   levels have replicates or when their mean signals are indistinguishable
#'   (the slope is then not determined).
#'
#' Weights are normalised to a mean of 1.
#'
#' @return Character vector of weighting codes.
#' @export
weighting_schemes <- function() c('none', '1/y', '1/y2', '1/x', '1/x2', 'power')


#' Translation Key of the Scale of a Model
#'
#' @param model Model code (see [calibration_models()]).
#' @param linear For LRM and QRM: fitted on untransformed values.
#'
#' @return `"scale.log"`, `"scale.linear"` or `"scale.logistic"`.
#' @keywords internal
scale_key <- function(model, linear = FALSE) {
    if (poly_degree(model) == 0) return('scale.logistic')
    if (model_log_scale(model, linear)) 'scale.log' else 'scale.linear'
}


#' Number of Parameters of a Model
#' @keywords internal
n_params <- function(model) c(LIN = 2, QUAD = 3, LRM = 2, QRM = 3, '4PL' = 4, '5PL' = 5)[[model]]


#' Weight Function for a Calibration
#'
#' @param weights Weighting code (see [weighting_schemes()]).
#' @param x,y Concentrations and ODs of the standards used in the fit.
#' @param log_scale Whether the model is fitted on log10(OD) (see
#'   [model_log_scale()]): the scale on which the variances of the replicates
#'   are estimated for `power`.
#'
#' @return A list with `fun(x, y)` returning unnormalised weights, `theta`
#'   (for `power`; within 0-4 on the OD scale and the equivalent -2-2 on log
#'   scales) and `notes`.
#' @keywords internal
weight_function <- function(weights, x, y, log_scale = FALSE) {
    if (!weights %in% weighting_schemes()) lin_abort('error.weights_unknown', weights = weights)
    notes <- list()
    level_mean <- stats::ave(y, x)
    nonzero <- abs(level_mean[!is.na(level_mean) & level_mean != 0])
    floor_y <- if (length(nonzero) > 0) min(nonzero) else 1
    pos <- function(v) pmax(abs(v), floor_y)
    theta <- NA_real_

    if (weights == 'power') {
        groups <- split(y, x)
        # The variance on the scale of the residuals: the weights must be inverse to it. For a fit on
        # log10(OD), var(log10 OD) ~ var(OD) / OD^2, so the same data give theta lower by 2
        stats <- do.call(rbind, lapply(groups, function(g) {
            if (length(g) < 2 || (log_scale && any(g <= 0))) return(NULL)
            data.frame(m = mean(g), v = stats::var(if (log_scale) log10(g) else g))
        }))
        ok <- !is.null(stats) && sum(stats$m > 0 & stats$v > 0) >= 3
        if (ok) {
            stats <- stats[stats$m > 0 & stats$v > 0, ]
            # The slope needs replicated levels with different mean signals: with equal means it is not
            # determined (NA), with means equal up to rounding it is huge and meaningless
            log_m <- log(stats$m)
            spread <- diff(range(log_m)) > 1e-8
            theta <- if (spread) unname(coef(lm(log(stats$v) ~ log_m))[2]) else NA_real_
            if (is.finite(theta)) {
                bounds <- if (log_scale) c(-2, 2) else c(0, 4)
                theta <- min(max(theta, bounds[1]), bounds[2])
            } else {
                theta <- 0
                notes <- list(lin_message('note.power_flat_means'))
            }
        } else {
            theta <- 0
            notes <- list(lin_message('note.power_fallback'))
        }
    }

    fun <- switch(weights,
                  'none' = function(x, y) rep(1, length(x)),
                  '1/y' = function(x, y) 1 / pos(y),
                  '1/y2' = function(x, y) 1 / pos(y)^2,
                  '1/x' = function(x, y) 1 / x,
                  '1/x2' = function(x, y) 1 / x^2,
                  'power' = function(x, y) 1 / pos(y)^theta)
    list(fun = fun, theta = theta, notes = notes, level_mean = level_mean)
}


#' Response Function of a Fitted Model
#'
#' @param fit A `lin_fit` object.
#' @param x Concentrations.
#' @param par Internal parameter vector (defaults to the estimates).
#'
#' @return Predicted OD on the original scale.
#' @keywords internal
curve_fun <- function(fit, x, par = fit$par) {
    if (poly_degree(fit$model) > 0) {
        u <- if (fit$log_scale) log10(x) else x
        quad <- if (poly_degree(fit$model) == 2) par[['c']] else 0
        v <- par[['a']] + par[['b']] * u + quad * u^2
        return(if (fit$log_scale) 10^v else v)
    }
    switch(fit$model,
           '4PL' = par[['d']] + (par[['a']] - par[['d']]) /
               (1 + exp(par[['b']] * (log(x) - par[['lc']]))),
           '5PL' = par[['d']] + (par[['a']] - par[['d']]) /
               (1 + exp(par[['b']] * (log(x) - par[['lc']])))^par[['g']])
}


#' Working Interval of a Fit
#'
#' The interval of concentrations used for quantification: the monotonic
#' interval set by the user (`fit$interval`), otherwise the range of the
#' standards of the fit.
#'
#' @param fit A `lin_fit` object.
#' @param scale If `TRUE`, on the scale of the fit (log10 for log-log models).
#'
#' @return Numeric vector of length 2.
#' @keywords internal
working_interval <- function(fit, scale = FALSE) {
    x_lim <- if (is.null(fit$interval)) fit$conc_range else fit$interval
    if (scale && fit$log_scale) log10(x_lim) else x_lim
}


#' Vertex of a Quadratic Fit
#'
#' @param fit A `lin_fit` object.
#' @param par Internal parameter vector.
#'
#' @return A list with `u` (vertex on the scale of the fit), `conc` (its
#'   concentration) and `side`: `"inside"` if it lies strictly inside the
#'   working interval (see [working_interval()]), `"below"` or `"above"` the
#'   interval; `NULL` for models other than the quadratic ones and for a zero
#'   quadratic coefficient (a straight line).
#' @keywords internal
quad_vertex <- function(fit, par = fit$par) {
    if (poly_degree(fit$model) != 2 || par[['c']] == 0) return(NULL)
    x_lim <- working_interval(fit, scale = TRUE)
    u <- -par[['b']] / (2 * par[['c']])
    # The vertex on a bound (up to rounding) leaves the interval on one branch
    tol <- 1e-6 * diff(x_lim)
    side <- if (u <= x_lim[1] + tol) 'below' else if (u >= x_lim[2] - tol) 'above' else 'inside'
    list(u = u, conc = if (fit$log_scale) 10^u else u, side = side)
}


#' Concentration for a Given OD (Inverse of the Curve)
#'
#' A quadratic curve is inverted on the branch of the parabola that holds the
#' working interval (see [working_interval()]). If the vertex lies strictly
#' inside the interval, an OD can match two concentrations: there is no
#' solution (no root is chosen). A zero quadratic coefficient gives a straight
#' line. ODs outside the range of the logistic asymptotes have no solution.
#'
#' @param fit A `lin_fit` object.
#' @param od Background-subtracted OD.
#' @param par Internal parameter vector.
#'
#' @return Concentrations (`NA` where there is no solution or OD <= 0 on log scales).
#' @keywords internal
inverse_fun <- function(fit, od, par = fit$par) {
    out <- rep(NA_real_, length(od))
    if (poly_degree(fit$model) > 0) {
        ok <- !is.na(od) & (!fit$log_scale | od > 0)
        y <- if (fit$log_scale) log10(od[ok]) else od[ok]
        # q*u^2 + b*u + a = y with the quadratic coefficient q
        q <- if (poly_degree(fit$model) == 2) par[['c']] else 0
        vertex <- quad_vertex(fit, par)
        if (q == 0) {
            u <- (y - par[['a']]) / par[['b']]
        } else if (vertex$side == 'inside') {
            u <- rep(NA_real_, length(y))
        } else {
            # Both roots in the stable form (no cancellation when q is close to 0, where the
            # root of the branch tends to the linear solution); the branch of the interval is
            # the larger root when the interval lies above the vertex
            b <- par[['b']]
            disc <- b^2 - 4 * q * (par[['a']] - y)
            half <- -(b + (if (b >= 0) 1 else -1) * sqrt(pmax(disc, 0))) / 2
            r1 <- half / q
            r2 <- ifelse(half == 0, vertex$u, (par[['a']] - y) / half)
            u <- if (vertex$side == 'below') pmax(r1, r2) else pmin(r1, r2)
            u[disc < 0] <- NA_real_
        }
        out[ok] <- if (fit$log_scale) 10^u else u
        # A flat line (zero slope) has no inverse
        out[!is.finite(out)] <- NA_real_
        return(out)
    }

    # Only an OD strictly between the asymptotes is reached by the curve; checked before the power
    # of the 5PL, which turns a negative ratio positive when 1/g is an even integer
    lo <- min(par[['a']], par[['d']])
    hi <- max(par[['a']], par[['d']])
    ok <- !is.na(od) & od > lo & od < hi
    ratio <- (par[['a']] - par[['d']]) / (od[ok] - par[['d']])
    if (fit$model == '5PL') ratio <- ratio^(1 / par[['g']])
    inner <- ratio - 1
    val <- rep(NA_real_, length(inner))
    good <- !is.na(inner) & is.finite(inner) & inner > 0
    val[good] <- exp(par[['lc']] + log(inner[good]) / par[['b']])
    # exp() can overflow or underflow near the asymptotes
    val[!is.finite(val) | val <= 0] <- NA_real_
    out[ok] <- val
    out
}


#' Why an OD Has No Back-Calculated Concentration
#'
#' Codes (translated with the prefix `backcalc.`):
#'
#' * `missing`: no OD;
#' * `od_le_blank`: OD <= 0 on log scales (no logarithm);
#' * `flat`: the line is horizontal;
#' * `beyond_vertex`: the OD lies beyond the vertex of the parabola;
#' * `non_monotonic`: the parabola turns inside the working interval, so the
#'   concentration is ambiguous (see [quad_vertex()]);
#' * `outside_asymptotes`: the OD lies outside the asymptotes of the logistic curve;
#' * `no_solution`: any other failure.
#'
#' @param fit A `lin_fit` object.
#' @param od Background-subtracted ODs.
#'
#' @return Character vector: a code where [inverse_fun()] gives `NA`, `NA`
#'   where it gives a concentration.
#' @keywords internal
inverse_reason <- function(fit, od) {
    calc <- inverse_fun(fit, od)
    out <- ifelse(is.na(calc), 'no_solution', NA_character_)
    par <- fit$par
    degree <- poly_degree(fit$model)
    if (degree > 0) {
        q <- if (degree == 2) par[['c']] else 0
        if (q == 0) {
            if (par[['b']] == 0) out[is.na(calc)] <- 'flat'
        } else if (quad_vertex(fit)$side == 'inside') {
            out[is.na(calc)] <- 'non_monotonic'
        } else {
            y <- suppressWarnings(fit_scale(fit, od))
            disc <- par[['b']]^2 - 4 * q * (par[['a']] - y)
            out[is.na(calc) & !is.na(disc) & disc < 0] <- 'beyond_vertex'
        }
    } else {
        lo <- min(par[['a']], par[['d']])
        hi <- max(par[['a']], par[['d']])
        out[is.na(calc) & !is.na(od) & (od <= lo | od >= hi)] <- 'outside_asymptotes'
    }
    if (fit$log_scale) out[is.na(calc) & !is.na(od) & od <= 0] <- 'od_le_blank'
    out[is.na(od)] <- 'missing'
    out
}


#' Starting Values for the Logistic Models
#'
#' @param x,y Concentrations and ODs.
#' @param extend How far beyond the observed range the asymptotes start,
#'   as a fraction of the OD span.
#'
#' @return Named vector `a`, `b`, `lc`, `d`.
#' @keywords internal
start_4pl <- function(x, y, extend = 0.1) {
    means <- stats::aggregate(y, list(lx = log(x)), mean)
    means <- means[order(means$lx), ]
    y_lo <- means$x[1]
    y_hi <- means$x[nrow(means)]
    span <- y_hi - y_lo
    a0 <- y_lo - extend * span
    d0 <- y_hi + extend * span
    z <- suppressWarnings(log((a0 - means$x) / (means$x - d0)))
    ok <- is.finite(z)
    if (sum(ok) >= 2) {
        cf <- coef(lm(z[ok] ~ means$lx[ok]))
        b0 <- unname(cf[2])
        lc0 <- if (abs(b0) > 1e-8) unname(-cf[1] / b0) else mean(means$lx)
    } else {
        b0 <- sign(span)
        lc0 <- mean(means$lx)
    }
    if (!is.finite(b0) || abs(b0) < 1e-3) b0 <- sign(span) + (span == 0)
    c(a = a0, b = b0, lc = lc0, d = d0)
}


#' Control of the Levenberg-Marquardt Fits
#'
#' The limit on the calls to the residual function is set from the limit on
#' the iterations (an iteration of `nls.lm` without an analytic Jacobian
#' takes at least one call per parameter and one more), so that the
#' iterations are the limit that applies; the default of `nls.lm` (100 calls
#' per parameter) stops a slow fit after about a hundred iterations. `nls.lm`
#' does not iterate more than 1024 times.
#'
#' @param n_par Number of parameters.
#'
#' @return An `nls.lm.control` list.
#' @keywords internal
logistic_control <- function(n_par) {
    maxiter <- 1000
    minpack.lm::nls.lm.control(maxiter = maxiter, maxfev = 2 * maxiter * (n_par + 1))
}


#' Has a Levenberg-Marquardt Fit Converged?
#'
#' @param info The stop code of `minpack.lm::nls.lm`.
#'
#' @return `TRUE` for the convergence tests (1-4: relative reduction of the
#'   sum of squares, relative change of the parameters, orthogonality of the
#'   residuals to the Jacobian) and for a solution that cannot be improved at
#'   machine precision (6-8); `FALSE` for improper input (0) and for the limits
#'   on the calls (5) and on the iterations (-1 and 9).
#' @keywords internal
lm_converged <- function(info) {
    isTRUE(info %in% c(1:4, 6:8))
}


#' Fit a Logistic Model with Several Starting Points
#'
#' Minimises the weighted residual sum of squares with the Levenberg-Marquardt
#' algorithm (`minpack.lm::nls.lm`) from several starting points and returns
#' the best converged solution (see [lm_converged()]): a start stopped by a
#' limit is not compared, even when its estimates are finite. `nls.lm` is
#' used directly (not `nlsLM`), because building an `nls` object fails when
#' the parameters are not identifiable (e.g. the upper asymptote is not
#' reached), although the curve itself is well determined.
#'
#' @param model `"4PL"` or `"5PL"`.
#' @param x,y,w Concentrations, ODs and weights.
#'
#' @return A list with `status` `"ok"`, `par` (named internal parameters) and
#'   `rss` (weighted residual sum of squares); `list(status =
#'   "not_converged")` if some start gave finite estimates but none
#'   converged; `NULL` if no start gave finite estimates.
#' @keywords internal
fit_logistic <- function(model, x, y, w) {
    shape <- list(model = model, log_scale = FALSE)

    try_fit <- function(start, lower = NULL) {
        # A start stopped by a limit warns; it is told by its stop code instead
        res <- tryCatch(
            suppressWarnings(
                minpack.lm::nls.lm(par = start, lower = lower, control = logistic_control(length(start)),
                                   fn = function(p) sqrt(w) * (y - curve_fun(shape, x, p)))),
            error = function(e) NULL)
        if (is.null(res)) return(NULL)
        par <- unlist(res$par)
        names(par) <- names(start)
        rss <- sum(res$fvec^2)
        if (!all(is.finite(par)) || !is.finite(rss)) return(NULL)
        list(par = par, rss = rss, converged = lm_converged(res$info))
    }
    best_of <- function(fits) {
        fits <- Filter(Negate(is.null), fits)
        if (length(fits) == 0) return(NULL)
        fits[[which.min(vapply(fits, function(f) f$rss, 0))]]
    }
    result <- function(fits) {
        fits <- Filter(Negate(is.null), fits)
        if (length(fits) == 0) return(NULL)
        best <- best_of(Filter(function(f) f$converged, fits))
        if (is.null(best)) return(list(status = 'not_converged'))
        list(status = 'ok', par = best$par, rss = best$rss)
    }

    fits4 <- lapply(c(0.1, 0.5, 1, 3), function(e) try_fit(start_4pl(x, y, e)))
    if (model == '4PL') return(result(fits4))

    # The best 4PL solution is only a start of the 5PL fit: converged if possible
    start4 <- best_of(Filter(function(f) f$converged, fits4))
    if (is.null(start4)) start4 <- best_of(fits4)
    lower <- c(a = -Inf, b = -Inf, lc = -Inf, d = -Inf, g = 0.05)
    starts <- list(c(start_4pl(x, y, 0.1), g = 1))
    if (!is.null(start4)) starts <- c(list(c(start4$par, g = 1)), starts)
    result(lapply(starts, function(s) try_fit(s, lower)))
}


#' Covariance Matrix of Logistic Parameters
#'
#' sigma^2 (J' W J)^-1 with the Jacobian J of the curve computed numerically.
#' Parameters are considered not identifiable from the standards when the
#' condition number of J' W J exceeds 1e10 (typically the upper asymptote is
#' not reached; the standard error of log c is then > 10). If the matrix
#' cannot be inverted at all, a pseudo-inverse is used (`exact = FALSE`) so
#' that concentrations still get confidence intervals.
#'
#' @param model `"4PL"` or `"5PL"`.
#' @param par Internal parameters.
#' @param x,w Concentrations and weights.
#' @param sigma Residual standard error.
#'
#' @return A list with `vcov`, `identifiable` and `exact`.
#' @keywords internal
logistic_vcov <- function(model, par, x, w, sigma) {
    shape <- list(model = model, log_scale = FALSE)
    jac <- vapply(seq_along(par), function(i) {
        h <- 1e-6 * max(1, abs(par[[i]]))
        up <- par
        dn <- par
        up[i] <- par[i] + h
        dn[i] <- par[i] - h
        (curve_fun(shape, x, up) - curve_fun(shape, x, dn)) / (2 * h)
    }, numeric(length(x)))
    jac <- matrix(jac, nrow = length(x))
    info <- crossprod(jac * sqrt(w))
    inv <- tryCatch(solve(info), error = function(e) NULL)
    exact <- !is.null(inv) && all(is.finite(inv)) && all(diag(inv) > 0)
    identifiable <- exact && kappa(info, exact = TRUE) < 1e10
    if (!exact) {
        s <- svd(info)
        keep <- s$d > max(s$d) * 1e-12
        inv <- s$v[, keep, drop = FALSE] %*% (t(s$u[, keep, drop = FALSE]) / s$d[keep])
    }
    vc <- sigma^2 * inv
    dimnames(vc) <- list(names(par), names(par))
    list(vcov = vc, identifiable = identifiable, exact = exact)
}


#' Fit a Calibration Curve
#'
#' @param standard_data Background-subtracted standards with `conc` and `OD`.
#' @param model Model code (see [calibration_models()]).
#' @param linear For LRM and QRM: fit on untransformed values instead of
#'   log10 scales (as LIN and QUAD).
#' @param weights Weighting code (see [weighting_schemes()]).
#'
#' @return A `lin_fit` object: list with `model`, `log_scale`, `weights`,
#'   `par` (internal parameters; for the polynomial models `a` is the
#'   intercept, `b` the linear and `c` the quadratic coefficient), `vcov`,
#'   `df`, `sigma`, `rss` (weighted residual sum of squares), `loglik`, `x`,
#'   `y`, `w`, `keys` (well IDs from `.key`, `NULL` without them),
#'   `interval` (monotonic interval for the inverse of a quadratic curve,
#'   `NULL` until [calculate()] sets it), `conc_range`, `weight_fun`, `w_scale`, `theta`, `notes` and, for the
#'   polynomial models, the underlying `lm` object in `object`. Stops with
#'   `error.coef_not_estimable` when a coefficient of a polynomial model
#'   cannot be estimated (rank-deficient design, as over a range of
#'   concentrations too narrow to separate the linear and quadratic terms).
#' @export
fit_curve <- function(standard_data, model = 'LRM', linear = FALSE, weights = 'none') {
    x <- standard_data$conc
    y <- standard_data$OD
    log_scale <- model_log_scale(model, linear)
    degree <- poly_degree(model)

    wf <- weight_function(weights, x, y, log_scale)
    w_raw <- wf$fun(x, wf$level_mean)
    w_scale <- mean(w_raw)
    w <- w_raw / w_scale

    if (degree > 0) {
        df <- data.frame(u = if (log_scale) log10(x) else x,
                         v = if (log_scale) log10(y) else y,
                         w = w)
        formula <- if (degree == 1) v ~ u else v ~ u + I(u^2)
        object <- lm(formula, data = df, weights = w)
        # lm() drops a term it cannot separate from the others (NA coefficient, as over a
        # narrow range of concentrations) instead of stopping
        if (object$rank < degree + 1 || !all(is.finite(coef(object)))) {
            lin_abort('error.coef_not_estimable', model = model)
        }
        # Intercept a, linear b and quadratic c, as in the equation of the model
        par <- stats::setNames(unname(coef(object)), c('a', 'b', 'c')[seq_len(degree + 1)])
        # summary.lm warns about essentially perfect fits (synthetic data); the matrix is still valid
        vc <- suppressWarnings(stats::vcov(object))
        dimnames(vc) <- list(names(par), names(par))
        df_res <- stats::df.residual(object)
        sigma <- stats::sigma(object)
        rss <- stats::deviance(object)
        loglik <- as.numeric(stats::logLik(object))
        vcov_exact <- TRUE
        notes <- wf$notes
    } else {
        best <- fit_logistic(model, x, y, w)
        if (is.null(best)) lin_abort('error.fit_failed', model = model)
        if (best$status == 'not_converged') lin_abort('error.fit_not_converged', model = model)
        object <- NULL
        par <- best$par
        rss <- best$rss
        n <- length(y)
        df_res <- n - length(par)
        sigma <- sqrt(rss / max(df_res, 1))
        # Gaussian log-likelihood of a weighted least-squares fit (as logLik.nls)
        loglik <- 0.5 * (sum(log(w)) - n * (log(2 * pi) + 1 - log(n) + log(rss)))
        cov_info <- logistic_vcov(model, par, x, w, sigma)
        vc <- cov_info$vcov
        vcov_exact <- cov_info$exact
        notes <- wf$notes
        if (!cov_info$identifiable) {
            notes <- c(notes, list(lin_message('note.par_unidentifiable', model = model)))
        }
    }

    fit <- structure(list(model = model, log_scale = log_scale, weights = weights,
                          par = par, vcov = vc, vcov_exact = vcov_exact, df = df_res,
                          sigma = sigma, rss = rss,
                          loglik = loglik, x = x, y = y, w = w,
                          keys = if ('.key' %in% names(standard_data)) as.character(standard_data$.key),
                          conc_range = range(x), weight_fun = wf$fun, w_scale = w_scale,
                          theta = wf$theta, notes = notes, object = object),
                     class = 'lin_fit')
    fit
}


#' Transform OD to the Scale of the Fit
#' @keywords internal
fit_scale <- function(fit, od) if (fit$log_scale) log10(od) else od


#' Residuals of the Fit
#'
#' @param fit A `lin_fit` object.
#'
#' @return A data.frame with `fitted` (OD), `resid` (scale of the fit) and
#'   `wresid` (weighted residuals).
#' @keywords internal
fit_residuals <- function(fit) {
    fitted <- curve_fun(fit, fit$x)
    resid <- fit_scale(fit, fit$y) - fit_scale(fit, fitted)
    data.frame(conc = fit$x, fitted = fitted, resid = resid, wresid = sqrt(fit$w) * resid)
}


#' Numerical Gradient
#' @keywords internal
num_grad <- function(f, p) {
    vapply(seq_along(p), function(i) {
        h <- 1e-6 * max(1, abs(p[i]))
        up <- p
        dn <- p
        up[i] <- p[i] + h
        dn[i] <- p[i] - h
        (f(up) - f(dn)) / (2 * h)
    }, numeric(1))
}


#' Inverse Prediction with Confidence Intervals
#'
#' The standard error of log(concentration) is obtained with the delta method
#' from the uncertainty of the curve parameters and of a single OD
#' measurement (residual variance divided by the weight of that OD).
#'
#' @param fit A `lin_fit` object.
#' @param od Background-subtracted ODs.
#' @param level Confidence level.
#'
#' @return A data.frame with `conc`, `lower`, `upper` and `cv` (%, from the
#'   standard error of log concentration).
#' @export
inverse_predict_ci <- function(fit, od, level = 0.95) {
    conc <- inverse_fun(fit, od)
    out <- data.frame(conc = conc, lower = NA_real_, upper = NA_real_, cv = NA_real_)
    t_q <- stats::qt(1 - (1 - level) / 2, max(fit$df, 1))
    for (i in which(!is.na(conc) & conc > 0)) {
        g_par <- num_grad(function(p) log(inverse_fun(fit, od[i], p)), fit$par)
        y_fit <- fit_scale(fit, od[i])
        g_y <- num_grad(function(v) {
            o <- if (fit$log_scale) 10^v else v
            log(inverse_fun(fit, o))
        }, y_fit)
        w_new <- fit$weight_fun(conc[i], od[i]) / fit$w_scale
        var_log <- drop(t(g_par) %*% fit$vcov %*% g_par) + g_y^2 * fit$sigma^2 / w_new
        if (!is.finite(var_log) || var_log < 0) next
        se <- sqrt(var_log)
        out$lower[i] <- conc[i] * exp(-t_q * se)
        out$upper[i] <- conc[i] * exp(t_q * se)
        out$cv[i] <- 100 * sqrt(exp(var_log) - 1)
    }
    out
}


#' Concentration at the Midpoint of the Response of a 5PL
#'
#' C50 = c (2^(1/g) - 1)^(1/b), where the 5PL reads (a + d) / 2; its
#' parameter `c` is a scale parameter, the midpoint only when g = 1.
#'
#' @param par Internal parameters of a 5PL (`a`, `b`, `lc`, `d`, `g`).
#'
#' @return log(C50), `NA` when it is not defined.
#' @keywords internal
log_c50 <- function(par) {
    v <- par[['lc']] + log(2^(1 / par[['g']]) - 1) / par[['b']]
    if (is.finite(v)) v else NA_real_
}


#' Coefficients and Confidence Intervals of a Fit
#'
#' The parameters of the model (for the logistic models `c` rather than its
#' logarithm) and, for a 5PL, the concentration at the midpoint of the response
#' `C50` (see [log_c50()]), whose interval comes from the delta method on
#' log(C50).
#'
#' @param fit A `lin_fit` object.
#' @param level Confidence level.
#'
#' @return A data.frame with `coef`, `est`, `ci.lower`, `ci.upper`.
#' @keywords internal
coef_table_fit <- function(fit, level = 0.95) {
    se <- sqrt(pmax(diag(fit$vcov), 0))
    t_q <- stats::qt(1 - (1 - level) / 2, max(fit$df, 1))
    est <- fit$par
    lower <- est - t_q * se
    upper <- est + t_q * se
    if (isFALSE(fit$vcov_exact)) {
        # pseudo-inverse: the intervals of the parameters would be misleadingly narrow
        lower[] <- NA_real_
        upper[] <- NA_real_
    }
    if ('lc' %in% names(est)) {
        i <- which(names(est) == 'lc')
        est[i] <- exp(est[i])
        lower[i] <- exp(lower[i])
        upper[i] <- exp(upper[i])
        names(est)[i] <- 'c'
    }
    tab <- data.frame(coef = names(est), est = unname(est),
                      ci.lower = unname(lower), ci.upper = unname(upper))
    tab <- tab[order(tab$coef), ]
    if (identical(fit$model, '5PL')) {
        # A derived quantity after the parameters: the midpoint of the response, which c is not
        l50 <- log_c50(fit$par)
        ci <- c(NA_real_, NA_real_)
        if (!is.na(l50) && !isFALSE(fit$vcov_exact)) {
            g <- num_grad(log_c50, fit$par)
            var_l50 <- drop(t(g) %*% fit$vcov %*% g)
            if (is.finite(var_l50) && var_l50 >= 0) ci <- exp(l50 + c(-1, 1) * t_q * sqrt(var_l50))
            # A bound of 0 or infinity (exp() out of range) says only that C50 is not determined
            ci[!is.finite(ci) | ci <= 0] <- NA_real_
        }
        tab <- rbind(tab, data.frame(coef = 'C50', est = exp(l50), ci.lower = ci[1], ci.upper = ci[2]))
    }
    tab
}


#' Lack-of-Fit F Test
#'
#' Compares the residual variance with the variance of replicates (pure error).
#' The test is unavailable without replicates, with no more levels than
#' parameters of the model, and when the replicates of every level read the
#' same OD (as after importing rounded OD): the pure error is then zero, up to
#' rounding errors relative to the total scatter of the OD.
#'
#' @param fit A `lin_fit` object.
#'
#' @return A list with `F`, `df1`, `df2`, `p` and `reason`: `NA` when the
#'   test is done, otherwise why `p` is `NA`: `"no_replicates"`,
#'   `"too_few_levels"` or `"zero_pure_error"`.
#' @keywords internal
lack_of_fit <- function(fit) {
    v <- fit_scale(fit, fit$y)
    r <- fit_residuals(fit)$resid
    rss <- sum(fit$w * r^2)
    groups <- split(seq_along(v), fit$x)
    ss_pe <- sum(vapply(groups, function(i) {
        m <- stats::weighted.mean(v[i], fit$w[i])
        sum(fit$w[i] * (v[i] - m)^2)
    }, 0))
    ss_total <- sum(fit$w * (v - stats::weighted.mean(v, fit$w))^2)
    m <- length(groups)
    n <- length(v)
    p <- length(fit$par)
    df1 <- m - p
    df2 <- n - m
    reason <- if (df2 <= 0) {
        'no_replicates'
    } else if (df1 <= 0) {
        'too_few_levels'
    } else if (ss_pe <= 1e-12 * ss_total) {
        # Identical replicates leave only rounding errors, which would make F huge
        'zero_pure_error'
    } else {
        NA_character_
    }
    if (!is.na(reason)) return(list(F = NA_real_, df1 = df1, df2 = df2, p = NA_real_, reason = reason))
    f_stat <- ((rss - ss_pe) / df1) / (ss_pe / df2)
    list(F = f_stat, df1 = df1, df2 = df2, p = stats::pf(f_stat, df1, df2, lower.tail = FALSE),
         reason = NA_character_)
}


#' Back-Calculated Concentrations of the Standards
#'
#' @param fit A `lin_fit` object.
#'
#' @return A data.frame with `conc` (nominal), `OD`, `calc`, `recovery` (%),
#'   `key` (well ID, `NA` if the fit has none) and `reason` (why `calc` is
#'   missing, see [inverse_reason()]; `NA` when it is not).
#' @export
back_calculate <- function(fit) {
    calc <- inverse_fun(fit, fit$y)
    keys <- if (is.null(fit$keys)) rep(NA_character_, length(fit$x)) else fit$keys
    data.frame(conc = fit$x, OD = fit$y, calc = calc, recovery = 100 * calc / fit$x,
               key = keys, reason = inverse_reason(fit, fit$y))
}


#' Information Criteria on the OD Scale
#'
#' For models fitted on log10(OD) the log-likelihood is converted to the OD
#' scale (Jacobian of the transformation), so that AIC and BIC are comparable
#' between log-log and logistic models.
#'
#' @param fit A `lin_fit` object.
#'
#' @return Named vector `AIC`, `BIC`.
#' @keywords internal
information_criteria <- function(fit) {
    ll <- fit$loglik
    k <- length(fit$par) + 1
    if (fit$log_scale) ll <- ll - sum(log(fit$y * log(10)))
    n <- length(fit$y)
    c(AIC = -2 * ll + 2 * k, BIC = -2 * ll + log(n) * k)
}


#' Compare Calibration Models
#'
#' Fits all models with the same weighting to the common set of calibration
#' wells and reports the number of parameters, AIC and BIC on the OD scale,
#' R-squared (OD scale), the mean and maximum absolute error of
#' back-calculated standards (%) and the lack-of-fit p-value. The errors use
#' the standards with a back-calculated concentration; `n.failed` counts the
#' others (failed inversions).
#'
#' Each model uses every well of `standard_data` it can use: the log-log
#' models leave out wells with OD <= 0 (no logarithm), the others use all of
#' them. The wells used by each model are kept; AIC and BIC are comparable
#' only between fits on the same wells (see [same_wells()]), so the model
#' with the lowest AIC is marked only then.
#'
#' @param standard_data Common set of calibration wells: background-subtracted
#'   standards after the exclusions by the user, before the restrictions of
#'   the models (`calibration_pool` of [calculate()]); with `.key` (well IDs),
#'   otherwise the rows are numbered. Wells with a missing OD are not counted.
#' @param linear,weights See [fit_curve()].
#'
#' @return A data.frame with one row per model; models that cannot be fitted
#'   have `NA` statistics. Besides the statistics: `n.used` (wells in the
#'   fit), `n.failed` (wells of the fit without a back-calculated
#'   concentration), `n.available` (wells of the common set), `status`
#'   (`"ok"`, `"too_few_levels"`, `"not_converged"` (no start of the
#'   optimiser converged, see [fit_logistic()]), `"not_estimable"` (a
#'   polynomial whose terms cannot be separated on these concentrations, see
#'   [fit_curve()]) or `"fit_failed"`), `n.levels` (standard
#'   levels the model can use) and `n.levels.required` (the number of
#'   parameters plus one, as for the calibration), the list columns `wells`
#'   (IDs of the wells used; `NULL` if not fitted) and `dropped` (IDs left
#'   out by the restrictions of the model) and `lowest.aic`.
#' @export
compare_models <- function(standard_data, linear = FALSE, weights = 'none') {
    pool <- standard_data[!is.na(standard_data$OD), , drop = FALSE]
    keys <- if ('.key' %in% names(pool)) as.character(pool$.key) else as.character(seq_len(nrow(pool)))
    rows <- lapply(calibration_models(), function(m) {
        usable <- if (model_log_scale(m, linear)) pool$OD > 0 else rep(TRUE, nrow(pool))
        std <- pool[usable, , drop = FALSE]
        # The same requirement as for the calibration (see calculate())
        n_levels <- length(unique(std$conc))
        stats <- data.frame(model = m, n.par = n_params(m), n.used = NA_integer_, n.failed = NA_integer_,
                            n.available = nrow(pool), status = 'too_few_levels',
                            n.levels = n_levels, n.levels.required = n_params(m) + 1,
                            AIC = NA_real_, BIC = NA_real_, R2 = NA_real_, mean.abs.error = NA_real_,
                            max.abs.error = NA_real_, lof.p = NA_real_)
        row <- list(stats = stats, wells = NULL, dropped = keys[!usable])
        if (n_levels < n_params(m) + 1) return(row)
        fit <- tryCatch(fit_curve(std, m, linear, weights), error = function(e) e)
        if (inherits(fit, 'error')) {
            # No statistics: a fit stopped by a limit has no AIC to compare
            row$stats$status <- switch(if (is.null(fit$key)) '' else fit$key,
                                       error.fit_not_converged = 'not_converged',
                                       error.coef_not_estimable = 'not_estimable',
                                       'fit_failed')
            return(row)
        }
        stats$status <- 'ok'
        ic <- information_criteria(fit)
        res <- fit$y - curve_fun(fit, fit$x)
        bc <- back_calculate(fit)
        err <- abs(bc$recovery - 100)
        stats$n.used <- nrow(std)
        stats$n.failed <- sum(is.na(bc$calc))
        stats$AIC <- ic[['AIC']]
        stats$BIC <- ic[['BIC']]
        stats$R2 <- 1 - sum(res^2) / sum((fit$y - mean(fit$y))^2)
        stats$mean.abs.error <- if (all(is.na(err))) NA_real_ else mean(err, na.rm = TRUE)
        stats$max.abs.error <- if (all(is.na(err))) NA_real_ else max(err, na.rm = TRUE)
        stats$lof.p <- lack_of_fit(fit)$p
        row$stats <- stats
        row$wells <- keys[usable]
        row
    })
    out <- do.call(rbind, lapply(rows, `[[`, 'stats'))
    out$wells <- lapply(rows, `[[`, 'wells')
    out$dropped <- lapply(rows, `[[`, 'dropped')
    best <- if (all(is.na(out$AIC))) NA_real_ else min(out$AIC, na.rm = TRUE)
    out$lowest.aic <- same_wells(out) & !is.na(out$AIC) & out$AIC == best
    out
}


#' Do the Compared Models Use the Same Wells?
#'
#' The sets of wells are compared, not only their sizes.
#'
#' @param cmp Output of [compare_models()].
#'
#' @return `TRUE` if all fitted models use the same wells.
#' @keywords internal
same_wells <- function(cmp) {
    sets <- lapply(Filter(Negate(is.null), cmp$wells), sort)
    length(sets) < 2 || all(vapply(sets[-1], identical, logical(1), sets[[1]]))
}
