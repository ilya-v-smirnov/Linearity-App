# A plate of seven duplicated levels from 100 (S1, column 7 and 8 of rows A-G): OD = 0.01 * conc over a
# blank of 0.05; `f` multiplies the ODs of the two wells of selected levels, by their number
verdict_plate <- function(f = list(), missing = character(0)) {
    conc <- 100 / 2^(0:6)
    od <- 0.01 * conc
    od1 <- od
    od2 <- od
    for (n in names(f)) {
        i <- as.integer(n)
        od1[i] <- od[i] * f[[n]][1]
        od2[i] <- od[i] * f[[n]][2]
    }
    wells <- c(paste0(LETTERS[1:7], 7), paste0(LETTERS[1:7], 8))
    std <- data.frame(type = 'standard', sample.name = NA, dilution = NA, conc = c(conc, conc),
                      OD = c(od1, od2) + 0.05, well = wells)
    std$OD[std$well %in% missing] <- NA
    rows <- rbind(std,
                  data.frame(type = 'blank', sample.name = NA, dilution = NA, conc = NA, OD = c(0.05, 0.05),
                             well = c('H7', 'H8')),
                  data.frame(type = 'sample', sample.name = 'A', dilution = 1, conc = NA, OD = 0.3, well = 'A1'))
    check_assay(normalize_table(rows))
}

# S3 (25) with a CV of 20.5 %, S4 (12.5) with a bias
cv_plate <- function() verdict_plate(list('3' = c(1.145, 0.855), '4' = c(1.25, 1.063)))

verdict_html <- function(r, lang = 'en') as.character(LinearityApp:::verdict_ui(r, lang))

test_that('the verdict names the criterion failed by a level inside the range and the wells of its fit', {
    r <- calculate(cv_plate(), 'LRM')
    html <- verdict_html(r)
    expect_match(html, 'Calibration accepted · 6 of 7 levels passed', fixed = TRUE)
    expect_match(html, '<p>Accepted calibration range: 1.56–100.</p>', fixed = TRUE)
    expect_match(html, paste('S3 (25) failed precision. CV of the back-calculated concentrations was 20.5%',
                             '(limit 20%). Both wells, C7 and C8, were included in the fit.'), fixed = TRUE)
    # the level that passed is not listed
    expect_false(grepl('S4 (12.5)', html, fixed = TRUE))
    ru <- verdict_html(r, 'ru')
    expect_match(ru, 'Калибровка принята · принято уровней: 6 из 7', fixed = TRUE)
    expect_match(ru, 'S3 (25) не прошёл по прецизионности. CV пересчитанных концентраций 20.5 % (предел 20 %).',
                 fixed = TRUE)
    expect_match(ru, 'Обе лунки, C7 и C8, вошли в регрессию.', fixed = TRUE)
})

test_that('a level accepted by its bias alone after a documented exclusion is named where its CV is unavailable', {
    r <- calculate(cv_plate(), 'LRM', exclude = data.frame(key = 'D7', reason = 'Pipetting error'))
    expect_equal(r$quality$calibration$status, 'accepted_no_cv')
    html <- verdict_html(r)
    expect_match(html, 'alert-warning', fixed = TRUE)
    expect_match(html, 'Calibration accepted · CV unavailable at S4', fixed = TRUE)
    expect_match(html, paste('Accepted calibration range: 1.56–100. 6 of 7 levels passed (at least 6 required);',
                             'the CV was measured at 5 of them.'), fixed = TRUE)
    expect_match(html, paste('S4 (12.5) passed on bias alone under the documented-exclusion rule.',
                             'Excluded: D7 (Pipetting error). D8 is the only remaining result (bias +'),
                 fixed = TRUE)
    # the level of the title first, then the levels not accepted
    levels <- regmatches(html, gregexpr('<summary>S[0-9]', html))[[1]]
    expect_equal(sub('<summary>', '', levels), c('S4', 'S3'))
    # the excluded well is listed with its reason under the level
    expect_match(html, '<td>excluded — Pipetting error</td>', fixed = TRUE)
})

test_that('a level failing both criteria shows both, and the levels outside the range say where it ends', {
    html <- verdict_html(calculate(verdict_plate(list('4' = c(1.65, 1.15))), 'LRM'))
    expect_match(html, paste('S4 (12.5) failed accuracy and precision. Mean bias was +33.7% (limit ±20%);',
                             'CV was 25.3% (limit 20%). Both wells, D7 and D8, were included in the fit.'),
                 fixed = TRUE)
    html <- verdict_html(calculate(verdict_plate(list('7' = c(1.6, 1.65))), 'LRM'))
    expect_match(html, 'S7 (1.56) failed accuracy. Mean bias of the back-calculated concentrations was +32.5%',
                 fixed = TRUE)
    expect_match(html, '(limit ±25%). The accepted range starts at S6 (3.12).</summary>', fixed = TRUE)
    # the level outside the range does not repeat the wells of its fit
    expect_false(grepl('included in the fit', html, fixed = TRUE))
    # the ELISA example: the highest level lost a well without an OD
    elisa <- verdict_html(calculate(load_sample('elisa'), 'LRM'))
    expect_match(elisa, 'S1 (5) lost a replicate', fixed = TRUE)
    expect_match(elisa, 'The accepted range ends at S2 (2.5).', fixed = TRUE)
})

test_that('a lost replicate says why the level is not accepted by its bias alone', {
    r <- calculate(verdict_plate(missing = 'A8'), 'LRM')
    html <- verdict_html(r)
    expect_match(html, paste('S1 (100) lost a replicate: A7 is the only back-calculated result. Lost: A8: no OD.',
                             'A level passes on bias alone only when every lost well was excluded with a reason.'),
                 fixed = TRUE)
    reject <- calculate(verdict_plate(), 'LRM', criteria = qc_criteria(replicate_loss = 'reject'),
                        exclude = data.frame(key = 'D7', reason = 'Bubble'))
    expect_match(verdict_html(reject), paste('S4 (12.5) lost a replicate: D8 is the only back-calculated result,',
                                             'and the criteria require at least two. Lost: D7: excluded — Bubble.'),
                 fixed = TRUE)
})

test_that('the dialog of a standard point tells the well, its level and what an exclusion leaves', {
    context <- function(r, key, excluded = FALSE, lang = 'en') {
        as.character(shiny::tagList(LinearityApp:::point_context_ui(r, key, excluded, lang)))
    }
    r <- calculate(cv_plate(), 'LRM')
    html <- context(r, 'C7')
    expect_match(html, 'This well: back-calculated 28.1, bias +12.4% (limit ±20% for its level).', fixed = TRUE)
    expect_match(html, 'S3 (25) failed precision.', fixed = TRUE)
    expect_match(html, 'After the exclusion S3 (25) will keep one result: it will be judged on bias alone',
                 fixed = TRUE)
    # a level that passed shows its values
    expect_match(context(r, 'B7'), 'S2 (50) passed. Mean bias ', fixed = TRUE)
    expect_match(context(r, 'C7', lang = 'ru'), 'После исключения у S3 (25) останется один результат', fixed = TRUE)

    # the last result of a level, and a level whose criteria require two results
    one <- calculate(cv_plate(), 'LRM', exclude = data.frame(key = 'D7', reason = 'Pipetting error'))
    expect_match(context(one, 'D8'), 'After the exclusion S4 (12.5) will have no back-calculated result',
                 fixed = TRUE)
    reject <- calculate(cv_plate(), 'LRM', criteria = qc_criteria(replicate_loss = 'reject'))
    expect_match(context(reject, 'C7'), 'the criteria require two, so the level will not be accepted', fixed = TRUE)
    # a level already left with one well after a loss
    lost <- calculate(verdict_plate(missing = 'C8'), 'LRM')
    html <- context(lost, 'C7')
    expect_match(html, 'S3 (25) lost a replicate', fixed = TRUE)
    expect_match(html, 'will have no back-calculated result', fixed = TRUE)

    # an excluded well: its level only, nothing about what an exclusion leaves
    html <- context(one, 'D7', excluded = TRUE)
    expect_match(html, 'S4 (12.5) passed on bias alone', fixed = TRUE)
    expect_false(grepl('This well|After the exclusion', html))
})

test_that('the comparison of models gives the levels passed with each model', {
    d <- cv_plate()
    results <- lapply(stats::setNames(calibration_models(), calibration_models()), function(m) {
        tryCatch(calculate(d, m), error = function(e) NULL)
    })
    results[['5PL']] <- NULL
    acc <- LinearityApp:::model_acceptance(c(results, list('5PL' = NULL)))
    expect_equal(acc$model, calibration_models())
    expect_equal(acc$n.pass[acc$model == 'LRM'], 6L)
    expect_true(is.na(acc$status[acc$model == '5PL']))
    cmp <- compare_models(calibration_pool(d, 0.05))
    tab <- LinearityApp:::comparison_table(cmp, 'en', acc)
    expect_equal(names(tab)[ncol(tab)], 'Levels passed')
    expect_equal(tab[['Levels passed']][cmp$model == 'LRM'], '6/7 ✓')
    expect_equal(tab[['Levels passed']][cmp$model == '5PL'], '')
    expect_equal(LinearityApp:::acceptance_text(data.frame(model = c('a', 'b'), n.pass = c(6L, 5L),
                                                           n.levels = 7L, status = c('accepted_no_cv', 'rejected')),
                                                c('b', 'a')),
                 c('5/7 ✗', '6/7 ⚠'))
    # without the acceptance the column is left out
    expect_false('Levels passed' %in% names(LinearityApp:::comparison_table(cmp, 'en')))
    expect_equal(utils::tail(names(LinearityApp:::comparison_table(cmp, 'ru', acc)), 1), 'Принято уровней')
})
