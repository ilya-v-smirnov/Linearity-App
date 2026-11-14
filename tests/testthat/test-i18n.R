placeholders <- function(x) sort(unique(regmatches(x, gregexpr('\\{[a-z_]+\\}', x))[[1]]))

test_that('all languages have the same keys and placeholders', {
    en <- LinearityApp:::i18n_dict('en')
    for (lang in setdiff(supported_languages(), 'en')) {
        other <- LinearityApp:::i18n_dict(lang)
        expect_setequal(names(other), names(en))
        for (key in names(en)) {
            expect_equal(placeholders(other[[key]]), placeholders(en[[key]]),
                         info = paste(lang, key))
        }
    }
})

test_that('no dictionary key is read as a YAML boolean (y, n, yes, no, on, off)', {
    keys <- names(LinearityApp:::i18n_dict('en'))
    expect_false(any(grepl('(^|[.])(TRUE|FALSE)($|[.])', keys)))
})

test_that('placeholders do not clash with the arguments of tr()', {
    all_placeholders <- unique(unlist(lapply(LinearityApp:::i18n_dict('en'), placeholders)))
    expect_false(any(c('{key}', '{lang}') %in% all_placeholders))
})

test_that('texts are translated and placeholders are filled', {
    expect_equal(tr('tab.results', 'en'), 'Results')
    expect_equal(tr('tab.results', 'ru'), 'Результаты')
    expect_equal(tr('error.mandatory_column', 'en', column = 'OD'), 'Mandatory OD column is missing')
    expect_match(tr('error.mandatory_column', 'ru', column = 'OD'), 'OD', fixed = TRUE)
})

test_that('unknown languages and keys fall back gracefully', {
    expect_equal(tr('tab.results', 'de'), 'Results')
    expect_equal(tr('tab.results', NULL), 'Results')
    expect_equal(tr('no.such.key', 'ru'), 'no.such.key')
})

test_that('flags are translated and unknown values are kept', {
    expect_equal(translate_flags(c('OK', '< LLOQ', 'odd'), 'en'), c('OK', '< LLOQ', 'odd'))
    ru <- translate_flags(unlist(FLAGS), 'ru')
    expect_length(unique(ru), length(FLAGS))
    expect_equal(ru[['below']], tr('flag.below', 'ru'))
})

test_that('notes and errors keep the key and are shown in any language', {
    r <- calculate(load_sample('griess'), 'LRM')
    en <- message_text(r$notes, 'en')
    ru <- message_text(r$notes, 'ru')
    expect_length(ru, length(en))
    expect_match(ru, tr('flag.od_le_nc', 'ru'), fixed = TRUE, all = FALSE)
    expect_output(print(r$notes[[1]]), 'sample wells')

    cnd <- tryCatch(check_input_file(data.frame(x = 1)), error = identity)
    expect_s3_class(cnd, 'lin_error')
    expect_equal(cnd$key, 'error.invalid_input_many')
    expect_match(LinearityApp:::condition_text(cnd, 'en'), '^Mandatory type column is missing')
    expect_false(LinearityApp:::condition_text(cnd, 'ru') == conditionMessage(cnd))
    expect_equal(LinearityApp:::condition_text(simpleError('plain'), 'ru'), 'plain')
})

test_that('localize_result translates only the flag column', {
    r <- calculate(load_sample('griess'), 'LRM')
    out <- localize_result(r$result, 'ru')
    expect_equal(out$conc, r$result$conc)
    expect_true(tr('flag.od_le_nc', 'ru') %in% out$flag)
})
