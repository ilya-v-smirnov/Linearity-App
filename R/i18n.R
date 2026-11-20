#' Translations
#'
#' Interface texts, messages and flags are stored in YAML dictionaries
#' (`inst/i18n/<lang>.yml`). Placeholders in texts have the form `{name}`.
#'
#' @name i18n
NULL

.i18n_cache <- new.env(parent = emptyenv())

#' Supported Languages
#'
#' @return Character vector of language codes.
#' @export
supported_languages <- function() c('en', 'ru')


#' Validate a Language Code
#'
#' @param lang Language code; unknown or empty values fall back to English.
#'
#' @return A supported language code.
#' @keywords internal
match_lang <- function(lang) {
    if (is.null(lang) || length(lang) != 1 || is.na(lang) || !lang %in% supported_languages()) {
        return('en')
    }
    lang
}


#' Flatten a Nested List into Dot-Separated Keys
#' @keywords internal
flatten_dict <- function(x, prefix = NULL) {
    out <- list()
    for (nm in names(x)) {
        key <- if (is.null(prefix)) nm else paste(prefix, nm, sep = '.')
        if (is.list(x[[nm]])) {
            out <- c(out, flatten_dict(x[[nm]], key))
        } else {
            out[[key]] <- as.character(x[[nm]])
        }
    }
    out
}


#' Load the Dictionary of a Language
#'
#' @param lang Language code.
#'
#' @return Named list of texts.
#' @keywords internal
i18n_dict <- function(lang) {
    lang <- match_lang(lang)
    if (is.null(.i18n_cache[[lang]])) {
        path <- system.file('i18n', paste0(lang, '.yml'), package = 'LinearityApp')
        .i18n_cache[[lang]] <- flatten_dict(yaml::read_yaml(path, fileEncoding = 'UTF-8'))
    }
    .i18n_cache[[lang]]
}


#' Format a Placeholder Value
#' @keywords internal
format_param <- function(x, lang) {
    if (inherits(x, 'lin_message')) {
        return(message_text(list(x), lang))
    }
    if (is.list(x) && length(x) > 0 && all(vapply(x, inherits, logical(1), 'lin_message'))) {
        return(paste(message_text(x, lang), collapse = '; '))
    }
    if (inherits(x, 'flag_counts')) {
        return(paste0(translate_flags(names(x), lang), ' (', unclass(x), ')', collapse = ', '))
    }
    paste(as.character(x), collapse = ', ')
}


#' Translate a Text
#'
#' @param key Dictionary key, e.g. `"tab.results"`.
#' @param lang Language code (see [supported_languages()]).
#' @param ... Values for `{placeholders}` in the text.
#'
#' @return The translated text. Missing keys fall back to English, then to the key itself.
#' @export
#' @examples
#' tr('tab.results', 'ru')
#' tr('error.mandatory_column', 'en', column = 'OD')
tr <- function(key, lang = 'en', ...) {
    lang <- match_lang(lang)
    txt <- i18n_dict(lang)[[key]]
    if (is.null(txt) && lang != 'en') txt <- i18n_dict('en')[[key]]
    if (is.null(txt)) return(key)

    params <- list(...)
    for (nm in names(params)) {
        txt <- gsub(paste0('{', nm, '}'), format_param(params[[nm]], lang), txt, fixed = TRUE)
    }
    txt
}


#' Translate Result Flags
#'
#' @param flags Character vector of flag codes (see [FLAGS]).
#' @param lang Language code.
#'
#' @return Character vector of translated flags; unknown values are returned unchanged.
#' @export
translate_flags <- function(flags, lang = 'en') {
    one <- function(f) {
        k <- names(FLAGS)[match(f, unlist(FLAGS))]
        if (is.na(k)) f else tr(paste0('flag.', k), lang)
    }
    out <- vapply(flags, function(f) {
        if (is.na(f)) return(NA_character_)
        paste(vapply(strsplit(f, '; ', fixed = TRUE)[[1]], one, ''), collapse = '; ')
    }, character(1), USE.NAMES = FALSE)
    names(out) <- names(flags)
    out
}


#' Translatable Messages and Errors
#'
#' Calculation functions return warnings as `lin_message` objects and signal
#' errors of class `lin_error`. Both keep the dictionary key and parameters, so
#' the interface can show them in the selected language; the English text is
#' used for the console.
#'
#' @param key Dictionary key.
#' @param ... Placeholder values.
#'
#' @name lin_messages
#' @keywords internal
NULL

#' @rdname lin_messages
lin_message <- function(key, ...) {
    structure(list(key = key, params = list(...)), class = 'lin_message')
}

#' @param .fields Additional fields of the condition (e.g. `problems`).
#' @rdname lin_messages
lin_abort <- function(key, ..., .fields = list()) {
    cnd <- structure(
        class = c('lin_error', 'error', 'condition'),
        c(list(message = tr(key, 'en', ...), call = NULL, key = key, params = list(...)), .fields)
    )
    stop(cnd)
}

#' @export
print.lin_message <- function(x, ...) {
    cat(message_text(list(x)), '\n')
    invisible(x)
}


#' Text of Messages in a Language
#'
#' @param messages A list of `lin_message` objects (e.g. `calculate()$notes`).
#' @param lang Language code.
#'
#' @return Character vector.
#' @export
message_text <- function(messages, lang = 'en') {
    vapply(messages, function(m) do.call(tr, c(list(m$key, lang), m$params)), character(1))
}


#' Text of an Error Condition in a Language
#'
#' @param cnd A condition.
#' @param lang Language code.
#'
#' @return Character string.
#' @keywords internal
condition_text <- function(cnd, lang = 'en') {
    if (inherits(cnd, 'lin_error')) {
        return(do.call(tr, c(list(cnd$key, lang), cnd$params)))
    }
    conditionMessage(cnd)
}


#' Count Flags for a Message
#' @keywords internal
flag_counts <- function(flags) {
    counts <- table(factor(flags, levels = unique(flags)))
    structure(as.vector(counts), names = names(counts), class = 'flag_counts')
}
