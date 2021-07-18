#' Inverted versions of in, is.null and is.na
#'
#' @noRd
#'
#' @examples
#' 1 %not_in% 1:10
#' not_null(NULL)
`%not_in%` <- Negate(`%in%`)

not_null <- Negate(is.null)

not_na <- Negate(is.na)

#' Removes the null from a vector
#'
#' @noRd
#'
#' @example
#' drop_nulls(list(1, NULL, 2))
drop_nulls <- function(x) {
    x[!sapply(x, is.null)]
}

#' If x is `NULL`, return y, otherwise return x
#'
#' @param x,y Two elements to test, one potentially `NULL`
#'
#' @noRd
#'
#' @examples
#' NULL %||% 1
"%||%" <- function(x, y) {
    if (is.null(x)) {
        y
    } else {
        x
    }
}

#' If x is `NA`, return y, otherwise return x
#'
#' @param x,y Two elements to test, one potentially `NA`
#'
#' @noRd
#'
#' @examples
#' NA %||% 1
"%|NA|%" <- function(x, y) {
    if (is.na(x)) {
        y
    } else {
        x
    }
}

#' Typing reactiveValues is too long
#'
#' @inheritParams reactiveValues
#' @inheritParams reactiveValuesToList
#'
#' @noRd
rv <- shiny::reactiveValues
rvtl <- shiny::reactiveValuesToList

extract_2875 <- function(pdf_path) {
    txt <- .read_pdf(pdf_path)

    name <- .ext_name(txt)
    organization <- .ext_org(txt)
    office <- .ext_office(txt)
    title_rank <- .ext_title_rank(txt)
    dodid <- .ext_dodid(txt)

    dplyr::tibble(
        DoDID = dodid,
        LastName = name[1],
        FirstName = name[2],
        MiddleInitial = ifelse(
            name[3] == "NA",
            "NA",
            paste0(substr(name[3], 1, 1), ".")
        ),
        Organization = !!organization,
        Office = !!office,
        Rank = title_rank,
        PDF_PATH = pdf_path
    ) %>%
        dplyr::mutate(
            dplyr::across(
                .cols = c(dplyr::everything(), -DoDID),
                .fns = stringr::str_replace_na,
                replacement = ""
            )
        )
}

.read_pdf <- function(pdf_path) {
    if (stringr::str_detect(
        pdftools::pdf_info(pdf_path)$metadata,
        ".oxps"
    )) {
        txt <- pdftools::pdf_ocr_text(pdf_path)
    } else {
        txt <- pdftools::pdf_text(pdf_path)
    }

    txt %>%
        stringr::str_split("\n") %>%
        unlist() %>%
        `[`(. != "")
}

.ext_name <- function(txt) {
    end <- .index_phrase(txt, "3. OFFICE SYMBOL") - 1

    txt[end] %>%
        stringr::str_trim() %>%
        stringr::str_sub(
            end = ifelse(
                is.na(stringr::str_locate(., "\\d")[1]),
                stringr::str_locate(., "  ")[1],
                stringr::str_locate(., "\\d")[1]
            )
        ) %>%
        stringr::str_trim() %>%
        stringr::str_remove(".$") %>%
        stringr::str_to_upper() %>%
        stringr::str_split("[, | ]") %>%
        unlist() %>%
        stringr::str_trim() %>%
        stringr::str_to_upper() %>%
        `[`(. != "")
}

.ext_org <- function(txt) {
    end <- .index_phrase(txt, "3. OFFICE SYMBOL") - 1

    base <- txt[end] %>%
        stringr::str_trim() %>%
        stringr::str_sub(
            start = ifelse(
                is.na(stringr::str_locate(., "\\d")[1]),
                stringr::str_locate(., "  ")[1],
                stringr::str_locate(., "\\d")[1]
            )
        ) %>%
        stringr::str_trim() %>%
        stringr::str_split("[/|,]") %>%
        unlist() %>%
        `[`(1) %>%
        stringr::str_to_upper() %>%
        stringr::str_split_fixed(" ", n = 2) %>%
        as.vector()

    base[1] <- base[1] %>%
        stringr::str_extract("\\d+|HAF") %>%
        stringr::str_trim()

    if (is.na(base[2])) {
        min_length <- 1
    } else if (stringr::str_count(base[2], " ") == 0) {
        min_length <- nchar(base[2])
    } else {
        min_length <- stringr::str_count(base[2], " ") + 1
    }

    .abbreviate <- function(string, minlength) {
        if (toupper(string) == "WING" & !is.na(string)) {
            return("WG")
        } else {
            abbreviate(string, minlength)
        }
    }

    base[2] <- base[2] %>%
        .abbreviate(minlength = min_length) %>%
        stringr::str_trim()

    paste(base, collapse = " ") %>%
        stringr::str_trim()
}

.ext_office <- function(txt) {
    end <- .index_phrase(txt, "5. OFFICIAL E-MAIL") - 1

    txt[end] %>%
        stringr::str_sub(end = stringr::str_locate(., "^(?!\\d+).*") - 1) %>%
        stringr::str_trim() %>%
        stringr::str_split("/") %>%
        unlist() %>%
        stringr::str_trim() %>%
        stringr::str_to_upper() %>%
        `[`(. != "") %>%
        `[`(1) %>%
        stringr::str_sub(end = 10) %>%
        stringr::str_remove_all("\\d+") %>%
        stringr::str_remove_all("-") %>%
        stringr::str_trim() %>%
        stringr::str_split(" ") %>%
        `[[`(1) %>%
        `[`(1)
}

.ext_title_rank <- function(txt) {
    index <- .index_phrase(txt, "6. JOB TITLE AND GRADE/RANK") + 1

    abbreviations <- c(
        "AIRMAN BASIC"           = "AB",
        "AIRMAN"                 = "AMN",
        "AIRMAN FIRST CLASS"     = "A1C",
        "SENIOR AIRMAN"          = "SRA",
        "STAFF SERGEANT"         = "SSGT",
        "TECHNICAL SERGEANT"     = "TSGT",
        "MASTER SERGEANT"        = "MSGT",
        "SENIOR MASTER SERGEANT" = "SMSGT",
        "CHIEF MASTER SERGEANT"  = "CMSGT",
        "SECOND LIEUTENANT"      = "2LT",
        "FIRST LIEUTENANT"       = "1LT",
        "CAPTAIN"                = "CPT",
        "LIEUTENANT COLONEL"     = "LT COL",
        "COLONEL"                = "COL",
        "BRIGADIER GENERAL"      = "BRIG GEN",
        "MAJOR GENERAL"          = "MAJ GEN",
        "LIEUTENANT GENERAL"     = "LT GEN",
        "GENERAL"                = "GEN",
        "CAPT"                   = "CPT"
    )

    pay_grades <- c(
        paste0("E-", 1:9),
        paste0("O-", 1:9)
    )

    names(pay_grades) <- abbreviations[1:18]

    ranks <- paste(abbreviations, names(abbreviations),
        sep = "|", collapse = "|"
    )

    rank <- txt[index] %>%
        stringr::str_sub(start = 50) %>%
        stringr::str_trim() %>%
        stringr::str_split("[/|,]") %>%
        unlist() %>%
        stringr::str_trim() %>%
        stringr::str_to_upper()

    if (any(stringr::str_detect(rank, "[E|O|0]-[\\d]"))) {
        rank <- stringr::str_extract(rank, "[E|O|0]-[\\d]") %>%
            na.omit() %>%
            stringr::str_replace(
                "[E|O|0]-[\\d]",
                names(pay_grades)[which(pay_grades == .)]
            )

        if (length(rank) == 1 & !identical(rank, character(0))) {
            return(rank)
        }
    }

    rank <- rank %>%
        stringr::str_remove_all("[E|O|0]-[\\d]") %>%
        stringr::str_remove_all("[E|O|0][\\d]") %>%
        `[`(. != "") %>%
        stringr::str_trim() %>%
        stringr::str_extract_all(ranks) %>%
        unlist() %>%
        `[`(!is.na(.))

    .abbreviate_rank <- function(string, abbreviations) {
        if (identical(string, character(0)) | is.null(string)) {
            return(NA)
        }

        string <- stringr::str_to_upper(string)

        if (string %in% abbreviations | is.na(string)) {
            return(string)
        }

        .index <- names(abbreviations) == string

        if (!any(.index)) {
            return(NA)
        }

        unname(abbreviations[.index])
    }

    .abbreviate_rank(rank, abbreviations)
}

.ext_dodid <- function(txt) {
    dodid <- txt %>%
        stringr::str_extract_all("[0-9]{10}") %>%
        unlist() %>%
        `[`(1)

    if (length(dodid) != 1) {
        warning("Something is wrong with the DoD ID parsing... Check the 2875 manually.")
    }

    dodid
}

.index_phrase <- function(txt, phrase) {
    stringr::str_detect(txt, phrase) %>%
        which()
}
