to_anki_list <- function(string_list, location) {
    anki_list <- list(name = string_list[[1]],
      parameters = tail(string_list,-1),
      location = location)
    return(anki_list)
}

#' finds lines with ankixtract start comments
#'
#' @param source_text text to find in
#' @return list of param and locations of ankixtract start comments
#' @export
find_ankis <- function(source_text,
                              filetype = "tex",
                              comment_string = "%") {
    #pattern <- paste0(comment_string, " anki[^%$]*")
    if (gregexpr("\\s", comment_string)[[1]] != -1) {
        warning("Using whitespace characters in comment_string can make it break")
    }
    pattern <- paste0(comment_string, "\\s+anki[^",comment_string, "$]*")
    m  <- gregexpr(pattern, source_text)
    anki_matches <- regmatches(source_text, m)
    #sub_pattern <- paste0("(", comment_string, " anki )([^%$]*)")
    sub_pattern <- paste0("(", comment_string, "\\s+anki )([^",comment_string, "$]*)")
    only_parameters <- lapply(anki_matches, function(x) gsub(sub_pattern, "\\2", x))
    results <- vector("list", length(only_parameters)) 
    for (i in seq_along(only_parameters)) {
        strings <- only_parameters[[i]]
             if (length(strings >= 1)) {
                 split_strings <- strsplit(strings, split = " ")
                 results[[i]] <- lapply(split_strings, function(x) to_anki_list(x,i))
             }
    }
    return(unlist(results, recursive = FALSE))
}

extract_ankis <- function(source_text, ankis) {
    ankis <- lapply(ankis, function(anki) {
                        c(name = anki[['name']], 
                          location = anki[['location']])
                              })
    ankis <- do.call(rbind, ankis)
    ankis_df <- as.data.frame(ankis, stringsAsFactors = FALSE)
    ankis_df$location <- as.numeric(ankis_df$location)
    ankis_grouped <- tapply(ankis_df[["location"]], ankis_df[["name"]], range,
                            simplify = FALSE)
    ankis_extracted <- lapply(ankis_grouped,
                              function(x) source_text[seq(x[1], x[2] - 1)])
    return(ankis_extracted)
}

remove_comments <- function (texts) {
    warning('not yet implemented')
    return(texts)
}

parse_ankis <- function(source_text, ankis) {
    all_names <- unique(unlist(lapply(ankis, function(anki) anki["name"])))
    for (name in all_names) {

    }
}


ankis_to_csv <- function(filepath = NA, ankis) {
    write.csv(filepath, ankis)
}

ankixtract <- function(input_filename,
                       output_filename = NA,
                       filetype = "tex",
                       comment_string = "%") {
    source_text <- readLines(input_filename)
    ankis <- extract_ankis(source_text,
                           filetype,
                           comment_string)
    ankis_parsed  <- parse_ankis(source_text, ankis)
    if(!is.na(filepath)) {
        ankis_to_csv(output_filename, ankis_parsed)
    } else {
        return(ankis_parsed)
    }
    return(1)
}


