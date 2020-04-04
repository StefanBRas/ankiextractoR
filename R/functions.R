to_anki_vec <- function(string_list, location) {
    anki_vec <- list(name = string_list[[1]],
      location = location,
      parameters = paste(tail(string_list,-1), collapse = ' '))
    return(anki_vec)
}

bind <- function(ankis) {
    return(do.call(rbind.data.frame, c(ankis, stringsAsFactors = FALSE)))

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
    sub_pattern <- paste0("(", comment_string, "\\s+anki )([^",comment_string, "$]*)")
    only_parameters <- lapply(anki_matches, function(x) gsub(sub_pattern, "\\2", x))
    results <- vector("list", length(only_parameters)) 
    for (i in seq_along(only_parameters)) {
        strings <- only_parameters[[i]]
             if (length(strings >= 1)) {
                 split_strings <- strsplit(strings, split = " ")
                 results[[i]] <- lapply(split_strings, function(x) to_anki_vec(x,i))
             }
    }
    results <- unlist(results, recursive = FALSE)
    if(is.null(results)) {
        return(NULL)
    } else {
    return(do.call(rbind.data.frame, c(results, stringsAsFactors = FALSE)))
    }
}

extract_parameter <- function(parameter_string, parameter) {
    pattern <- paste0(parameter,'=(.*$)')
    has_parameter <- grepl(pattern, parameter_string)
    results <- ifelse(has_parameter, 
                      gsub(pattern, '\\1', parameter_string),
                      NA)
    return(results)
}

add_fields <- function(ankis) {
    ankis$field <- extract_parameter(ankis$parameters, 'f')
    for (name in unique(ankis$name)) {
        ankis[ankis$name == name & is.na(ankis$field), 'field'] <- 
            seq_along(ankis[ankis$name == name & is.na(ankis$field), 'field'])
    }
    return(ankis)
}   

match_fields <- function(ankis) {
    for (name in unique(ankis$name)) {
        counts <- table(ankis[ankis$name == name, 'field'])
        print(counts)
    }
    return(ankis)
}   

merge_by_fields <- function(ankis) {
    return(ankis)
}


extract_ankis <- function(source_text, ankis) {
    ankis$location <- as.numeric(ankis$location)
    ankis_grouped <- tapply(ankis[["location"]], 
                            interaction(ankis[["name"]], ankis[["field"]],
                                        drop = TRUE, sep = ".."),
                            range,
                            simplify = FALSE)
    ankis_extracted <- lapply(ankis_grouped,
                              function(x) source_text[seq(x[1], x[2])] )
    return(ankis_extracted)
}

remove_comments <- function(texts, comment_string = "%") {
    sub_pattern <- paste0(comment_string, "\\s+anki [^",comment_string, "$]*")
    results <- gsub(sub_pattern, "", texts)
    return(results)
}

parse_ankis <- function(extracted, ankis) {
    list_names <- names(extracted)
    merged <- lapply(extracted, function(x) paste(x, collapse='\n'))
    return(merged)
}


ankis_to_csv <- function(extracted, filepath = NA) {
    temp <- strsplit(names(extracted), '..', fixed = TRUE)
    dataframe <- bind(temp)
    names(dataframe)  <- c('name','field')
    dataframe$text  <- unlist(extracted)
    dataframe_wide <- reshape(data = dataframe,
                              idvar = 'name',
                              v.names = 'text',
                              timevar = 'field',
                              direction = 'wide')
    write.table(x = dataframe_wide, file = filepath, 
                row.names = FALSE, col.names = FALSE,
                sep = ';', qmethod = 'double')

}

ankixtract <- function(input_filename,
                       output_filename = NA,
                       filetype = "tex",
                       comment_string = "%") {
    source_text <- readLines(con = input_filename)
    ankis <- find_ankis(source_text)
    ankis <- add_fields(ankis)
    source_text <- remove_comments(source_text)
    extracted <- extract_ankis(source_text, ankis)
    final <- parse_ankis(extracted, ankis)
    if(!is.na(output_filename)) {
        ankis_to_csv(final, output_filename)
    } else {
        return(final)
    }
    return(1)
}


