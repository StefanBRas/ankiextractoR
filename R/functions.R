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
    pattern <- paste0(parameter,'=([[:alnum:]]*)')
    has_parameter <- grepl(pattern, parameter_string)
    parameters <- regmatches(parameter_string, regexpr(pattern, parameter_string))
    vals <- gsub(pattern, '\\1', parameters)
    results <- has_parameter
    results[!has_parameter] <- NA
    results[has_parameter] <- vals
    return(as.character(results))
}

add_fields <- function(ankis) {
    ankis$field <- extract_parameter(ankis$parameters, 'f')
    for (name in unique(ankis$name)) {
        ankis[ankis$name == name & is.na(ankis$field), 'field'] <- 
            seq_along(ankis[ankis$name == name & is.na(ankis$field), 'field'])
    }
    return(ankis)
}   

add_name_field <- function(ankis) {
    ankis[['name_field']] <- interaction(ankis[["name"]], ankis[["field"]],
                                        drop = TRUE, sep = "..")
    return(ankis)
}

extract_ankis <- function(source_text, ankis) {
    ankis$location <- as.numeric(ankis$location)
    ankis_grouped <- tapply(ankis[["location"]],
                            ankis[['name_field']],
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

cloze_parser <- function(extracted, ankis) {
    arg_vals <- extract_parameter(ankis$parameters, 'c')
    temp_fun <- function (text_lines, location, value) {
        text_lines[[location]] <- paste0('{{c', value, '::', text_lines[[location]],'}}')
        return(text_lines)
    }
    results <- mapply(FUN = temp_fun ,
           extracted, ankis[['rel_loc']], arg_vals,
           SIMPLIFY = FALSE)
    return(results)
}




PARSERS <- list(list(argument = 'c', parser = cloze_parser)
                )

latex_parser <- function(extracted, ankis) {
    arg_vals <- extract_parameter(ankis$parameters, 'l')
    temp_fun <- function (field_text, location, value) {
        field_text <- paste0('[latex]', field_text, '[/latex]')
        return(field_text)
    }
    results <- mapply(FUN = temp_fun ,
           extracted,
           arg_vals,
           SIMPLIFY = FALSE)
    return(results)
}
# parses after all strings in a field is merged into a single string
POST_PARSERS <- list(
                     list(argument = 'l', parser = latex_parser)

)




add_relative_location <- function(ankis) {
    min_loc <- tapply(ankis[["location"]], ankis[['name_field']], min)
    ankis[['rel_loc']] <- ankis[['location']] - min_loc[ankis[['name_field']]] + 1
    return(ankis)
}

parse_ankis <- function(extracted, ankis, parsers = 'all') {
    list_names <- names(extracted)
    for (parser in PARSERS) {
        has_arg <- which(!is.na(extract_parameter(ankis[['parameters']],
                                            parser[['argument']])))
        for (anki in has_arg) {
            name_field <- ankis[anki, 'name_field']
            extracted[name_field] <- parser$parser(extracted[name_field], ankis[anki,])
        }
        name_fields <- ankis[has_arg, 'name_field']
    }
    merged <- lapply(extracted, function(x) paste(x, collapse='\n'))
    for (parser in POST_PARSERS) {
        has_arg <- !is.na(extract_parameter(ankis[['parameters']],
                                            parser[['argument']]))
        name_fields <- ankis[has_arg, 'name_field']
        merged[name_fields] <- parser$parser(merged[name_fields], ankis[has_arg,])
    }
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
    fields_amount <- apply(dataframe_wide, 1, function(x) sum(!is.na(x))) - 1 # name column
    if (length(unique(fields_amount)) == 1) {
        write.table(x = dataframe_wide, file = filepath, 
                    row.names = FALSE, col.names = FALSE,
                    sep = ';', qmethod = 'double')
    } else {
        for (fields in unique(fields_amount)) {
            dataframe_subset<- dataframe_wide[fields_amount == fields, seq(1, fields + 1)]
            write.table(dataframe_subset,
                        file = paste0(filepath, fields), 
                        row.names = FALSE, col.names = FALSE,
                        sep = ';', qmethod = 'double')
        }
    }
}

ankixtract <- function(input_filename,
                       output_filename = NA,
                       filetype = "tex",
                       comment_string = "%") {
    source_text <- readLines(con = input_filename)
    ankis <- find_ankis(source_text)
    ankis <- add_fields(ankis)
    ankis <- add_name_field(ankis)
    ankis <- add_relative_location(ankis)
    source_text <- remove_comments(source_text, comment_string)
    extracted <- extract_ankis(source_text, ankis)
    final <- parse_ankis(extracted, ankis)
    if(!is.na(output_filename)) {
        ankis_to_csv(final, output_filename)
    } else {
        return(final)
    }
    return(1)
}


