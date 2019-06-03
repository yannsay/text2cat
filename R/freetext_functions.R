#' Extract words strings
#'
#' @param free_text_vector a character vector of free text
#' @details  splits words on spaces. removes punctuation and white space.
#' @return list with one vector per value in the input vector. Each element in the vector is a word with the first letter
#' in capital.
#'
#'
#' @export
extract_words <- function(free_text_vector) {

free_text_vector <- stringr::str_replace_all(free_text_vector, pattern = "[[:punct:]]", " ")
free_text_vector <- stringr::str_replace_all(free_text_vector, pattern = "\\s+", " ")
free_text_vector <- stringr::str_to_title(free_text_vector)
free_text_vector <- stringr::str_trim(free_text_vector, side = "right")

list_words <- stringr::str_split(free_text_vector , pattern = " ")

return(list_words)
}


#' Counts words in free text
#' @param free_text_vector a character vector of free text
#' @details Counts words identified with `extract_word()`
#' @return A dataframe with 2 columns, "word": the identified word; "count": how many time the word appeared.
#' @export
word_frequency <- function(free_text_vector) {
    word_list <- extract_words(free_text_vector)
    word_count <- table(unlist(word_list))
    word_count_df <- as.data.frame(word_count)
    colnames(word_count_df) <- c("word", "count")
    return(word_count_df)
}


which_list <-
    function(word_to_check, bad_mother_fucker) {
        vector_of_list <-
            sapply(bad_mother_fucker, function(x) {
                word_to_check %in% x
            })
        return(ifelse(
            is.character(names(bad_mother_fucker[which(vector_of_list)])),
            names(bad_mother_fucker[which(vector_of_list)]),
            "dontmess"
        ))
    }
