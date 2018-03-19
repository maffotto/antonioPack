#' Scoring the STAI-Y2 (State version)
#'
#' Function to automatically score the STAI-Y2
#'
#' @author Franecsca Fusina, Antonio Maffei
#' @importFrom magrittr %>%
#'
#' @usage
#' scoring_staiS(input_staiS = x)
#' scoring_staiS(input_staiS = x, indices = c(1,20))
#'
#' @param input_staiS Input data. It must be a rectangular matrix with column indicating items and rows
#'                   indicating subjects.
#' @param indices Two values vector indicating the first and the last item in the input data,
#'                in order to extract MPQ items from a larger dataframe.
#'
#' @return A dataframe with 1 columns representing the scores for STAI State version
#'
#' @export

scoring_staiS <- function (input_staiS, indices = NULL) {
  if (dim(input_staiS)[1] < 2) {
    stop("The input data must have at least two rows")
  }
  if (is.null(indices)) {
    raw_resp <- input_staiS
  }
  else {
    raw_resp <- input_staiS[, indices[1]:indices[2]]
  }

  item_standard_stais <- c(3, 4, 6, 7, 9, 12, 13, 14, 17,
                           18)
  item_inverted_stais <- seq(1:20)[-item_standard_stais]
  stais_corrected <- raw_resp
  stais_corrected[, item_inverted_stais] <- 5 - stais_corrected[,
                                                                item_inverted_stais]

  STAI_S <- rowSums(stais_corrected) %>% tibble::as_data_frame()
  return(STAI_S)
}
