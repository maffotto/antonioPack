#' Scoring the STAI-Y1 (Trait version)
#'
#' Function to automatically score the STAI-Y1
#'
#' @author Antonio Maffei
#' @importFrom magrittr %>%
#'
#' @usage
#' scoring_staiT(input_staiT = x)
#' scoring_staiT(input_staiT = x, indices = c(1,20))
#'
#' @param input_staiT Input data. It must be a rectangular matrix with column indicating items and rows
#'                   indicating subjects.
#' @param indices Two values vector indicating the first and the last item in the input data,
#'                in order to extract STAI-Y1 items from a larger dataframe.
#'
#' @return A dataframe with 1 columns representing the scores for STAI Trait version
#'
#' @export
#'
scoring_staiT <- function(input_staiT,indices = NULL){

  if (dim(input_staiT)[1] < 2){ stop("The input data must have at least two rows")}

  if (is.null(indices)){
    raw_resp <- input_staiT
  } else {
    raw_resp <- input_staiT[,indices[1]:indices[2]]
  }

  item_standard_stait <- c(2,5,8,9,11,12,15,17,18,20)
  item_inverted_stait <- seq(1:20)[-item_standard_stait]

  stait_corrected <- raw_resp
  stait_corrected[,item_inverted_stait] <- 5 - stait_corrected[,item_inverted_stait]

  STAI_T <- rowSums(stait_corrected) %>% tibble::as_data_frame()

  return(STAI_T)
}
