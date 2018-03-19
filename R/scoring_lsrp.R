#' Scoring of the Levenson Self-Report Psychopathy Scale
#'
#' Function to automatically score the Levenson Self-Report Psychopathy Scale
#'
#' @author Antonio Maffei
#' @importFrom magrittr %>%
#'
#' @usage
#' scoring_lsrp(input_lsrp = x)
#' scoring_lsrp(input_lsrp = x, indices = c(1,26))
#'
#' @param input_lsrp Input data. It must be a rectangular matrix with column indicating items and rows
#'                   indicating subjects.
#' @param indices Two values vector indicating the first and the last item in the input data,
#'                in order to extract LRSP items from a larger dataframe.
#'
#' @return A dataframe with 2 columns representing the subscales from the Levenson Self Report Psychopathy Scale
#'
#' @export
#'
scoring_lsrp <- function(input_lsrp,indices = NULL){

  if (dim(input_lsrp)[1] < 2){ stop("The input data must have at least two rows")}

  if (is.null(indices)){
    raw_resp <- input_lsrp
  } else {
    raw_resp <- input_lsrp[,indices[1]:indices[2]]
  }

  # item correction #
  item_to_correct_lsrp <- c(5,11,14,17,19,23,24)
  lsrp_raw_corrected <- raw_resp
  lsrp_raw_corrected[,item_to_correct_lsrp] <- 5-raw_resp[,item_to_correct_lsrp]

  # subscale identification and scoring #
  Factor_1_index <- c(1,4,6,7,9:15,17,19,20,22,24)
  Factor_2_index <- c(2,3,5,8,16,18,21,23,25,26)

  Factor_1 <- lsrp_raw_corrected[,Factor_1_index] %>% rowSums()
  Factor_2 <- lsrp_raw_corrected[,Factor_2_index] %>% rowSums()

  lsrp_scored <- tibble::data_frame(Factor_1,Factor_2)

  return(lsrp_scored)
}
