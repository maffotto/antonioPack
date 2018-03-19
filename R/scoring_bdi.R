#' Scoring the Beck Depression Inventory - II
#'
#' Function to automatically score the BDI-II
#'
#' @author Francesca Fusina, Antonio Maffei
#' @importFrom magrittr %>%
#'
#' @usage
#' scoring_staiT(input_staiT = x)
#' scoring_staiT(input_staiT = x, indices = c(1,21))
#'
#' @param input_bdi Input data. It must be a rectangular matrix with column indicating items and rows
#'                   indicating subjects.
#' @param indices Two values vector indicating the first and the last item in the input data,
#'                in order to extract BDI items from a larger dataframe.
#'
#' @return A dataframe with 1 columns representing the scores for BDI-II version
#'
#' @export
#'
scoring_bdi <- function(input_bdi, indices = NULL)

  ### controlla di avere almeno 2 soggetti ###
{
  if (dim(input_bdi)[1] < 2) {
    stop("The input data must have at least two rows")
  }
  if (is.null(indices)) {
    raw_resp <- input_bdi
  }
  else {
    raw_resp <- input_bdi[, indices[1]:indices[2]]
  }

  bdi_raw <- raw_resp

  #### indicizza le sottoscale ###
  TOT_index <- c(1:21)
  SOM_AFF_index <- c(11, 15, 16, 17, 18, 19, 20)
  COG_index <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 13, 14, 21)

  ### calcola le sottoscale per tutti i soggetti ###
  TOT <- bdi_raw[, TOT_index] %>% rowSums()
  SOM_AFF <- bdi_raw[, SOM_AFF_index] %>% rowSums()
  COG <- bdi_raw[, COG_index] %>% rowSums()

  ### output ###
  bdi_scored <- tibble::data_frame(TOT, SOM_AFF, COG)
  return(bdi_scored)

}
