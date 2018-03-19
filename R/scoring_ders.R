#' Scoring of the Difficulties in Emotion Regulation Scale
#'
#' Function to automatically score the Difficulties in Emotion Regulation Scale (DERS)
#'
#' @author Francesca Fusina, Antonio Maffei
#' @importFrom magrittr %>%
#'
#' @usage
#' scoring_ders(input_ders = x)
#' scoring_ders(input_ders = x, indices = c(1,36))
#'
#' @param input_ders Input data. It must be a rectangular matrix with column indicating items and rows
#'                  indicating subjects.
#' @param indices Two values vector indicating the first and the last item in the input data,
#'                in order to extract DERS items from a larger dataframe.
#'
#' @return A dataframe with 7 columns representing the subscales from the DERS
#'
#' @export

scoring_ders <- function(input_ders, indices = NULL)

  ### controlla di avere almeno 2 soggetti ###
{
  if (dim(input_ders)[1] < 2) {
    stop("The input data must have at least two rows")
  }
  if (is.null(indices)) {
    raw_resp <- input_ders
  }
  else {
    raw_resp <- input_ders[, indices[1]:indices[2]]
  }

  ### item il cui punteggio va invertito e item standard ###
  item_inverted_ders <- c(1,2,6,7,8,10,20,22,24)
  item_standard_ders <- seq(1,36)[-item_inverted_ders]
  ders_raw_corrected <- raw_resp
  ders_raw_corrected[, item_inverted_ders] <- 6 - ders_raw_corrected[,item_inverted_ders] #perché il punteggio va da 1 a 5

  #### indicizza le sottoscale ###
  TOT_index <- c(1:36)
  MDA_index <- c(11,12,21,25,29,30)
  DND_index <- c(13,18,23,26,33)
  MDF_index <- c(1,15,16,20,22,24,28,31)
  MDC_index <- c(3,14,19,27,32,36)
  DNR_index <- c(4,5,7,9,10)
  RA_index <- c(2,6,8)

  ### calcola le sottoscale per tutti i soggetti ###
  TOT <- ders_raw_corrected[, TOT_index] %>% rowSums()
  MDA <- ders_raw_corrected[, MDA_index] %>% rowSums()
  DND <- ders_raw_corrected[, DND_index] %>% rowSums()
  MDF <- ders_raw_corrected[, MDF_index] %>% rowSums()
  MDC <- ders_raw_corrected[, MDC_index] %>% rowSums()
  DNR <- ders_raw_corrected[, DNR_index] %>% rowSums()
  RA <- ders_raw_corrected[, RA_index] %>% rowSums()

  ### output ###
  ders_scored <- tibble::data_frame(TOT, MDA, DND, MDF, MDC, DNR, RA)
  return(ders_scored)

}
