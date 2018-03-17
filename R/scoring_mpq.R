#' Scoring of the Multidimensional Personality Questionnaire
#' 
#' Function to automatically score the Multidimensional Personality Questionnaire
#' 
#' @author Antonio Maffei
#' @importFrom magrittr %>%
#' 
#' @usage 
#' scoring_mpq(input_mpq = x)
#' scoring_mpq(input_mpq = x, indices = c(1,155))
#' 
#' @param input_mpq Input data. It must be a rectangular matrix with column indicating items and rows 
#'                  indicating subjects.
#' @param indices Two values vector indicating the first and the last item in the input data, 
#'                in order to extract MPQ items from a larger dataframe.
#'
#' @return A dataframe with 15 columns representing the subscales from the MPQ
#' 
#' @export
#' 
scoring_mpq <- function(input_mpq,indices = NULL){
  
  if (dim(input_mpq)[1] < 2){ stop("The input data must have at least two rows")}
  
  if (is.null(indices)){
    raw_resp <- input_mpq
  } else {
    raw_resp <- input_mpq[,indices[1]:indices[2]]
  }

  item_inverted_mpq <- c(4,11,14,21,22,28,33,34,37,46,47,61,63:65,69,79:81,84,93,98,99,105,108,118,122,129,131,134,
                         145,148,152,153)
  item_standard_mpq <- seq(1,155)[-item_inverted_mpq]
  
  mpq_raw_corrected <- raw_resp
  mpq_raw_corrected[,item_standard_mpq] <- 2-mpq_raw_corrected[,item_standard_mpq]
  mpq_raw_corrected[,item_inverted_mpq] <- mpq_raw_corrected[,item_inverted_mpq] - 1
  
  # subscale identification and scoring #
  WBS_index <- c(1,26,38,50,62,74,85,97,109,121,133,144)
  SPS_index <- c(2,15,39,51,63,75,87,98,110,122,134,145)
  ACS_index <- c(3,16,27,52,64,76,88,99,111,123,135,146)
  SCS_index <- c(5,17,28,40,65,77,89,100,112,124,136,148)
  SRS_index <- c(6,18,29,41,53,78,90,101,113,125,137,149)
  ALS_index <- c(7,19,30,42,54,66,91,102,114,126,138,150)
  AGS_index <- c(8,20,31,43,55,67,79,103,115,127,139,151) 
  CLS_index <- c(9,21,33,44,56,68,80,92,116,128,140,152)
  HAS_index <- c(11,22,34,46,57,69,81,93,105,129,141,153)
  TDS_index <- c(12,23,35,47,58,70,82,94,106,118,142,154)
  ABS_index <- c(13,24,36,48,59,71,83,95,107,119,130,155)
  UNVIR_index <- c(4,14,25,37,49,61,72,84,96,108,120,131,143,147)
  BOLD_index <- c(2,11,15,22,25,29,45,46,51,60,63,75,109,116,117,122,123,132,133,145)
  MEAN_index <- c(17,31,40,43,55,67,77,79,103,115,124,127,138,139,148,151)
  DIS_index <- c(8,18,20,21,33,42,44,54,56,68,78,80,86,90,102,114,140,149)
  
  
  WBS <- mpq_raw_corrected[,WBS_index] %>% rowSums()
  SPS <- mpq_raw_corrected[,SPS_index] %>% rowSums()
  ACS <- mpq_raw_corrected[,ACS_index] %>% rowSums()
  SCS <- mpq_raw_corrected[,SCS_index] %>% rowSums()
  SRS <- mpq_raw_corrected[,SRS_index] %>% rowSums()
  ALS <- mpq_raw_corrected[,ALS_index] %>% rowSums()
  AGS <- mpq_raw_corrected[,AGS_index] %>% rowSums()
  CLS <- mpq_raw_corrected[,CLS_index] %>% rowSums()
  HAS <- mpq_raw_corrected[,HAS_index] %>% rowSums()
  TDS <- mpq_raw_corrected[,TDS_index] %>% rowSums()
  ABS <- mpq_raw_corrected[,ABS_index] %>% rowSums()
  UNVIR <- mpq_raw_corrected[,UNVIR_index] %>% rowSums()
  BOLD <- mpq_raw_corrected[,BOLD_index] %>% rowSums()
  MEAN <- mpq_raw_corrected[,MEAN_index] %>% rowSums()
  DIS <- mpq_raw_corrected[,DIS_index] %>% rowSums()
  
  mpq_scored <- tibble::data_frame(WBS,SPS,ACS,SCS,SRS,ALS,AGS,CLS,HAS,TDS,ABS,UNVIR,BOLD,MEAN,DIS)
  
  return(mpq_scored)
}

