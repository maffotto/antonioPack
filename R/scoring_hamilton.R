#' Function to automatically score the Hamilton Rating Scale for Depression
#' 
#' \code{scoring_hamilton} Returns the corrected scores for HAM-D
#' 
#' @author Antonio Maffei
#' @importFrom magrittr %>%
#' 
#' @usage 
#' scoring_hamilton(dataframe = x, init_col = 55, end_col = 94, ID_col = 8)
#' 
#' @param dataframe Input data. It must be a rectangular matrix with column indicating items and rows 
#'                  indicating subjects.
#' @param init_col Index of the first column storing the first item of the questionnaire.
#' @param end_col  Index of the last column storing the first item of the questionnaire.
#' @param ID_col   Index of the column storing subject ID
#'
#' @return A dataframe with Hamilton scores for each subject
#' 
#' @export
#' 
scoring_hamilton <- function(dataframe, init_col = 55, end_col = 94, ID_col = 8){
  
  hamilton_df <- dataframe[,init_col:end_col]
  hamilton_df <- as.data.frame(sapply(hamilton_df, substring, 1, 1), stringsAsFactors = F)
  names(hamilton_df) <- substring(names(hamilton_df), 1, 3)
  hamilton_df[is.na(hamilton_df)] <- "0"                         # Converts NA (empty answers) to 0
  hamilton_df <- as.data.frame(sapply(hamilton_df, as.numeric))  # Converts everything to numeric
  
  ID <- dataframe[,ID_col]
  n_id <- dim(hamilton_df)[1]
  item1  <- vector("numeric", length = n_id)
  item2  <- vector("numeric", length = n_id)
  item3  <- vector("numeric", length = n_id)
  item4  <- vector("numeric", length = n_id)
  item5  <- vector("numeric", length = n_id)
  item6  <- vector("numeric", length = n_id)
  item7  <- vector("numeric", length = n_id)
  item8  <- vector("numeric", length = n_id)
  item9  <- vector("numeric", length = n_id)
  item10 <- vector("numeric", length = n_id)
  item11 <- vector("numeric", length = n_id)
  item12 <- vector("numeric", length = n_id)
  item13 <- vector("numeric", length = n_id)
  item14 <- vector("numeric", length = n_id)
  item15 <- vector("numeric", length = n_id)
  item16 <- vector("numeric", length = n_id)
  item17 <- vector("numeric", length = n_id)
  item18 <- vector("numeric", length = n_id)
  item19 <- vector("numeric", length = n_id)
  item20 <- vector("numeric", length = n_id)
  item21 <- vector("numeric", length = n_id)
  item22 <- vector("numeric", length = n_id)
  item23 <- vector("numeric", length = n_id)
  
  sum16  <- vector("numeric", length = n_id)
  
  for (i in 1:n_id){
    item1[i]  <- ifelse(hamilton_df[i,1] == 0, 0, sum(hamilton_df[i,1:4])/3.5) %>% plyr::round_any(.5)
    item2[i]  <- hamilton_df[i,6] %>% plyr::round_any(.5)
    item3[i]  <- hamilton_df[i,7] %>% plyr::round_any(.5)
    item4[i]  <- ifelse(hamilton_df[i,8] == 0, 0, sum(hamilton_df[i,8:9])/3) %>% plyr::round_any(.5)
    item5[i]  <- ifelse(hamilton_df[i,10] == 0, 0, sum(hamilton_df[i,10:11])/3) %>% plyr::round_any(.5)
    item6[i]  <- ifelse(hamilton_df[i,12] == 0, 0, sum(hamilton_df[i,12:13])/3) %>% plyr::round_any(.5)
    item7[i]  <- (sum(hamilton_df[i,14:15])/2) %>% plyr::round_any(.5)
    item8[i]  <- hamilton_df[i,16] %>% plyr::round_any(.5)
    item9[i]  <- hamilton_df[i,17] %>% plyr::round_any(.5)
    item10[i] <- ifelse(hamilton_df[i,18] == 0, 0, sum(hamilton_df[i,18:19])/2) %>% plyr::round_any(.5)
    item11[i] <- (sum(hamilton_df[i,20:23])/3.5) %>% plyr::round_any(.5)
    item12[i] <- hamilton_df[i,24] %>% plyr::round_any(.5)
    item13[i] <- max(hamilton_df[i,25:26]) %>% plyr::round_any(.5)
    item14[i] <- hamilton_df[i,27] %>% plyr::round_any(.5)
    item15[i] <- ifelse(hamilton_df[i,29] == 4, 0, hamilton_df[i,28]) %>% plyr::round_any(.5)
    item17[i] <- sum(hamilton_df[i,31:32]) %>% plyr::round_any(.5)
    
    sum16     <- sum(item1[i], item2[i], item3[i], item4[i], item5[i],
                     item6[i], item7[i], item8[i], item9[i], item10[i],
                     item11[i], item12[i], item13[i], item14[i], item15[i],
                     item17[i]) %>% round(0)
    item16[i] <- ifelse(hamilton_df[i,30] == 0 & sum16 <= 15, 0,
                        ifelse(hamilton_df[i,30] == 0 & sum16 >= 16 & sum16 <= 23, 1,
                               ifelse(hamilton_df[i,30] == 0 & sum16 >= 24, 2,
                                      ifelse(hamilton_df[i,30] == 1 & sum16 <= 9, 1,
                                             ifelse(hamilton_df[i,30] == 1 & sum16 >= 10 & sum16 <= 15, 0.5,
                                                    ifelse(hamilton_df[i,30] == 1 & sum16 >= 16, 0,
                                                           ifelse(hamilton_df[i,30] == 2 & sum16 <= 15, 0,
                                                                  ifelse(hamilton_df[i,30] == 2 & sum16 >= 16 & sum16 <= 23, 1,
                                                                         ifelse(hamilton_df[i,30] == 2 & sum16 >= 24, 1.5,
                                                                                ifelse(hamilton_df[i,30] == 3 & sum16 <= 15, 0,
                                                                                       ifelse(hamilton_df[i,30] == 3 & sum16 >= 16 & sum16 <= 23, 1,
                                                                                              ifelse(hamilton_df[i,30] == 3 & sum16 >= 24, 2)
                                                                                              )
                                                                                       )
                                                                                )
                                                                         )
                                                                  )
                                                           )
                                                    )
                                             )
                                      )
                               )
                        )  %>% plyr::round_any(.5)
    
    item18[i] <- ifelse(hamilton_df[i,34] == 0, 0, sum(hamilton_df[i,34:35])/3) %>% plyr::round_any(.5)
    item19[i] <- hamilton_df[i,36] %>% plyr::round_any(.5)
    item20[i] <- hamilton_df[i,37] %>% plyr::round_any(.5)
    item21[i] <- hamilton_df[i,38] %>% plyr::round_any(.5)
    item22[i] <- hamilton_df[i,39] %>% plyr::round_any(.5)
    item23[i] <- hamilton_df[i,40] %>% plyr::round_any(.5)
  }
  
  hamilton_raw_scores <- cbind(item1,item2,item3,item4,item5,item6,item7,item8,item9,
                               item10,item11,item12,item13,item14,item15,item16,item17,
                               item18,item19,item20,item21,item22,item23)
  hamilton_scored <- data.frame(ID  = ID,
                                HAM = rowSums(hamilton_raw_scores))
  names(hamilton_scored) <- c("ID","HAM")
  
  return(hamilton_scored)
  
}


