#' Download PaleoClim bioclimatic data
#'
#' @description This function allows to download bioclimatic data from PaleoClim
#' (http://www.paleoclim.org) considering multiple time periods.
#' @usage PaleoClim_data(tp='current', res=10)
#' @param tp Period in which you want to retrieve data. Possible entries are:
#' 'm2', 'mpwp', 'MIS19', 'LIG', 'LGM', 'HS1', 'BA', 'YDS', 'EH', 'MH', 'LH',
#' 'LH' and/or 'current'. You can select more than one period per run through a vector.
#' Default value is 'current'.
#' @param res Resolution you want to obtain the data. Possible entries are:
#' '10', '5', '2.5' and/or '30'. You can  use a vector to provide more than one entry.
#' @details This function will create a folder entitled 'PaleoClim_data'. All the data downloaded will be stored in this folder. Note that, despite being possible to retrieve a lot of data at once, it is not recommended to do so, since the data is very heavy. Note that PaleoClim is based on CHELSA layers.
#' @references http://www.paleoclim.org
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#' @examples
#' \dontrun{# download bioclimatic variables for YDS period and 10 arc-minutes
#' PaleoClim_data(c('current', 'YDS'), 10)}
#' @importFrom utils download.file
#' @importFrom utils unzip
#' @export

PaleoClim_data <- function(tp='current', res=10){
  all_tp <- c("m2","mpwp","MIS19","LIG","HS1","BA",
              "YDS","EH","MH","LH","LH","LGM","current")
  tp2 <- c("m2","mpwp","MIS19","LIG","HS1","BA","YDS","EH","MH","LH","LH", "chelsa_LGM", "chelsa_cur")
  tp3 <- c("M2","mPWP","MIS19","LIG","HS1","BA","YDS","EH","MH","LH","LH", "chelsa_LGM_v", "CHELSA_cur_V")
  tp4 <- c("M2","MP","MI","LI","HS","BA","YD","EH","MH","LH","LH", "LGM", "current")

  for (g in 1:length(tp)) {
    v <- which(tp[g] == all_tp)
    if (v > 11){
      for (s in 1:length(res)) {
        print(paste0(tp[g], "_", res[s]))
        if(res[s] == 30){
          r = 's'
        } else {
          r = 'm'
        }
        if(res[s] == 2.5){
          res2 = '2_5'
        } else {
          res2 = res[s]
        }
        if(!dir.exists("PaleoClim_data")){ dir.create("PaleoClim_data") }
        download.file(url = paste0("http://sdmtoolbox.org/paleoclim.org/data/",
                                   tp2[v],"/",tp3[v],"1_2B_r",res2,r,".zip"),
                      destfile = paste0("PaleoClim_data/",tp4[v], "_", res[s],".zip"),
                      method = "auto")
        unzip(zipfile = paste0("PaleoClim_data/",tp4[v], "_", res[s],".zip"),
              exdir = paste0("PaleoClim_data/", tp4[v], "_", res[s]),overwrite = TRUE)
      }
    } else {
      for (s in 1:length(res)) {
        if(res[s] == 30){
          print('30s resolution is not available for this time period. Skiping...')
        } else {
          print(paste0(tp[g], "_", res[s]))
          if(res[s] == 2.5){
            res2 = '2_5'
          } else {
            res2 = res[s]
          }
          if(!dir.exists("PaleoClim_data")){ dir.create("PaleoClim_data") }
          download.file(url = paste0("http://sdmtoolbox.org/paleoclim.org/data/",
                                     tp2[v],"/",tp3[v],"_v1_",res2,"m.zip"),
                        destfile = paste0("PaleoClim_data/",tp4[v], "_", res[s],".zip"),
                        method = "auto")
          unzip(zipfile = paste0("PaleoClim_data/",tp4[v], "_", res[s],".zip"),
               exdir = paste0("PaleoClim_data/", tp4[v], "_", res[s]),overwrite = TRUE)
        }
      }
    }
  }
}
