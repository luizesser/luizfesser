#' Download CHELSA bioclimatic data
#'
#' @description This function allows to download bioclimatic data from CHELSA
#' (https://chelsa-climate.org) considering multiple GCMs, time periods and SSPs.
#' @usage CHELSA_data(period='current',year=NA,bioclims='all', gcm=NA, ssp=NA)
#' @param period Period in which you want to retrieve data. Possible entries are:
#' 'past', 'current' or 'future'. It is only possible to select one period per run.
#' Default value is 'current'.
#' @param year If you select 'future' as the period, you need to specify which year range.
#' Possible entries are: '2011-2040', '2041-2070 and/or '2071-2100'.
#' You can  use a vector to provide more than one entry.
#' @param bioclims With this argument it is possible to download only desired bioclimatic
#' layers, instead of the whole 19. Possible entries are: 'all' (default - retrieve all 19
#' bioclimatic variables), 'bio1', 'bio2', 'bio3', 'bio4', 'bio5', 'bio6', 'bio7', 'bio8',
#' 'bio9', 'bio10', 'bio11', 'bio12', 'bio13', 'bio14', 'bio15', 'bio16', 'bio17', 'bio18'
#' and/or 'bio19'. You can use a vector to provide more than one entry.
#' @param gcm GCMs to be considered in either past or future periods. Possible entries for
#' future: 'GFDL-ESM4','IPSL-CM6A-LR','MPI-ESM1-2-HR','MRI-ESM2-0','UKESM1-0-LL'.
#' Possible entries for past: 'CCSM4','CNRM-CM5','FGOALS-g2','IPSL-CM5A-LR','MIROC-ESM',
#' 'MPI-ESM-P','MRI-CGCM3'. You can use a vector to provide more than one entry.
#' @param ssp SSPs for future data. Possible entries are: 'ssp126', 'ssp370', 'ssp585'.
#' You can use a vector to provide more than one entry.
#' @details This function will create a folder entitled 'CHELSA_data'. All the data
#' downloaded will be stored in this folder. Note that, despite being possible to
#' retrieve a lot of data at once, it is not recommended to do so, since the data
#' is very heavy. If you are running models to past and future scenarios, consider
#' to use GCMs from the same research group, like IPSL, MPI and MRI (see Arguments
#' section above).
#' @references https://chelsa-climate.org
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#' @examples
#' \dontrun{# download all bioclimatic variables for current period
#' CHELSA_data()
#'
#' # download data from future period
#' CHELSA_data('future', year = '2011-2040', gcm = 'MPI-ESM1-2-HR',
#' ssp='ssp370', bioclims = c('bio1', 'bio12'))
#'
#' # download data from past period
#' CHELSA_data('past', gcm = 'IPSL-CM5A-LR', bioclims = c('bio1','bio12'))}
#' @importFrom utils download.file
#' @export

CHELSA_data <- function(period='current',year=NA,bioclims='all', gcm=NA, ssp=NA){
  #require(RCurl)
  # Check period:
  periods <- c('current','past','future')
  if(!period %in% periods){
    stop("Period must be either 'current', 'future' or 'past'.")
  }
  period <- periods[which(periods %in% period)]
  print(paste0('Selected period: ', period))

  # Check bioclims:
  bioclimatics <- c('bio1','bio2','bio3','bio4','bio5','bio6','bio7','bio8','bio9','bio10',
                    'bio11','bio12','bio13','bio14','bio15','bio16','bio17','bio18','bio19')
  bioclims_past <- c('BIO_01', 'BIO_02', 'BIO_03', 'BIO_04', 'BIO_05', 'BIO_06',
                     'BIO_07', 'BIO_08', 'BIO_09', 'BIO_10', 'BIO_11', 'BIO_12',
                     'BIO_13', 'BIO_14', 'BIO_15', 'BIO_16', 'BIO_17', 'BIO_18', 'BIO_19')

  if('all' %in% bioclims){
    if(period == 'past'){
      bioclims <- bioclims_past
    } else {
      bioclims <- bioclimatics
    }
    print(paste('Selected bioclimatic variables:', paste(bioclims, collapse = ', ')))
  } else {
    if(length(bioclims) > length(bioclimatics) | !all(bioclims %in% bioclimatics)){
      stop("Acceptable values for bioclims are: 'bio01', 'bio02', 'bio03', 'bio04',
           'bio05', 'bio06', 'bio07', 'bio08', 'bio09', 'bio10', 'bio11', 'bio12',
           'bio13', 'bio14', 'bio15', 'bio16', 'bio17', 'bio18' and 'bio19'.")
    } else {
      if(period == 'past'){
        bioclims <- bioclims_past[which(bioclimatics %in% bioclims)]
        print(paste('Selected bioclimatic variables:', paste(bioclims, collapse = ', ')))
      } else {
        bioclims <- bioclimatics[which(bioclimatics %in% bioclims)]
        print(paste('Selected bioclimatic variables:', paste(bioclims, collapse = ', ')))
      }
    }
  }

  # Check current arguments:
  if(period == 'current'){
    year <- '1981-2010'
    print(paste0('Year range: ', year))
    gcm <- NA
    ssp <- NA
  }

  # Check future arguments:
  if(period == 'future'){
    # Check years:
    years <- c('2011-2040','2041-2070','2071-2100')
    if(all(is.na(year)) | !all(year %in% years)){
      stop("Argument 'year' is missing or invalid. Please provide one or multiple
            options between the following: '2011-2040','2041-2070' and/or '2071-2100'.")
    }
    year <- years[which(years %in% year)]

    # Check GCMs:
    print(paste('Selected year range:', paste(year, collapse = ', ')))
    gcm_future <- c('GFDL-ESM4','IPSL-CM6A-LR','MPI-ESM1-2-HR','MRI-ESM2-0','UKESM1-0-LL')
    if(all(is.na(gcm)) | !all(gcm %in% gcm_future)){
      stop("Argument 'gcm' is missing or invalid. Please provide one or multiple
            options between the following: 'GFDL-ESM4','IPSL-CM6A-LR','MPI-ESM1-2-HR',
           'MRI-ESM2-0' and/or 'UKESM1-0-LL'.")
    }
    gcm <- gcm_future[which(gcm_future %in% gcm)]
    print(paste('Selected GCM:', paste(gcm, collapse = ', ')))

    # Check SSPs:
    ssps <- c('ssp126', 'ssp370', 'ssp585')
    if(all(is.na(ssp)) | !all(ssp %in% ssps)){
      stop("Argument 'ssp' is missing or invalid. Please provide one or multiple
            options between the following: 'ssp126', 'ssp370' and/or 'ssp585'.")
    }
    ssp <- ssps[which(ssps %in% ssp)]
    print(paste('Selected GCM:', paste(ssp, collapse = ', ')))
  }

  # Check past arguments:
  if(period == 'past'){
    # Check GCMs:
    gcm_past <- c('CCSM4','CNRM-CM5','FGOALS-g2','IPSL-CM5A-LR','MIROC-ESM','MPI-ESM-P','MRI-CGCM3')
    if(all(is.na(gcm)) | !all(gcm %in% gcm_past)){
      stop("Argument 'gcm' is missing or invalid. Please provide one or multiple
            options between the following: 'CCSM4','CNRM-CM5','FGOALS-g2','IPSL-CM5A-LR',
           'MIROC-ESM','MPI-ESM-P' and/or 'MRI-CGCM3'.")
    }
    gcm <- gcm_past[which(gcm_past %in% gcm)]
    print(paste('Selected GCM:', paste(gcm, collapse = ', ')))
  }

  #months <- c('01','02','03','04','05','06','07','08','09','10','11','12')
  #source <- c('annual','climatologies','daily','daily_normals','monthly') # data source

  # Download current data
  if(period == 'current'){
    if(!dir.exists('CHELSA_data')){ dir.create('CHELSA_data') }
    for(b in bioclims){
      n <- paste0('CHELSA_',b,'_1981-2010_V.2.1.tif')
      download.file(url = paste0('https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/1981-2010/bio/',
                                 n),
                    destfile = paste0('CHELSA_data/',n),
                    method = 'auto')
      #print(url.exists(paste0('https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/1981-2010/bio/',
      #                        n)))
    }
  }

  # Download future data
  if(period == 'future'){
    if(!dir.exists('CHELSA_data')){ dir.create('CHELSA_data') }
    for(b in bioclims) {
      for (y in year) {
        for (g in gcm) {
          for(s in ssp) {
            n <- paste0('CHELSA_',b,'_',y,'_',tolower(g),'_',s,'_V.2.1.tif')
            download.file(url = paste0('https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/',
                                 y, '/', g, '/', s, '/bio/', n),
                          destfile = paste0('CHELSA_data/',n),
                          method = 'auto')
            #print(url.exists(paste0('https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/',
            #                        y, '/', g, '/', s, '/bio/', n)))
          }
        }
      }
    }
  }

  # Download past data
  if(period == 'past'){
    if(!dir.exists('CHELSA_data')){ dir.create('CHELSA_data') }
    for(b in bioclims) {
      for (g in gcm) {
        n <- paste0('CHELSA_PMIP_',g,'_',b,'.tif')
        download.file(url = paste0('https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V1/pmip3/bioclim/',
                                   n),
                      destfile = paste0('CHELSA_data/',n),
                      method = 'auto')
        #print(url.exists(paste0('https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V1/pmip3/bioclim/',
        #                        n)))
      }
    }
  }
}
