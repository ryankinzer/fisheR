#' @title Clean DART-queried PIT Tag data
#'
#' @description Clean data queried from DART using the fisheR package.
#'
#' @author Tyler Stright
#'
#' @param data data returned through a fisheR::DART_ function.
#'
#' @import lubridate
#' @export
#' @return NULL
#' @examples
#'
#' clean_dart(dart_df)

clean_DART = function(data){

  data_clean <- data %>%
    mutate(
      stage = gsub(' ','',stage),
      sprrt = ifelse(nchar(sprrt) == '2',paste0(spp_code,sprrt),sprrt),
      species =  case_when(  #sprrt is only 3 characters long with obs dat and 2 characters with release_dat.
        substr(sprrt,1,1) == '1' ~ 'Chinook salmon',
        substr(sprrt,1,1) == '2' ~ 'Coho salmon',
        substr(sprrt,1,1) == '3' ~ 'Steelhead',
        substr(sprrt,1,1) == '4' ~ 'Sockeye salmon',
        TRUE ~ 'Unknown'
      ),
      run =  case_when(
        substr(sprrt,2,2) == '1' ~ 'Spring',
        substr(sprrt,2,2) == '2' ~ 'Summer',
        substr(sprrt,2,2) == '3' ~ 'Fall',
        substr(sprrt,2,2) == '4' ~ 'Winter',
        substr(sprrt,2,2) == '5' ~ 'Unknown',
        TRUE ~ 'Unknown'
      ),
      rear =  case_when(
        grepl('W', sprrt) ~ 'Natural',
        grepl('H', sprrt) ~ 'Hatchery',
        TRUE ~ 'Unknown'
      ),
      obs_time = lubridate::ymd_hms(obs_time),
      release_date = lubridate::ymd(release_date),
      release_year = lubridate::year(release_date),
      min_time = lubridate::ymd_hms(min_time),
      max_time = lubridate::ymd_hms(max_time),
      travel_days = as.numeric(travel_days),
      length = as.numeric(length),
      tag_season = case_when(
        lubridate::month(release_date) <= 6 ~ 'Spring',
        between(lubridate::month(release_date),7,8) ~ 'Summer',
        lubridate::month(release_date) >= 9 ~ 'Fall',
        TRUE ~ 'Unknown'
      )
    )

  return(data_clean)
}
