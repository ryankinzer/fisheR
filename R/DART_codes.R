#' @title Dart Code Transcription - Helper function
#'
#' @description Transcribe character values into DART codes for: Observation Site, Species, Run, Rear.
#'
#' @author Tyler Stright
#'
#' @param obs_site PTAGIS code of desired observation site
#' @param species desired species
#' @param run desired run
#' @param rear_type desired rear
#' @param life_stage desired life stage
#'
#' @import httr lubridate
#' @export
#' @return NULL
#' @examples
#'
#' DART_codes(obs_site, species, run, rear_type)
DART_codes <- function(obs_site, species, run, rear_type, life_stage) {

  source('./R/DART_sites.R')

  # observation site
  if(!is.null(obs_site)) {
    obs_code_df <- DART_sites('observation')

    obs_name = obs_code_df$site[match(obs_site, obs_code_df$code)]
  } else {obs_name <- NULL}

  # species
  if(!is.null(species)){
    spp_code_df = data.frame(Species = c('Chinook', 'Coho', 'Steelhead', 'Sockeye'),
                             code = 1:4)

    spp_code = spp_code_df$code[match(species, spp_code_df$Species)]
  } else {spp_code <- NULL}

  # run
  if(!is.null(run)){
    run_code_df = data.frame(Run = c('All', 'Spring', 'Summer', 'Fall', 'Winter', 'Unknown'),
                             code = c('Null',1:5))

    run_code = run_code_df$code[match(run, run_code_df$Run)]
  } else {run_code <- NULL}

  # rear
  if(!is.null(rear_type)){
    rear_code_df = data.frame(Rear = c('All','W+H', 'Wild', 'Hatchery', 'Unknown'),
                              code = c('Null', 'WH','W', 'H', 'U'))

    rear_code = rear_code_df$code[match(rear_type, rear_code_df$Rear)]
  } else {rear_code <- NULL}

  # life stage
  if(!is.null(life_stage)){
    life_stage_df = data.frame(life_stage = c('All', 'Adult', 'Juvenile', 'Unknown'),
                              code = c('Null', 'A', 'J', 'U'))

    ls_code = life_stage_df$code[match(life_stage, life_stage_df$life_stage)]
  } else {ls_code <- NULL}

  return(list(obs_name, spp_code, run_code, rear_code, ls_code))
}
