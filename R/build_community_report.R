

#' Generate EJScreen Multisite (EJAM) Summary Report in HTML within shiny app
#' 
#' Creates a 2 page report on overall results or for one site, with demographic and environmental indicators, and EJ Indexes if needed.
#' 
#' @details For the same function for use in RStudio, 
#' see [ejam2report()] which relies on `build_community_report()`
#' 
#' This function gets called by 
#'  app_server,
#'  ejam2report(), and
#'  community_report_template.Rmd
#' 
#' @param output_df single row of results table from doaggregate - either results_overall or one row of bysite
#' @param analysis_title title to use in header of report
#' @param totalpop total population included in location(s) analyzed
#' @param locationstr description of the location(s) analyzed
#' @param include_ejindexes whether to build tables for EJ and EJ supp. indexes
#' @param in_shiny whether the function is being called in or outside of shiny - affects location of header
#' @param filename path to file to save HTML content to; if null, returns as string (used in Shiny app)
#' 
#' @param show_ratios_in_report logical, whether to add columns with ratios to US and State overall values, in main table of envt/demog. info.
#' @param extratable_show_ratios_in_report logical, whether to add columns with ratios to US and State overall values, in extra table of demog. subgroups, etc.
#' 
#' @param extratable_title Text of overall title of report table, in extra table of demog. subgroups, etc.
#' 
#' @param extratable_list_of_sections This defines what extra indicators are shown. 
#'   It is a named list of vectors, 
#'   where each name is text phrase that is title of a section of the table,
#'   and each vector is the vector of colnames of output_df that are indicators 
#'   to show in that section, in extra table of demog. subgroups, etc.
#'   
#' @param extratable_hide_missing_rows_for only for the indicators named in this vector, 
#'   leave out rows in table where raw value is NA, 
#'   as with many of names_d_language, in extra table of demog. subgroups, etc.' 
#' @seealso [ejam2report()]
#' 
#' @keywords internal
#' @export
#' 
build_community_report <- function(output_df, analysis_title = "Report", totalpop, locationstr, 
                                   include_ejindexes = FALSE, in_shiny = FALSE, 
                                   # ejscreen_vs_ejam_caveat = "Note: Some numbers as shown on the EJScreen report for a single location will in some cases appear very slightly different than in multisite reports. All numbers shown in both types of reports are estimates, and any differences are well within the range of uncertainty inherent in the American Community Survey data as used in EJScreen. Slight differences are inherent in very quickly calculating results for multiple locations.",
                                   # diesel_caveat = paste0("Note: Diesel particulate matter index is from the EPA's Air Toxics Data Update, which is the Agency's ongoing, comprehensive evaluation of air toxics in the United States. This effort aims to prioritize air toxics, emission sources, and locations of interest for further study. It is important to remember that the air toxics data presented here provide broad estimates of health risks over geographic areas of the country, not definitive risks to specific individuals or locations. More information on the Air Toxics Data Update can be found at: ",
                                   # url_linkify("https://www.epa.gov/haps/air-toxics-data-update", "https://www.epa.gov/haps/air-toxics-data-update")),
                                   filename = NULL,
                                   show_ratios_in_report = FALSE,
                                   extratable_show_ratios_in_report = FALSE,
                                   extratable_title = 'EJScreen environmental and socioeconomic indicators data',
                                   extratable_list_of_sections = list(
                                     `Breakdown by Race/Ethnicity` = names_d_subgroups,
                                     `Language Spoken at Home` = names_d_language,
                                     `Language in Limited English Speaking Households` = names_d_languageli,
                                     `Breakdown by Sex` = c('pctmale','pctfemale'),
                                     `Health` = names_health,
                                     `Community` = names_community[!(names_community %in% c( "pctmale", "pctfemale", "pctownedunits_dupe"))],
                                     'Poverty' = names_d_extra #,  # pctpoor
                                     # `Site counts and distance` = names_e_other
                                   ),
                                   extratable_hide_missing_rows_for = c(names_d_language, names_health)
) {

  ## check that analysis was run with EJ columns; if not, don't add them
  if (include_ejindexes) {
    ejcols <- c(names_ej,      names_ej_state, 
                names_ej_supp, names_ej_supp_state)
    if (!(all(ejcols %in% names(output_df)))) {
      include_ejindexes <- FALSE
    } 
  }
  
  output_df_rounded <-   as.data.frame(output_df)
  output_df_rounded <- format_ejamit_columns(output_df_rounded, names(output_df_rounded))
  if (missing(locationstr)) {
    warning('locationstr parameter missing')
    locationstr <- ""
  }
  if (missing(totalpop)) {
    if ("pop" %in% names(output_df_rounded)) {
      totalpop <- prettyNum(round(output_df_rounded$pop, 0), big.mark = ',')  
    } else {
      warning('totalpop parameter or output_df_rounded$pop is required')
      totalpop <- "NA"
    }
  }
  
  ############################################################# #
  
  # 1. Overall header ####
  
  full_page <- paste0(
    generate_html_header(analysis_title = analysis_title, totalpop = totalpop, locationstr = locationstr, in_shiny = in_shiny), #   report_title if in shiny is .community_report_title or outside shiny is eg "EJAM Multisite Report"
    
    ############################################################# #
  
  # 2. main Envt & Demog table ####
  
    generate_demog_header(), # title = 'Environmental and Socioeconomic Indicators Data'
    
    fill_tbl_full(output_df = output_df_rounded, 
                  show_ratios_in_report = show_ratios_in_report
    ),
    collapse = ''
  )
  ############################################################# #
  
  # 2. EJ index table ####
  
  ## add EJ index and Supp EJ index tables 
  ## only if those columns are available
  if (include_ejindexes) {
    full_page <- paste0(full_page,
                        generate_ej_header(),
                        fill_tbl_full_ej(output_df_rounded), 
                        #generate_ej_supp_header(),
                        #fill_tbl_full_ej_supp(output_df_rounded),
                        collapse = '') 
  }
  ############################################################# #
  
  # 3. Subgroups and Additional info table ####
  
  full_page <- paste0(
    full_page,
    fill_tbl_full_subgroups(output_df = output_df_rounded, 
                            title =  extratable_title, 
                            list_of_sections      = extratable_list_of_sections,
                            extratable_show_ratios_in_report = extratable_show_ratios_in_report,
                            hide_missing_rows_for = extratable_hide_missing_rows_for
                            ), 
    ############################################################# #
    
    # 4. footnote ####
    
    generate_report_footnotes(
      # ejscreen_vs_ejam_caveat = "Note: Some numbers as shown on the EJScreen report for a single location will in some cases appear very slightly different than in EJScreen's multisite reports. All numbers shown in both types of reports are estimates, and any differences are well within the range of uncertainty inherent in the American Community Survey data as used in EJScreen. Slight differences are inherent in very quickly calculating results for multiple locations.",
      diesel_caveat = paste0("Note: Diesel particulate matter index is from the EPA's Air Toxics Data Update, which is the Agency's ongoing, comprehensive evaluation of air toxics in the United States. This effort aims to prioritize air toxics, emission sources, and locations of interest for further study. It is important to remember that the air toxics data presented here provide broad estimates of health risks over geographic areas of the country, not definitive risks to specific individuals or locations. More information on the Air Toxics Data Update can be found at: ",
                             url_linkify("https://www.epa.gov/haps/air-toxics-data-update", "https://www.epa.gov/haps/air-toxics-data-update"))
    ),
    collapse = ''
  )
  ############################################################# #
  if (is.null(filename)) {
    return(HTML(full_page))
  } else {
    junk <- capture.output({
      cat(HTML(full_page))
    })
  }
}
