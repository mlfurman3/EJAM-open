
##############################################  #
# Function to copy necessary files to temp directory

## can this get replaced by just using references using app_sys( ) instead of requiring the files be in the same folder as the html or rmd file?
# setup_temp_files() is now called report_setup_temp_files() and defined outside server file

report_setup_temp_files <- function(Rmd_name = 'community_report_template.Rmd', Rmd_folder = 'report/community_report/') {
  # or Rmd_name = 'barplot_report_template.Rmd' for single site barplot report
  tempReport <- file.path(tempdir( ), Rmd_name)
  # Copy Rmd file to temp directory
  file.copy(from = EJAM:::app_sys(paste0(Rmd_folder, Rmd_name)),
            to = tempReport, overwrite = TRUE)
  # Copy CSS file
  if (!file.exists(file.path(tempdir( ), 'communityreport.css'))) {
    file.copy(from = EJAM:::app_sys(paste0(Rmd_folder, 'communityreport.css')),
              to = file.path(tempdir( ), 'communityreport.css'), overwrite = TRUE)
  }
  
  # Copy logo file
  if (!file.exists(file.path(tempdir( ), 'www', 'EPA_logo_white_2.png'))) {
    dir.create(file.path(tempdir( ), 'www'), showWarnings = FALSE, recursive = TRUE)
    file.copy(from = EJAM:::app_sys(paste0(Rmd_folder, 'EPA_logo_white_2.png')),
              to = file.path(tempdir( ), 'www', 'EPA_logo_white_2.png'), overwrite = TRUE)
  }
  return(tempReport)
}
##############################################  #


# Function to generate HTML content ***
# Modified community_download function to accept a row_index parameter and work outside shiny
# FUNCTION TO RENDER HTML REPORT ####


report_community_download <- function(file,
                                      row_index = NULL, 
                                      inshiny = FALSE, 

                                      input_analysis_title,
                                      input_include_ejindexes,
                                      input_plotkind_1pager,
                                      input_Custom_title_for_bar_plot_of_indicators,
                                      input_circleweight_in,
                                      
                                      react_cur_button, # event button asking for a 1-site report; was isolated in one line, not in another
                                      
                                      react_sanitized_analysis_title,
                                      react_sanitized_bt_rad_buff,    # radius
                                      react_total_pop,
                                      react_submitted_upload_method,  # points vs shapefile etc.
                                      
                                      react_data_uploaded,
                                      react_data_processed,
                                      react_data_summarized,
                                      react_ratio.to.us.d,
                                      
                                      react_report_map,         # map of all sites Overall
                                      # single_location_map( )  # map of 1 site; function inside here that had been a reactive
                                      
                                      # v1_summary_plot_report, #     # barplot   USA ratios for Overall; function inside here that had been a reactive 
                                      react_v1_summary_plot,          # barplot   USA ratios for 1 site
                                      react_v1_summary_plot_state,    # barplot State ratios for 1 site
                                      input_show_ratios_in_report,
                                      input_extratable_show_ratios_in_report
                                      
) {
  
  if (inshiny) {
    # Create a progress object
    progress <- shiny::Progress$new( )
    progress$set(message = "Generating report", value = 0)
    # Ensure that the progress bar is closed when we exit this function
    on.exit(progress$close( ))
    progress$set(value = 0.1, detail = "Setting up temporary files...")
  }
  
  if (!is.null(row_index)) {
    # single-site report
    tempReport <- report_setup_temp_files(Rmd_name = 'barplot_report_template.Rmd')
  } else {
    # overall summary multisite report
    tempReport <- report_setup_temp_files(Rmd_name = 'community_report_template.Rmd')
  }
  
  if (inshiny) {progress$set(value = 0.2, detail = "Defining parameters...")}
  # Define parameters for Rmd rendering
  rad <- react_data_processed$results_overall$radius.miles
  if (inshiny) {progress$set(value = 0.3, detail = "Adjusting data...")}
  ################################################################### #
  
  # Adjust the data based on whether a specific row is selected
  
  if (!is.null(row_index)) {
    
    ## > report on just 1 site ####
    
    output_df <- react_data_processed$results_bysite[row_index, ]
    popstr <- prettyNum(round(output_df$pop, 0), big.mark = ',')
    
    # Get the name of the selected location
    location_name <- output_df$statename
    
    locationstr <- paste0("Residents within ", rad, " mile", ifelse(rad > 1, "s", ""), 
                          " of this ", ifelse(react_submitted_upload_method == 'SHP', "polygon", 
                                              ifelse(react_submitted_upload_method == 'FIPS', "Census unit", "point")))
    
    # Create a filtered version of react_report_map for single location #####################  #
    
  if (inshiny) {progress$set(value = 0.4, detail = "Creating map...")}
    
    ################################################################### #          was a REACTIVE inside overall reactive
   
    ################################################################### #          was a REACTIVE inside overall reactive    
    
    map_to_use <- map_single_location(row_index = row_index, 
                                      inshiny = inshiny, 
                                      input_circleweight_in,
                                      react_sanitized_bt_rad_buff,    # radius
                                      react_submitted_upload_method,  # points vs shapefile etc.
                                      react_data_uploaded,
                                      react_data_processed)
    # end of map code  #####################  #
    
    ## make sure the barplots are for the 1 selected site
    
    # react_v1_summary_plot  
    
    # react_v1_summary_plot_state  
    
    params <- list(
      # output_df = output_df,
      analysis_title = input_analysis_title,
      totalpop = popstr,
      locationstr = locationstr,
      # include_ejindexes = (input_include_ejindexes == 'TRUE'),
      in_shiny = FALSE,
      filename = NULL,
      map = map_to_use,
      summary_plot       = react_v1_summary_plot,
      summary_plot_state = react_v1_summary_plot_state
    )
    # end of report on 1 site 
    
  } else {
    ################################################################### #
    
    ## > report on just all sites overall ####
    
    output_df <- react_data_processed$results_overall
    popstr <- prettyNum(round(react_total_pop, 0), big.mark = ',')
    locationstr <- paste0("Residents within ", rad, " mile", ifelse(rad > 1, "s", ""), 
                          " of any of the ", NROW(react_data_processed$results_bysite[react_data_processed$results_bysite$valid == T,]), 
                          " selected ", ifelse(react_submitted_upload_method == 'SHP', "polygons", 
                                               ifelse(react_submitted_upload_method == 'FIPS', "shapes", "points")))
    map_to_use <- react_report_map
    
    ################################################################### #           was a REACTIVE inside overall reactive    
  
    ################################################################### #           was a REACTIVE inside overall reactive      
    
    params <- list(
      output_df = output_df,
      analysis_title =  react_sanitized_analysis_title,
      totalpop = popstr,
      locationstr = locationstr,
      include_ejindexes = (input_include_ejindexes == 'TRUE'),
      in_shiny = FALSE,
      filename = NULL,
      map = map_to_use,
      summary_plot = v1_summary_plot_report( row_index, 
                                             input_plotkind_1pager,
                                             input_Custom_title_for_bar_plot_of_indicators,
                                             react_cur_button, # event button asking for a 1-site report; was isolated in one line, not in another
                                             react_sanitized_bt_rad_buff,    # radius
                                             react_data_processed,
                                             react_data_summarized,
                                             react_ratio.to.us.d),
      show_ratios_in_report = input_show_ratios_in_report,
      extratable_show_ratios_in_report = input_extratable_show_ratios_in_report
    )
  }
  
  # Render Rmd to HTML
  rmarkdown::render(tempReport,
                    output_format = 'html_document',
                    output_file = file,
                    params = params,
                    envir = new.env(parent = globalenv( )),
                    intermediates_dir = tempdir( )
  )
}

map_single_location <- function( row_index = NULL, 
                                 inshiny = FALSE, 
                                 input_circleweight_in,
                                 react_sanitized_bt_rad_buff,    # radius
                                 react_submitted_upload_method,  # points vs shapefile etc.
                                 react_data_uploaded,
                                 react_data_processed
                                 ) {
  if (inshiny) {
    # shiny::req(react_data_processed)
    shiny::validate(shiny::need(react_data_processed, 'Please run an analysis to see results.'))
  }
  filtered_data <- react_data_processed$results_bysite[row_index, ]
  
  ####################### #        ####################### #
  if (react_submitted_upload_method == "SHP") {
    # Handle shapefile case
    
    shp_valid <- react_data_uploaded[react_data_uploaded$ejam_uniq_id == filtered_data$ejam_uniq_id, ]
    d_up <- shp_valid
    d_up_geo <- d_up[,c("ejam_uniq_id","geometry")]
    d_merge = merge(d_up_geo, filtered_data, by = "ejam_uniq_id", all.x = FALSE, all.y = TRUE)
    
    popup_labels <- fixcolnames(namesnow = setdiff(names(d_merge),c('geometry', 'valid', 'invalid_msg')), oldtype = 'r', newtype = 'shortlabel')
    rad_buff <- react_sanitized_bt_rad_buff
    
    if (!is.na(rad_buff) && rad_buff > 0) {
      d_uploads <- sf::st_buffer(d_merge[d_merge$valid == T, ], dist = units::set_units(rad_buff, "mi"))
      map <- leaflet(d_uploads) %>% addTiles( ) %>%
        addPolygons(data = d_uploads, color = '#000080',
                    popup = popup_from_df(d_uploads %>% sf::st_drop_geometry( ) %>% dplyr::select(-valid, -invalid_msg), labels = popup_labels),
                    popupOptions = popupOptions(maxHeight = 200))
    } else {
      data_spatial_convert <- d_merge[d_merge$valid == T, ] %>%
        dplyr::select(-valid, -invalid_msg) %>%
        sf::st_zm( ) %>% as('Spatial')
      map <- leaflet(data_spatial_convert) %>% addTiles( ) %>%
        addPolygons(color = '#000080',
                    popup = popup_from_df(data_spatial_convert %>% sf::st_drop_geometry( ),
                                          labels = popup_labels),
                    popupOptions = popupOptions(maxHeight = 200))
    }
    
    # Get the bounding box of the shape
    bbox <- sf::st_bbox(d_merge)
    map %>% fitBounds(bbox[1], bbox[2], bbox[3], bbox[4]) %>%
      setView(lng = mean(c(bbox[1], bbox[3])), 
              lat = mean(c(bbox[2], bbox[4])), 
              zoom = 14)  
    
    ####################### #        ####################### #
  } else if (react_submitted_upload_method != "FIPS") {
    # Handle non-FIPS case
    
    popup_labels <- fixcolnames(namesnow = names(filtered_data), oldtype = 'r', newtype = 'shortlabel')
    popup_labels[is.na(popup_labels)] <- names(filtered_data)[is.na(popup_labels)]
    
    map <- leaflet(filtered_data) %>%
      addTiles( ) %>%
      addCircles(
        radius = 1 * meters_per_mile,
        color = '#000080', fillColor = '#000080',
        fill = TRUE, weight = input_circleweight_in,
        popup = popup_from_df(
          filtered_data %>%
            dplyr::mutate(dplyr::across(
              dplyr::where(is.numeric), \(x) round(x, digits = 3))),
          labels = popup_labels),
        popupOptions = popupOptions(maxHeight = 200)
      )
    
    # Set view with a slightly more zoomed out level
    map %>% setView(lng = filtered_data$lon, 
                    lat = filtered_data$lat, 
                    zoom = 14) 
    
  } else {
    ####################### #        ####################### #
    # FIPS case
    
    leaflet( ) %>% addTiles( ) %>% fitBounds(-115, 37, -65, 48)
  }
} # end of map code  ########  #

v1_summary_plot_report <- function( row_index = NULL, 
                                    input_plotkind_1pager,
                                    input_Custom_title_for_bar_plot_of_indicators,
                                    react_cur_button, # event button asking for a 1-site report; was isolated in one line, not in another
                                    react_sanitized_bt_rad_buff,    # radius
                                    react_data_processed,
                                    react_data_summarized,
                                    react_ratio.to.us.d
                                    ) {
  
  # shiny::req(react_data_summarized # it used to say this is required here but I dont think it actually is used
  # shiny::req(react_data_processed) #  ***
  ## react_data_processed needed for ridgeline or boxplot, and react_ratio.to.us.d which is made from react_data_processed is needed for boxplots,
  
  if (input_plotkind_1pager == 'bar') {
    if (!is.null(react_cur_button)) {##################################### #           was a REACTIVE inside overall reactive
      # selected_row <- as.numeric(gsub('button_', '', isolate(react_cur_button )))##################################### #    ***       was a REACTIVE inside overall reactive
      selected_row <- as.numeric(gsub('button_', '',          (react_cur_button )))##################################### #          was a REACTIVE inside overall reactive
      plot_barplot_ratios_ez(
        out = react_data_processed,
        varnames = c(names_d_ratio_to_avg, names_d_subgroups_ratio_to_avg),
        main = "Demographics at the Analyzed Location \n Compared to US Overall",
        single_location = TRUE,
        row_index = selected_row
      )
    } else {
      
      if (input_Custom_title_for_bar_plot_of_indicators == '') {
        
        #Default way
        plot_barplot_ratios_ez(
          out = react_data_processed,
          varnames = c(names_d_ratio_to_avg, names_d_subgroups_ratio_to_avg), 
          main = "Demographics at the Analyzed Location \n Compared to US Overall"
        )
        
      } else {
        #If there is a new title in advanced settings
        plot_barplot_ratios_ez(
          out = react_data_processed,
          varnames = c(names_d_ratio_to_avg, names_d_subgroups_ratio_to_avg), 
          main = input_Custom_title_for_bar_plot_of_indicators
        )
      }
      
    }
  }
  else if (input_plotkind_1pager == 'ridgeline') {
    
    ## ratios by site  (demog each site / demog avg in US)
    ratio.to.us.d.bysite <- react_data_processed$results_bysite[ ,  c(
      ..names_d_ratio_to_avg,
      ..names_d_subgroups_ratio_to_avg
    )]
    plot_ridgeline_ratios(ratio.to.us.d.bysite)
    
  } else if (input_plotkind_1pager == "box") {
    
    ## *BOXPLOTS for short report (all sites D ratios vs US avg) ####
    ejam2boxplot_ratios(react_data_processed, react_sanitized_bt_rad_buff, varnames = c(names_d, names_d_subgroups))
  }# box
} 