library(tidyverse)

compose_action_df = function(TF = "~/PhD/proj/code/pre-study/prestudy_app/responses/", SF = list.files(TF)[-c(1, 2)]){ # drop lottery consents and followups){
  ds <- do.call(
    rbind, # combine all dataframes into one
    lapply(SF, function(x) { # iterate over the subfolders 
      file_path <- paste(TF, x, "/log_user.csv", sep = "") # file path
      read.csv(file_path) # read the CSV from the file path
    }))
  return(ds)
}

elicit_respondents = function(ds = ds, no_timestamp = TRUE){
  resp = ds |> 
    filter(action %in% c('age', 'gender', 'gender_oth', 'subj', 'edu_year', 'occup', 'family_acad', 'orient')) |> 
    pivot_wider(names_from = action, values_from = value) |> 
    (\(x) if (no_timestamp) select(x, -timestamp) else x)() # make select only if no_timestamp == TRUE
  return(resp)
}

institutes = list(
  'Faculty of Fine Art, Music and Design' = c('Department of Design', 'Department of Contemporary Art', 'Department of Music'),
  'Faculty of Humanities' = c('Department of Archaeology, History, Cultural Studies and Religion', 'Department of Foreign Languages', 'Department of Linguistic, Literary and Aesthetic Studies', 'Department of Philosophy'),
  'Faculty of Law' = c('Faculty of Law'),
  'Faculty of Mathematics and Natural Sciences' = c('Department of Biological Sciences', 'Department of Chemistry', 'Department of Earth Science', 'Department of Informatics', 'Department of Mathematics', 'Department of Physics and Technology', 'Geophysical Institute'), 
  'Faculty of Medicine' = c('Department of Biomedicine', 'Department of Clinical Dentistry', 'Department of Clinical Medicine', 'Department of Clinical Science', 'Department of Global Public Health and Primary Care'), 
  'Faculty of Psychology' = c('Department of Biological and Medical Psychology', 'Department of Clinical Psychology', 'Department of Education', 'Department of Health Promotion and Development (HEMIL)', 'Department of Psychosocial Science'), 
  'Faculty of Social Sciences' = c('Department of Comparative Politics', 'Department of Economics', 'Department of Geography', 'Department of Government', 'Department of Information Science and Media Studies', 'Department of Social Anthropology', 'Department of Sociology'))  

institute_lookup <- do.call(rbind, lapply(names(institutes), function(inst) {
  data.frame(institute = inst, dep = institutes[[inst]], stringsAsFactors = FALSE)
}))

generate_department_colors <- function(df, institute_colors) {
  df_with_colors <- df %>%
    left_join(institute_colors, by = "institute")
  
  # Split the dataframe by institute
  department_colors <- df_with_colors %>%
    group_by(institute) %>%
    group_map(~ {
      # Generate gradient colors for each department within the institute
      departments <- .x$subj
      base_color <- .x$color[1]  # Get the base color for the institute
      n_departments <- nrow(.x)  # Number of departments in the institute
      
      # Create gradient for departments within the institute
      gradient_colors <- colorRampPalette(c("white", base_color))(n_departments +1)[-1]
      
      # Return a named vector of department colors
      setNames(gradient_colors, departments)
    }) |> 
    unlist()
  
  return(department_colors)
}