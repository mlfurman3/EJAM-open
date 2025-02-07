# Launch the ShinyApp (Do not remove this comment)

# This app.R file is used by the RStudio Connect server to launch the app since the 
#                 EJAM
#  app is a package unlike a typical shiny app,
#  and run_app() is loaded as an exported function that actually runs the app,
#  and while shiny normally sources all files in the /R folder, 
#  here _disable_autoload.R is used to avoid that 

options( "golem.app.prod" = FALSE)

library(EJAM)
run_app(isPublic = TRUE)
