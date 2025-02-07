library(golem)
library(rmarkdown)
colcounter <- EJAM:::colcounter
unlink("tests/shinytestlog.txt")
shinytest2::load_app_env()