# save issues list

save_issues = function(repo = "USEPA/EJAM", when = Sys.Date()) {
 
  
### require(githubr) # AVOID CREATING ANOTHER DEPENDENCY
  
snapshot_of_issues_full <- issues <-  get_issues(repo)
fname = paste0("data-raw/", when, "_snapshot_of_issues_all.rda")
save(snapshot_of_issues_full, 
     file = fname)
cat('saved detailed info about open issues as a list in', fname, '\n')

# simpler format:
issues_df = data.frame(
  id =   1:length(issues$id), 
  number = sapply(issues$number, function(x) x[[1]]),  
  state  = sapply(issues$state, function(x) x[[1]]), 
  title  = sapply(issues$title, function(x) x[[1]]), 
  labels = sapply(seq_along(issues$labels), function(x) {
    paste0(sapply(issues$labels[x][[1]], function(y) y$name), collapse = "|")
    }))

print(head(issues_df))

fname = paste0("data-raw/", when, "_snapshot_of_issues_df.rda")
save(issues_df , 
     file = fname )
cat('saved basic info about open issues as a data.frame in', fname, '\n')

invisible(issues_df)

}

issues_df <- save_issues()
