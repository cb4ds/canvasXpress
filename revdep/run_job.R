# Script to Run In A Separate Job

library(revdepcheck)

# uncomment to reset
# revdepcheck::revdep_reset()

revdep_check(quiet = FALSE, num_workers = 2, pkg = ".", timeout = as.difftime(120, units = "mins"))
