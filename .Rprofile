biocSource <- function () {
    source("https://bioconductor.org/biocLite.R")
}

options(
  usethis.full_name = "Sam Brightman",
  usethis.description = list(
    `Authors@R` = 'person("Sam", "Brightman", email = "sam.brightman@gmail.com", role = c("aut", "cre"))',
     Version = "0.0.0.9000"
  ),
  usethis.protocol  = "ssh",
  Ncpus = max(1, parallel::detectCores(logical = TRUE) %/% 2 - 1)
)
