# install helpers (modified from https://rdrr.io/cran/ISLR2/src/inst/helpers/install.R)
source("install_helpers.R")

# remove existing installations
tryCatch(
  remove.packages(c("keras", "tensorflow", "reticulate")),
  error = function(e) "Some or all packages not previously installed, that's ok!"
)

# install keras
install.packages("keras", repos = 'https://cloud.r-project.org')

# update environment variables
write('RETICULATE_AUTOCONFIGURE=FALSE', file = "~/.Renviron", append = TRUE)
write(sprintf('RETICULATE_MINICONDA_PATH=%s',
              normalizePath("~/islr-miniconda", winslash = "/", mustWork = FALSE)),
      file = "~/.Renviron", append = TRUE)
Sys.setenv(RETICULATE_AUTOCONFIGURE='FALSE',
           RETICULATE_MINICONDA_PATH=normalizePath("~/islr-miniconda", 
                                                   winslash = "/", 
                                                   mustWork = FALSE))

# install separate miniconda, called islr-miniconda
install_miniconda()

# install Tensorflow
install_tensorflow()

# check Python configuration
print_py_config()