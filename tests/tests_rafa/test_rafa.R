library(sf)
library(uci)
library(data.table)
library(dplyr)
library(ggplot2)
library(mapview)
library(mapedit)
library(leaflet)
library(sp)
library(spdep)



mapviewOptions(platform = 'leafgl')

data_path <- 'R:/Dropbox/other_projects/0_felix_urban_form/data_raw/uci_input'
cities <- list.files(path = data_path, full.names = TRUE)


# 1. EDIT cities to connect 'islands' ------------------------------------------


## 1.1 bogota  ------------------------------------------

# read city
f <- cities[ grep('bog.gpkg', cities) ]
df <- sf::st_read( f )

# check (un)connected polygons
df <- st_make_valid(df)
nb <- spdep::poly2nb(df, queen=TRUE)
nb


df2 <- df[c(321, 623, 624, 625, 626, 627, 628),]

mapview(df) + df2





## 1.1 Rio  ------------------------------------------

# read city
f <- cities[ grep('rio_strip.gpkg', cities) ]
df <- sf::st_read( f )

# check (un)connected polygons
df <- st_make_valid(df)
nb <- spdep::poly2nb(df, queen=TRUE)
nb

# df2 <- df[c(321, 623, 624, 625, 626, 627, 628),]

mapview(df) + df2







## 1.1 Boston  ------------------------------------------

# read city
f <- cities[ grep('sfo.gpkg', cities) ]
df <- sf::st_read( f )

# check (un)connected polygons
df <- st_make_valid(df)
nb <- spdep::poly2nb(df, queen=TRUE)
nb

# df2 <- df[c(321, 623, 624, 625, 626, 627, 628),]

mapview(df) + df2









# 2. Calculate UCI -------------------------------------------------------------

# all files
data_path <- 'R:/Dropbox/other_projects/0_felix_urban_form/data_raw/uci_input'
cities <- list.files(path = data_path, full.names = TRUE)

cities_ok <- c('ber', 'bos', 'lax', 'rio_strip', 'sfo_strip')

get_uci <- function(ct){ # ct='bos'
  
  # select city file
  f <- cities[ grep(ct, cities) ]
  df <- sf::st_read( f )

  # calculate uci
  out <- uci::uci(sf_object = df, 
                  var_name = 'job_count', 
                  dist_type = 'spatial_link', 
                  bootstrap_border = TRUE, 
                  showProgress = TRUE)
  out$city <- ct
  return(out)
  }

out_list <- lapply(X=cities_ok, FUN = get_uci)

out_df <- rbindlist(out_list)



##### Coverage ------------------------
# library(uci)
library(testthat)
library(covr)
Sys.setenv(NOT_CRAN = "true")


# each function separately
t1 <- covr::function_coverage(fun=uci, test_file("tests/testthat/test_uci.R"))
# t2 <- covr::function_coverage(fun=read_airports, test_file("tests/testthat/test_read_airports.R"))
t1

# nocov start

# nocov end

# the whole package
Sys.setenv(NOT_CRAN = "true")
cov <- covr::package_coverage(path = ".", type = "tests", clean = FALSE)
cov

rep <- covr::report()

x <- as.data.frame(cov)
covr::codecov( coverage = cov, token ='aaaaa' )




# checks spelling
library(spelling)
devtools::spell_check(pkg = ".", vignettes = TRUE, use_wordlist = TRUE)

# Update documentation
# devtools::document(pkg = ".")





### Check URL's----------------

urlchecker::url_update()


### CMD Check ----------------
# Check package errors

# LOCAL
Sys.setenv(NOT_CRAN = "true")
devtools::check(pkg = ".",  cran = FALSE, env_vars = c(NOT_CRAN = "true"))

# CRAN
Sys.setenv(NOT_CRAN = "false")
devtools::check(pkg = ".",  cran = TRUE, env_vars = c(NOT_CRAN = "false"))


# quick no vignettes
devtools::check(pkg = ".",  cran = TRUE, env_vars = c(NOT_CRAN = "false"),vignettes = F)

devtools::check_win_release(pkg = ".")

# devtools::check_win_oldrelease()
# devtools::check_win_devel()


beepr::beep()



tictoc::tic()
devtools::check(pkg = ".",  cran = TRUE, env_vars = c(NOT_CRAN = "false"))
tictoc::toc()


# submit to CRAN -----------------
# usethis::use_cran_comments('teste 2222, , asdadsad')


devtools::submit_cran()


# build binary -----------------
system("R CMD build . --resave-data") # build tar.gz







library(dlstats)
library(ggplot2)
x <- dlstats::cran_stats(packages = c( 'cppRouting'))

head(x)
ggplot(x, aes(end, downloads, group=package, color=package)) +
  geom_line() + geom_point(aes(shape=package))
