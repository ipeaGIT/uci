#aop <- aopdata::read_landuse(city = 'all', year = 2019, geometry = T)
#st_write(aop, 'aop.gpkg')

aop <- st_read('aop.gpkg')

test <- function(city){ # city = 'mac'
  
  message(paste0(city))
  temp_city <- subset(aop, abbrev_muni == city)
  
  # temp_city2 <- subset(temp_city, T001 >0 )
  # conc <- concaveman::concaveman(temp_city2)
  # conc <- st_zm(conc)
  # temp_city <- st_crop(temp_city, conc)
  # rm(temp_city2)
  
  a <- uci2(sf_object = temp_city, var_name = 'T001', full_border = FALSE, parallel = T)
  gc()
  gc()
  
  b <- uci2(sf_object = temp_city, var_name = 'T001', full_border = TRUE, parallel = F)
  gc()
  gc()
  

    
  
  a$full_border <- FALSE
  b$full_border <- TRUE
  
  temp_df <- rbind(a,b)
  # temp_df <- b
  temp_df$city <- city
  #head(temp_df)

  fwrite(temp_df, file = "temp_df.csv", append = T)
  return(temp_df)
}


# ttt <- pbapply::pblapply(X = c('nat', 'rec', 'sgo', 'for', 'bho', 'gua', 'mac', 'duq', 'cur'),
#                          FUN = test)

ttt <- pbapply::pblapply(X = unique(aop$abbrev_muni),
                         FUN = test)

df <- rbindlist(ttt)


# 
# setDT(aop)
# aop[, .N , by=abbrev_muni][order(N)]
# aop <- st_sf(aop)



df <-   fread(file = "temp_df.csv")



df[,  UCI / shift(UCI, type = 'lead'), by =city]$V1 |> summary()


df[,  spatial_separation_max / shift(spatial_separation_max, type = 'lead'), 
   by =city]$V1 |> summary()
