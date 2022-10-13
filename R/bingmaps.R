box::use(tdy = tidyr[pivot_wider])


api_key <- readLines("~/.cache/api_keys/bing_maps.txt")

._drive_detail <- function(start, end, api_key) {
  
  req <- glue::glue(
    "https://dev.virtualearth.net/REST/V1/Routes/Driving?o=xml&wp.0=",
    "{start}",
    "&wp.1=",
    "{end}",
    "&avoid=minimizeTolls&key=",
    "{api_key}" 
  )
  
  res <- RCurl::getURL(req)
  
  res <- gsub("ï»¿", "", res)
  
  parse <- XML::xmlToList(XML::xmlParse(res))
  
  out <- list(
    dist = as.double(parse$ResourceSets$ResourceSet$Resources$Route$TravelDistance),
    dur = as.double(parse$ResourceSets$ResourceSet$Resources$Route$TravelDuration)
  )
  
  return(out)
  
}

._to_matrix <- function(data, values_col) {
  
  data <- tdy$pivot_wider(data, names_from = end, values_from = values_col)
  
  rn <- as.character(data$start)
  
  data <- as.matrix(data[2:ncol(data)])
  
  row.names(data) <- rn
  
  return(data)
}

get_matrices <- function(locations, api_key) {
  
  mat <- expand.grid(start = locs, end = locs)
  
  len <- length(mat)
  
  mat[c("dist", "dur")] <- NA
  
  mat[mat$start == mat$end, c("dist", "dur")] <- 0
  
  for (ii in seq_len(nrow(mat))) {
    
    if (!is.na(mat$dist[ii])) {
      
      next
      
    } else {
      
      dd <- ._drive_detail(
        start =  mat$start[ii],
        end = mat$end[ii],
        api_key = api_key
      )
      
      mat$dist[ii] <- dd$dist
      mat$dur[ii] <- dd$dur
      
    }
  }
  
  dist <- ._to_matrix(mat[c("start","end","dist")], "dist")
  dur <- ._to_matrix(mat[c("start","end","dur")], "dur")
  
  return(list(dist = dist, dur = dur))
  
}

# just trying things out

locs <- c()

t1 <- get_matrices(locs,key)


pos <- rownames(t1$dur)


routes <- expand.grid(lapply(1:5, function(x) pos))

routes <- routes[routes$Var1 == "start_point",] # would be start point

routes$uniques <- NA

for (ii in seq_len(nrow(routes))) {
  
  routes$uniques[ii] <- length(unique(as.vector(routes[ii,1:(ncol(routes)-1)])))
  
}

routes <- routes[routes$uniques == (ncol(routes)-1), !names(routes) %in% c("uniques")]


