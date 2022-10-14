box::use(RCurl[getURL])
box::use(XML[xmlToList, xmlParse])
box::use(dplyr[left_join])
box::use(glue[glue])

#' @export
routes <- function(locations, api_key) {
  
  mat <- ._get_stages(locations, api_key)
  
  map <- data.frame(x = mat$start, y = mat$end)
  mapping <- TRUE
  paths <- 0
  moves <- map
  
  # Part 1 loop - find all paths visiting locations a maximum of 1 time
  while (mapping) {
    names(map) <- c(paste0("pos",1:(ncol(map) - 1)), "x")
    
    map <- left_join(map, moves, by = "x")
    
    # Check location not visited twice
    for (ii in seq_along(locations)) {
      map <- map[rowSums(map == locations[ii], na.rm = T) < 2,]
    }
    
    # Stop when all paths resolve
    if (ncol(map) == length(locations)) {
      mapping <- FALSE
    } 
  }
  
  names(map) <- c(paste0("pos",1:(ncol(map))))
  
  row.names(map) <- NULL
  
  dist <- vector(mode = "double", length = nrow(map))
  dura <- vector(mode = "double", length = nrow(map))
  
  for (row in seq_len(nrow(map))) {
    
    tmp <- as.character(unlist(map[row,]))
    
    dist_sum <- 0
    dura_sum <- 0
    
    for (ii in seq_len(length(tmp) - 1)) {
      dist_sum <- dist_sum + mat[mat$start == tmp[ii] & mat$end == tmp[ii + 1],]$dist
      dura_sum <- dura_sum + mat[mat$start == tmp[ii] & mat$end == tmp[ii + 1],]$dur
    }
    
    dist[row] <- dist_sum
    dura[row] <- dura_sum
    
  }
  
  map$dist <- dist
  map$dura <- dura
  
  return(map)
}

._drive_detail <- function(start, end, api_key) {
  
  req <- glue(
    "https://dev.virtualearth.net/REST/V1/Routes/Driving?o=xml&wp.0=",
    "{start}",
    "&wp.1=",
    "{end}",
    "&avoid=minimizeTolls&key=",
    "{api_key}" 
  )
  
  res <- getURL(req)
  
  res <- gsub("ï»¿", "", res)
  
  parse <- xmlToList(xmlParse(res))
  
  out <- list(
    dist = as.double(parse$ResourceSets$ResourceSet$Resources$Route$TravelDistance),
    dur = as.double(parse$ResourceSets$ResourceSet$Resources$Route$TravelDuration)
  )
  
  return(out)
  
}

._get_stages <- function(locations, api_key) {
  
  mat <- expand.grid(start = locations, end = locations)
  
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
  
  return(mat)
}