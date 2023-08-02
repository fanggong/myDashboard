
.format_date <- function(date = Sys.Date()) {
  format(date, "%Y%m%d")
}

.format_date2 <- function(date = Sys.Date()) {
  format(date, "%Y-%m-%d")
}

.as_character <- function(datetime = Sys.time()) {
  as.character(datetime, tz = "Asia/Shanghai")
}


.str_replace <- function(str, pattern, replacement) {
  stringr::str_replace_all(str, paste0("\\{", pattern, "\\}"), replacement)
}


.format_number <- function(num, digits = 2) {
  format(round(num, digits = digits), nsmall = 2)
}


.mid_date <- function(start, end) {
  start <- as.Date(start)
  end <- as.Date(end)
  res <- start + (end - start) / 2
  res
}


.format_partition <- function(par) {
  res <- NULL
  for (i in 1:length(par)) {
    if (length(par[[i]]) == 1) {
      res <- c(res, paste0(names(par)[i], " = '", par[[i]], "'"))
    } else if (length(par[[i]] == 2)) {
      res <- c(res, paste0(names(par)[i], " between ", paste(paste0("'", par[[i]], "'"), collapse = " and ")))
    } else {
      stop("Partition format is not right")
    }
  }
  res <- paste(res, collapse = " and ")
  res
}


.ifelse <- function(test, yes, no) {
  mapply(ifelse, test = test, yes = as.character(yes), no = as.character(no))
}





