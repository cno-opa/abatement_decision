library(rvest)

scrape <- function(x, root, node) {
  
  if (str_length(x) < 5) {
    return("")
  } else {
  
    url <- str_c(root, x)
    out <- vector(mode = "character", length = 1)
    
    tryCatch(
      expr = {
        
        text <- read_html(url) %>% 
          # locate style element on web page
          html_nodes(node) %>%
          html_text()
        
        if (length(text > 0)) {
          out <- text[1]
        } else {out <- ""}
  
      },
      error = function(e) {
        print(writeLines("\nError\n"))
        out <- ""
      },
      finally = {
        print(x)
        print(url)
        print(out)
        return(out)
      }
    )
  }
}


