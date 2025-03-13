##======================================================================##
## Retrieve Google Machine Learning Glossary as Anki Deck
## https://developers.google.com/machine-learning/glossary
##======================================================================##


## Load packages
library(dplyr)
library(rvest)


#' Function to restrict stringr::str_replace() to between two values

str_replace_between <- function(string, pattern = "\\n", replacement = "<br>", left = "<devsite-code>", right = "</devsite-code>"){
  
  ## Split by left and right
  split_left <- strsplit(string, left)[[1]]
  split_left_right <- strsplit(split_left, right)
  
  ## Replace portion between left and right, and re-merge with right
  split_left[-1] <- sapply(split_left_right[-1], function(x){
    
    x[1] <- stringr::str_replace(x[1], pattern, replacement)
    paste(x, collapse = right)
  })
  
  ## Re-merge with left
  return(paste(split_left, collapse = left))
}


## URL of the glossary page
glossary_url <- "https://developers.google.com/machine-learning/glossary"

## Read the webpage
doc <- read_html(glossary_url)

## Extract terms (h2 elements with class 'hide-from-toc')
terms <- doc %>% 
  html_elements("h2.hide-from-toc")

term_ids <- terms %>% 
  html_attr("id")

term_texts <- terms %>% 
  html_text(trim = TRUE)

## Get all element classes
doc %>%
  html_nodes("*") %>%
  html_attr("class") %>%
  unique()
  
## Extract paragraph elements
p <- doc %>% 
  rvest::html_elements("p") %>% 
  as.character() %>%
  `[`(-length(.) + c(0, 1))  # Trim license and last updated

## Paragraph elements that correspond to terms (exclude alphabetical glossary)
anchors <- p[grepl("glossary-anchor", p)] %>%
  `[`(!. %in% sprintf("<p><a class=\"glossary-anchor\" name=\"%s\"></a>\n</p>\n", 
                      letters))

## Initialize deck
deck <- data.frame(id = term_ids) %>%
  mutate(text = term_texts, 
         anchor = anchors,
         term_url = sprintf("%s#%s", glossary_url, id),
         term = sprintf('<a href=\"%s\"><strong>%s</strong></a>', term_url, text),
         definition = "",
         tags = "")

## Get all elements from first anchor to before license
license <- "<p>Except as otherwise noted, the content of this page is licensed under the <a href=\"https://creativecommons.org/licenses/by/4.0/\">Creative Commons Attribution 4.0 License</a>, and code samples are licensed under the <a href=\"https://www.apache.org/licenses/LICENSE-2.0\">Apache 2.0 License</a>. For details, see the <a href=\"https://developers.google.com/site-policies\">Google Developers Site Policies</a>. Java is a registered trademark of Oracle and/or its affiliates.</p>"

elem <- doc %>%
  rvest::html_elements("*") %>%
  `[`(seq(which(as.character(.) == deck$anchor[1]), which(as.character(.) == license)-1)) %>%
  ## Prune children
  setdiff(html_children(.))

elem_text <- as.character(elem)

## Get unique tags
icon_containers <- elem_text[grepl("glossary-icon-container", elem_text)]
tags <- regmatches(icon_containers, gregexpr('(?<=title=")[^"]*(?=")', 
                                             icon_containers, perl = TRUE)) %>%
  unlist() %>%
  unique() %>%
  tolower() %>%
  gsub(" ", "-", .)

## Populate definition from paragraph elements
j <- k <- 1

for (i in seq_len(nrow(deck))){
  
  ## Find paragraph element corresponding to next term anchor
  repeat{
    
    k <- k + 1
    
    if (i == nrow(deck)){
      
      k <- length(elem_text) + 1
      break
    }
    if (elem_text[k] == deck$anchor[i+1]){
      
      break
    }
  }
  elem_text_i <- elem_text[seq(j+1, k-1)]
  
  ## Extract tags
  if (length(icon_containers_i <- elem_text_i[grepl("glossary-icon-container", elem_text_i)])){
    
    tags_i <- regmatches(icon_containers_i, gregexpr('(?<=title=")[^"]*(?=")', 
                                                     icon_containers_i, perl = TRUE))[[1]] %>%
      tolower() %>%
      gsub(" ", "-", .)
    
    deck$tags[i] <- paste(tags_i, collapse = " ")
    
    elem_text_i <- setdiff(elem_text_i, icon_containers_i)
  }
  
  ## Get definition by combining paragraph elements between anchors
  deck$definition[i] <- paste(elem_text_i, collapse = "\n")
  
  ## Replace $$ with anki-mathjax
  deck$definition[i] <- gsub("\\$\\$(.*?)\\$\\$", 
                             "<anki-mathjax block=\"true\">\\1</anki-mathjax>", 
                             deck$definition[i])
  
  ## Replace \n with <br> only within <devsite-code>
  if (grepl("<devsite-code>", deck$definition[i])){
    
    deck$definition[i] <- str_replace_between(deck$definition[i])
  }
  
  ## Progress to next term
  j <- k
}

cloze_template <- '<h2><a href=\"{{{term_url}}}\"><strong>{{{text}}}</strong></a></h2><br>{{c1::<br>{{{definition}}}<br>}}\t\t{{{tags}}}'

deck <- deck %>%
  ## Update hyperlinks to include glossary_url
  mutate(definition = gsub("<a href=\"#", sprintf("<a href=\"%s#", glossary_url), definition),
         definition = gsub("<a href=\"", sprintf("<a href=\"https://%s", 
                                                 urltools::domain(glossary_url)), definition),
         definition = gsub("<img src=\"", sprintf("<img src=\"https://%s", 
                                                  urltools::domain(glossary_url)), definition)) %>%
  ## Escape premature end cloze
  mutate(definition = gsub("}}", "} }", definition)) %>%
  ## Remove line breaks
  mutate_all(.funs = gsub, pattern = "\\n", replacement = " ") %>%
  ## Create cloze card from template
  mutate(cloze = glue::glue(cloze_template, .open = "{{{", .close = "}}}"))

## Write deck
header <- paste(c("#separator:tab", "#html:true", "#tags column:3", collapse = "\n"))

time_id <- gsub("\\.", "-", format(Sys.time(), "%Y-%m-%d_%H-%M"))

cat(paste(c(header, deck$cloze), collapse = "\n"),
    file = sprintf("Google Machine Learning Glossary Anki Deck.txt"))
