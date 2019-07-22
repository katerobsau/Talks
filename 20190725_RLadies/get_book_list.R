library(rvest)

book_url <- "https://mizparker.wordpress.com/the-lists/1001-books-to-read-before-you-die/"

# Examples of getting data from the web
text_data <- read_html(book_url) %>%
  html_text()

paragraph_data <- read_html(book_url) %>%
  html_nodes("p") 

css_data <- read_html(book_url) %>%
  html_nodes(css = "#post-13 > div > p:nth-child(4)")

xpath_data <- read_html(book_url) %>%
  html_nodes(xpath = '//*[@id="post-13"]/div/p[4]')

# -----------------------------------------------------------------------------

# Let's put our list together
# but .... Web scraping often means string handling

# Resources: 
#   * https://regexr.com/
#   * stringr cheatsheet from RStudio

book_str <- paragraph_data[4] %>%
  html_text(trim = TRUE) %>% 
  gsub("\n", "", .)  #remove newline character

book_df <- book_str %>% 
  strsplit(split = "\\d+?\\.") %>% #match the number index with a full stop
  # careful, don't want to match book titles like Catch 22, or authors like J.R.R Tolkien
  # actuallya bit tricky!
  as.data.frame(stringsAsFactors = FALSE) %>% 
  dplyr::filter(. != "")  # remove empty first row
names(book_df) <- "book_string"
book_df <- book_df %>%
  tidyr::separate(book_string, sep = "\\–", into = c("book", "author"))
  # very lucky that whoever coded this used a long hash!

# Could vectorise it probably but later task!
# For now we'll just wrap that in a function and use lapply
Get_book_data <- function(para_ind){
  
  book_str <- paragraph_data[para_ind] %>%
    html_text(trim = TRUE) %>% 
    gsub("\n", "", .)  #remove newline character
  
  book_df <- book_str %>% 
    strsplit(split = "\\d+?\\.") %>% #match the number index
    as.data.frame(stringsAsFactors = FALSE) %>% 
    dplyr::filter(. != "")  # remove empty first row
  names(book_df) <- "book_string"
  book_df <- book_df %>%
    tidyr::separate(book_string, sep = "\\–", into = c("book", "author"))
  
  return(book_df)
  
}
book_data <- lapply(seq(4,12,2) %>% as.list(), Get_book_data) %>% do.call(rbind, .)
View(book_data)
# Have 1001 rows so let's assume we are all good!

# -----------------------------------------------------------------------------

# Let's get info about our books!

# Hard way

# Use Wikipedia / Google

# Give examples of tables from earlier talk

# -----------------------------------------------------------------------------
#
# Easy way
# Can use an API Key
# * see https://www.goodreads.com/api
#
# Luckily for us there is a package:
# install.packages("devtools")
# devtools::install_github("famguy/rgoodreads")
#
# -----------------------------------------------------------------------------

library(rgoodreads)

book_name = book_data$book[1]
book_info <- book_by_title(book_name)

author_name = book_data$author[1]
author_info <- author_by_name(author_name)

## Basic form of the query
# book_by_title
# function (title) 
# {
#   tbl <- NULL
#   ggr <- goodreads_GET("book/title", title = title)
#   tbl <- goodreads_parse_book(ggr)
#   tbl
# }
# <bytecode: 0x1080dd920>
#   <environment: namespace:rgoodreads>
#   > book_by_isbn
# function (isbn) 
# {
#   tbl <- NULL
#   ggr <- goodreads_GET("book/isbn", isbn = isbn)
#   tbl <- goodreads_parse_book(ggr)
#   tbl
# }
# <bytecode: 0x108109ae8>
#   <environment: namespace:rgoodreads>
#   > 

# -----------------------------------------------------------------------------

kates_read = c(19, 43, 93, 190, 242, 301, 367, 451, 456, 494, 564, 574, 603, 
               610, 676, 781, 831, 863, 868, 931, 938)

kates_book_info <- sapply(kates_read, function(i){
  book_name = book_data$book[i]
  book_info = book_by_title(book_name)
})
publication_year = lapply(kates_book_info, function(l){l$publication_year %>% as.numeric()}) %>%
  unlist()
table(publication_year)

kates_author_info <- sapply(kates_read, function(i){
  author_name = book_data$author[i]
  author_info <- author_by_name(author_name)
  return(author_info)
})
genders = lapply(kates_author_info, function(l){l$gender}) %>% unlist()
table(genders)

# ---------------------------------------------------------------------

## What of nationalities?? 
hometown = lapply(kates_author_info, function(l){l$hometown %>% as.character}) %>% unlist()
table(hometown)
## No standard format! Good luck!
## Tricky there are states, countries, places I can't process because I don't know 

# ---------------------------------------------------------------------

# Get table of nationalities
url <- "http://www.vocabulary.cl/Basic/Nationalities.htm"
xpath <- "/html/body/div[1]/article/table[2]"
nationalities <- url %>%
  read_html() %>%
  html_nodes(xpath=xpath) %>%
  html_table() %>% 
  as.data.frame()

fix_footnote1 = "Colombia *"
i1 = which(nationalities == fix_footnote1, arr.ind = TRUE)
nationalities[i1] = strsplit(fix_footnote1, split = ' ')[[1]][1]

fix_footnote2 = "American **"
i2 = which(nationalities == fix_footnote2, arr.ind = TRUE)
nationalities[i2] = strsplit(fix_footnote2, split = ' ')[[1]][1]

possible_nationalities <- nationalities$Nationality........Adjective. #c("Australian", "Chinese", "Mexican", "English", "Ethiopian")
about = lapply(kates_author_info, function(l){
  about_para = l$about
  bool_vals = sapply(possible_nationalities, function(nationality, str){
    grepl(nationality, str)
  }, str = about_para)
  another_author_nationality <- possible_nationalities[bool_vals == TRUE]
  another_author_nationality 
})

# -----------------------------------------------------------------------------

# Lazy option:
google_query = 'https://google.com/search?q='
sep_pattern = '%20'
author_name = book_data$author[kates_read[1]]
author_search = paste(c(strsplit(author_name, split = ' ')[[1]] %>% setdiff(""), 
                        "Nationality"), collapse = sep_pattern)
url_search = paste(google_query, author_search, sep="")
search_info <- read_html(url_search) %>% 
  html_text()
bool_vals = sapply(possible_nationalities, function(nationality, str){
  grepl(nationality, str)
}, str = search_info)
another_author_nationality <- possible_nationalities[bool_vals == TRUE]
another_author_nationality 