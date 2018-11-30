participants <- tibble::tribble(
       ~name,                               ~email, ~size,
     "Kevin",               "kevin.olsen@fbfs.com",  10.5,
    "Jordan",            "jordan.hammond@fbfs.com",    12,
      "Matt",         "matthew.cleveland@fbfs.com",   9.5,
  "Ashley C",             "Ashley.Covell@fbfs.com",    10,
  "Ashley W", "Ashley.Warrington@fblfinancial.com",     9,
     "Corey",             "Corey.Broxson@fbfs.com",    11,
     "James",             "James.Pearson@fbfs.com",    10,
      "Dana",         "Dana.Rinderknecht@fbfs.com",   7.5,
    "Teresa",     "Teresa.Crouse@fblfinancial.com",     8
  )

generate_derangement <- function(n) {
  sample <- sample(n)
  if(any(sample == 1:n)) {
    return(generate_derangement(n))
  }
  sample
}

#' Email Secret Santa List
#'
#'
#' @param participants tibble containing secret santa participants (name, email, sock size)
#' @param seed random seed to generate the listing
#'
#' @export
secret_santa <- function(participants, seed = 42) {
  set.seed(seed)
  n <- nrow(participants)

  participants$id <- 1:n
  participants$match_id <- generate_derangement(n)

  outcome <- dplyr::left_join(dplyr::select(participants, name, email, id),
                              dplyr::select(participants, match = name, size, match_id),
                              by = c("id" = "match_id"))

  purrr::pwalk(outcome %>%
                 dplyr::select(email, name, match, size),
               ~outlookSend(To = ..1,
                            Subject = glue::glue("Property Pricing Secret Santa {lubridate::year(lubridate::today())}"),
                            HTMLBody = match_message(..2, ..3, ..4)))

  invisible(outcome)
}

#' @export
match_message <- function(name, match, sock_size) {
  style <- "<style> div.message	{{margin:0in; font-size:24.0pt; font-family:'Vivaldi',sans-serif;}} </style>"
  msg <- glue::glue("Greetings {name},<br><br>",
             "I have randomly selected you to find socks for <span style='color:red'>{match}</span>.<br>",
             "{match}'s sock size is: <span style='color:red'>{sock_size}</span>.<br><br>",
             "Beep beep boop,<br>",
             "SecretSantaMatcheR v{packageVersion('SecretSantaMatcheR')}")

  glue::glue(style, "<div class=message>",msg, "</div>")
}


