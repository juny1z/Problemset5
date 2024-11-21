install.packages("Rcpp")
library(Rcpp)

# a
setClass(
  "rational",
  slots = list(numerator = "numeric", denominator = "numeric"),
  prototype = list(numerator = 0, denominator = 1)
)

setMethod(
  "initialize", "rational",
  function(.Object, numerator, denominator) {
    if (denominator == 0) {
      stop("Denominator is non-zero.")
    }
    .Object@numerator <- numerator
    .Object@denominator <- denominator
    .Object
  }
)

setMethod(
  "show", "rational",
  function(object) {
    cat(object@numerator, "/", object@denominator, "\n")
  }
)

simplify <- function(object) {
  gcd <- function(a, b) {
    if (b == 0) return(a)
    gcd(b, a %% b)
  }
  g <- gcd(object@numerator, object@denominator)
  object@numerator <- object@numerator / g
  object@denominator <- object@denominator / g
  object
}

setGeneric("quotient", function(object, digits = 7) standardGeneric("quotient"))
setMethod(
  "quotient", "rational",
  function(object, digits = 7) {
    result <- object@numerator / object@denominator
    round(result, digits)
  }
)

setMethod(
  "+", signature(e1 = "rational", e2 = "rational"),
  function(e1, e2) {
    num <- e1@numerator * e2@denominator + e2@numerator * e1@denominator
    den <- e1@denominator * e2@denominator
    simplify(new("rational", numerator = num, denominator = den))
  }
)

setMethod(
  "-", signature(e1 = "rational", e2 = "rational"),
  function(e1, e2) {
    num <- e1@numerator * e2@denominator - e2@numerator * e1@denominator
    den <- e1@denominator * e2@denominator
    simplify(new("rational", numerator = num, denominator = den))
  }
)

setMethod(
  "*", signature(e1 = "rational", e2 = "rational"),
  function(e1, e2) {
    num <- e1@numerator * e2@numerator
    den <- e1@denominator * e2@denominator
    simplify(new("rational", numerator = num, denominator = den))
  }
)

setMethod(
  "/", signature(e1 = "rational", e2 = "rational"),
  function(e1, e2) {
    num <- e1@numerator * e2@denominator
    den <- e1@denominator * e2@numerator
    simplify(new("rational", numerator = num, denominator = den))
  }
)


cppFunction('
  int gcdC(int a, int b) {
    if (b == 0) return abs(a);  // Ensure absolute value for negatives
    return gcdC(b, a % b);
  }

  int lcmC(int a, int b) {
    if (a == 0 || b == 0) return 0; // Handle edge case of zero
    return abs(a * b) / gcdC(a, b);
  }
')

# b
r1 <- new("rational", numerator = 24, denominator = 6)
r2 <- new("rational", numerator = 7, denominator = 230)
r3 <- new("rational", numerator = 0, denominator = 4)

r1 <- simplify(r1)
r2 <- simplify(r2)
r3 <- simplify(r3)

show(r1)
show(r2)
show(r3)

r1
r3
r1 + r2
r1 - r2
r1 * r2
r1 / r2
r1 + r3
r1 * r3
r2 / r3
quotient(r1)
quotient(r2)
quotient(r2, digits = 3)
quotient(r2, digits = 3.14)
quotient(r2, digits = "avocado")
q2 <- quotient(r2, digits = 3)
q2
quotient(r3)
simplify(r1)
simplify(r2)
simplify(r3)

# c

library(methods)

setClass(
  "rational",
  slots = list(numerator = "numeric", denominator = "numeric"),
  prototype = list(numerator = 0, denominator = 1)
)

setMethod(
  "initialize", "rational",
  function(.Object, numerator, denominator) {
    if (denominator == 0) {
      stop("Error: Denominator is non-zero.")
    }
    if (!is.numeric(numerator) || !is.numeric(denominator)) {
      stop("Error: Both numerator and denominator must be numeric.")
    }
    .Object@numerator <- numerator
    .Object@denominator <- denominator
    .Object
  }
)

setMethod(
  "show", "rational",
  function(object) {
    cat(object@numerator, "/", object@denominator, "\n")
  }
)

simplify <- function(object) {
  gcd <- function(a, b) {
    if (b == 0) return(abs(a))
    gcd(b, a %% b)
  }
  g <- gcd(object@numerator, object@denominator)
  object@numerator <- object@numerator / g
  object@denominator <- object@denominator / g
  object
}

# Invalid: denominator is zero
tryCatch({
  r1 <- new("rational", numerator = 24, denominator = 0)
}, error = function(e) {
  print(e$message)
})

# Invalid: numerator is string
tryCatch({
  r2 <- new("rational", numerator = "24", denominator = 6)
}, error = function(e) {
  print(e$message)
})

# Valid
tryCatch({
  r3 <- new("rational", numerator = 0, denominator = 4) 
  show(r3)
}, error = function(e) {
  print(e$message)
})



###2###
# a
art <- read.csv("/Users/zjyyy/Desktop/df_for_ml_improved_new_market.csv")
unique(art[, grep("^Genre", names(art))])
art$Genre___Others[art$Genre___Painting == 1] <- 0
unique(art[, grep("^Genre", names(art))])
art$genre <- "Photography"
art$genre[art$Genre___Print == 1] <- "Print"
art$genre[art$Genre___Sculpture == 1] <- "Sculpture"
art$genre[art$Genre___Painting == 1] <- "Painting"
art$genre[art$Genre___Others == 1] <- "Other"
table(art$genre)

(yeargenre <- with(art, table(year, genre)))
ygperc <- yeargenre/apply(yeargenre, 1, sum)
ygperc <- ygperc[, c("Painting", "Sculpture", "Photography", "Print", "Other")]
ygpercm <- as.data.frame(ygperc)
# Reverse level of factors so ggplot draws it the same way
ygpercm$genre <- factor(ygpercm$genre, levels = rev(unique(ygpercm$genre)))
head(ygpercm)

ggplot(ygpercm, aes(y = Freq, x = year, fill = genre)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(y = NULL, x = NULL, title = "Proportion of Genre of Art Sales") +
  theme(legend.position = "off") +
  geom_text(data = ygpercm[ygpercm$year == 2012 & ygpercm$genre != "Other", ],
            aes(label = genre),
            position = position_stack(vjust = 0.5),
            color = "white",
            size = 4) +
  # Add the Other label
  geom_segment(aes(xend = 16, yend = 1, x = 15, y = 1.02),
               arrow = arrow(length = unit(0.15, "inches")),
               linewidth = .5, color = "black") +
  annotate("text", x = 14.9, y = 1.02, label = "Other", hjust = 0, angle = 270)

# b
library(tidyverse)
library(plotly)

genre_columns <- names(art)[grepl("Genre___", names(art))]

price_data <- art %>%
  select(year, price_usd, all_of(genre_columns)) %>%
  pivot_longer(cols = all_of(genre_columns), 
               names_to = "Genre", 
               values_to = "Count") %>%
  filter(Count == 1) %>%
  mutate(Genre = str_replace(Genre, "Genre___", ""))

interactive_plot <- price_data %>%
  plot_ly(
    x = ~year,
    y = ~price_usd,
    color = ~Genre,
    type = 'scatter',
    mode = 'markers+lines',
    hoverinfo = 'text',
    text = ~paste("Year:", year, "<br>Price (USD):", price_usd, "<br>Genre:", Genre)
  ) %>%
  layout(
    title = "Change in Sales Price Over Time by Genre",
    xaxis = list(title = "Year"),
    yaxis = list(title = "Sales Price (USD)"),
    legend = list(title = list(text = "Genre"))
  )

interactive_plot



##3##
#a
library(nycflights13)
library(data.table)

flights_dt <- as.data.table(flights)
airports_dt <- as.data.table(airports)

flights_dt <- merge(flights_dt, airports_dt[, .(faa, name)], by.x = "origin", by.y = "faa", all.x = TRUE)
setnames(flights_dt, "name", "origin_name")
flights_dt <- merge(flights_dt, airports_dt[, .(faa, name)], by.x = "dest", by.y = "faa", all.x = TRUE)
setnames(flights_dt, "name", "dest_name")

# Departure Delay table
dep_delay_table <- flights_dt[, .(
  mean_dep_delay = mean(dep_delay, na.rm = TRUE),
  median_dep_delay = median(dep_delay, na.rm = TRUE),
  num_flights = .N
), by = origin_name][num_flights >= 10] 

dep_delay_table <- dep_delay_table[order(-mean_dep_delay)]
print(dep_delay_table)

#Arrival Delay table
arr_delay_table <- flights_dt[, .(
  mean_arr_delay = mean(arr_delay, na.rm = TRUE),
  median_arr_delay = median(arr_delay, na.rm = TRUE),
  num_flights = .N
), by = dest_name][num_flights >= 10] 

arr_delay_table <- arr_delay_table[order(-mean_arr_delay)]
print(arr_delay_table)

#b
flights_dt <- as.data.table(flights)
planes_dt <- as.data.table(planes)

flights_dt <- merge(flights_dt, planes_dt, by = "tailnum", all.x = TRUE)

fastest_aircraft <- flights_dt[
  !is.na(air_time) & air_time > 0 & !is.na(distance),
  .(
    avgmph = mean(distance / (air_time / 60), na.rm = TRUE),
    nflights = .N
  ),
  by = model
][order(-avgmph)][1] 

print(fastest_aircraft)