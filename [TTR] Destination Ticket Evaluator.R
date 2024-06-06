library(tidyverse)
library(readr)

#setwd("/Users/Jeff/Documents/Game Simulations/Ticket to Ride")

cities.file <- "https://raw.githubusercontent.com/jeff-townsend/game-simulations/main/data/Ticket%20to%20Ride%20Cities.csv"
routes.file <- "https://raw.githubusercontent.com/jeff-townsend/game-simulations/main/data/Ticket%20to%20Ride%20Routes.csv"
tickets.file <- "https://raw.githubusercontent.com/jeff-townsend/game-simulations/main/data/Ticket%20to%20Ride%20Tickets.csv"

route.points <- data.frame(route_length = c(1:6),
                           route_points =c(1, 2, 4, 7, 10, 15))

cities <- read.csv(cities.file)
routes <- read.csv(routes.file) %>% inner_join(route.points, by = "route_length")
tickets <- read.csv(tickets.file)

city.combos <-
  cities %>%
  cross_join(cities) %>%
  filter(id.y > id.x) %>%
  select(city.x, city.y) %>%
  rename(departure = city.x,
         arrival = city.y) %>%
  mutate(destination_name = paste0(departure, " - ", arrival))

calculatePath <- function(departure, arrival){
  ### use dijkstra's algorithm to find shortest possible route
  
  # set up the data
  # departure <- "Duluth"
  # arrival <- "El Paso"
  
  priority.queue <- data.frame(city = departure,
                               via = NA,
                               total_trains = 0,
                               total_points = 0,
                               completed = 0)
  iteration <- 0
  
  # perform the algorithm
  repeat{
    iteration <- iteration + 1
    # assess next in queue
    new.queue <-
      priority.queue %>%
      filter(completed == 0) %>%
      filter(row_number() == 1) %>%
      inner_join(routes, by = c("city" = "starting_city")) %>%
      distinct(city, total_trains, total_points, ending_city, route_length, route_points) %>%
      mutate(route_total = total_trains + route_length,
             route_total_points = total_points + route_points) %>%
      select(ending_city, city, route_total, route_total_points) %>%
      left_join(priority.queue, by = c("ending_city" = "city")) %>%
      filter(is.na(total_trains) | (route_total - route_total_points/100) < (total_trains - total_points/100)) %>%
      select(ending_city, city, route_total, route_total_points) %>%
      rename(city = ending_city,
             via = city,
             total_trains = route_total,
             total_points = route_total_points) %>%
      mutate(completed = 0)
    
    # add to queue and re-prioritize
    priority.queue <-
      rbind(priority.queue, new.queue) %>%
      arrange(desc(completed), total_trains, desc(total_points)) %>%
      group_by(city) %>%
      filter(row_number() == 1) %>%
      ungroup()
    
    # mark city as completed
    priority.queue[iteration, 5] = 1
    
    # is the next city our destination?
    next.city <-
      priority.queue %>%
      filter(completed == 0) %>%
      filter(row_number() == 1) %>%
      select(city)
    
    if(next.city == arrival){
      break
    }
  }
  
  # collect answer
  
  collect.route <-
    priority.queue %>%
    filter(completed == 0) %>%
    filter(row_number() == 1) %>%
    select(city, via) %>%
    mutate(order = 1)
  
  iteration <- 1
  
  repeat{
    iteration <- iteration + 1
    next.route <-
      collect.route %>%
      filter(order == iteration - 1) %>%
      inner_join(priority.queue, by = c("via" = "city")) %>%
      select(via, via.y) %>%
      rename(city = via,
             via = via.y) %>%
      mutate(order = iteration)
    
    collect.route <- rbind(collect.route, next.route)
    collect.route <- collect.route %>% arrange(desc(order))
    
    via.na.check <-
      next.route %>%
      filter(row_number() == 1) %>%
      select(via)
    
    if(is.na(via.na.check)){
      break
    }
  }
  
  destination.route <-
    routes %>%
    inner_join(collect.route, by = c("starting_city" = "via", "ending_city" = "city")) %>%
    arrange(desc(order)) %>%
    mutate(destination_name = paste0(departure, " - ", arrival),
           departure = departure,
           arrival = arrival) %>%
    distinct(destination_name, departure, arrival, starting_city, ending_city, route_length, route_points) %>%
    mutate(order = row_number())
  
  return(destination.route)
}

# test a ticket
calculatePath(departure = "Duluth",
              arrival = "El Paso")

# every possible combination
destination.routes <- matrix(nrow = 0, ncol = 8)
col.names <- c("destination_name", "departure", "arrival", "starting_city",
               "ending_city", "route_length", "route_points", "order")
colnames(destination.routes) <- col.names

# start.time <- Sys.time()
c <- 1
for(c in 1:nrow(city.combos)){
  destination.route <- calculatePath(city.combos$departure[c], city.combos$arrival[c])
  
  destination.routes <- rbind(destination.routes, destination.route)
  c <- c + 1
}

destination.inverse <-
  destination.routes %>%
  mutate(tmp = departure,
         departure = arrival,
         arrival = tmp,
         destination_name = paste0(departure, " - ", arrival)) %>%
  select(-tmp) %>%
  mutate(tmp = starting_city,
         starting_city = ending_city,
         ending_city = tmp) %>%
  select(-tmp) %>%
  group_by(destination_name) %>%
  mutate(order = rank(desc(order))) %>%
  ungroup() %>%
  arrange(destination_name, order)

destination.routes <- rbind(destination.routes, destination.inverse)

# end.time <- Sys.time()
# run.time <- end.time - start.time
# print(run.time)

# write.csv(destination.routes, "Ticket to Ride Destination Combos.csv", row.names = FALSE)
# read.csv("Ticket to Ride Destination Combos.csv")

destination.paths <-
  rbind(
        city.combos %>%
          select(-destination_name) %>%
          inner_join(destination.routes, by = c("departure", "arrival")) %>%
          group_by(departure, arrival) %>%
          summarize(trains = sum(route_length),
                    points = sum(route_points)) %>%
          ungroup() %>%
          mutate(points_per_train = points / trains),
        city.combos %>%
          select(-destination_name) %>%
          mutate(tmp = departure,
                 departure = arrival,
                 arrival = tmp) %>%
          select(-tmp) %>%
          inner_join(destination.routes, by = c("departure", "arrival")) %>%
          group_by(departure, arrival) %>%
          summarize(trains = sum(route_length),
                    points = sum(route_points)) %>%
          ungroup() %>%
          mutate(points_per_train = points / trains)
        )

ticket.cities <- data.frame(city = c("Vancouver", "Santa Fe", "Duluth", "El Paso"))
possible.paths <-
  ticket.cities %>%
  cross_join(ticket.cities, suffix = c("1", "2")) %>%
  filter(city1 != city2) %>%
  cross_join(ticket.cities) %>%
  filter(city1 != city,
         city2 != city) %>%
  cross_join(ticket.cities, suffix = c("3", "4")) %>%
  filter(city1 != city4,
         city2 != city4,
         city3 != city4)

ticket.paths <-
  possible.paths %>%
  inner_join(destination.paths %>% select(-points_per_train),
             by = c("city1" = "departure", "city2" = "arrival")) %>%
  inner_join(destination.paths %>% select(-points_per_train),
             by = c("city2" = "departure", "city3" = "arrival"),
             suffix = c("1", "2")) %>%
  inner_join(destination.paths %>% select(-points_per_train),
             by = c("city3" = "departure", "city4" = "arrival")) %>%
  mutate(trains = trains1 + trains2 + trains,
         points = points1 + points2 + points) %>%
  select(-trains1, -trains2, -points1, -points2)

fastest.route <-
  rbind(
        ticket.paths %>%
          arrange(trains, desc(points)) %>%
          filter(row_number() == 1) %>%
          mutate(destination_name = paste0(city1, " - ", city2)) %>%
          inner_join(destination.routes, by = "destination_name"),
        ticket.paths %>%
          arrange(trains, desc(points)) %>%
          filter(row_number() == 1) %>%
          mutate(destination_name = paste0(city2, " - ", city3)) %>%
          inner_join(destination.routes, by = "destination_name"),
        ticket.paths %>%
          arrange(trains, desc(points)) %>%
          filter(row_number() == 1) %>%
          mutate(destination_name = paste0(city3, " - ", city4)) %>%
          inner_join(destination.routes, by = "destination_name")
        ) %>%
  select(starting_city, ending_city, route_length, route_points)
