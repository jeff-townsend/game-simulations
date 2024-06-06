library(tidyverse)
library(readr)

cities.file <- "https://raw.githubusercontent.com/jeff-townsend/game-simulations/main/data/Ticket%20to%20Ride%20Cities.csv"
routes.file <- "https://raw.githubusercontent.com/jeff-townsend/game-simulations/main/data/Ticket%20to%20Ride%20Routes.csv"
tickets.file <- "https://raw.githubusercontent.com/jeff-townsend/game-simulations/main/data/Ticket%20to%20Ride%20Tickets.csv"

route.points <- data.frame(route_length = c(1:6),
                           route_points =c(1, 2, 4, 7, 10, 15))

cities <- read.csv(cities.file)
routes <- read.csv(routes.file) %>% inner_join(route.points, by = "route_length")
tickets <- read.csv(tickets.file)

calculatePath <- function(departure, arrival){
  ### use dijkstra's algorithm to find shortest possible route
  
  # set up the data
  # departure <- "Duluth"
  # arrival <- "El Paso"
  
  priority.queue <- data.frame(city = departure,
                               via = NA,
                               total_trains = 0,
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
      distinct(city, total_trains, ending_city, route_length) %>%
      mutate(route_total = total_trains + route_length) %>%
      select(ending_city, city, route_total) %>%
      left_join(priority.queue, by = c("ending_city" = "city")) %>%
      filter(is.na(total_trains) | route_total < total_trains) %>%
      select(ending_city, city, route_total) %>%
      rename(city = ending_city,
             via = city,
             total_trains = route_total) %>%
      mutate(completed = 0)
    
    # add to queue and re-prioritize
    priority.queue <-
      rbind(priority.queue, new.queue) %>%
      arrange(desc(completed), total_trains) %>%
      group_by(city) %>%
      filter(row_number() == 1) %>%
      ungroup()
    
    # mark city as completed
    priority.queue[iteration, 4] = 1
    
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
  
  ticket.route <-
    routes %>%
    inner_join(collect.route, by = c("starting_city" = "via", "ending_city" = "city")) %>%
    arrange(desc(order)) %>%
    mutate(ticket_name = paste0(departure," - ",arrival)) %>%
    distinct(ticket_name, starting_city, ending_city, route_length, route_points) %>%
    mutate(order = row_number())
  
  return(ticket.route)
  #return(c(sum(ticket.route$route_length), sum(ticket.route$route_points)))
}

# test a ticket
calculatePath(departure = "Duluth",
              arrival = "El Paso")

ticket.routes <- matrix(nrow = 0, ncol = 6)
col.names <- c("ticket_name", "starting_city", "ending_city", "route_length", "route_points", "order")
colnames(ticket.routes) <- col.names

t <- 1
for(t in 1:nrow(tickets)){
  ticket.route <- calculatePath(tickets$ticket_departure[t], tickets$ticket_arrival[t])
  
  ticket.routes <- rbind(ticket.routes, ticket.route)
  t <- t + 1
}

ticket.paths <-
  tickets %>%
  inner_join(ticket.routes, by = "ticket_name") %>%
  group_by(id, ticket_name, ticket_points) %>%
  summarize(trains = sum(route_length),
            train_points = sum(route_points)) %>%
  ungroup() %>%
  mutate(total_points = ticket_points + train_points,
         points_per_train = total_points / trains)
