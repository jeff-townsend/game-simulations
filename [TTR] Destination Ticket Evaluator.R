library(tidyverse)
library(readxl)

cities <- read_excel("Game Simulations/Ticket to Ride.xlsx", sheet = "Cities")
routes <- read_excel("Game Simulations/Ticket to Ride.xlsx", sheet = "Routes")
tickets <- read_excel("Game Simulations/Ticket to Ride.xlsx", sheet = "Tickets")

route.points <- data.frame(id = c(1:6),
                           route_length = c(1:6),
                           route_points =c(1, 2, 4, 7, 10, 15))

calculatePath <- function(departure, arrival){
  ### use dijkstra's algorithm to find shortest possible route
  
  # set up the data
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
      filter(row_number()==1) %>%
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
    priority.queue <- rbind(priority.queue, new.queue)
    priority.queue <- priority.queue %>% arrange(desc(completed), total_trains)
    
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
    filter(row_number()==1) %>%
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
    distinct(starting_city, ending_city, route_length)
  return(priority.queue)
  #return(sum(ticket.route$route_length))
}

# grab random ticket
destination.ticket <-
  tickets %>%
  mutate(rng = runif(nrow(tickets))) %>%
  top_n(1, wt = rng) %>%
  select(-rng)

calculatePath(departure = destination.ticket$ticket_departure,
              arrival = destination.ticket$ticket_arrival)

ticket.info <-
  tickets %>%
  group_by(id, ticket_name, ticket_points, ticket_departure, ticket_arrival) %>%
  summarize(fastest_route = calculatePath(ticket_departure, ticket_arrival))

## note: I need to fix the collect route table; it's duplicating cities and I need the one with the minimum total_trains
