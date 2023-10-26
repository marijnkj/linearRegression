#' Map of Airport Delays 
#' 
#' Plot a map of mean delays at airports throughout the US.
#' 
#' @import nycflights13
#' @importFrom dplyr group_by summarize full_join mutate left_join filter recode
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot geom_polygon aes geom_point theme ggtitle coord_map map_data
#' 
#' @export

visualize_airport_delays <- function() {
  mean_dep_delays <- nycflights13::flights %>% 
    dplyr::group_by(nycflights13::flights$origin) %>%
    dplyr::summarize(mean_dep_delay=mean(dep_delay, na.rm=TRUE)) # Ignoring rows with NA departure delay
  
  mean_arr_delays <- nycflights13::flights %>%
    dplyr::group_by(nycflights13::flights$dest) %>%
    dplyr::summarize(mean_arr_delay=mean(arr_delay, na.rm=TRUE)) # Ignoring rows with NA arrival delay
  
  # Change airport code column names for joining
  names(mean_dep_delays)[names(mean_dep_delays) == "origin"] <- "faa"
  names(mean_arr_delays)[names(mean_arr_delays) == "dest"] <- "faa"
  
  mean_delays <- dplyr::full_join(mean_dep_delays, mean_arr_delays, by="faa") %>% # Join departure and arrival data
    tidyr::pivot_longer(cols=c("mean_dep_delay", "mean_arr_delay"), names_to="Delay Type", values_to="Mean Delay", values_drop_na=TRUE) # One column with delay values and one with delay type
    
  mean_delays <- mean_delays %>%
    dplyr::mutate("Delay Type"=dplyr::recode(mean_delays$`Delay Type`, "mean_dep_delay"="Departure Delay", "mean_arr_delay"="Arrival Delay")) # Making column names and type values nicer for plotting
  mean_delays <- dplyr::left_join(mean_delays, nycflights13::airports, by="faa") # Join delay data with airport info
  
  map_coord <- ggplot2::map_data(map="world") %>%
    dplyr::filter(ggplot2::map_data(map="world")$region %in% c("USA", "Honduras") & !(ggplot2::map_data(map="world")$group %in% c(1502, 1508, 1509, 1511, 1515, 1518)))
  
  plot <- mean_delays %>%
    ggplot() +
    geom_polygon(data=map_coord, aes(x=map_coord$long, y=map_coord$lat, group=map_coord$group), fill="grey") +
    geom_point(aes(x=mean_delays$lon, y=mean_delays$lat, size=mean_delays$`Mean Delay`, color=mean_delays$`Delay Type`), alpha=0.5) + 
    theme(axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank()) +
    ggtitle("Mean Delay (minutes) by Airport") +
    coord_map()
  
  plot
}
