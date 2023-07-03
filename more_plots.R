# analysing the plots 

#Install packages

#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("stats")
library(ggplot2)
library(dplyr)
library(stats)

#This data set is from SullyGnome, Top 200 Watched games channels from 6/21/20 - 6/21/21.
#added the game genre into each game.
#also removed channels which are not for a video game.

raw_data = read.csv(file.choose())
Twitch = raw_data
as.factor(Twitch$Genre)

#reducing the watch and stream times to millions of hours

Twitch$Watch.time = Twitch$Watch.time / 1000000
Twitch$Stream.time = Twitch$Stream.time / 1000000

#added in a ratio that is Watch / Stream time just to see what is the highest

Twitch = mutate(Twitch, Watch_to_Stream_Ratio = Watch.time / Stream.time)

#split out the data set into the different genres

Action = filter(Twitch, Genre == "Action")
Action_Adventure = filter(Twitch, Genre == "Action-Adventure")
Adventure = filter(Twitch, Genre == "Adventure")
Fighting = filter(Twitch, Genre == "Fighting")
Misc = filter(Twitch, Genre == "Misc")
MMO = filter(Twitch, Genre == "MMO")
Platform = filter(Twitch, Genre == "Platform")
Puzzle = filter(Twitch, Genre == "Puzzle")
Racing = filter(Twitch, Genre == "Racing")
RPG = filter(Twitch, Genre == "RPG")
Shooter = filter(Twitch, Genre == "Shooter")
Simulation = filter(Twitch, Genre == "Simulation")
Sports = filter(Twitch, Genre == "Sports")
Strategy = filter(Twitch, Genre == "Strategy")

#Plot watch time versus stream time for channels with more than 10 games excluding Misc.

ggplot(Action, aes(x = Stream.time, y = Watch.time)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Stream Time (Millions of Hours)", y = "Watch Time (Millions of Hours)", title = "Watch Time vs. Stream Time for Action Games on Twitch 06/20 - 06/21") +
  theme(plot.title = element_text(hjust = 0.5)) 

ggplot(Action_Adventure, aes(x = Stream.time, y = Watch.time)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Stream Time (Millions of Hours)", y = "Watch Time (Millions of Hours)", title = "Watch Time vs. Stream Time for Action-Adventure Games on Twitch 06/20 - 06/21") +
  theme(plot.title = element_text(hjust = 0.5)) 

ggplot(RPG, aes(x = Stream.time, y = Watch.time)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Stream Time (Millions of Hours)", y = "Watch Time (Millions of Hours)", title = "Watch Time vs. Stream Time for RPG Games on Twitch 06/20 - 06/21") +
  theme(plot.title = element_text(hjust = 0.5)) 

ggplot(Shooter, aes(x = Stream.time, y = Watch.time)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Stream Time (Millions of Hours)", y = "Watch Time (Millions of Hours)", title = "Watch Time vs. Stream Time for Shooter Games on Twitch 06/20 - 06/21") +
  theme(plot.title = element_text(hjust = 0.5)) 

ggplot(Strategy, aes(x = Stream.time, y = Watch.time)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Stream Time (Millions of Hours)", y = "Watch Time (Millions of Hours)", title = "Watch Time vs. Stream Time for Strategy Games on Twitch 06/20 - 06/21") +
  theme(plot.title = element_text(hjust = 0.5))
