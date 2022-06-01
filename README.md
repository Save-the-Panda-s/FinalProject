# FinalProject
The goal of our final project is to utilized player and team information and use Machine learning to predict the 2022 NBA Finals Champions. Currently, the remaining teams in the playoff are Miami Heat, Philadelphia 76ers, Boston Celtics, Milwaukee Bucks in the Eastern Conference.  Phoenix Suns, Dallas Mavericks, Memphis Grizzlies and Golden State Warriors in the Western Conference. Our goal is to predict the outcome of each game withing the series. Each series consists of a potential 7-games. With the winning teams meeting up for the Easter and Western conference 7-game series.  The winner of the Conference game series will face off for the NBA Conference finals another 7-games series.  The winner would be the 2022 NBA Champions.  Who will it be?  

## Logic  - Offense
1.	Create a Data frame for each Players shot chart selection for each of the 8 rotational players 
a.	Total of 8 individuals Shot charts DFs
2.	Create a Team Data Frame by appending each of the players shot chart selections.
3.	Compile the average minutes played for each of the 8 players for each game.  
a.	Use this average minutes and team Data frame to generate the team game score.
b.	This would be the input to generate the scores from the Team Data frame.
4.	Randomly generate each team score based on the best of 7 in a seven-game series.
5.	By game elimination we reduce the teams in each series.
6.	Then by elimination we will have the final 2 teams.  
7.	The last game we will predict the winning team and score.
Logic - Defensive 

## The following steps will outline the process:
- Identify Relevant Data – preferably team and player data where we gather the relevant team and player statistics of the remaining team.  Data we could use to identify team and player tendencies to predict the final results.
- Clean and Processing the Data – based on the data we select we will have to clean the data for efficient outcomes.  Useable data to analyze tendencies for each matchup series based team and player matchups. 
- Feature Engineering — what additional metrics can we append to our datasets that would help any user or ML model to better understand and predict, respectively, outcomes and trends from the data?
- Data Analysis — can we determine any collinearity or other relations within the data that may better inform our predictions?
- Predictions – The results of a 7-game series for the eight remaining teams. Outcome for each game in the series.  Then predict both Eastern and Western conference finals champion winners.  These two teams will then compete for the final series outcome of the NBA Conference final. It will be key to determine which models and features would be most beneficial for us in developing an accurate prediction for each game in the series? Do we focus on team or aggregated individuals’ statistics and matchups?

## Resources: 
- https://www.nbastuffer.com/2021-2022-nba-player-stats/
- www.basketball-reference.com

GitHub Repository. https://github.com/Save-the-Panda-s/FinalProject.git

## Machine Learning Model
- Logistic Regression 
- Neural Network


## Database: 
PgAdmin/ AWS Server to host Database
Project Database AWS - Database - PostgresDB
Instance: 	savepandas
user: 		Postgres
PW: 		FinalPJ22
Connectivity:  	savepandas.cwahfsnfyvht.us-east-2.rds.amazonaws.com
Port: 		5432

## Dashboard: Tableau
