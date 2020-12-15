
library(dplyr)
library(rvest)
r = xml2::read_html("http://www.borischen.co/p/quarterback-tier-rankings.html")
r2 = xml2::read_html("https://s3-us-west-1.amazonaws.com/fftiers/out/text_QB.txt") %>%
  html_text()

positions = c("QB", "RB", "WR", "TE", "FLX", "DST")
results = list()
for(i in 1:length(positions)){
  results[[i]] = list()
  results[[i]]$tiers = xml2::read_html(glue::glue("https://s3-us-west-1.amazonaws.com/fftiers/out/text_{positions[i]}.txt")) %>%
    rvest::html_text()
  #results[[i]]$tiers = gsub("\\n", "", results[[i]]$tiers)
  results[[i]]$position = positions[[i]]
  t = stringr::str_split(results[[i]]$tiers, "\\n")[[1]]
  tiers = sapply(t, function(x){stringr::str_split(x, ":")})
  tiers = tiers[tiers != ""]
  tiers_list = lapply(tiers, function(x){stringr::str_split(trimws(x[[2]]), ", ")})
  tiers_number = lapply(tiers, function(x){gsub("Tier ([0-9])","\\1", x[[1]])})
  together = lapply(1:length(tiers_list), function(x){data.frame(Type = results[[i]]$position,
                                                      Number = tiers_number[[x]],
                                                      Players = tiers_list[[x]][[1]])})
  results[[i]]$df = do.call(rbind, together)
}

df_tiers = do.call(rbind, lapply(results, function(x){x$df}))
library(magrittr)
df_tiers %<>%
  mutate(
    Players = gsub("\\s([A-Z][A-Z])", "",
                   gsub("\\s([A-Z][A-Z][A-Z])", "", Players))
  )
  

## Add in Yahoo costs --------------

yahoo_costs = read.csv("C:\\Users\\haunf\\Downloads\\Yahoo_DF_contest_lineups_update_template.csv",
                       head = T)[-(1:7), ]



## Join Costs and Tiers together ------------

df_joined = yahoo_costs %>%
  mutate(
    FullName = paste(First.Name, Last.Name)
  ) %>% 
  left_join(
    df_tiers,
    by = c("FullName" = "Players")
  )

library(fuzzyjoin)
df_fjoined = yahoo_costs %>%
  mutate(
    FullName = paste(First.Name, Last.Name)
  ) %>% 
  stringdist_right_join(df_tiers, by = c(FullName = "Players"), max_dist = 1) %>%
  mutate(
    Tier = ifelse(is.na(Number), 999, Number)
  ) %>%
  arrange(Type) 
## Check for drops
sum(!is.na(df_fjoined %>%
             distinct(Players, Number) %>%
             .$Number))
nrow(df_tiers)
df_fjoined %>%
  filter(is.na(Number))


## Optimization --------------------

df_in_both = df_fjoined %>%
  filter(!is.na(Salary))
library(lpSolve)
f.obj = df_in_both$Tier
f.con = matrix(
  c(df_in_both$Salary,
  (df_in_both$Type == "QB")*1,
  (df_in_both$Type == "RB")*1,
  (df_in_both$Type == "WR")*1,
  (df_in_both$Type == "TE")*1,
  (df_in_both$Type == "FLX")*1,
  (df_in_both$Type == "DST")*1,
  ## Uncomment in case a player is duplicated in flex and position
  (df_in_both$Players == "Robby Anderson")*1,
  (df_in_both$Players == "J.K. Dobbins")*1
  ),
  byrow = T,
  nrow = 9
)
f.dir = c("<=", "==", "==", "==", "==", "==", "==", "==", "==")
f.rhs = c(200, 1, 2, 3, 1, 1, 1, 1, 1)

optimization = lp ("min", f.obj, f.con, f.dir, f.rhs,
                   binary.vec = 1:nrow(df_in_both),
                   all.bin = T, num.bin.solns = 100)
#df_in_both$Player[optimization$solution == 1]
#df_in_both$Salary[optimization$solution == 1]
#df_in_both$Tier[optimization$solution == 1]

sol_list = lapply(1:optimization$num.bin.solns,
       function(x){
        sol_list = data.frame(
          "Players" = df_in_both$Player[which(optimization$solution == 1)[9*(x-1) + (1:9)] %% ncol(f.con)],
          "Salary" = df_in_both$Salary[which(optimization$solution == 1)[9*(x-1) + (1:9)] %% ncol(f.con)],
          "Total Salary" = sum(df_in_both$Salary[which(optimization$solution == 1)[9*(x-1) + (1:9)] %% ncol(f.con)]),
          "Tiers" = df_in_both$Tier[which(optimization$solution == 1)[9*(x-1) + (1:9)] %% ncol(f.con)],
          "Position" = c("QB", "RB", "RB", "WR", "WR", "WR", "TE", "FLX", "DST"),
          "Solution" = x
        )
       }
)

df_all_sols = do.call("rbind", sol_list)
df_all_sols %>%
  group_by(Position, Players, Salary) %>%
  tally() %>%
  arrange(Position, desc(n)) %>%
  data.frame()
