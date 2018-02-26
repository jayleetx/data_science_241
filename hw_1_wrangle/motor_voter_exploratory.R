overlap <- semi_join(ORMV, ORreg, by = "VOTER_ID")
overlap2 <- semi_join(ORreg, ORMV, by = "VOTER_ID")
extra <- anti_join(ORMV, ORreg, by = "VOTER_ID")
extra2 <- anti_join(ORreg, ORMV, by = "VOTER_ID")
# Why are there 83 extra rows in one than the other?
biguns <- ORMV %>% group_by(VOTER_ID) %>% filter(n()>1)
combined <- ORMV %>%
  +   group_by(VOTER_ID) %>%
  +   filter(n()>1) %>%
  +   mutate(MV1 = ifelse(DESCRIPTION == "Motor Voter", 1, 0), MV2 = ifelse(DESCRIPTION == "MVPhase2", 1, 0)) %>%
  +   select(1:5, -(2:3)) %>%
  +   summarise_each(funs(sum))