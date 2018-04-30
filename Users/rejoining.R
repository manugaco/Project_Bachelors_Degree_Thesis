

#Getting the 7 variables from all the users ID till now

list_comp <- as.vector(UsersDFp$.)

listdf_comp <- twListToDF(lookupUsers(list_comp))
listdf_comp <- listdf_comp %>%
  arrange(by=id) %>%
  unique() %>%
  dplyr:::select(all)

list_comp2 <- as.vector(UsersDF_clean$id)

listdf_comp2 <- twListToDF(lookupUsers(list_comp2))
listdf_comp2 <- listdf_comp2 %>%
  arrange(by=id) %>%
  unique() %>%
  dplyr:::select(all)

list_comp3 <- as.vector(UsersDFLOOP$id_str)

listdf_comp3 <- twListToDF(lookupUsers(list_comp3))
listdf_comp3 <- listdf_comp3 %>%
  arrange(by=id) %>%
  unique() %>%
  dplyr:::select(all)

list_comp4 <- as.vector(UsersDFPRO$id_str)

listdf_comp4 <- twListToDF(lookupUsers(list_comp4))
listdf_comp4 <- listdf_comp4 %>%
  arrange(by=id) %>%
  unique() %>%
  dplyr:::select(all)

list_comp_c <- rbind(listdf_comp, listdf_comp2, listdf_comp3, listdf_comp4)

rm(list_comp,list_comp2, list_comp3, list_comp3, listdf_comp, listdf_comp2, listdf_comp3, listdf_comp4, 
   UsersDFLOOP, UsersDF_clean, UsersDFp)

UsersDF_1 <- rbind(list_comp_c, UsersDF_FR, UsersDF)

save(UsersDF_1, file = "Objects/USERS_TESTING.RData")

#Fourth loop using 3 coputers (initial users - 378474)

UsersDF_clean4 <- rbind(UsersDF_clean3, UsersDF_FR1_L1, UsersDF_FR1_L2, UsersDF_FR1_L3, UsersDF_FR10_L2, UsersDF_FR11_L2, 
                        UsersDF_FR12_L2, UsersDF_FR13_L2, UsersDF_FR2_L1, UsersDF_FR2_L2, UsersDF_FR3_L1, UsersDF_FR3_L2,
                        UsersDF_FR4_L1, UsersDF_FR4_L2, UsersDF_FR5_L2, UsersDF_FR6_L2, UsersDF_FR7_L2, 
                        UsersDF_FR9_L2, UsersDF_FR9_L2)

UsersDF_clean4 <- UsersDF_clean4[!duplicated(UsersDF_clean4$id),]

UsersDF_clean5 <- rbind(UsersDF_clean5, UsersDF_clean4)
UsersDF_clean5 <- UsersDF_clean5[!duplicated(UsersDF_clean5$id),]

UsersDF_clean7 <- rbind(UsersDF_clean6, UsersDF_FR15_L2, UsersDF_FR16_L2, UsersDF_FR17_L2, UsersDF_FR18_L2, 
                        UsersDF_FR19_L2, UsersDF_FR20_L2)

UsersDF_clean7 <- UsersDF_clean7[!duplicated(UsersDF_clean7$id),]

UsersDF_clean8 <- rbind(UsersDF_clean7, UsersDF_FR1_L1, UsersDF_FR2_L1, UsersDF_FR1_L3, UsersDF_FR2_L3, UsersDF_FR3_L3)

UsersDF_clean8 <- UsersDF_clean8[!duplicated(UsersDF_clean8$id),]

UsersDF_clean9 <- rbind(UsersDF_clean8, UsersDF_FR1_L2, UsersDF_FR2_L2, UsersDF_FR3_L2,UsersDF_FR4_L2, 
                        UsersDF_FR5_L2, UsersDF_FR1_L3)

UsersDF_clean9 <- UsersDF_clean9[!duplicated(UsersDF_clean9$id),]

UsersDF_clean10 <- rbind(UsersDF_clean9, UsersDF_FR1_L2, UsersDF_FR2_L2, UsersDF_FR3_L2, UsersDF_FR4_L2, 
                        UsersDF_FR5_L2, UsersDF_FR6_L2, UsersDF_FR7_L2, UsersDF_FR8_L2, UsersDF_FR9_L2, 
                        UsersDF_FR10_L2, UsersDF_FR11_L2, UsersDF_FR12_L2, UsersDF_FR13_L2, UsersDF_FR14_L2, 
                        UsersDF_FR1_L3, UsersDF_FR2_L3, UsersDF_FR3_L3, UsersDF_FR4_L3, UsersDF_FR5_L3, 
                        UsersDF_FR6_L3, UsersDF_FR4_L4,UsersDF_FR5_L4, UsersDF_FR7_L4, UsersDF_FR10_L4, 
                        UsersDF_FR11_L4, UsersDF_FR12_L4)

UsersDF_clean10 <- UsersDF_clean10[!duplicated(UsersDF_clean10$id),]

UsersDF_clean11 <- rbind(UsersDF_clean10, UsersDF_FR1_L2, UsersDF_FR3_L2, UsersDF_FR4_L2, UsersDF_FR6_L2, 
                         UsersDF_FR7_L2, UsersDF_FR9_L2, UsersDF_FR10_L2, UsersDF_FR11_L2, UsersDF_FR12_L2, 
                         UsersDF_FR13_L2, UsersDF_FR14_L2, 
                         UsersDF_FR1_L3, UsersDF_FR2_L3, UsersDF_FR1_L4,UsersDF_FR2_L4, UsersDF_FR3_L4, UsersDF_FR5_L4, 
                         UsersDF_FR6_L4, UsersDF_FR7_L4)

UsersDF_clean11 <- UsersDF_clean11[!duplicated(UsersDF_clean11$id),]

UsersDF_clean12 <- rbind(UsersDF_clean11, UsersDF_FR1_L3, UsersDF_FR2_L3, UsersDF_FR3_L3, UsersDF_FR4_L3, 
                         UsersDF_FR5_L3, UsersDF_FR6_L3, UsersDF_FR1_L4, UsersDF_FR2_L4, UsersDF_FR3_L4, 
                         UsersDF_FR4_L4, UsersDF_FR5_L4, UsersDF_FR6_L4, UsersDF_FR7_L4, UsersDF_FR8_L4, 
                         UsersDF_FR9_L4, UsersDF_FR10_L4, UsersDF_FR11_L4, UsersDF_FR12_L4, UsersDF_FR13_L4)

UsersDF_clean12 <- UsersDF_clean12[!duplicated(UsersDF_clean12$id),]

UsersDF_clean13 <- rbind(UsersDF_clean13, UsersDF_FR1_L4, UsersDF_FR2_L4, UsersDF_FR3_L4, UsersDF_FR4_L4)

UsersDF_clean13 <- UsersDF_clean13[!duplicated(UsersDF_clean13$id),]

UsersDF_clean14 <- rbind(UsersDF_clean13, UsersDF_FR1_L3, UsersDF_FR2_L3, UsersDF_FR3_L3, UsersDF_FR4_L3, 
                         UsersDF_FR5_L3, UsersDF_FR6_L3, UsersDF_FR7_L3, UsersDF_FR8_L3, UsersDF_FR1_L4, 
                         UsersDF_FR2_L4, UsersDF_FR3_L4, UsersDF_FR4_L4, UsersDF_FR5_L4, UsersDF_FR6_L4, 
                         UsersDF_FR7_L4, UsersDF_FR8_L4, UsersDF_FR9_L4, UsersDF_FR10_L4, UsersDF_FR11_L4, 
                         UsersDF_FR12_L4)

UsersDF_clean14 <- UsersDF_clean14[!duplicated(UsersDF_clean14$id),]

save(UsersDF_clean, file = "Objects/UDFF_C_L1.RData") #(180000)
save(UsersDF_clean2, file = "Objects/UDFF_C_L2.RData") #(270000)
save(UsersDF_clean3, file = "Objects/UDFF_C_L3.RData") #(380000)
save(UsersDF_clean4, file = "Objects/UDFF_C_L4.RData") #(510000)
save(UsersDF_clean5, file = "Objects/UDFF_C_L5.RData") #(580000)
save(UsersDF_clean6, file = "Objects/UDFF_C_L6.RData") #(710000)
save(UsersDF_clean7, file = "Objects/UDFF_C_L7.RData") #(750000)
save(UsersDF_clean8, file = "Objects/UDFF_C_L8.RData") #(780000)
save(UsersDF_clean9, file = "Objects/UDFF_C_L9.RData") #(825000)
save(UsersDF_clean10, file = "Objects/UDFF_C_L10.RData") #(1005000)
save(UsersDF_clean11, file = "Objects/UDFF_C_L11.RData") #(1105000)
save(UsersDF_clean12, file = "Objects/UDFF_C_L12.RData") #(1218000)
save(UsersDF_clean13, file = "Objects/UDFF_C_L13.RData") #(1260000)
save(UsersDF_clean14, file = "Objects/UDFF_C_L14.RData") #(1340000)
