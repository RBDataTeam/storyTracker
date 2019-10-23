setwd("G:/Reap Benefit/Data/Story Tracker")


library(tidyverse)
library(lubridate)
library(stringi)

# Set input and output folders

input_folder <- "G:/Reap Benefit/Data/Story Tracker Main/Input/"
output_folder <- "G:/Reap Benefit/Data/Story Tracker Main/Output/"

# Read data
story_tracker <- readxl::read_xlsx(paste0(input_folder,"StoryTracker.xlsx"), sheet = 1)

# Calculate Actions
story_tracker <- story_tracker %>%
   mutate (
     `Name of Student ?` = str_to_title(`Name of Student ?`),
    Add_report = case_when(
      `Type of Story?` =="New Action - Issue Reported" ~ 1,
      TRUE ~ 0),
    Report_follow = case_when(
      `How many time did the student follow up with the stakeholder` > 0  ~ 1,
      TRUE ~ 0),
    Add_campaign = case_when(
      `Type of Story?` =="New Action - Campaign Creation / Participation" ~ 1,
        TRUE ~ 0),
    Campaign_join = case_when(
      `How many ninjas participated in the campaign?` > 0  ~ 1,
      TRUE ~ 0),
    Campaign_teamupdate = case_when(
      stri_length(`Please write if you want to highlight more about this - please include campaign team members names Please separate names with semi-colons (;)`) > 3 ~ 1,
      TRUE ~ 0),
    Solutions_desc = case_when(
      stri_length(`If it is a new solutions built, please describe  the steps to build it`) > 3 ~ 1,
      TRUE ~ 0),
    Solutions_bene = case_when(
      `Electricity saved (in units) (through the product)`> 1 ~ 1,
      `Water Saved (In Litres) (through the product)`> 1 ~ 1,
      `Waste Diverted (Kg) (through the product)`> 1 ~ 1,
      TRUE ~ 0),
    Solutions_idea = case_when(
      `Solution Type` == "Idea" ~ 1,
      TRUE ~ 0),
    Solutions_proto = case_when(
      `Solution Type` == "Prototype" ~ 1,
      TRUE ~ 0),
    Solutions_product = case_when(
      `Solution Type` == "Final product" ~ 1,
      TRUE ~ 0),
    Successstory_report = case_when(
      `Describe action taken` =="Reported an Issue and then Followed multiple times"  ~ 1,
      TRUE ~ 0),
    Successstory_audit = case_when(
      `Describe action taken` =="Did Waste/Water Audit"  ~ 1,
      TRUE ~ 0),
    Successstory_campaign = case_when(
      `Describe action taken` =="Created a Campaign and solved the Issue"  ~ 1,
      TRUE ~ 0),
    Successstory_product_implement = case_when(
      `Describe action taken` =="Implemented a Product/Service to Solve a Problem"  ~ 1,
      TRUE ~ 0),
    Successstory_product_suggest = case_when(
      `Describe action taken` =="Suggested a product to solve a Problem"  ~ 1,
      TRUE ~ 0),
    Successstory_impact = case_when(
      stri_length(`What is the impact of this action? (through the story)`) > 5 ~ 1,
      TRUE ~ 0),
    Report_metofficial = case_when(
      `Did  the Student meet Govt Official/Stakeholder for reporting the issue?` == "Yes" ~ 1,
      TRUE ~ 0),
    Report_convincingother = case_when(
      stri_length(`How many team members reported the issues? (include total number of students)`) > 1 ~ 1,
      TRUE ~ 0),
    Report_multipleagency = case_when(
      stri_length(`If student reported the same issue to multiple stakeholder then select the specific agency`) > 1 ~ 1,
      TRUE ~ 0),
    Report_calculatingimpact = case_when(
      `Did the student calculate the impact from the reporting actions taken?` == "Yes" ~ 1,
      TRUE ~ 0),
    Campaign_Impactdetails = case_when(
      `Water Saved (in Litres) (Through the campaign)` > 0 ~ 1,
      `Waste Redirected (in kg) (Through the campaign)` > 0 ~ 1,
      `Electricity Saved (in Units) (Through the campaign)` > 0 ~ 1,
      TRUE ~ 0),
    Campaign_fundraised = case_when(
      `Value of funds raised so far (in the campaign)` > 0 ~ 1,
      TRUE ~ 0),
    Campaign_peoplereached = case_when(
      `How many community members did the campaign reach out to?` > 0 ~ 1,
      TRUE ~ 0),
    Solution_noofimplement = case_when(
      `Number of solutions (UPOs / Urinals etc) implemented (of this solutions)` > 0 ~ 1,
      TRUE ~ 0),
    Solutions_feedback = case_when(
      `Did the student document feedback from the user?` == "Yes" ~ 1,
      TRUE ~ 0),
    Solutions_involved = case_when(
      `How many ninjas were involved creation and design of the solution?` > 0 ~ 1,
      TRUE ~ 0),
    Solutions_multipleprototype = case_when(
      `Did the student make multiple prototypes of the product?` == "Yes" ~ 1,
      TRUE ~ 0),
    Success_story = case_when(
      `Type of Story?` =="New Action - Success Story" ~ 1,
      TRUE ~ 0),

    
    
    #Skills
    Data_orientation = Add_report +  Campaign_teamupdate + 
      Successstory_report + Successstory_product_implement + 
      Successstory_product_suggest + Successstory_impact + 
      Solutions_bene + Solutions_idea + Solutions_proto + Solutions_product + 
      Report_calculatingimpact + Campaign_Impactdetails + Solutions_feedback, 
    
    hands_on = Successstory_product_implement + Solutions_desc + 
      Solutions_idea + Solutions_proto + Solution_noofimplement 
      + Solutions_multipleprototype,
    
    citizenship = Add_report + Report_follow + Add_campaign + 
      Campaign_join + Successstory_report + 
      Successstory_audit + Successstory_campaign + 
      Successstory_product_suggest + Successstory_product_implement + 
      Solutions_desc + Solutions_product + Report_metofficial + 
      Report_convincingother + Report_multipleagency + Campaign_peoplereached +
      Solutions_involved,
    
    problem_solving = Add_report + Report_follow + Add_campaign + 
      Successstory_campaign + Successstory_product_implement + 
      Solutions_idea + Solutions_proto + Solutions_product + 
      Report_calculatingimpact + Campaign_Impactdetails +
      Campaign_fundraised + Solutions_involved +
      Solutions_multipleprototype,
    
    communications = Add_report + Report_follow + 
      Add_campaign + Campaign_teamupdate + Successstory_report + 
      Successstory_campaign + Solutions_desc + Solutions_idea + 
      Solutions_proto + Solutions_product + Report_metofficial +
      Report_convincingother + Report_multipleagency + Campaign_peoplereached,
    
    critical_thinking = Add_report + Campaign_teamupdate + 
      Successstory_report + Successstory_product_implement + 
      Successstory_impact + Solutions_product + Solutions_proto + 
      Solutions_idea + Solutions_desc + Solutions_bene + 
      Report_calculatingimpact + Campaign_Impactdetails + Solutions_feedback +
      Solutions_multipleprototype,
    
    community_collaboration = Add_campaign + Campaign_join + 
      Successstory_campaign + Report_metofficial + Report_convincingother+
      Report_multipleagency + Campaign_peoplereached + 
      Solution_noofimplement + Solutions_involved,
    
    Applied_empathy = Add_report + Campaign_teamupdate + Successstory_impact +
      Solutions_product + Solutions_proto + Solutions_idea + 
      Solutions_desc + Solutions_bene + Report_convincingother+
      Report_calculatingimpact + Campaign_peoplereached + 
      Campaign_Impactdetails,
    
    Grit = Report_follow + Add_campaign + Campaign_join + 
      Campaign_teamupdate + Successstory_campaign + 
      Successstory_product_implement + Solutions_idea + Solutions_proto+
      Report_metofficial + Report_multipleagency  +
      Solution_noofimplement + Solutions_multipleprototype,
    
    Entrepreneurship = Report_follow + Add_campaign + Campaign_teamupdate +
      Successstory_campaign + Successstory_product_implement + 
      Solutions_idea + Solutions_proto + Report_convincingother +
      Solution_noofimplement + Solutions_feedback + Solutions_involved +
      Solutions_multipleprototype,
    
    SNI_total = Add_report*11 + Report_follow*11 +
      Add_campaign*14 + Campaign_join*7  + 
      Campaign_teamupdate*14 + Successstory_audit*1 + 
      Successstory_impact*6 + Successstory_product_suggest*2 + 
      Successstory_product_implement*14 + 
      + Successstory_campaign*14 + Successstory_report*6  +
      Solutions_product*11 + Solutions_proto*17 + Solutions_idea*17 + 
      Solutions_bene*6 + Solutions_desc*9 + 
      Report_metofficial * 9 + Report_convincingother * 12 + 
      Report_multipleagency * 9 + Report_calculatingimpact * 8 +
    Campaign_Impactdetails * 8 + Campaign_fundraised * 5 +
      Campaign_peoplereached * 9 + 
      Solution_noofimplement * 11 + Solutions_feedback * 6 +
      Solutions_involved * 9 + Solutions_multipleprototype * 11
    )

story_summary <- story_tracker %>%
  
  group_by(`School Name`,`Name of Student ?`) %>% 
  summarise(sum_reports = sum(`Add_report`),
            sum_reports_follow = sum(`Report_follow`),
            sum_campaign = sum(`Add_campaign`), sum_campaign_join = sum(`Campaign_join`),
            sum_idea = sum(`Solutions_idea`), sum_prototype = sum(`Solutions_proto`),
            sum_product = sum(`Solutions_product`),sum_successstory = sum(`Success_story`),
            sum_data = sum(`Data_orientation`), sum_hands_on = sum(`hands_on`),
            sum_citizen = sum(`citizenship`),sum_problem = sum(`problem_solving`),
            sum_critical = sum(`critical_thinking`), 
            sum_communications = sum(`communications`),
            sum_comm_Collaboration = sum(`community_collaboration`),
            sum_applied = sum(`Applied_empathy`), sum_grit = sum(`Grit`),
            sum_entre = sum(`Entrepreneurship`),
            sum_sni = sum(`SNI_total`),
            persona = case_when(
              sum_campaign > 0 & 
                (sum_prototype + sum_product > 0) &
                (sum_prototype + sum_product + sum_campaign + 
                   sum_reports + sum_successstory > 5)~ "Action Ant",
              sum_campaign > 0 & 
                (sum_prototype + sum_product <= sum_campaign) ~ "Campaign Chameleon",
              sum_campaign == 0 & 
                (sum_prototype + sum_product > sum_campaign) ~ "Hands On Hippo",
              TRUE ~ "Reporting Rhino")) 

write.csv(story_summary,paste0(output_folder,"Storysummary_",Sys.Date(),".csv"))
write.csv(story_tracker,paste0(output_folder,"Storytracker_sni_",Sys.Date(),".csv"))


