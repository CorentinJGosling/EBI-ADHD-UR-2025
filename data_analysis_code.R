
dcct = readxl::read_excel("datasets/CCT_complete.csv")
dma = readxl::read_excel("datasets/MA_complete.csv")
dcct_rr = dcct %>%
  group_by(factor) %>%
  filter(all(measure %in% c("OR", "RR")))

dcct_smd = dcct %>%
  group_by(factor) %>%
  filter(!all(measure %in% c("OR", "RR")))

res_list_rr = umbrella(dcct_rr,
                       verbose = TRUE,
                       max_asymmetry = 15,
                       mult.level = TRUE, method.var = "PM", 
                       r = 0.8, pre_post_cor = 0.5)

res_list_smd = umbrella(dcct_smd,
                        verbose = FALSE,
                        mult.level = TRUE, method.var = "REML", 
                        r = 0.8, pre_post_cor = 0.5)

union_smd_rr = union.umbrella(res_list_rr, res_list_smd) 

res_GRADE = add.evidence(union_smd_rr, criteria = "GRADE")

dat_metaumb = data.frame(
  indirectness_metaumbrella = as.character(sapply(res_GRADE, function(x) x$indirectness)),
  rob_metaumbrella = as.numeric(as.character(sapply(res_GRADE, function(x) x$overall_rob))),
  report_metaumbrella = as.numeric(as.character(sapply(res_GRADE, function(x) x$report_rob))),
  Factor = as.character(sapply(res_GRADE, function(x) x$factor))
)

res_smd_rr = summary(res_GRADE) %>%
  left_join(dat_metaumb)


res_m_cred = res_smd_rr
res_m_cred$Outcome = t(do.call(cbind,
                               stringr::str_split(res_m_cred$Factor, "_")))[, 3]
res_m_cred$Outcome_raters = t(do.call(cbind,
                                      stringr::str_split(res_m_cred$Factor, "_")))[, 4]
res_m_cred$Outcome_followup = t(do.call(cbind,
                                        stringr::str_split(res_m_cred$Factor, "_")))[, 5]
res_m_cred$Intervention = t(do.call(cbind, 
                                    stringr::str_split(res_m_cred$Factor, "_")))[, 2]
res_m_cred$"Meta-review" = t(do.call(cbind, 
                                     stringr::str_split(res_m_cred$Factor, "_")))[, 1]


res_m = left_join(res_m_cred, dma, by="Factor") 

res_m$rob_num = res_m$low_RoB_num
res_m$report_rob_num = res_m$RoB_Report_num
res_m$indirectness_num = 
  with(res_m,
       ifelse(!grepl("Homogeneous", res_m$PICO_amstar_precise, fixed = TRUE), "serious",
              ifelse(res_m$available_control < 75 , "serious", "no")))

res_m$n_studies = res_m$n_studies.x
measure = res_m$measure
I2 = res_m$I2


indirectness = res_m$indirectness_metaumbrella
report_rob = res_m$report_metaumbrella
rob = res_m$rob_metaumbrella

indirectness = res_m$indirectness_num
report_rob = res_m$report_rob_num
rob = res_m$rob_num

View(with(res_m, cbind(
  res_m$Factor, res_m$indirectness_num, res_m$report_rob_num, res_m$rob_num,
  res_m$indirectness_metaumbrella, res_m$report_metaumbrella, res_m$rob_metaumbrella)))
# GRADE
conf.inter_g <- str_extract_all(res_m$eG_CI, "-?\\d+\\.\\d+", simplify=TRUE)
res_m$ci_lo_g <- as.numeric(as.character(conf.inter_g[,1]))
res_m$ci_up_g <- as.numeric(as.character(conf.inter_g[,2]))

conf.inter_or <- str_extract_all(res_m$eOR_CI, "-?\\d+\\.\\d+", simplify=TRUE)
res_m$ci_lo_or <- as.numeric(as.character(conf.inter_or[,1]))
res_m$ci_up_or <- as.numeric(as.character(conf.inter_or[,2]))

pred.inter_g <- str_extract_all(res_m$PI_eG, "-?\\d+\\.\\d+", simplify=TRUE)
res_m$PI_lo_g <- as.numeric(as.character(pred.inter_g[,1]))
res_m$PI_up_g <- as.numeric(as.character(pred.inter_g[,2]))

pred.inter_or <- str_extract_all(res_m$PI_eOR, "-?\\d+\\.\\d+", simplify=TRUE)
res_m$PI_lo_or <- as.numeric(as.character(pred.inter_or[,1]))
res_m$PI_up_or <- as.numeric(as.character(pred.inter_or[,2]))

res_m$CI_lo = CI_lo = with(res_m, 
                           ifelse(measure == "G", ci_lo_g, ci_lo_or))
res_m$CI_up = CI_up = with(res_m, 
                           ifelse(measure == "G", ci_up_g, ci_up_or))

res_m$PI_lo = PI_lo = with(res_m, 
                           ifelse(measure == "G", PI_lo_g, PI_lo_or))
res_m$PI_up = PI_up = with(res_m, 
                           ifelse(measure == "G", PI_up_g, PI_up_or))

ASD_symptoms = c("Overall ASD symptoms" , "Social-communication", 
                 "Restricted/repetitive behaviors", "Sensory Profile")
functioning = c("Global cognition (IQ)", "Specific cognition (nvIQ)",
                "Adaptive behaviors", "Quality of life", 
                "Language (Expressive skills)", "Language (Overall skills)", 
                "Language (Receptive skills)") 
safety = c("Acceptability", "Tolerability", "Adverse events")
comorbidities = c("ADHD symptoms", "Anxiety", "Mood related symptoms")
ASD_related = c("Disruptive behaviors")
sleep = c("Sleep quality", "Sleep quantity")

res_m$sig = ifelse(as.numeric(as.character(res_m$p_value)) < 0.05, TRUE, FALSE)

res_m = res_m %>%
  mutate(
    age_short = case_when(
      Age == "< 6 yo" ~ "Pre-school (<6yo)",
      Age == "6-17 yo" ~ "Children/Adolescents (6-17yo)",
      Age == ">= 18 yo" ~ "Adults (>=18yo)"),
    age_vshort = case_when(
      Age == "< 6 yo" ~ "Pre-school",
      Age == "6-17 yo" ~ "Children/Adolescents",
      Age == ">= 18 yo" ~ "Adults"),
    
    col_sig = case_when(
      eG <= -0.8 ~ "#9e6b4c",
      eG <= -0.5 &  eG > -0.8 ~ "#ec240a",
      eG <= -0.2 &  eG > -0.5 ~ "#df958c",
      eG < 0.2 &  eG > -0.2 ~ "#D4D4D4",
      eG >= 0.2 &  eG < 0.5 ~ "#a6d2a7",
      eG >= 0.5 &  eG < 0.8 ~ "#02d70e",
      eG >= 0.8 ~ "#19d2ff"),
    effect_text = case_when(
      eG <= -0.8 & sig ~ paste0("a very large, statistically significant, and harmful effect"),
      eG <= -0.8 & !sig ~ paste0("no effect"),
      (eG <= -0.5 &  eG > -0.8) & sig ~ paste0("a moderate-to-large, statistically significant, harmful effect"),
      (eG <= -0.5 &  eG > -0.8) & !sig ~ paste0("no effect (not statistically significant)"),
      (eG <= -0.2 &  eG > -0.5) & sig ~ paste0("a small-to-moderate, statistically significant, harmful effect"),
      (eG <= -0.2 &  eG > -0.5) & !sig ~ paste0("no effect (not statistically significant)"),
      (eG < 0.2 &  eG > -0.2) & sig ~ paste0("a negligeable but statistically significant, harmful effect"),
      (eG < 0.2 &  eG > -0.2) & !sig ~ paste0("no effect (not statistically significant)"),
      (eG >= 0.2 &  eG < 0.5) & sig ~ paste0("a small-to-moderate, statistically significant, clinically beneficial effect"),
      (eG >= 0.2 &  eG < 0.5) & !sig ~ paste0("no effect (not statistically significant)"),
      (eG >= 0.5 &  eG < 0.8) & sig  ~ paste0("a moderate-to-large, statistically significant, clinically beneficial effect"),
      (eG >= 0.5 &  eG < 0.8) & !sig ~ paste0("no effect (not statistically significant)"),
      eG >= 0.8 & sig  ~ paste0("a very large, statistically significant, clinically beneficial effect"),
      eG >= 0.8 & !sig  ~ paste0("no effect (not statistically significant)")),
    col_grade = case_when(
      GRADE == "Very low" ~ "#000000",
      GRADE == "Low" ~ "#ffa064",
      GRADE == "Moderate" ~ "#cf2061",
      GRADE == "High" ~ "#3d0173"),           
    GRADE_rank = case_when(
      GRADE == "Very low" ~ 1,
      GRADE == "Low" ~ 1.5,
      GRADE == "Moderate" ~ 3,
      GRADE == "High" ~ 4
    ),
    col_contour = case_when(
      GRADE == "Very low" ~ "transparent",
      GRADE != "Very low" ~ "#000"
    )
    
  )

res_m$Intervention[grepl("transcranial magnetic", res_m$Intervention, fixed=TRUE)] <- "TMS-rTMS"
res_m$Intervention[grepl("transcranial direct", res_m$Intervention, fixed=TRUE)] <- "tDCS"

res_m$o = res_m$Outcome
res_m$o[res_m$o == "Combined ADHD symptoms (inattentive + hyperactive/impulsive)"] <- "Core ADHD symptoms"
res_m$o[res_m$o == "Conduct Disorder symptoms"] <- "Conduct disorder symptoms (CD)"
res_m$o[res_m$o == "Conduct Disorder symptoms/Oppositional Defiant Disorder symptoms"] <- "CD/ODD symptoms"
res_m$o[res_m$o == "Oppositional Defiant Disorder symptoms"] <- "Oppositional symptoms (ODD)"
res_m$o[res_m$o == "Executive functioning (tests)"] <- "Exec. functions (tests)"
res_m$o[res_m$o == "Executive functioning (report)"] <- "Exec. functions (report)"
res_m$o[res_m$o == "Quality of life (patients)"] <- "Quality of life"
res_m$o[res_m$o == "Tolerability (discontinuation due to adverse events)"] <- "Tolerability"
res_m$o[res_m$o == "Acceptability: all-cause discontinuation"] <- "Acceptability"
res_m$o[res_m$o == "Specific adverse events: decreased appetite"] <- "AE (decreased appetite)"
res_m$o[res_m$o == "Specific adverse events: sleep problems"] <- "AE (sleep problems)"


res_m$f = res_m$Outcome_followup
res_m$f[res_m$f == "At study endpoint (closest to 12 weeks)"] <- "Endpoint (~12wk)"
res_m$f[res_m$f == "At follow-up (closest to 26 weeks)"] <- "Follow-up (~26wk)"
res_m$f[res_m$f == "At follow-up (closest to 52 weeks)"] <- "Follow-up (~52wk)"
res_m$f[res_m$f == "Closest to 52 weeks"] <- "Follow-up (~52wk)"
res_m$f[res_m$f == "Mixed follow-up"] <- "Mixed"
res_m$f[res_m$f == "Mixed follow-ups (not specified)"] <- "Mixed"

# sort(unique(res_m$o))
# sort(unique(res_m$r))
# sort(unique(res_m$f))

res_m$Outcome = res_m$o
res_m$Raters = ifelse(!grepl("ADHD symp",res_m$Outcome, fixed=TRUE) & res_m$outcome_raters == "Mixed", "N/A", res_m$outcome_raters)
res_m$Follow_up = res_m$f

# 2. Add raters for specific outcomes
res_m$Outcome <- ifelse(
  res_m$Outcome %in% c("Core ADHD symptoms", "CGI"),
  paste0(res_m$Outcome, " [", res_m$Raters, "]"),
  res_m$Outcome)
res_m$Outcome[grepl("CGI ", res_m$Outcome, fixed=TRUE)]<- "CGI"
res_m$Outcome[res_m$Outcome == "Executive functioning (mixed)"] <- "Exec. functions (mixed)"

# n_participants = dcct %>%
#   ungroup() %>%
#   group_by(intervention_type, trial_id) %>%
#   filter(n_tot == max(n_tot)) %>%
#   slice(1) %>%
#   ungroup() %>%
#   group_by(intervention_type) %>%
#   summarise(n_intervention_type = unique(paste0("n-", intervention_type, 
#                                                 " = ", sum(n_tot))),
#             n_studies_manual = length(unique(trial_id)))


# 
# res_m$outcome_short = res_m$outcome_general
# res_m$outcome_short[res_m$outcome_short == "Restricted/repetitive behaviors"] <- "Rest./repet. behaviors"
# res_m$outcome_short[res_m$outcome_short == "Language (Expressive skills)"] <- "Language (Expressive)"
# res_m$outcome_short[res_m$outcome_short == "Language (Receptive skills)"] <- "Language (Receptive)"
# res_m$outcome_short[res_m$outcome_short == "Language (Overall skills)"] <- "Language (Overall)"
# res_m$outcome_short[res_m$outcome_short == "Specific cognition (nvIQ)"] <- "Specific cognition"

# !!!!!!!!!!!!!!!!!!!
# if (length(unique(res_m$intervention_general)) != 
#     length(unique(na.omit(res_m$intervention_spell)))) {
#   print(unique(res_m$intervention_general[is.na(res_m$intervention_spell)]))
#   stop("problem identifying interventions")
# }

res_export = res_m %>%
  filter(n_studies.x > 1) %>%
  extract(
    col = value_CI, 
    into = c("CI_lower", "CI_upper"), 
    regex = "\\[([-0-9.]+),\\s*([-0-9.]+)\\]", 
    remove = FALSE, 
    convert = TRUE
  ) %>%
  mutate(es = ifelse(measure == "RR", log(value), value),
         ci_lo = ifelse(measure == "RR", log(CI_lower), CI_lower),
         ci_up = ifelse(measure == "RR", log(CI_upper), CI_upper))

if (restrict_low_bias) {
  rio::export(res_export, 
              paste0(chemin, "UR_ADHD_analysis_low_RoB.xlsx"), 
              overwrite = TRUE)
  rio::export(res_export, 
              paste0("C:/Data/drive_gmail/Recherche/",
                     "Article 34 - EBI-ADHD/data_analysis/supplementary/",
                     "UR_ADHD_analysis_low_RoB.xlsx"
              ), 
              overwrite = TRUE)
} else {
  rio::export(res_export, paste0(chemin, "UR_ADHD_analysis.xlsx"), overwrite = TRUE)
  
  rio::export(res_export, 
              "C:/Data/drive_gmail/Recherche/app_ebi/ebi_apps/automated_gen/ebiadhd_database/UR_TOTAL_analysis.xlsx", 
              overwrite = TRUE)
  
  rio::export(res_export, 
              paste0("C:/Data/drive_gmail/Recherche/",
                     "Article 34 - EBI-ADHD/data_analysis/supplementary/",
                     "UR_ADHD_analysis.xlsx"
              ), 
              overwrite = TRUE)
  
  zioup_dataset = function(y) {
    DAT = data.frame(y["x"])
    names(DAT) = gsub("x.", "", names(DAT))
    DAT[, "Factor"] <- y["factor"]$factor
    DAT[, "factor"] <- y["factor"]$factor
    DAT[, "measure"] <- y["measure"]
    return(DAT)
  }
  dat_rct = dplyr::bind_rows(do.call(dplyr::bind_rows,
                                     lapply(res_list_smd, zioup_dataset)),
                             do.call(dplyr::bind_rows,
                                     lapply(res_list_rr, zioup_dataset)))
  # View(dcct %>%
  #        select(intervention_type, intervention_comment, intervention_label,
  #               duration_month, dose, dose_unit, setting, factor) %>%
  #        distinct())
  dat_export_rct = left_join(
    dcct %>%
      select(author, year, 
             duration_month, dose, dose_unit, factor) %>%
      distinct(),
    dat_rct %>%
      select(author, year, measure, value, ci_lo, ci_up, n_cases, n_controls, factor, Factor)
  ); nrow(dat_export_rct)
  
  
  all(dat_export_rct$Factor %in% res_m$Factor)
  dat_export_rct$Factor[which(!dat_export_rct$Factor %in% res_m$Factor)]
  
  rio::export(dat_export_rct,
              "C:/Data/drive_gmail/Recherche/app_ebi/ebi_apps/automated_gen/ebiadhd_database/RCT_TOTAL_analysis.xlsx",
              overwrite = TRUE)
  
  
}
