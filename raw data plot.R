
library(janitor)
library(tidyverse)

# Load the data dictionary and the raw data and correct the variable names
# Load the data dictionary and the raw data and correct the variable names
raw_data <- read_csv("/Users/zhangduoduo/Desktop/a3/scripts/AAMLrYO8.csv")
dict <- read_lines("/Users/zhangduoduo/Desktop/a3/scripts/gss_dict.txt", skip = 18) 
# Now we need the labels because these are the actual responses that we need
labels_raw <- read_file("/Users/zhangduoduo/Desktop/a3/scripts/gss_labels.txt")

#### Set-up the dictionary ####
# What we want is a variable name and a variable definition
variable_descriptions <- as_tibble(dict) %>% 
  filter(value!="}") %>% 
  mutate(value = str_replace(value, ".+%[0-9].*f[ ]{2,}", "")) %>% 
  mutate(value = str_remove_all(value, "\"")) %>% 
  rename(variable_description = value) %>% 
  bind_cols(tibble(variable_name = colnames(raw_data)[-1]))
# Now we want a variable name and the possible values
labels_raw_tibble <- as_tibble(str_split(labels_raw, ";")[[1]]) %>% 
  filter(row_number()!=1) %>% 
  mutate(value = str_remove(value, "\nlabel define ")) %>% 
  mutate(value = str_replace(value, "[ ]{2,}", "XXX")) %>% 
  mutate(splits = str_split(value, "XXX")) %>% 
  rowwise() %>% 
  mutate(variable_name = splits[1], cases = splits[2]) %>% 
  mutate(cases = str_replace_all(cases, "\n [ ]{2,}", "")) %>%
  select(variable_name, cases) %>% 
  drop_na()

# Now we have the variable name and the different options e.g. age and 0-9, 10-19, etc.
labels_raw_tibble <- labels_raw_tibble %>% 
  mutate(splits = str_split(cases, "[ ]{0,}\"[ ]{0,}"))

# The function sets up the regex (I know, I know, but eh: https://xkcd.com/208/)
add_cw_text <- function(x, y){
  if(!is.na(as.numeric(x))){
    x_new <- paste0(y, "==", x,"~")
  }
  else{
    x_new <- paste0("\"",x,"\",")
  }
  return(x_new)
}

# The function will be in the row, but it'll get the job done
cw_statements <- labels_raw_tibble %>% 
  rowwise() %>% 
  mutate(splits_with_cw_text = list(modify(splits, add_cw_text, y = variable_name))) %>% 
  mutate(cw_statement = paste(splits_with_cw_text, collapse = "")) %>% 
  mutate(cw_statement = paste0("case_when(", cw_statement,"TRUE~\"NA\")")) %>% 
  mutate(cw_statement = str_replace(cw_statement, ",\"\",",",")) %>% 
  select(variable_name, cw_statement)
# So for every variable we now have a case_when() statement that will convert 
# from the number to the actual response.

# Just do some finally cleanup of the regex.
cw_statements <- 
  cw_statements %>% 
  mutate(variable_name = str_remove_all(variable_name, "\\r")) %>% 
  mutate(cw_statement = str_remove_all(cw_statement, "\\r"))

#### Apply that dictionary to the raw data ####
# Pull out a bunch of variables and then apply the case when statement for the categorical variables
gss <- raw_data %>% 
  select(CASEID, 
         agedc, 
         achd_1c, 
         achdmpl, 
         totchdc, 
         acu0c,
         agema1c,
         achb1c,
         rsh_131a,
         arretwk,
         slm_01, 
         sex, 
         brthcan, 
         brthfcan,
         brthmcan,
         brthmacr,
         brthprvc,
         yrarri,
         prv, 
         region, 
         luc_rst, 
         marstat, 
         amb_01, 
         vismin, 
         alndimmg,
         bpr_16, 
         bpr_19,
         ehg3_01b, 
         odr_10, 
         livarr12, 
         dwelc, 
         hsdsizec,
         brthpcan,
         brtpprvc, # CC's part of the variables
         visminpr,
         rsh_125a, 
         eop_200,
         uhw_16gr,
         lmam_01, 
         acmpryr,
         srh_110,
         srh_115,
         religflg, 
         rlr_110,
         lanhome, 
         lan_01,
         famincg2, 
         ttlincg2, 
         noc1610, 
         cc_20_1,
         cc_30_1,
         ccmoc1c,
         cor_031,
         cor_041,
         cu0rnkc,
         pr_cl,
         chh0014c,
         nochricc,
         grndpa,
         gparliv,
         evermar,
         ma0_220,
         nmarevrc,
         ree_02,
         rsh_131b,
         rto_101,
         rto_110,
         rto_120,
         rtw_300,
         sts_410,
         csp_105,
         csp_110a,
         csp_110b,
         csp_110c,
         csp_110d,
         csp_160,      # values 96,97,98 represent missing responses.
         fi_110) %>%   # therefore, we replace them with NA, for later modelling's covenience
  mutate_at(vars(agedc:fi_110), .funs = funs(ifelse(.>=96, NA, .))) %>% 
  mutate_at(.vars = vars(sex:fi_110),
            .funs = funs(eval(parse(text = cw_statements %>%
                                      filter(variable_name==deparse(substitute(.))) %>%
                                      select(cw_statement) %>%
                                      pull()))))

# Fix the names
gss <- gss %>% 
  clean_names() %>% 
  rename(age = agedc, # AGE OF THE RESPONDENT
         age_first_child = achd_1c,
         age_youngest_child_under_6 = achdmpl,
         total_children = totchdc,
         age_start_relationship = acu0c,
         age_at_first_marriage = agema1c,
         age_at_first_birth = achb1c,
         distance_between_houses = rsh_131a,
         age_youngest_child_returned_work = arretwk,
         feelings_life = slm_01,
         sex = sex,
         place_birth_canada = brthcan, # BIRTH PLACE OF CANADA
         place_birth_father = brthfcan,
         place_birth_mother = brthmcan,
         place_birth_macro_region = brthmacr,
         place_birth_province = brthprvc,# BIRTH PLACE OFTHE RESPONDENTS
         year_arrived_canada = yrarri,
         province = prv,
         region = region, # REGION OF THE RESPONDENTS 5- ALTANNTIC
         pop_center = luc_rst,
         marital_status = marstat,
         aboriginal = amb_01,
         vis_minority = vismin,
         age_immigration = alndimmg,
         landed_immigrant = bpr_16,
         citizenship_status = bpr_19,
         education = ehg3_01b, # SEVEN LEVELS OF EUDCATIONS
         own_rent = odr_10,
         living_arrangement = livarr12,
         hh_type = dwelc,
         hh_size = hsdsizec,
         partner_birth_country = brthpcan, 
         partner_birth_province = brtpprvc,
         # CC's part part of the variables
         partner_vis_minority = visminpr,
         partner_sex = rsh_125a,
         partner_education = eop_200,
         average_hours_worked = uhw_16gr,
         worked_last_week = lmam_01,
         partner_main_activity = acmpryr,
         self_rated_health = srh_110,
         self_rated_mental_health = srh_115,
         religion_has_affiliation = religflg,
         regilion_importance = rlr_110,
         language_home = lanhome,
         language_knowledge = lan_01,
         income_family = famincg2,
         income_respondent = ttlincg2,
         occupation = noc1610,
         childcare_regular = cc_20_1,
         childcare_type = cc_30_1,
         childcare_monthly_cost = ccmoc1c,
         ever_fathered_child = cor_031,
         ever_given_birth = cor_041,
         number_of_current_union = cu0rnkc,
         lives_with_partner = pr_cl,
         children_in_household = chh0014c,
         number_total_children_intention = nochricc,
         has_grandchildren = grndpa,
         grandparents_still_living = gparliv,
         ever_married = evermar,
         current_marriage_is_first = ma0_220,
         number_marriages = nmarevrc,
         religion_participation = ree_02,
         partner_location_residence = rsh_131b,
         full_part_time_work = rto_101,
         time_off_work_birth = rto_110,
         reason_no_time_off_birth = rto_120,
         returned_same_job = rtw_300,
         satisfied_time_children = sts_410,
         provide_or_receive_fin_supp = csp_105,
         fin_supp_child_supp = csp_110a,
         fin_supp_child_exp = csp_110b,
         fin_supp_lump = csp_110c,
         fin_supp_other = csp_110d,
         fin_supp_agreement = csp_160,
         future_children_intention = fi_110) 
gss5 <- gss %>%
  select(age,
         total_children,
         education,
         
         sex,
         province,
         region)
## Descriptive Graphs ##
# Divide total_children into two categories
gss5 <- gss5 %>%
  mutate(total_children1 = if_else(total_children == 0, 'No child', 'Have child'))

# Shorten the name of provinces
gss5 <- gss5 %>%
  mutate(province1 = case_when(
    province == 'Alberta'~'AL',
    province == 'British Columbia'~ 'BC',
    province == 'Manitoba'~'MA',
    province == 'New Brunswick'~'NB',
    province == 'Newfoundland and Labrador'~'NL',
    province == 'Nova Scotia'~ 'NS',
    province == 'Ontario'~'ON',
    province == 'Prince Edward Island'~'PEI',
    province == 'Quebec'~'QC',
    province == 'Saskatchewan'~'SA'
  ))

raw_data <- raw_data %>%
  mutate(children = ifelse(totchdc == 0,'No child', 'Have child'))
  
# First Graph: A bar chart showing the distribution of total children along with age 
gss5 %>%
  ggplot(aes(x = total_children, y = age)) +
  geom_bin2d()+
  labs(x = 'Age',
       y = 'Number of Total Children',
       title = 'Age Effects on the Number of Children',
       caption = 'Source: 2017 GSS')+ theme_bw()
# Second: A bar chart showing the province distribution and children intention distribution
gss5 %>%
  ggplot(aes(province1, fill = total_children1))+
  geom_bar()+
  theme(legend.position = 'bottom')
# Third: A bar chart showing education and children intention relationship in raw dataset
raw_data %>%
  ggplot(aes(ehg3_01b, fill = children))+
  geom_bar()+
  xlim(0,8)+
  labs(x = 'Education Level', y = 'Count',
       title = 'Education Effects on Children Intention Plot',
       caption = '2017 GSS')+
  theme(legend.position = 'bottom')
# Fourth: Gender effects description in raw data
gss5 %>%
  ggplot(aes(sex, fill= total_children1))+
  geom_bar(width = 0.5)+
  theme(legend.position = 'bottom')

