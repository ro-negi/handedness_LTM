# ==============================================================
# STAGE B: FEMALE FOOD-SPECIFIC FORAGING TIME, WEIGHTED
# Produces row audit, food-item and food-category monthly datasets,
# unique exposure file and consistency checks.
# ==============================================================
packages <- c('dplyr','tidyr','readr','stringr','lubridate','glmmTMB','emmeans','ggplot2')
need_install <- setdiff(packages, rownames(installed.packages()))
if(length(need_install)) install.packages(need_install)
invisible(lapply(packages, library, character.only=TRUE))

input_file <- 'C:/Users/rohit_negi/Desktop/Nutrition_RN/analysis/Daily_activity_budget/data/2024.03.12 - 2026.04.01_focal_data_cleaned_final_weights_RN.csv'
output_dir <- 'C:/Users/rohit_negi/Desktop/Nutrition_RN/analysis/food_analysis/food_proportion_per_day/males_plus_18_females'
dir.create(output_dir, recursive=TRUE, showWarnings=FALSE)
# Optional: place the attached category reference file in output_dir,
# otherwise script uses the embedded dictionary below.
category_file <- file.path(output_dir, 'macaque_food_item_categories.csv')

x <- readr::read_csv(input_file, col_types=readr::cols(.default=readr::col_character()),
                     name_repair='unique', show_col_types=FALSE, progress=FALSE)
needed <- c('focal_id','Focal ID','sex','weight','Date (Capture local)',
 'Time (Capture local)','repro_state_corrected','landscape_corr',
 'Behavior category','Foraging behaviours','Food','Food with tools',
 'Fruit/nut species','Fruit/nut species with tool','Shellfish species with tool',
 'Shellfish species','Insect species','Other food','Other fruits/nuts',
 'Other insects','Other shellfish','Other food with tools',
 'Other fruits/nuts with tool','Other shellfish with tool')
if(length(setdiff(needed,names(x)))) stop('Missing columns: ',paste(setdiff(needed,names(x)),collapse=', '))
norm <- function(v) {
 v <- as.character(v); v <- stringr::str_replace_all(v,'\\u00a0',' ')
 v <- stringr::str_squish(v); v[v %in% c('','NA','NaN','NULL')] <- NA_character_; v
}
low <- function(v) tolower(norm(v))
row_count_input <- nrow(x)
x$original_row <- seq_len(nrow(x))
x$timestamp <- as.POSIXct(paste(norm(x[['Date (Capture local)']]),
 norm(x[['Time (Capture local)']])),format='%m/%d/%Y %H:%M:%S',tz='Asia/Bangkok')
bad <- x %>% filter(is.na(timestamp)|is.na(suppressWarnings(as.integer(focal_id))))
readr::write_csv(bad,file.path(output_dir,'00_malformed_rows.csv'))
x <- x %>% filter(!is.na(timestamp),!is.na(suppressWarnings(as.integer(focal_id))))
x$focal_id <- as.integer(x$focal_id)
x <- x %>% arrange(focal_id,timestamp,original_row) %>% group_by(focal_id) %>%
 mutate(duration_seconds=as.numeric(difftime(lead(timestamp),timestamp,units='secs')),
        duration_seconds=coalesce(duration_seconds,0)) %>% ungroup()
if(any(x$duration_seconds<0)) stop('Negative intervals found')
# Keep identified males plus precisely the same female selection as the prior GLMM.
# Adult status cannot be inferred reliably from sex alone; review age categories
# if you intend to limit the male sample to adults.
x <- x %>% filter(low(sex) %in% c('m','f'), !grepl('\\?', `Focal ID`),
                   !(low(sex)=='f' & norm(`Focal ID`) %in% c('Oreo','Taipan','Pinky','Betina')),
                   low(sex)=='m' | norm(repro_state_corrected) %in%
                     c('Non-lactating & non-pregnant','Pregnant','Lactating'))
x$sex <- ifelse(low(x$sex)=='f','Female','Male')
# Male rows are not assigned female reproductive states.
x$repro_state_corrected[x$sex=='Male'] <- 'Male'

x$weight <- suppressWarnings(as.numeric(x$weight))
if(any(!is.finite(x$weight)|x$weight<=0)) stop('Nonpositive or missing focal weights')
w_check <- x %>% group_by(focal_id) %>% summarise(n_weights=n_distinct(weight),.groups='drop')
if(any(w_check$n_weights!=1)) stop('A focal contains multiple weights')
x$weighted_seconds <- x$duration_seconds * x$weight
x$subj_id <- norm(x[['Focal ID']]); x$repro_state <- norm(x$repro_state_corrected)
x$landscape <- norm(x$landscape_corr)
x$year_month <- format(x$timestamp,'%Y-%m')
x$year <- lubridate::year(x$timestamp); x$month_number <- lubridate::month(x$timestamp)
x$sin_season <- sin(2*pi*x$month_number/12); x$cos_season <- cos(2*pi*x$month_number/12)
bc <- low(x[['Behavior category']]); fb <- low(x[['Foraging behaviours']])
x$is_search <- (!is.na(bc)&bc=='foraging category'&!is.na(fb)&fb=='search')|(!is.na(bc)&bc=='search')
x$is_tool <- !is.na(bc)&bc=='foraging category' & !is.na(fb)&fb=='tool assisted'
x$is_non_tool <- !is.na(bc)&bc=='foraging category' & !is.na(fb)&fb %in% c('without tools','drink')
# Match the activity-budget handling of search: consecutive Search run is
# tool/non-tool if immediately followed by that method, otherwise Moving.
x$search_assigned_target_row <- NA_integer_
x$search_method <- NA_character_
# No crossing focal boundaries; consecutive search rows attach only to the
# immediate next row when it is feeding with a recognized method.
for(idx in split(seq_len(nrow(x)),x$focal_id)) {
 run <- integer()
 for(i in idx) {
  if(isTRUE(x$is_search[i])) run <- c(run,i)
  else {
   if(length(run)) {
    if(isTRUE(x$is_tool[i])) {x$search_assigned_target_row[run] <- x$original_row[i];x$search_method[run] <- 'tool assisted'}
    if(isTRUE(x$is_non_tool[i])) {x$search_assigned_target_row[run] <- x$original_row[i];x$search_method[run] <- 'without tools'}
   }
   run <- integer()
  }
 }
}
x$foraging_method <- case_when(x$is_tool~'tool assisted',x$is_non_tool~'without tools',
 !is.na(x$search_method)~x$search_method,TRUE~NA_character_)
# Other behaviours are included in observation exposure as in activity budget.
x$behavior_class <- case_when(!is.na(x$foraging_method)&x$foraging_method=='tool assisted'~'Foraging with tools',
 !is.na(x$foraging_method)&x$foraging_method=='without tools'~'Foraging without tools',
 x$is_search~'Moving',bc %in% c('resting','self-directed category','vocalization category','roosting')~'Resting',
 bc=='moving category'~'Moving',
 bc %in% c('affiliative category','agonistic category','human-macaque interaction','sexual category','peering')~'Socializing',
 TRUE~NA_character_)
# Conservative backfill analogous to the activity-budget model: between
# two equal adjacent classified behaviors, or trailing unspecified foraging.
for(idx in split(seq_len(nrow(x)),x$focal_id)) {
 if(length(idx)<2) next
 old <- x$behavior_class[idx]
 for(j in seq_along(idx)) {
  i <- idx[j]
  if(!is.na(old[j])) next
  prev <- if(j>1) old[j-1] else NA_character_
  nxt <- if(j<length(idx)) old[j+1] else NA_character_
  if(!is.na(prev)&&!is.na(nxt)&&prev==nxt) x$behavior_class[i] <- prev
  else if(!is.na(prev)&& bc[i]=='foraging category'&&is.na(fb[i]) &&
          (is.na(nxt)||x$duration_seconds[i]==0) && prev %in% c('Foraging with tools','Foraging without tools')) x$behavior_class[i] <- prev
 }
}
# Food names directly from raw columns, respecting nested species columns.
# All search rows inherit the target food's name and category.
choose <- function(...) {
 z <- list(...); out <- rep(NA_character_,length(z[[1]]))
 for(v in z) {sel <- is.na(out)&!is.na(v);out[sel] <- v[sel]}
 out
}
food <- low(x[['Food']]); ft <- low(x[['Food with tools']])
leaf <- !is.na(food)&food=='leaves'
fruit <- low(x[['Fruit/nut species']]); tfruit <- low(x[['Fruit/nut species with tool']])
shell <- low(x[['Shellfish species']]); tshell <- low(x[['Shellfish species with tool']]); insect <- low(x[['Insect species']])
nofood_item <- choose(norm(x[['Fruit/nut species']]), norm(x[['Other fruits/nuts']]))
food_name <- rep(NA_character_,nrow(x))
# Non-tool method
j <- which(x$is_non_tool)
food_name[j] <- ifelse(food[j]=='fruit/nut species',nofood_item[j],
 ifelse(food[j]=='insect species',choose(norm(x[['Insect species']]),norm(x[['Other insects']]))[j],
 ifelse(food[j]=='shellfish species',choose(norm(x[['Shellfish species']]),norm(x[['Other shellfish']]))[j],norm(x[['Food']])[j])))
# Sonneratia alba recorded as leaves via Food=Leaves, fruit species field.
sel <- x$is_non_tool & leaf & fruit=='sonneratia alba'
food_name[sel] <- 'Sonneratia alba leaves'
# Non-tool Seeds: prefer species when present, and annotate seed part
sel <- x$is_non_tool & food %in% c('seeds','seed') & fruit=='diospyros wallichii'
food_name[sel] <- 'Diospyros wallichii seeds'
# Tool method
j <- which(x$is_tool)
food_name[j] <- ifelse(ft[j]=='shellfish species',choose(norm(x[['Shellfish species with tool']]),norm(x[['Other shellfish with tool']]))[j],
 ifelse(ft[j]=='fruit/nut species',choose(norm(x[['Fruit/nut species with tool']]),norm(x[['Other fruits/nuts with tool']]))[j],
 choose(norm(x[['Food with tools']]),norm(x[['Other food with tools']]))[j]))
# Other food non-tool, if the main Food says Other / other food
sel <- x$is_non_tool & (is.na(food)|food %in% c('other','other food')) & !is.na(norm(x[['Other food']]))
food_name[sel] <- norm(x[['Other food']])[sel]
# Explicit user rules for absent species and unrecorded foods.
# Distinguish foraging without food identification from true Unknown entries.
sel <- x$is_non_tool & food=='fruit/nut species' & is.na(nofood_item)
food_name[sel] <- 'unknown.fruit'
sel <- x$is_non_tool & food=='insect species' &
  is.na(choose(norm(x[['Insect species']]),norm(x[['Other insects']])))
food_name[sel] <- 'unknown insect'
sel <- x$is_non_tool & food=='shellfish species' &
  is.na(choose(norm(x[['Shellfish species']]),norm(x[['Other shellfish']])))
food_name[sel] <- 'unknown shellfish'
sel <- x$is_tool & ft=='shellfish species' &
  is.na(choose(norm(x[['Shellfish species with tool']]),norm(x[['Other shellfish with tool']])))
food_name[sel] <- 'unknown shellfish'
sel <- x$is_tool & ft=='fruit/nut species' &
  is.na(choose(norm(x[['Fruit/nut species with tool']]),norm(x[['Other fruits/nuts with tool']])))
food_name[sel] <- 'unknown.fruit'
sel <- x$is_non_tool & is.na(food_name) & is.na(food)
food_name[sel] <- 'unknown.fruit'
# Generalized names are classifiable even when species is absent.
food_name[x$is_non_tool & food %in% c('seeds','seed') & is.na(fruit)] <- 'Seeds'
food_name[x$is_tool & ft=='shellfish species' & is.na(food_name)] <- 'unknown shellfish'
food_name[x$is_non_tool & food=='other shellfish'] <- 'unknown shellfish'
food_name[x$is_non_tool & food=='other fruits/nuts'] <- 'unknown.fruit'
food_name[x$is_non_tool & fb=='drink'] <- 'drink water'
# Only genuinely unidentifiable food defaults to Unknown.
food_name[is.na(food_name) & (x$is_tool|x$is_non_tool)] <- 'Unknown'
# All Search sequences inherit exactly the NEXT qualifying feeding row's food.
pos <- match(x$search_assigned_target_row,x$original_row)
has_search_target <- which(!is.na(pos))
food_name[has_search_target] <- food_name[pos[has_search_target]]
# Unsuccessful Search is not food-acquisition using either method.
# It is Moving in the activity-budget model; retain it as a separate
# method='unassigned' to avoid inflating tool/non-tool denominator.
food_name[x$is_search & is.na(x$search_assigned_target_row)] <- 'search without success'
x$food.items <- food_name
# Reference dictionary included here, with an optional CSV override.
ref_text <- 'food_item|food_group
Acanthopleura gemmata|Shellfish
Adenanthera pavonina seeds|Seeds
Aegiceras corniculatum|Fruits
Atrina vexillum|Shellfish
Beach almond|Seeds
Betal nut palm|Seeds
Blue crab|Others
Bouea microphylla|Fruits
Caterpillar|Insects
Clam|Shellfish
Clay|Others
Cocos nucifera (Coconut)|Seeds
Crab|Others
Crab claw|Others
Crab leg|Others
Diospyros wallichii|Fruits
Diospyros wallichii seeds|Seeds
Ficus|Fruits
Ficus vasculosa|Fruits
Ficus virens|Fruits
food processing|Others
fungus|Others
Gluta sp.|Fruits
Gnetum sp.|Fruits
Grass|Leaves
Green fruit, orange inside|Fruits
Heritiera littoralis|Fruits
Holothuria atra|Others
Isopod|Others
Knema sp.|Fruits
Larvae|Insects
leaves|Leaves
Littoraria scabra|Shellfish
Lunella cinerea|Shellfish
Mahad|Fruits
Mangosteen|Fruits
Mangrove pneumatophore|Fruits
Mangrove root|Fruits
Manila clam|Shellfish
Mealybug|Insects
Mealybugs|Insects
Nerita|Shellfish
Nerita balteata|Shellfish
Nerita chamaeleon|Shellfish
Oyster|Shellfish
Oyster debris|Shellfish
Oyster non-sessile|Shellfish
Oyster sessile|Shellfish
Oyster sessile (attached)|Shellfish
Palm balls|Fruits
Periwinkle|Shellfish
Polinices mammilla|Shellfish
Rattan|Fruits
Red ant eggs|Insects
Rhizophora apiculata|Fruits
Rhizophora mucronata|Fruits
Sea almond mesocarp|Fruits
Sea slaters|Others
Shoot|Leaves
Shore eugenia|Fruits
Shrimp|Others
Small crab|Others
Small mangrove fruit/shoot|Others
Small crustacean|Others
Smilax sp.|Fruits
Snail|Shellfish
Some old droped fruit|Fruits
Sonneratia Alba|Fruits
Sonneratia alba leaves|Leaves
Spider|Insects
Stick Insect|Insects
Swollen Circe|Shellfish
Syzygium pyrifolium|Fruits
Tape worm type|Insects
Terminalia catappa (Sea almond nut)|Seeds
Termites|Insects
Thais bitubercularis|Shellfish
Thais sp.|Shellfish
Tree bark|Leaves
twig|Leaves
Unknown|Unknown
Unknown branch|Unknown
Unknown flowerd buds|Leaves
unknown food|Unknown
unknown food with tool|Unknown
unknown food without tool|Unknown
unknown fruit|Fruits
unknown fruit tool|Fruits
unknown insect|Insects
unknown shellfish|Shellfish
unknown shellfish tool|Shellfish
unknown tool|Unknown
Uvaria grandiflora|Fruits
Vitex pinnata|Fruits
Wild jackfruit|Fruits
X001|Fruits'
if(file.exists(category_file)) {
 dictionary <- readr::read_csv(category_file,show_col_types=FALSE)
 names(dictionary) <- sub('^\\ufeff','',names(dictionary))
 dictionary <- dictionary[,c('food_item','food_group')]
} else {
 dictionary <- readr::read_delim(I(ref_text),delim='|',show_col_types=FALSE)
}
key <- function(v) {
 v <- low(v);v <- stringr::str_replace_all(v,'[^[:alnum:]]','')
 v
}
dictionary$key <- key(dictionary$food_item)
dictionary$food.category <- case_when(
 grepl('^others',low(dictionary$food_group))~'Others',
 grepl('unknown',low(dictionary$food_group))~'Unknown',
 TRUE~norm(dictionary$food_group))
# user overrides: Unknown -> Unknown and Small crustacean -> Others
bad_dictionary <- dictionary %>% count(key) %>% filter(n>1)
if(nrow(bad_dictionary)) {
 dictionary <- dictionary %>% arrange(key) %>% distinct(key,.keep_all=TRUE)
}
lookup <- setNames(dictionary$food.category,dictionary$key)
x$food.category <- unname(lookup[key(x$food.items)])
x$food.category[grepl('unknown',low(x$food.items)) & !is.na(x$food.items)] <- 'Unknown'
# Specific classification rules override the generic unknown-name rule.
x$food.category[low(x$food.items)=='unknown.fruit'] <- 'Fruits'
x$food.category[low(x$food.items)=='unknown shellfish'] <- 'Shellfish'
x$food.category[low(x$food.items)=='unknown insect'] <- 'Insects'
x$food.category[low(x$food.items)=='seeds'] <- 'Seeds'
x$food.category[low(x$food.items)=='other shellfish'] <- 'Shellfish'
x$food.category[low(x$food.items)=='other fruits/nuts'] <- 'Fruits'
x$food.category[low(x$food.items)=='search without success'] <- 'Search'
x$food.category[low(x$food.items)=='drink water'] <- 'Drink'
x$food.category[grepl('small.*crustacean',low(x$food.items)) & !is.na(x$food.items)] <- 'Others'
# Split identifiable oysters from other shellfish before aggregating.
# Retain both labels independently for the adjusted models and chart.
oyster_aliases <- c('oyster sessile (attached)','oyster sessile','oyster non-sessile','oyster')
x$food.category[x$food.category=='Shellfish' & low(x$food.items) %in% oyster_aliases] <- 'Oysters'
x$food.category[x$food.category=='Shellfish'] <- 'Other shellfish'
# Preserve time in the denominator, even for unrecognized food labels.
x$food.category[is.na(x$food.category)&!is.na(x$foraging_method)] <- 'Unknown'
x$food.items[is.na(x$food.items)&!is.na(x$foraging_method)] <- 'Unknown'
readr::write_csv(x %>% filter(!is.na(foraging_method),food.category=='Unknown') %>%
 distinct(food.items,foraging_method),file.path(output_dir,'01_food_names_needing_review.csv'))
# Raw interval audit includes every classified row; search attribution visible.
intervals <- x %>% transmute(original_row,focal_id,subj_id,sex,year_month,repro_state,landscape,
 foraging_method,food.items,food.category,behavior_class,is_search,
 search_assigned_target_row,weight,duration_seconds,weighted_seconds)
readr::write_csv(intervals,file.path(output_dir,'02_row_level_audit.csv'))
# Exclude truly unclassified observations, as the final activity-budget model does.
valid <- x %>% filter(duration_seconds>0,!is.na(behavior_class),
 !is.na(landscape),landscape!='')
keys <- c('subj_id','sex','year_month','repro_state','landscape')
exposure <- valid %>% group_by(across(all_of(keys))) %>% summarise(
 observation_seconds=sum(duration_seconds),weighted_observation_seconds=sum(weighted_seconds),
 n_focals=n_distinct(focal_id),.groups='drop') %>%
 mutate(month_number=as.integer(substr(year_month,6,7)),
 sin_season=sin(2*pi*month_number/12),cos_season=cos(2*pi*month_number/12))
readr::write_csv(exposure,file.path(output_dir,'03_unique_monthly_observation_effort.csv'))
# Include tool/non-tool foraging and separately tabulate unsuccessful searches.
# The latter were classified as Moving in the original activity budget, not
# tool/non-tool foraging; keep a distinct method and exposure.
forage <- valid %>% filter(behavior_class %in% c('Foraging with tools','Foraging without tools') |
                            (is_search & is.na(search_assigned_target_row))) %>%
 mutate(foraging_method=case_when(
   behavior_class=='Foraging with tools' ~ 'tool assisted',
   behavior_class=='Foraging without tools' ~ 'without tools',
   is_search & is.na(search_assigned_target_row) ~ 'unassigned search'),
 food.items=coalesce(food.items,'Unknown'),food.category=coalesce(food.category,'Unknown'))
# Safety: no search interval may appear as both unsuccessful and allocated.
stopifnot(!any(forage$food.category=='Search' & forage$foraging_method!='unassigned search'))
method_exposure <- forage %>% group_by(across(all_of(keys)),foraging_method) %>% summarise(
 total_method_seconds=sum(duration_seconds),weighted_method_seconds=sum(weighted_seconds),.groups='drop')
methods <- c('tool assisted','without tools','unassigned search')
all_methods <- tidyr::crossing(exposure %>% select(all_of(keys)),foraging_method=methods) %>%
 left_join(method_exposure,by=c(keys,'foraging_method')) %>%
 mutate(across(c(total_method_seconds,weighted_method_seconds),~replace_na(.x,0)))
readr::write_csv(all_methods,file.path(output_dir,'04_unique_monthly_method_effort.csv'))
# Item-level totals: foods tracked only when recognized at event time.
item <- forage %>% group_by(across(all_of(keys)),foraging_method,food.category,food.items) %>%
 summarise(foraging_time_seconds=sum(duration_seconds),weighted_foraging_seconds=sum(weighted_seconds),
 contributing_focals=n_distinct(focal_id),.groups='drop') %>%
 left_join(exposure,by=keys) %>%
 left_join(all_methods,by=c(keys,'foraging_method')) %>%
 mutate(within_method_fraction=if_else(weighted_method_seconds>0,weighted_foraging_seconds/weighted_method_seconds,NA_real_))
readr::write_csv(item,file.path(output_dir,'05_monthly_food_items.csv'))
# 10 categories (oyster split) x 3 methods; structural zeros retained.
categories <- c('Oysters','Other shellfish','Fruits','Leaves','Seeds','Insects','Others','Unknown','Search','Drink')
cat_times <- forage %>% group_by(across(all_of(keys)),foraging_method,food.category) %>%
 summarise(foraging_time_seconds=sum(duration_seconds),weighted_foraging_seconds=sum(weighted_seconds),.groups='drop')
monthly <- tidyr::crossing(all_methods,food.category=categories) %>%
 left_join(cat_times,by=c(keys,'foraging_method','food.category')) %>%
 mutate(across(c(foraging_time_seconds,weighted_foraging_seconds),~replace_na(.x,0))) %>%
 left_join(exposure,by=keys) %>%
 mutate(within_method_fraction=if_else(weighted_method_seconds>0,weighted_foraging_seconds/weighted_method_seconds,NA_real_),
 log_weighted_observation_time=log(weighted_observation_seconds))
# A category aggregate cannot have a unique original food-item name.
# Provide transparent names: actual name when unique; otherwise a list of
# all observed original food names in this stratum/category.
item_names <- forage %>% group_by(across(all_of(keys)),foraging_method,food.category) %>%
 summarise(food.items=paste(sort(unique(food.items)),collapse='; '), .groups='drop')
monthly <- monthly %>% left_join(item_names,by=c(keys,'foraging_method','food.category')) %>%
 mutate(food.items=if_else(is.na(food.items) & foraging_time_seconds==0,
                          '(no food item observed)',food.items))
readr::write_csv(monthly,file.path(output_dir,'06_monthly_food_categories_GLMM_ready.csv'))
check <- monthly %>% group_by(across(all_of(keys)),foraging_method) %>% summarise(
 sum_food_seconds=sum(foraging_time_seconds),sum_weighted_food_seconds=sum(weighted_foraging_seconds),
 method_seconds=first(total_method_seconds),weighted_method_seconds=first(weighted_method_seconds),
 diff_seconds=sum_food_seconds-method_seconds,
 diff_weighted=sum_weighted_food_seconds-weighted_method_seconds,.groups='drop')
readr::write_csv(check,file.path(output_dir,'07_time_conservation_checks.csv'))
if(any(abs(check$diff_seconds)>1e-6|abs(check$diff_weighted)>1e-6)) stop('Food category times fail conservation check')
# Descriptive only, pooled time (NOT model adjusted).
desc <- monthly %>% group_by(sex,repro_state,foraging_method,food.category) %>%
 summarise(weighted_food_seconds=sum(weighted_foraging_seconds),.groups='drop') %>%
 group_by(sex,repro_state,foraging_method) %>%
 mutate(total_group_seconds = sum(weighted_food_seconds, na.rm = TRUE),
        proportion = if_else(total_group_seconds > 0,
                             weighted_food_seconds / total_group_seconds,
                             NA_real_)) %>%
 select(-total_group_seconds) %>% ungroup()
readr::write_csv(desc,file.path(output_dir,'08_descriptive_food_proportions_NOT_model_adjusted.csv'))
report <- c('STAGE B FOOD FORAGING DATASET: BUILD REPORT',
 paste('Source rows:',row_count_input),paste('Malformed rows audited:',nrow(bad)),
 paste('Total individual identities included:',n_distinct(valid$subj_id)),
 paste('Unique exposure strata:',nrow(exposure)),
 paste('Monthly category rows:',nrow(monthly)),
 paste('Monthly item rows:',nrow(item)),
 paste('Total weighted observation seconds:',round(sum(exposure$weighted_observation_seconds),2)),
 paste('Total weighted categorized seconds (including unassigned search):',round(sum(forage$weighted_seconds),2)),
 paste('Maximum absolute category-vs-method difference:',max(abs(check$diff_weighted))),
 'IMPORTANT: Monthly exposure repeats on 10 categories x 3 method classes; NEVER sum repeated exposure.',
 'Unassigned search is kept SEPARATE from tool/non-tool. It was Moving in activity budget.',
 'Drink stays without tools (consistent with the prior activity budget) but is not calorie-bearing food.',
 'Unknown contains unidentified foods. Search without success and Drink are explicit categories.',
 'The 08 descriptive proportions are raw weighted proportions and are NOT season/landscape/model adjusted.',
 'Oyster share among shellfish computed by predicted Oysters/(Oysters+Other shellfish).',
 'Dates are parsed as Thai local times from the weighted raw file.')
writeLines(report,file.path(output_dir,'09_build_report.txt'))
cat(paste(report,collapse='\n'),'\n')


# ================================================================
# MODEL-ADJUSTED SINGLE PIE: MALES + 18 FEMALES
# No hard-coded method shares from female-only analyses.
# ================================================================
retained <- c('Oysters','Other shellfish','Fruits','Leaves','Seeds','Insects','Others','Unknown')
methods_food <- c('tool assisted','without tools')

# METHOD SPLIT: use the SAME classified weighted observation exposure, and
# add 0-tool/0-nontool rows in each observed stratum.
method_model_dat <- all_methods %>%
  filter(foraging_method %in% methods_food) %>%
  left_join(exposure %>% select(all_of(keys),weighted_observation_seconds,sin_season,cos_season),
            by=keys) %>%
  filter(weighted_observation_seconds>0) %>%
  mutate(subj_id=factor(subj_id),sex=factor(sex),landscape=factor(landscape),
         foraging_method=factor(foraging_method,levels=methods_food),
         log_observation=log(weighted_observation_seconds))

model_method <- glmmTMB::glmmTMB(
  weighted_method_seconds ~ foraging_method * sex + landscape + sin_season + cos_season +
    offset(log_observation) + (1|subj_id),
  family=glmmTMB::tweedie(link='log'),data=method_model_dat)
if(!isTRUE(model_method$sdr$pdHess) || model_method$fit$convergence!=0)
  stop('Method time GLMM failed convergence; inspect model_method.rds')
saveRDS(model_method,file.path(output_dir,'10_method_time_model.rds'))

# Balanced sex averaging gives each SEX equal representation rather than
# preferentially weighting the more frequently sampled sex.
get_response <- function(emm) {
  tmp <- as.data.frame(summary(emm,type='response'))
  cname <- intersect(c('response','rate','emmean'),names(tmp))
  if(length(cname)!=1L) stop('Cannot identify emmeans response column: ',paste(names(tmp),collapse=', '))
  tmp$predicted_rate <- tmp[[cname]]
  tmp
}
method_prediction <- get_response(emmeans::emmeans(model_method,~ foraging_method,
                                                  offset=0,weights='equal'))
method_prediction <- method_prediction %>%
 mutate(method_share=predicted_rate/sum(predicted_rate))
readr::write_csv(method_prediction,file.path(output_dir,'11_model_adjusted_method_shares.csv'))

# COMPOSITION: exclude Search and Drink from both response and denominator.
food_model_dat <- monthly %>%
  filter(foraging_method %in% methods_food, food.category %in% retained) %>%
  group_by(across(all_of(c(keys,'foraging_method')))) %>%
  mutate(weighted_food_exposure=sum(weighted_foraging_seconds,na.rm=TRUE)) %>%
  ungroup() %>% filter(weighted_food_exposure>0) %>%
  mutate(subj_id=factor(subj_id),sex=factor(sex),landscape=factor(landscape),
         food.category=factor(food.category,levels=retained),
         log_food_exposure=log(weighted_food_exposure))

# Use categories with nonzero observed time per method; otherwise entire
# zero-response categories cause non-identifiability and unstable fits.
fit_food_model <- function(m) {
  d <- food_model_dat %>% filter(foraging_method==m)
  present <- d %>% group_by(food.category) %>%
    summarise(s=sum(weighted_foraging_seconds),.groups='drop') %>% filter(s>0) %>%
    pull(food.category) %>% as.character()
  d <- d %>% filter(as.character(food.category) %in% present) %>%
    mutate(food.category=droplevels(food.category))
  if(length(present)<2) stop('Only one food category observed for method: ',m)
  mod <- glmmTMB::glmmTMB(
    weighted_foraging_seconds ~ food.category + sex + landscape + sin_season + cos_season +
      offset(log_food_exposure) + (1|subj_id),
    family=glmmTMB::tweedie(link='log'), data=d)
  if(!isTRUE(mod$sdr$pdHess) || mod$fit$convergence!=0)
    stop('Food model failed convergence for ',m,'; inspect category counts and zeros')
  list(model=mod,data=d)
}
mod_tool <- fit_food_model('tool assisted')
mod_non <- fit_food_model('without tools')
saveRDS(mod_tool$model,file.path(output_dir,'12_tool_food_model.rds'))
saveRDS(mod_non$model,file.path(output_dir,'13_nontool_food_model.rds'))

food_predictions <- dplyr::bind_rows(lapply(methods_food,function(m){
  fit <- if(m=='tool assisted') mod_tool else mod_non
  z <- get_response(emmeans::emmeans(fit$model,~ food.category,
                                    offset=0,weights='equal'))
  z %>% mutate(foraging_method=m,
               within_method_prop=predicted_rate/sum(predicted_rate)) %>%
    select(foraging_method,food.category,predicted_rate,within_method_prop)
}))
readr::write_csv(food_predictions,file.path(output_dir,'14_food_category_adjusted_proportions_all.csv'))
# Conditional oyster share among the two shellfish subcategories, for each method.
oyster_breakdown <- food_predictions %>%
 filter(food.category %in% c('Oysters','Other shellfish')) %>%
 group_by(foraging_method) %>%
 mutate(shellfish_fraction=within_method_prop / sum(within_method_prop)) %>%
 ungroup()
readr::write_csv(oyster_breakdown,file.path(output_dir,'14b_oyster_share_of_shellfish_adjusted.csv'))

# Pool each category less than 10% OF ITS OWN METHOD into 'Others'.
# Existing 'Others' is merged into the pooled slice.
collapsed <- food_predictions %>% mutate(
  slice=ifelse(within_method_prop<0.10 & !(foraging_method=='tool assisted' & as.character(food.category)=='Oysters'),'Others',as.character(food.category))) %>%
  group_by(foraging_method,slice) %>%
  summarise(within_method_prop=sum(within_method_prop),.groups='drop') %>%
  left_join(method_prediction %>% select(foraging_method,method_share),by='foraging_method') %>%
  mutate(overall_prop=within_method_prop*method_share,
         percentage=100*overall_prop,
         method_label=ifelse(foraging_method=='tool assisted','Tool-assisted','Without tools'),
         legend_label=sprintf('%s: %s (%.1f%%)',method_label,slice,percentage))
stopifnot(abs(sum(collapsed$overall_prop)-1)<1e-8)
readr::write_csv(collapsed,file.path(output_dir,'15_final_pie_slice_data.csv'))

# Color-blind-friendly Okabe-Ito families: orange and blue.
fill_map <- c(
  'tool assisted|Oysters'='#C87900','tool assisted|Other shellfish'='#E0A13E','tool assisted|Fruits'='#A95D00',
  'tool assisted|Insects'='#D88D00','tool assisted|Leaves'='#E1A12C',
  'tool assisted|Seeds'='#B46D00','tool assisted|Unknown'='#EDBF65',
  'tool assisted|Others'='#F0BA50',
  'without tools|Fruits'='#0072B2','without tools|Insects'='#3B91C3',
  'without tools|Oysters'='#005C91','without tools|Other shellfish'='#267DA8','without tools|Leaves'='#63ABD0',
  'without tools|Seeds'='#2679A2','without tools|Unknown'='#8CBAD4',
  'without tools|Others'='#76ABD0'
)
collapsed$fill_key <- paste(collapsed$foraging_method,collapsed$slice,sep='|')
cols <- fill_map[collapsed$fill_key]
if(anyNA(cols)) stop('No color defined for: ',paste(collapsed$fill_key[is.na(cols)],collapse=', '))
names(cols) <- collapsed$fill_key

# Explicit slice order: orange sector first, blue sector second, without
# alphabetic reordering of the pie segments.
collapsed <- collapsed %>% mutate(
 method_order=ifelse(foraging_method=='tool assisted',0,1),
 slice_order=match(slice,c('Oysters','Other shellfish','Fruits','Insects','Leaves','Seeds','Unknown','Others'))) %>%
 arrange(method_order,slice_order) %>%
 mutate(fill_key=factor(fill_key,levels=fill_key),
        label=ifelse(percentage>=8,sprintf('%s\n%.1f%%',slice,percentage),''))
cols <- cols[as.character(collapsed$fill_key)]
p <- ggplot2::ggplot(collapsed,ggplot2::aes(x=1,y=overall_prop,fill=fill_key))+
  ggplot2::geom_col(width=1,color='white',linewidth=1.05)+
  ggplot2::coord_polar(theta='y')+
  ggplot2::scale_fill_manual(values=cols,breaks=as.character(collapsed$fill_key),
                            labels=collapsed$legend_label)+
  ggplot2::geom_text(ggplot2::aes(label=label),position=ggplot2::position_stack(vjust=0.5),
                     size=4.2,lineheight=0.95)+
  ggplot2::labs(title='Model-adjusted composition of foraging time',
                subtitle='Males and 18 retained females | Oysters always shown separately',fill=NULL)+
  ggplot2::theme_void(base_size=15)+
  ggplot2::theme(plot.title=ggplot2::element_text(face='bold',hjust=0.5,size=18),
                plot.subtitle=ggplot2::element_text(hjust=0.5,size=11),
                legend.position='right',legend.text=ggplot2::element_text(size=11),
                plot.background=ggplot2::element_rect(fill='white',color=NA))
print(p)
ggplot2::ggsave(file.path(output_dir,'16_combined_males_females_food_pie.png'),
                plot=p,width=12,height=8,dpi=300,bg='white')
ggplot2::ggsave(file.path(output_dir,'16_combined_males_females_food_pie.pdf'),
                plot=p,width=12,height=8,bg='white')

capture.output({
 cat('MALES + 18 FEMALES: MODEL-ADJUSTED FORAGING COMPOSITION\n')
 cat('Subjects, female:',dplyr::n_distinct(valid$subj_id[valid$sex=='Female']),
     'male:',dplyr::n_distinct(valid$subj_id[valid$sex=='Male']),'\n')
 cat('Sexes are averaged equally in adjusted predictions.\n')
 cat('Missing food-category time treated as zero within sampled strata.\n')
 cat('Search and Drink removed from composition denominator; unsuccessful Search\n')
 cat('is not included in either tool/non-tool method.\n\n')
 cat('Method model:\n');print(summary(model_method))
 cat('\nTool food model:\n');print(summary(mod_tool$model))
 cat('\nNon-tool food model:\n');print(summary(mod_non$model))
 cat('\nAdjusted method shares:\n');print(method_prediction)
 cat('\nAdjusted within-method food composition:\n');print(food_predictions)
 cat('\nOyster share within shellfish:\n');print(oyster_breakdown)
 cat('\nFinal pie slices:\n');print(collapsed %>% select(method_label,slice,percentage))
},file=file.path(output_dir,'17_model_and_figure_report.txt'))
cat('\nComplete. Pie and audits saved to: ',output_dir,'\n')
