
# users
# compute age at registration and at time of study
users$age_registration = year(users$creation_date) - users$year
users$age_now = 2018 - users$year


# add goals, method (ST or B) year to cycledays
m = match(cycledays$cycle_id, cycles$cycle_id)
cycledays$goal = cycles$goal_txt[m]
cycledays$method = cycles$method[m]
m = match(cycledays$user_id, users$user_id)
cycledays$year = users$year[m]
cycledays$age_at_obs = year(cycledays$obs_day) - cycledays$year


# compute age per cycle
m = match(cycles$user_id, users$user_id)
cycles$year = users$year[m]
cycles$age_at_obs = year(cycles$first_day_of_cycle) - cycles$year

# compute number of observation per cycle
agg = aggregate(input_source ~ cycle_id, cycledays[cycledays$input_source > 0 ,], length)
agg$n_obs = agg$input_source
m = match(cycles$cycle_id, agg$cycle_id)
cycles$n_obs = agg$n_obs[m]
rm(agg, m)


# number of cycles
agg = aggregate(cycle_nb ~ user_id, cycles[!is.na(cycles$n_obs),], max)
agg$n_cycles = agg$cycle_nb
m = match(users$user_id, agg$user_id)
users$n_cycles = agg$n_cycles[m]-1


# last cycles
cycles$is_last_cycle = FALSE
m = match(cycles$user_id, agg$user_id)
cycles$is_last_cycle[cycles$cycle_nb == agg$cycle_nb[m]] = TRUE
rm(agg, m)

# first cycles
cycles$is_first_cycle = FALSE
cycles$is_first_cycle[cycles$cycle_nb <= 0] = TRUE

# cycles
# has a yellow phase (ovulation has been validated by the SymptoThermal method rules)
cycles_with_yellow_phase = unique(cycledays$cycle_id[cycledays$out_phase == 3])
m = match(cycles$cycle_id, cycles_with_yellow_phase)
cycles$has_yellow_phase  = !is.na(m)
rm(cycles_with_yellow_phase, m)

# has bleeding
cycles_with_bleeding = unique(cycledays$cycle_id[cycledays$blood >0])
m = match(cycles$cycle_id, cycles_with_bleeding)
cycles$has_bleeding  = !is.na(m)
rm(cycles_with_bleeding, m)

# cycle length
agg = aggregate(out_cycleday ~cycle_id, cycledays, max)
agg$cycle_length = agg$out_cycleday
m = match(cycles$cycle_id, agg$cycle_id)
cycles$cycle_length = agg$cycle_length[m]  
rm(m)
m = match(cycledays$cycle_id, agg$cycle_id)
cycledays$cycle_length = agg$cycle_length[m]
rm(agg, m)

cycledays$cycleday_from_end = cycledays$out_cycleday - cycledays$cycle_length -1

# tracking frequency per cycle_id (cycles)
cycles$tracking_freq = cycles$n_obs / cycles$cycle_length

# observation gaps
j = (cycledays$input_source>0) | (cycledays$cycleday_from_end == -1)
gap = diff(cycledays$out_cycleday[j])
cycle_ids = cycledays$cycle_id[j]
phase = cycledays$out_phase[j]
gap = data.frame(gap = gap, cycle_id = cycle_ids[1:length(gap)], phase =phase[1:length(gap)])
#in the whole cycle
agg = aggregate(gap ~ cycle_id, gap, max)
agg$gap[agg$gap<0] = 0
m = match(cycles$cycle_id, agg$cycle_id)
cycles$gap = agg$gap[m]
rm(agg, m)
#before ovu
agg = aggregate(gap ~ cycle_id, gap[gap$phase %in% c(1,2,4),], max)
agg$gap[agg$gap<0] = 0
m = match(cycles$cycle_id, agg$cycle_id)
cycles$gap_blue_pink = agg$gap[m]
rm(agg, m)
#after ovu
agg = aggregate(gap ~ cycle_id, gap[gap$phase == 3,], max)
agg$gap[agg$gap<0] = 0
m = match(cycles$cycle_id, agg$cycle_id)
cycles$gap_yellow = agg$gap[m]
rm(agg, m)


# check if we have a out_cycleday == 1 for each cycle
agg = aggregate(out_cycleday ~ cycle_id, cycledays, min)
m = match(cycles$cycle_id, agg$cycle_id)
cycles$data_from_day_1 = (agg$out_cycleday[m] == 1)
cycles$first_day_with_data = agg$out_cycleday[m]

# check if we have all out_cycleday for each cycle
agg = aggregate(out_cycleday ~ cycle_id, cycledays, length)
m = match(cycles$cycle_id, agg$cycle_id)
cycles$data_for_all_cycledays = (agg$out_cycleday[m] == cycles$cycle_length)


# breastfeeding
agg = aggregate(bf ~ cycle_id, cycledays, max)
m = match(cycles$cycle_id, agg$cycle_id)
cycles$bf = agg$bf[m]
rm(agg, m)




# add first_obs and last_obs to users from cycledays data
agg = aggregate(obs_day ~user_id, cycledays, min)
m = match(users$user_id, agg$user_id)
users$first_obs = agg$obs_day[m]

agg = aggregate(obs_day ~user_id, cycledays, max)
m = match(users$user_id, agg$user_id)
users$last_obs = agg$obs_day[m]
rm(agg, m)

# compute difference between 1st and last observation + if currently observing
users$obs_duration_days = as.numeric(users$last_obs - users$first_obs +1)
date_of_snapshot = max(users$last_obs, na.rm = TRUE)
users$currently_active = users$last_obs>=(date_of_snapshot-days(30))

# number of observation per users
agg = aggregate(n_obs ~ user_id, cycles, sum)
m = match(users$user_id, agg$user_id)
users$n_obs = agg$n_obs[m]
rm(agg,m)

# SEX

# user has logged sex
agg = aggregate(sex~cycle_id, cycledays[cycledays$sex>0,], length)
m = match(cycles$cycle_id, agg$cycle_id)
cycles$has_logged_sex = agg$sex[m]
cycles$has_logged_sex[is.na(cycles$has_logged_sex)] = 0

# user has logged unprotected sex
agg = aggregate(sex~cycle_id, cycledays[cycledays$sex==1,], length)
m = match(cycles$cycle_id, agg$cycle_id)
cycles$has_logged_unprot_sex = agg$sex[m]
cycles$has_logged_unprot_sex[is.na(cycles$has_logged_unprot_sex)] = 0
rm(agg, m)

# user has logged protected sex
agg = aggregate(sex~cycle_id, cycledays[cycledays$sex==2,], length)
m = match(cycles$cycle_id, agg$cycle_id)
cycles$has_logged_prot_sex = agg$sex[m]
cycles$has_logged_prot_sex[is.na(cycles$has_logged_prot_sex)] = 0
rm(agg, m)


# user has logged unprotected sex during fertile window
agg = aggregate(sex~cycle_id, cycledays[(cycledays$sex==1) & (cycledays$out_phase %in% c(2,4)),], length)
m = match(cycles$cycle_id, agg$cycle_id)
cycles$unprot_sex_fert_window = agg$sex[m]
cycles$unprot_sex_fert_window[is.na(cycles$unprot_sex_fert_window)] = 0
rm(agg, m)


# user has logged unprotected sex when very fertile
agg = aggregate(sex~cycle_id, cycledays[(cycledays$sex==1) & (cycledays$out_phase == 4),], length)
m = match(cycles$cycle_id, agg$cycle_id)
cycles$unprot_sex_very_fertile = agg$sex[m]
cycles$unprot_sex_very_fertile[is.na(cycles$unprot_sex_very_fertile)] = 0
rm(agg, m)


# PREGNANCY

# got pregnancy msg
#pregnancy.msg.ids = c(1177, 1178,1448,1228, 9784,9215) # the two last seems like personalized messages built on 1178 but I don't know where to find the info
pregnancy.msg.id = 1177

cycle_ids_preg = unique(cycledays$cycle_id[cycledays$out_motiv == pregnancy.msg.id])
m = match(cycles$cycle_id, cycle_ids_preg)
cycles$got_preg_msg = FALSE
cycles$got_preg_msg[!is.na(m)] = TRUE
rm(cycle_ids_preg, m)


# bf before yellow phase

bf.min.day = aggregate(out_cycleday ~ cycle_id ,cycledays[(cycledays$bf %in% 1:3),], min)
colnames(bf.min.day) = c('cycle_id','first.day.bf')
yp.min.day = aggregate(out_cycleday ~ cycle_id ,cycledays[(cycledays$out_phase == 3),], min)
colnames(yp.min.day) = c('cycle_id','first.day.yp')

t = merge(bf.min.day, yp.min.day, by = 'cycle_id')
t$bf.before.yellow = (t$first.day.bf < t$first.day.yp)
m = match(cycles$cycle_id, t$cycle_id)
cycles$bf_before_yellow = t$bf.before.yellow[m]
cycles$bf_before_yellow[is.na(cycles$bf_before_yellow)] = FALSE


# suspected pregnancy with birth
cycles$susp_preg_birth = cycles$got_preg_msg & 
  (cycles$cycle_length > 9*30) & 
  (cycles$gap_blue_pink < 25) &
  (!cycles$bf_before_yellow)

# confirmed pregnancy with birth
cycles$confirmed_preg = cycles$susp_preg_birth & (cycles$bf %in% 1:3)


# PHASE length (as # of days & fraction of the cycle)

# fertile window
agg = aggregate(out_phase ~ cycle_id, cycledays[cycledays$out_phase %in% c(2,4),], length)
m = match(cycles$cycle_id, agg$cycle_id)
cycles$fert_wind_length = agg$out_phase[m]
rm(agg, m)


# infertile after ovulation
agg = aggregate(out_phase ~ cycle_id, cycledays[cycledays$out_phase == 3,], length)
m = match(cycles$cycle_id, agg$cycle_id)
cycles$infert_post_ovu = agg$out_phase[m]
rm(agg, m)


# infertile before ovulation
agg = aggregate(out_phase ~ cycle_id, cycledays[cycledays$out_phase == 1,], length)
m = match(cycles$cycle_id, agg$cycle_id)
cycles$infert_pre_ovu = agg$out_phase[m]
rm(agg, m)



# PEAK day
agg = aggregate(out_cycleday ~ cycle_id, cycledays[cycledays$out_js == 1, ], max)
m = match(cycles$cycle_id, agg$cycle_id)
cycles$peak_day = agg$out_cycleday[m]
rm(agg, m)


# 1st star day
agg = aggregate(out_cycleday ~ cycle_id, cycledays[cycledays$out_star %in% 1:2, ], min)
m = match(cycles$cycle_id, agg$cycle_id)
cycles$day_1st_star = agg$out_cycleday[m]
rm(agg, m)


# suspected double cycles
cond = (cycledays$cycle_length >=45) & (cycledays$out_cycleday >= 18) & (cycledays$cycleday_from_end <= -15)
agg.sum = aggregate(blood ~ cycle_id, cycledays[cond, ], sum)
colnames(agg.sum) = c('cycle_id','blood.sum')
agg.max = aggregate(blood ~ cycle_id, cycledays[cond, ], max)
colnames(agg.max) = c('cycle_id','blood.max')

cond.gap = cond & (cycledays$blood > 0)
diff.blood = diff(diff(cycledays$out_cycleday[cond.gap])) # 3 consecutive days of bleeding
cycle_ids = cycledays$cycle_id[cond.gap]
bleeding.gaps = data.frame(gap = diff.blood, cycle_id = cycle_ids[1:length(diff.blood)], stringsAsFactors = FALSE)
cycle_ids = unique(bleeding.gaps$cycle_id[bleeding.gaps$gap == 0])

agg = merge(agg.sum, agg.max)
m = match(agg$cycle_id, cycle_ids)
agg$consecutive.days.of.bleeding = !is.na(m)
agg = agg[(agg$blood.sum >=5)&(agg$blood.max >=2)&(agg$consecutive.days.of.bleeding),]
m = match(cycles$cycle_id, agg$cycle_id)
cycles$susp_double_cycle = FALSE
cycles$susp_double_cycle[!is.na(m)] = TRUE



