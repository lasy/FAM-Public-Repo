
# average, median, min and max and sd cycle length
agg = aggregate(cycle_length ~ user_id, cycles[ok.cycles,], mean)
m = match(users$user_id, agg$user_id)
users$cycle_length.avg = agg$cycle_length[m];rm(agg, m)
agg = aggregate(cycle_length ~ user_id, cycles[ok.cycles,], median)
m = match(users$user_id, agg$user_id)
users$cycle_length.med = agg$cycle_length[m];rm(agg, m)
agg = aggregate(cycle_length ~ user_id, cycles[ok.cycles,], min)
m = match(users$user_id, agg$user_id)
users$cycle_length.min = agg$cycle_length[m];rm(agg, m)
agg = aggregate(cycle_length ~ user_id, cycles[ok.cycles,], max)
m = match(users$user_id, agg$user_id)
users$cycle_length.max = agg$cycle_length[m];rm(agg, m)
agg = aggregate(cycle_length ~ user_id, cycles[ok.cycles,], sd)
m = match(users$user_id, agg$user_id)
users$cycle_length.sd = agg$cycle_length[m];rm(agg, m)



# average & median number of observation
agg = aggregate(n_obs ~ user_id, cycles[ok.cycles,], mean)
m = match(users$user_id, agg$user_id)
users$n_obs.avg = agg$n_obs[m];rm(agg, m)
agg = aggregate(n_obs ~ user_id, cycles[ok.cycles,], median)
m = match(users$user_id, agg$user_id)
users$n_obs.med = agg$n_obs[m];rm(agg, m)
agg = aggregate(n_obs ~ user_id, cycles[ok.cycles,], min)
m = match(users$user_id, agg$user_id)
users$n_obs.avg.min = agg$n_obs[m];rm(agg, m)
agg = aggregate(n_obs ~ user_id, cycles[ok.cycles,], max)
m = match(users$user_id, agg$user_id)
users$n_obs.max = agg$n_obs[m];rm(agg, m)
agg = aggregate(n_obs ~ user_id, cycles[ok.cycles,], sd)
m = match(users$user_id, agg$user_id)
users$n_obs.sd = agg$n_obs[m];rm(agg, m)


