#### These are the functions for Data analysis.R file

#### explanation file of different activities
load_activity_explanation = function() {
  act_explanation = data.frame(num = NULL, meaning = NULL)
  act_explanation = rbind(act_explanation, c(2, "logged off"), c(3, "taking calls"), c(7, "break"), c(8, "unpaid lunch break"),
                          c(16, "wrap up"), c(35, "logging in"), c(39, "meeting"), c(40, "meeting2"), c(41, "other work"), 
                          c(42, "other work2"), c(43, "unpaid break (tussenuur)"), c(44, "logged off2"), c(61, "outbound calling"), 
                          c(71, "logged off3"))
  colnames(act_explanation) = c("num", "meaning")##change the name of frame
  act_explanation$note = " "
  act_explanation$note[which(act_explanation$num == 8)] = "unavailable and unpaid"
  act_explanation$note[which(act_explanation$num == 35)] = "preparing to use the system, unavailable and unpaid"
  act_explanation$note[which(act_explanation$num == 43)] = "unvailable and unpaid"
  act_explanation$note[which(act_explanation$num == 44)] = "logged off by system"
  act_explanation$note[which(act_explanation$num == 61)] = "not in call log"
  act_explanation$note[which(act_explanation$num == 71)] = "corrected hours (treat this as logged out)"

  return(act_explanation)
}

#### load activity data
load_activity_data = function() {
  act.data = read.csv("activity-2014-01.csv")  
  act.data = rbind(act.data, read.csv("activity-2014-02.csv"))
  act.data = rbind(act.data, read.csv("activity-2014-03.csv"))
  act.data = rbind(act.data, read.csv("activity-2014-04.csv"))
  act.data = rbind(act.data, read.csv("activity-2014-05.csv"))
  act.data = rbind(act.data, read.csv("activity-2014-06.csv"))
  act.data = rbind(act.data, read.csv("activity-2014-07.csv"))
  act.data = rbind(act.data, read.csv("activity-2014-08.csv"))
  act.data = rbind(act.data, read.csv("activity-2014-09.csv"))
  act.data = rbind(act.data, read.csv("activity-2014-10.csv"))
  act.data = rbind(act.data, read.csv("activity-2014-11.csv"))
  act.data = rbind(act.data, read.csv("activity-2014-12.csv"))
  act.data$startdatetime = as.POSIXct(as.character(act.data$startdatetime))
  
  outlier.row = which(act.data$enddatetime == "0000-00-00 00:00:00")
  act.data = act.data[-outlier.row,]
  act.data$enddatetime = as.POSIXct(as.character(act.data$enddatetime))
  
  return(act.data)
}

#### load call records/log data
load_call_data = function() {
  call.data = read.csv("calls-2014-01.csv")
  call.data = rbind(call.data, read.csv("calls-2014-02.csv"))
  call.data = rbind(call.data, read.csv("calls-2014-03.csv"))
  call.data = rbind(call.data, read.csv("calls-2014-04.csv"))
  call.data = rbind(call.data, read.csv("calls-2014-05.csv"))
  call.data = rbind(call.data, read.csv("calls-2014-06.csv"))
  call.data = rbind(call.data, read.csv("calls-2014-07.csv"))
  call.data = rbind(call.data, read.csv("calls-2014-08.csv"))
  call.data = rbind(call.data, read.csv("calls-2014-09.csv"))
  call.data = rbind(call.data, read.csv("calls-2014-10.csv"))
  call.data = rbind(call.data, read.csv("calls-2014-11.csv"))
  call.data = rbind(call.data, read.csv("calls-2014-12.csv"))
  
  null.row = which(as.character(call.data$answered) == "NULL")
  call.data$answered[null.row] = NA
  null.row = which(as.character(call.data$consult) == "NULL")
  call.data$consult[null.row] = NA
  null.row = which(as.character(call.data$transfer) == "NULL")
  call.data$transfer[null.row] = NA
  
  ### in row 158751, hangup is NULL which is not correct
  null.row = which(as.character(call.data$hangup) == "NULL" )
  call.data$hangup[null.row] = NA
  
  call.data$date_received = as.POSIXct(call.data$date_received)
  call.data$answered = as.POSIXct(as.character(call.data$answered))
  call.data$consult = as.POSIXct(as.character(call.data$consult))
  call.data$transfer = as.POSIXct(as.character(call.data$transfer))
  call.data$hangup = as.POSIXct(as.character(call.data$hangup))
  
  return(call.data)
}  

#### transfer time to seconds
time_to_seconds = function (time) {
  return (as.numeric(format(time, "%H")) * 3600 + as.numeric(format(time, "%M")) * 60 + as.numeric(format(time, "%S")))
}

#### transfer time to 15/30 minutes interval time
time_to_interval = function(time, interval_length) {
  y = paste(format(time, "%H"), ((as.numeric(format(time, "%M")) %/% (interval_length / 60)) * interval_length / 60), "00", sep = ":")
  interval = paste(format(time, "%Y-%m-%d"), format(strptime(y, "%H:%M:%S"), "%H:%M:%S"), sep = " ")
  return (interval)
}

### calculate the mean service duration of a data set
calculate_mean_ser_dur = function(data) {
  if (length(data[,1]) > 0) {
    temp = data
    ab.row = which(is.na(temp$answered))
    if (length(ab.row) > 0) {
      temp = temp[-ab.row,]
    }
    temp = data.frame(answered = temp$answered, transfer = temp$transfer, hangup = temp$hangup)
    y = apply(temp, 1, function(x) min(x[2], x[3], na.rm = T))
    temp$service_dur = difftime(as.POSIXct(y), temp$answered, units = "secs")
    return(mean(temp$service_dur))
  } else {
    return (0)
  }
  
}

### Kaplan-Meier funciton, input data should contain a column called waiting_time and a column called 
###result (Abandon or Agent), the data need not to be ordered. 
kaplan_meier_estimator = function(data) {
  
  if (length(data$answered) > 0) {
    
    ab.row = which(is.na(data$answered))
    data$waiting_time = NA
    data$waiting_time[-ab.row] = difftime(data$answered[-ab.row], data$date_received[-ab.row], units = "secs")
    data$waiting_time[ab.row] = difftime(data$hangup[ab.row],  data$date_received[ab.row], units = "secs")
   
    x = data
    ab.row = which(is.na(x$answered))
    x$result = "Agent"
    x$result[ab.row] = "Abandon"
    x$result = factor(x$result, levels = c("Abandon", "Agent"))  
    x = x[order(x$waiting_time, x$result), ]
    ## remove the last few connected calls (because they can not be used in Kaplan-Meier estimator)
    x = x[1:tail(which(x$result == "Abandon"), 1),]    
    
    ## remove all the calls that have waited 0 seconds
    row = which(x$result == "Agent" & x$waiting_time <= 2)
    if (length(row) > 0) {
      x = x[-row,]
    }
    
    if (length(which(x$result == "Abandon")) > 0) {
      x$n = seq(from = length(x$result), to = 1, by = -1)
      x$d = as.numeric(x$result == "Abandon")
      x$prob1 = (x$n - x$d) / x$n
      
      ### answered calls have to be removed
      ans.row = which(x$result == "Agent")
      if (length(ans.row) > 0 ) {
        x = x[-ans.row,]
      }
      
      x$tail.prob = cumprod(x$prob1)
      x$cdf = 1 - x$tail.prob
      
      ## remove the duplicated waiting times
      dup.row = which(duplicated(x$waiting_time, fromLast = T))
      if (length(dup.row) > 0) {
        x = x[-dup.row,]
      }
      
      x$density = diff(c(0, x$cdf))
      result = data.frame(result = x$result, waiting_time = x$waiting_time, tail.prob = x$tail.prob, aban_prob = x$density)
      
    } else {  ## if there are no abandonment, then I assume the patience is close to infinity.
      result = data.frame(result = "Abandon", waiting_time = 100000, aban_prob = 10000)
    }
  } else {
    result = data.frame(result = "Abandon", waiting_time = 100000, aban_prob = 10000)
  }
  
  return(result)
}
  
#### calculate the aggregated number of arrivals, ser_dur, SL, etc in 15 or 30 minutes intervals
calculate_info_15_minutes_interval = function(selected_days, selected_queue_name, interval_length) {
  
  temp = call.data[which(call.data$queue_name %in% selected_queue_name & format(call.data$date_received,
                                                                                "%Y-%m-%d") %in% as.character(selected_days)),]
  starttime = strptime("08:00:00", "%H:%M:%S")
  if (interval_length ==  900) {
    endtime = strptime("19:45:00", "%H:%M:%S")
  } else if (interval_length == 1800) {            
    endtime = strptime("19:30:00", "%H:%M:%S")
  }
  time = format(seq(starttime, endtime, by = interval_length), "%H:%M:%S")
  x = expand.grid(time, selected_days)
  
  if (length(temp$date_received) == 0) {
    result = data.frame(time = as.POSIXct(paste(x[,2], x[,1], sep = " ")), arrival= NA, abandon = 0,
                        mean_ser_dur =NA, mean_wait_time = NA, SL = NA)
  } else {
    result = data.frame(time = as.POSIXct(paste(x[,2], x[,1], sep = " ")), arrival = 0)
    
    ## calculate the number of arrivals
    y = paste(format(temp$date_received, "%H"), ((as.numeric(format(temp$date_received, "%M")) %/%
                                                    (interval_length / 60)) * interval_length / 60), "00", sep = ":")
    interval = paste(format(temp$date_received, "%Y-%m-%d"), format(strptime(y, "%H:%M:%S"), "%H:%M:%S"), sep = " ")
    temp$interval = factor(as.character(as.POSIXct(interval)), levels = as.character(result$time))
    temp$count = 1
   ## for(drindex in 1:length(temp$date_received)-1)                       8:39:21   39/30=1*30
    ##{temp$date_receivednext[drindex]=temp$date_received[drindex+1]}
    result$arrival = tapply(temp$count, temp$interval, sum)     
    result$arrival[which(is.na(result$arrival))] = 0
    
    ##calculate the number of abandonments
    temp$abandon_count = 0
    ab.row = which(is.na(temp$answered))
    if (length(ab.row) == 0) {
      temp$abandon_count = 0
    } else {
      temp$abandon_count[ab.row] = 1 
    }
    result$abandon = tapply(temp$abandon_count, temp$interval, sum)
    result$abandon[which(is.na(result$abandon))] = 0
    
    ## calculate the mean service duration
    temp2 = data.frame(answered = temp$answered, transfer = temp$transfer, hangup = temp$hangup)
    x = apply(temp2, 1, function(x) min(x[2], x[3], na.rm = T))
    temp$service_dur = difftime(as.POSIXct(x), temp2$answered, units = "secs")
    result$mean_ser_dur = tapply(temp$service_dur, temp$interval, mean, na.rm = T)
    
    ### calculate the SL and the mean (actual) waiting time, where the SL is defined as #served within AWT/#served
    ab.row = which(is.na(temp$answered))
    temp$waiting_time = NA
    if (length(ab.row) == 0) {
      temp$waiting_time = temp$answered - temp$date_received
      result$mean_wait_time = tapply(temp$waiting_time, temp$interval, mean)
      temp$SL_indi = 0
      temp$SL_indi[which(temp$waiting_time < acceptable_waiting_time)] = 1
    } else {
      temp$waiting_time[-ab.row] = temp$answered[-ab.row] - temp$date_received[-ab.row]
      temp$waiting_time[ab.row] = temp$hangup[ab.row] - temp$date_received[ab.row]
      result$mean_wait_time = tapply(temp$waiting_time[-ab.row], temp$interval[-ab.row], mean)
      temp$SL_indi = 0
      temp$SL_indi[which(temp$waiting_time < acceptable_waiting_time)] = 1
      temp$SL_indi[ab.row] = 0
    }
    
    result$SL = tapply(temp$SL_indi, temp$interval, sum) 
    result$SL = result$SL / (result$arrival-result$abandon)
  }
  
  return (result)
}

##### this function calculate the agent-skill matrix
calculate_agent_skill_matrix = function(selected_queue_name, selected_days, call.data) {
  x = call.data[which(format(call.data$date_received, "%Y-%m-%d") %in% as.character(selected_days)),]
  x = x[which(x$queue_name %in% selected_queue_name), ]
  agent_id = unique(x$agent_number)
  
  ## consider all the months prior to the considered month, 
  ## I assume if an agent can answer a type of calls in month x, then he can also do it in x+1
  x = call.data[which(as.numeric(format(call.data$date_received, "%m")) %in% (1:format(as.Date(selected_days), "%m"))),]
  x = x[which(x$agent_number %in% agent_id & x$queue_name %in% selected_queue_name),]
  
  agent.skill.data = data.frame(matrix(0, nrow = length(agent_id), ncol = length(selected_queue_name)))
  colnames(agent.skill.data) = selected_queue_name
  rownames(agent.skill.data) = agent_id
  
  for (j in 1:length(agent_id)) {
    temp.x = x[which(x$agent_number == agent_id[j]),]
    agent.skill.data[j,which(selected_queue_name %in% unique(temp.x$queue_name))] = 1
  }  
  
  agent.skill.data = agent.skill.data[-which(rownames(agent.skill.data) == "NULL"),]
  
  return (agent.skill.data) 
}

#### from agent.skill.data calculate skill set matrix, if output == TRUE, then it will generate the "skill set.txt" file
calculate_skill_set_matrix = function (selected_queue_name, agent.skill.data, output) {
  skill.set = unique(apply(agent.skill.data, 1, function(x) paste(x, collapse = "")))
  skill.set.length = length(skill.set)
  skill.set = unlist(strsplit(paste(skill.set, collapse = ""), spli = ""))
  skill.set = matrix(as.numeric(skill.set), nrow = length(selected_queue_name), ncol = skill.set.length, byrow =FALSE)
  row.names(skill.set) = selected_queue_name
  
  ### output in a txt file or not
  if (output == TRUE | output == T) {
    write.table(skill.set, file = "skill.set.txt", sep = "\t", row.names = F, col.names = F)
  } 
  
  return(skill.set)
}


## calculate the number of agents per 30 min interval per skill groups for one specific considered day.
calculate_num_agents_30_interval_skill_group = function(selected_days, selected_queue_name, skill.set, agent.skill.data, 
                                                        act.data, working_activity, output) {
  temp.data = act.data[which(format(act.data$startdatetime, "%Y-%m-%d") %in% as.character(selected_days)),]
  

  ### we only consider people working when they are doing working_activity
  temp.data = temp.data[which(temp.data$dnd_id %in% working_activity),]
  
  ### generate a matrix/data.frame where row is the time, and col is the skill groups
  starttime = strptime("08:00:00", "%H:%M:%S")
  if (interval_length == 900) {
    endtime = strptime("19:45:00", "%H:%M:%S")
  } else if (interval_length == 1800) {
    endtime = strptime("19:30:00", "%H:%M:%S")
  }
  time = format(seq(starttime, endtime, by = interval_length), "%H:%M:%S")
  x = expand.grid(time, selected_days)
  time = as.POSIXct(paste(x[,2], x[,1], sep = " "))
  result = matrix(0, nrow = length(time), ncol = dim(skill.set)[2]+1)
  result = data.frame(result)
  colnames(result) = c("time", unique(apply(agent.skill.data, 1, function(x) paste(x, collapse = ""))))
  result$time = time
  result$sec = unlist(lapply(result$time, time_to_seconds))
  
  agent_id = rownames(agent.skill.data)
  ##calculate the starting time and the ending time of each agents. 
  startworktime = NULL
  endworktime = NULL
  for (j in 1:length(agent_id)) {
    row = which(rownames(agent.skill.data) == agent_id[j])
    skill.col.num = which(colnames(result) == paste(agent.skill.data[row,], collapse = ""))
    
    x = temp.data[which(temp.data$agent_id == agent_id[j]), ]
    startworktime = unlist(lapply(x$startdatetime, time_to_seconds))
    endworktime = unlist(lapply(x$enddatetime, time_to_seconds))
    
    ## remove those startworktime and endworktime which ends earlier than 8:00:00
    if (length(which(endworktime < time_to_seconds(time[1]))) > 0) {
      startworktime = startworktime[-which(endworktime < time_to_seconds(time[1]))]
      endworktime = endworktime[-which(endworktime < time_to_seconds(time[1]))]
    }
    ## if the startworktime is earlier than 8:00:00, then I assume it starts with 8:00:00
    if(length(which(startworktime < time_to_seconds(time[1]))) > 0) {
      startworktime[which(startworktime < time_to_seconds(time[1]))] = time_to_seconds(time[1])
    }
    ## remove those startworktime and endworktime which ends later than 20:00:00
    bad.row = which(startworktime > (time_to_seconds(time[length(time)]) + interval_length))
    if (length(bad.row) > 0) {
      startworktime = startworktime[-bad.row]
      endworktime = endworktime[-bad.row]
    }
    ## if the endworktime is later than 20:00:00, then I assume it ends at 20:00:00
    bad.row = which(endworktime > (time_to_seconds(time[length(time)]) + interval_length))
    if (length(bad.row) > 0) {
      endworktime[bad.row] = time_to_seconds(time[length(time)] + interval_length)
    }
    
    a = data.frame(time = seq(result$sec[1], (result$sec[length(result$sec)]+interval_length), 1), num = 0)
    for (k in 1:length(startworktime)) {
      a$num[which(a$time == startworktime[k])] = a$num[which(a$time == startworktime[k])] + 1
      a$num[which(a$time == endworktime[k])] = a$num[which(a$time == endworktime[k])] - 1
    }
    a$num = cumsum(a$num)
    for (k in 1:(length(result$sec)-1)) {
      start.row = which(result$sec[k] == a$time)
      end.row = which(result$sec[k+1] == a$time)
      result[k, skill.col.num] = result[k, skill.col.num] + sum(a$num[start.row:(end.row-1)]) / interval_length
    }
    start.row = which(result$sec[length(result$sec)] == a$time)
    end.row = which((result$sec[length(result$sec)]+interval_length) == a$time)
    result[length(result$sec), skill.col.num] = result[length(result$sec), skill.col.num] + sum(a$num[start.row:(end.row-1)]) / interval_length 
  }

  result$sec = NULL
  
  if (output == TRUE) {
    write.table(result, file = "staffing per skill group.txt", sep = "\t", row.names = T, col.names = T)
  }
  
  return(result)
}

#### calculate the manpower that spends on answer other un-selected queues
calcualte_manpower_for_working_other_skills = function(selected_days, call.distribution, selected_queue_name,
                                             skill.set, agent.skill.data, call.data, working_activity) {
  
  temp.data = call.data[which(format(call.data$date_received, "%Y-%m-%d") %in% as.character(selected_days)),]
  other_activity_row = which(as.numeric(as.character(call.distribution$queue_name)) %in% selected_queue_name)
  other_q_activity = as.numeric(as.character(call.distribution$queue_name[-other_activity_row]))
  ### we only consider people working when they are doing working_activity
  temp.data = temp.data[which(temp.data$queue_name %in% other_q_activity),]
  
  ## only consider those answered calls
  ab.row = which(is.na(temp.data$answered))
  if (length(ab.row) > 0) {
    temp.data = temp.data[-ab.row,]
  }
  ## only consider those calls answerd by the agents who are listed in agent.skill.data
  considered_agents_row = which(temp.data$agent_number %in% rownames(agent.skill.data))
  temp.data = temp.data[considered_agents_row,]
  ## calculate the service duration of each answered calls
  a = data.frame(answered = temp.data$answered, transfer = temp.data$transfer, hangup = temp.data$hangup)
  x = apply(a, 1, function(x) min(x[2], x[3], na.rm = T))
  temp.data$service_dur = difftime(as.POSIXct(x), a$answered, units = "secs")
  
  ### generate a matrix/data.frame where row is the time, and col is the skill groups
  starttime = strptime("08:00:00", "%H:%M:%S")
  if (interval_length == 900) {
    endtime = strptime("19:45:00", "%H:%M:%S")
  } else if (interval_length == 1800) {
    endtime = strptime("19:30:00", "%H:%M:%S")
  }
  time = format(seq(starttime, endtime, by = interval_length), "%H:%M:%S")
  x = expand.grid(time, selected_days)
  time = as.POSIXct(paste(x[,2], x[,1], sep = " "))
  result = matrix(0, nrow = length(time), ncol = dim(skill.set)[2]+1)
  result = data.frame(result)
  colnames(result) = c("time", unique(apply(agent.skill.data, 1, function(x) paste(x, collapse = ""))))
  result$time = time
  
  
  if (length(temp.data$date_received) == 0) {
    result = result
  } else {
    result$sec = unlist(lapply(result$time, time_to_seconds))
    agent_id = rownames(agent.skill.data)
    
    starttime = NULL
    endtime = NULL
    for (j in 1:length(temp.data$date_received)) {
      starttime = time_to_seconds(temp.data$answered[j])
      endtime = time_to_seconds(temp.data$answered[j]) + as.numeric(temp.data$service_dur[j])
      
      ## remove those startworktime and endworktime which ends earlier than 8:00:00
      if (length(which(endtime < time_to_seconds(time[1]))) > 0) {
        starttime = starttime[-which(endtime < time_to_seconds(time[1]))]
        endtime = endtime[-which(endtime < time_to_seconds(time[1]))]
      }
      ## if the startworktime is earlier than 8:00:00, then I assume it starts with 8:00:00
      if(length(which(starttime < time_to_seconds(time[1]))) > 0) {
        starttime[which(starttime < time_to_seconds(time[1]))] = time_to_seconds(time[1])
      }
      ## remove those startworktime and endworktime which ends later than 20:00:00
      bad.row = which(starttime > (time_to_seconds(time[length(time)]) + interval_length))
      if (length(bad.row) > 0) {
        starttime = starttime[-bad.row]
        endtime = endtime[-bad.row]
      }
      ## if the endworktime is later than 20:00:00, then I assume it ends at 20:00:00
      bad.row = which(endtime > (time_to_seconds(time[length(time)]) + interval_length))
      if (length(bad.row) > 0) {
        endtime[bad.row] = time_to_seconds(time[length(time)] + interval_length)
      }
      
      a = data.frame(time = seq(result$sec[1], (result$sec[length(result$sec)]+interval_length), 1), num = 0)    
      a$num[which(a$time == starttime)] = a$num[which(a$time == starttime)] - 1
      a$num[which(a$time == endtime)] = a$num[which(a$time == endtime)] + 1
      a$num = cumsum(a$num)
      
      row = which(as.character(temp.data$agent_number[j]) == rownames(agent.skill.data))
      col = which(paste(agent.skill.data[row,], collapse = "") == colnames(result))
      for (k in 1:(length(result$sec)-1)) {
        start.row = which(result$sec[k] == a$time)
        end.row = which(result$sec[k+1] == a$time)
        result[k, col] = result[k, col] + sum(a$num[start.row:(end.row-1)]) / interval_length
      }
      start.row = which(result$sec[length(result$sec)] == a$time)
      end.row = which((result$sec[length(result$sec)]+interval_length) == a$time)
      result[length(result$sec), col] = result[length(result$sec), col] + sum(a$num[start.row:(end.row-1)]) / interval_length 
    }
  }
  result$sec = NULL
  
  return(result)
  
}

#### calculate the SL of each skill of the selected days.
calculate_SL_of_each_skill = function (selected_days, selected_queue_name, acceptable_waiting_time, interval_length) {
  all_result = NULL
  
  for (i in 1:length(selected_queue_name)) {
    temp = call.data[which(call.data$queue_name %in% selected_queue_name[i] & format(call.data$date_received, 
                                                                                     "%Y-%m-%d") %in% as.character(selected_days)),]
    starttime = strptime("08:00:00", "%H:%M:%S")
    if (interval_length == 900) {
      endtime = strptime("19:45:00", "%H:%M:%S")
    } else if (interval_length == 1800) {
      endtime = strptime("19:30:00", "%H:%M:%S")
    }
    
    time = format(seq(starttime, endtime, by = interval_length), "%H:%M:%S")
    x = expand.grid(time, selected_days)
    
    if (length(temp$date_received) == 0) {
      result = data.frame(time = as.POSIXct(paste(x[,2], x[,1], sep = " ")), SL = NA)
    } else {
      result = data.frame(time = as.POSIXct(paste(x[,2], x[,1], sep = " ")), SL = NA)
      
      ## calculate the number of arrivals
      y = paste(format(temp$date_received, "%H"), 
                ((as.numeric(format(temp$date_received, "%M")) %/% (interval_length / 60)) * interval_length / 60), "00", sep = ":")
      interval = paste(format(temp$date_received, "%Y-%m-%d"), format(strptime(y, "%H:%M:%S"), "%H:%M:%S"), sep = " ")
      temp$interval = factor(as.character(as.POSIXct(interval)), levels = as.character(result$time))
      temp$count = 0
      ab.row = which(is.na(temp$answered))
      if (length(ab.row) == 0) {
        temp$count = 1
      } else {
        temp$count[-ab.row] = 1
      }
      num_answered = tapply(temp$count, temp$interval, sum)
      temp$count = 1
      arrival = tapply(temp$count, temp$interval, sum)
      
      ## calculate the ab percentage 
      ab.row = which(is.na(temp$answered))
      temp$waiting_time = NA
      if (length(ab.row) == 0) {
        temp$waiting_time = temp$answered - temp$date_received
        temp$SL_indi = 0
        temp$SL_indi[which(temp$waiting_time < acceptable_waiting_time)] = 1
      } else {
        temp$waiting_time[-ab.row] = temp$answered[-ab.row] - temp$date_received[-ab.row]
        temp$waiting_time[ab.row] = temp$hangup[ab.row] - temp$date_received[ab.row]
        temp$SL_indi = 0
        temp$SL_indi[which(temp$waiting_time < acceptable_waiting_time)] = 1
        temp$SL_indi[ab.row] = 0
      }
      
      # result$mean_wait_time = tapply(temp$waiting_time, temp$interval, mean)
      result$SL = tapply(temp$SL_indi, temp$interval, sum) 
      result$SL = result$SL / num_answered
    }
  
    if (i == 1) {
      all_result = result
    } else {
      all_result = cbind(all_result, result$SL)
    }    
  }
  row.names(all_result) = 1:length(all_result$time)
  
  return(all_result)
}


### calcualte the wapes of the performance measures
calculate_wape = function (data, sim.SL.overall, act.SL.overall, sim.Ab.overall, act.Ab.overall,
                           sim.ASA.overall, act.ASA.overall) {
  data$date = format(data$date_received, "%Y-%m-%d")
  data$count = 1
  arrival = tapply(data$count, data$date, sum)
  data$abcount = 0
  ab.row = which(is.na(data$answered))
  if (length(ab.row) > 0) {
    data$abcount[ab.row] = 1
  }
  ab = tapply(data$abcount, data$date, sum)
  
  wape_SL = sum((arrival-ab)*abs(act.SL.overall - sim.SL.overall)) / sum(arrival - ab)
  wape_Ab = sum(arrival*abs(act.Ab.overall - sim.Ab.overall)) / sum(arrival)
  wape_ASA = sum((arrival-ab)*abs(act.ASA.overall - sim.ASA.overall)) / sum(arrival - ab)
  
  return (list(wape_SL = wape_SL, wape_Ab = wape_Ab, wape_ASA = wape_ASA))
}

### calcualte how much percentage of actuals fall in the CI
calculate_inclusion = function (sim.SL.low.ci, sim.SL.up.ci, act.SL.overall,
                                sim.Ab.low.ci, sim.Ab.up.ci, act.Ab.overall,
                                sim.ASA.low.ci, sim.ASA.up.ci, act.ASA.overall) {
  included.row.SL = which(act.SL.overall >= sim.SL.low.ci & act.SL.overall <= sim.SL.up.ci)
  inclusion.SL = length(included.row.SL ) / length(act.SL.overall)
  
  included.row.Ab = which(act.Ab.overall >= sim.Ab.low.ci & act.Ab.overall <= sim.Ab.up.ci)
  inclusion.Ab = length(included.row.Ab ) / length(act.Ab.overall)
  
  included.row.ASA = which(act.ASA.overall >= sim.ASA.low.ci & act.ASA.overall <= sim.ASA.up.ci)
  inclusion.ASA = length(included.row.ASA ) / length(act.ASA.overall)
  
  return(list(in.SL = inclusion.SL, in.Ab = inclusion.Ab, in.ASA = inclusion.ASA))
}

### calcualte the empirical HT of each router, and output in a txt file called output name 
calculate_output_empirical_HT = function (data, selected_queue_name, wrap_up_time,
                                          wrap_up_time_consideration, output_name) {
  sink(output_name)
  for(i in 1:length(selected_queue_name)) {
    temp = data[which(data$queue_name == selected_queue_name[i]),]
    if (length(temp[,1]) > 0) {
      ab.row = which(is.na(temp$answered))
      if (length(ab.row) > 0) {
        temp = temp[-ab.row,]
      }
      temp = data.frame(answered = temp$answered, transfer = temp$transfer, hangup = temp$hangup)
      y = apply(temp, 1, function(x) min(x[2], x[3], na.rm = T))
      service_dur = as.numeric(difftime(as.POSIXct(y), temp$answered, units = "secs") / 60)
      na.row = which(is.na(service_dur))
      if (length(na.row) > 0) {
        service_dur[na.row] = 0
      }
      if (wrap_up_time_consideration) {
        service_dur = service_dur + wrap_up_time
      }
      cat(service_dur, "\n")
    } else {
      cat(0, "\n")
    }
  }  
  sink()
}


### calculate the output empirical patience
calculate_output_empirical_patience = function (data, selected_queue_name, numberofsamples, empirical_patience_output_name) {
  sink(empirical_patience_output_name)
  for (index in 1:length(selected_queue_name)) {
    result = kaplan_meier_estimator(call.data[which(call.data$queue_name == selected_queue_name[index]), ])
    vec = rep(result$waiting_time, round(numberofsamples * result$aban_prob)) /60
    cat(vec, "\n")
  }
  sink()
}

  
###calculate the mean wrap-up time (unit: seconds)
calculate_mean_wrap_up_time = function(data) {
  wrap.row = which(data$dnd_id == 16)
  dur = difftime(data$enddatetime[wrap.row], as.POSIXct(data$startdatetime[wrap.row]), units = "secs") 
  dur = as.numeric(dur)
  outlier.consider = quantile(dur, 0.99)
  outlier.row = which(dur > outlier.consider)
  if (length(outlier.row) > 0) {
    dur = dur[-outlier.row]
  }
  ans.num = length(which(data$dnd_id == 3))
  dur = c(dur, rep(0, length.out = ans.num - length(dur)))
  
  return(mean(dur))
}

#### output the empirical arrival time, if there is no arrival, there will be an arrival at time 0
calculate_output_empirical_arrival_time = function (data, selected_queue_name, output_name) {
  sink(output_name)
  for(i in 1:length(selected_queue_name)) {
    temp = data[which(data$queue_name == selected_queue_name[i]),]
    if (length(temp[,1]) > 0) {
      
      hour = as.numeric(format(temp$date_received, "%H")) * 60
      minute = as.numeric(format(temp$date_received, "%M")) 
      second = as.numeric(format(temp$date_received, "%S")) / 60
      time = hour+minute + second           
      
      cat(time, "\n")
    } else {
      cat(0, "\n")
    }
  }  
  sink()
}


#### output the empirical HT of each interval
calculate_output_empirical_HT_per_interval = function (data, selected_queue_name, wrap_up_time, service_time_vector,
                                                  wrap_up_time_consideration, output_name) {
  
  starttime = strptime("00:00:00", "%H:%M:%S")
  if (interval_length == 900) {
    endtime = strptime("19:45:00", "%H:%M:%S")
  } else if (interval_length == 1800) {
    endtime = strptime("23:30:00", "%H:%M:%S")
  }
  
  time = format(seq(starttime, endtime, by = interval_length), "%H:%M:%S")
  
  ## remove all the abandonment rows
  ab.row = which(is.na(data$answered))
  if (length(ab.row) > 0) {
    data = data[-ab.row,]
  }   
  
  sink(output_name)
  for(i in 1:length(selected_queue_name)) {
    temp = data[which(data$queue_name == selected_queue_name[i]),]
     
    if (length(temp[,1]) > 0) {
      aa = temp
      ## construct interval names
      y = paste(format(aa$date_received, "%H"), 
                ((as.numeric(format(aa$date_received, "%M")) %/% (interval_length / 60)) * interval_length / 60), "00", sep = ":")
      interval = paste(format(strptime(y, "%H:%M:%S"), "%H:%M:%S"))
      aa$interval = factor(interval, levels = time)  
      
      ### for each interval
      for (j in 1:length(time)) { 
        bb = aa[which(aa$interval == time[j]),]
        if (length(bb[,1]) > 0) {
          cc = data.frame(answered = bb$answered, transfer = bb$transfer, hangup = bb$hangup)
          y = apply(cc, 1, function(x) min(x[2], x[3], na.rm = T))
          service_dur = as.numeric(difftime(as.POSIXct(y), cc$answered, units = "secs") / 60)
          na.row = which(is.na(service_dur))
          if (length(na.row) > 0) {
            service_dur[na.row] = 0
          }
          if (wrap_up_time_consideration) {
            service_dur = service_dur + wrap_up_time
          }
          cat(service_dur, "\n")
        } else {
          cat(service_time_vector[i], "\n")
        }
        
      }
      
    } else {
      for (j in 1:length(time)) {
        cat(0, "\n")
      }
    }
  }
  sink()
}

#### This is the function for fitting the AHT of each day
fitting_AHT = function (data, considered_days) {
  ab.row = which(is.na(data$answered))
  if (length(ab.row) > 0) {
    data = data[-ab.row,]
  }
  
  agent_id = unique(data$agent_number)
  temp = data.frame(answered = data$answered, transfer = data$transfer, hangup = data$hangup)
  y = apply(temp, 1, function(x) min(x[2], x[3], na.rm = T))
  data$service_dur = as.numeric(difftime(as.POSIXct(y), data$answered, units = "secs"))
  data$date = format(data$date_received, "%Y-%m-%d")
  
  ## how about AHT of each agent per month
  data$month = as.numeric(format(data$date_received, "%m"))
  dd = tapply(data$service_dur, list(data$agent_number, data$month), mean)
  
  ### for each row of dd, fit an exponential distribution
  aht.result = data.frame(agent = rownames(dd), a = 0, b = 0)
  aht.result = cbind(aht.result, dd)
  for (agent_index in 1:dim(dd)[1]) {
    x = dd[agent_index,]
    if (length(which(is.na(x))) == length(x)) {
      aht.result$a[agent_index] = aht.result$b[agent_index] = NA
      aht.result[agent_index,4:dim(aht.result)[2]] = NA
    } else if (length(which(is.na(x))) == length(x)-1) {
      aht.result$a[agent_index] = aht.result$b[agent_index] = NA
      aht.result[agent_index,4:dim(aht.result)[2]] = as.numeric(x)
    } else if  (length(which(is.na(x))) == length(x) -2) {
      aht.result$a[agent_index] = aht.result$b[agent_index] = NA
      fitted = mean(x[-which(is.na(x))])
      aht.result[agent_index,4:dim(aht.result)[2]] = as.numeric(fitted)
      col = as.numeric(which(is.na(x)))
      aht.result[agent_index, (3+col)] = NA
    } else {
      if (unique(data$queue_name) == 30066 & agent_index == 63) {
        mod <- nls(as.numeric(x) ~ a * exp(b * as.numeric(names(x))),start=list(a=260,b=0))
      } else {
        mod <- nls(as.numeric(x) ~ a * exp(b * as.numeric(names(x))),start=list(a=250,b=0))
      }
      fitted = coef(mod)[1] * exp(coef(mod)[2] *as.numeric(names(x)))
      aht.result$a[agent_index] = coef(mod)[1]
      aht.result$b[agent_index] = coef(mod)[2]
      aht.result[agent_index, 4:dim(aht.result)[2]] = fitted
    }
  }
  
  ### Calculate number of answered calls
  ## from now the consideration become the day index instead of agent index
  ##temp is the data with all same day index
  ##na.row is the agent who didn't have calls that day
  aht.fit = NULL
  for (day_index2 in 1:length(considered_days)) {
    selected_days2 = considered_days[day_index2]
    temp = data[which(format(data$date_received, "%Y-%m-%d") == selected_days2),]
    if (length(temp$date_received) > 0) {
      temp$count = 1
      num = tapply(temp$count, temp$agent_number, sum)
      na.row = which(is.na(num))
      if (length(na.row) > 0) {
        num = num[-na.row]
      }
      agent = names(num)
      ## here the agent is the one contributed in this day
      ##agent.aht.data is the frame used to solve this problem!!!
      agent.aht.data = data.frame(agent = agent, num = as.numeric(num), fit.aht = 0)
      col = which(colnames(aht.result) == unique(temp$month))
      xx = unlist(lapply(agent.aht.data$agent, function(x) which(as.character(aht.result$agent) == as.character(x))))
      agent.aht.data$fit.aht = aht.result[xx,col]
      
      aht.fit = c(aht.fit, sum(agent.aht.data$num * agent.aht.data$fit.aht) / sum(agent.aht.data$num))
    } else {
      aht.fit = c(aht.fit, NA)
    }
  }
  RealAHT_per_day=NULL
  for (day_index3 in 1:length(considered_days))
  {
    selected_days3 = considered_days[day_index3]
    temp2 = data[which(format(data$date_received, "%Y-%m-%d") == selected_days3),]
    RealAHT_per_day=c(RealAHT_per_day,mean(temp2$service_dur))
  }
  RealAHT_overall=mean(RealAHT_per_day)
  
  R_SQUARE=1-sum((RealAHT_per_day-aht.fit)^2)/sum((RealAHT_per_day-RealAHT_overall)^2)
  return(aht.fit)
  
}
fitting_AHT_NEW=function(data, considered_days)
  {
  ab.row = which(is.na(data$answered))
if (length(ab.row) > 0) {
  data = data[-ab.row,]
}

agent_id = unique(data$agent_number)
temp = data.frame(answered = data$answered, transfer = data$transfer, hangup = data$hangup)
y = apply(temp, 1, function(x) min(x[2], x[3], na.rm = T))
data$service_dur = as.numeric(difftime(as.POSIXct(y), data$answered, units = "secs"))
data$date = format(data$date_received, "%Y-%m-%d")
data$month = as.numeric(format(data$date_received, "%m"))
##dd = tapply(data$service_dur, list(data$agent_number, data$month), mean)

##x=dd[1,]

for(index in 1:dim(data)[1])
{data$date_index[index]=which(as.character(considered_days)%in%format(data$date_received[index], "%Y-%m-%d")  )
}
## step 2 data with 12 attributes including data_index for fitting x!! (method 1: using the day as x value)

data$month = as.numeric(format(data$date_received, "%m"))
dd = tapply(data$service_dur, list(data$agent_number, data$date_index), mean,na.rm=T)
dim(dd)
### for each row of dd, fit an exponential distribution
aht.result = data.frame(agent = rownames(dd), a = 0, b = 0)
aht.result = cbind(aht.result, dd)
## aht.result restore the value with dim[agents, a,b,dd]
## this is used to become the initial value(mean value): vector

data$counting=1
Max_Day=tapply(data$date_index,data$agent_number,max)


for(agent_index in 1:dim(dd)[1])
{
  
  data.agent=data[which(data$agent_number== rownames(dd)[agent_index]),]
  ## this data.agent is the data can be managed based on each agent_index
  Num_of_Call=tapply(data.agent$counting,data.agent$date_index,sum)
  Num_of_Day=length(unique(data.agent$date_index))
  x=data.agent$date_index
  y=data.agent$service_dur
  xname=dd[agent_index,] 
  if (Num_of_Day==0) 
    ##No calls at all
  {
    aht.result$a[agent_index] = aht.result$b[agent_index] = NA
    aht.result[agent_index,4:dim(aht.result)[2]] = NA
  }else if(Num_of_Day<=10)
    ## At least work for 30 days and above considering that they have a pattern of learning!! decreasing time
  {
    aht.result$a[agent_index] = aht.result$b[agent_index] = NA
    fitted = mean(y,na.rm=T)
    aht.result[agent_index,4:dim(aht.result)[2]] = NA
    aht.result[agent_index,(3+as.numeric(names(Num_of_Call)))]=as.numeric(fitted)
    
  }else{
    mod <- nls(as.numeric(y) ~ a * exp(b * as.numeric(x)), start = list(a=mean(y,na.rm=T), b = 0), control = list(maxiter = 500),trace=TRUE)
    aht.result$a[agent_index] = coef(mod)[1]
    aht.result$b[agent_index] = coef(mod)[2]
    fitted=coef(mod)[1] * exp(coef(mod)[2] *as.numeric(names(xname)))
    aht.result[agent_index, 4:dim(aht.result)[2]] =fitted
  }
  
  
}


### Calculate number of answered calls
aht.fit = NULL
for (day_index2 in 1:length(considered_days)) {
  
  selected_days2 = considered_days[day_index2]
  temp = data[which(format(data$date_received, "%Y-%m-%d") == selected_days2),]
  if (length(temp$date_received) > 0) {
    ## temp$count = 1
    num = tapply(temp$counting, temp$agent_number, sum)
    na.row = which(is.na(num))
    if (length(na.row) > 0) {
      num = num[-na.row]
    }
    agent = names(num)
    
    agent.aht.data = data.frame(agent = agent, num = as.numeric(num), fit.aht = 0)
    col = which(colnames(aht.result) == day_index2)
    xx = unlist(lapply(agent.aht.data$agent, function(x) which(as.character(aht.result$agent) == as.character(x))))
    agent.aht.data$fit.aht = aht.result[xx,col]
    agent.aht.data$num[is.na(agent.aht.data$fit.aht)]=0
    agent.aht.data$fit.aht[is.na(agent.aht.data$fit.aht)]=0
    
    aht.fit = c(aht.fit, sum(agent.aht.data$num * agent.aht.data$fit.aht) / sum(agent.aht.data$num))
  } else {
    aht.fit = c(aht.fit, NA)
  }
}
return(aht.fit)
}
fitting_AHT_NEW2=function(data,considered_days)
  {
  ab.row = which(is.na(data$answered))
  if (length(ab.row) > 0) {
    data = data[-ab.row,]
  }
  ## per agent using agent_id
  agent_id = unique(data$agent_number)
  temp = data.frame(answered = data$answered, transfer = data$transfer, hangup = data$hangup)
  ##temp is the only matrix we needed with relevant attributes
  y = apply(temp, 1, function(x) min(x[2], x[3], na.rm = T))
  ## using y because y here is the fitting plot's y value,here is the minimal time between transfer or hangup since either means an end
  data$service_dur = as.numeric(difftime(as.POSIXct(y), data$answered, units = "secs"))
  day_level=as.character(considered_days)
  data$date = format(data$date_received, "%Y-%m-%d")
  data$date=factor(data$date,levels=day_level)
  data$month = as.numeric(format(data$date_received, "%m"))
  ##dd = tapply(data$service_dur, list(data$agent_number, data$month), mean)
  
  ## here the dd is mean the service_duration as an initial matrix used in the fit
  
  dd=tapply(data$service_dur,list(data$agent_number,data$date),mean)
  ##NA represent this agent on this day have no calls!!
  
  ##for(index in 1:dim(data)[1])
  ##{
  ##data$date_index[index]=which(as.character(considered_days)%in%format(data$date_received[index], "%Y-%m-%d")  )
  ##}
  
  ## step 2 data with 12 attributes including data_index for fitting x!! (method 1: using the day as x value)
  
  aht.result = data.frame(agent = rownames(dd), a = 0, b = 0)
  aht.result = cbind(aht.result, dd)
  ## aht.result restore the value with dim[agents, a,b,dd]
  ## this is used to become the initial value(mean value): vector
  
  data$counting=1
  ##Max_Day=tapply(data$date_index,data$agent_number,max)
  
  
  for(agent_index in 1:dim(dd)[1])
  {
    data.agent=data[which(data$agent_number== rownames(dd)[agent_index]),]
    ## this data.agent is the data can be managed based on each agent_index
    Num_of_Call=tapply(data.agent$counting,data.agent$date,sum)
    Num_of_Day=length(unique(data.agent$date))
    x=as.numeric(data.agent$date)
    y=data.agent$service_dur
    x_index_day=seq(1:length(considered_days))
    ##xname=dd[agent_index,] 
    if (Num_of_Day==0) 
      ##No calls at all
    {
      aht.result$a[agent_index] = aht.result$b[agent_index] = NA
      aht.result[agent_index,4:dim(aht.result)[2]] = NA
    }else if(Num_of_Day<=10)
      ## At least work for 30 days and above considering that they have a pattern of learning!! decreasing time
    {
      aht.result$a[agent_index] = aht.result$b[agent_index] = NA
      fitted = mean(y,na.rm=T)
      aht.result[agent_index,4:dim(aht.result)[2]] = as.numeric(fitted)
      aht.result[agent_index,(3+as.numeric(which(is.na(Num_of_Call))))]=NA
      
    }else{
      mod <- nls(as.numeric(y) ~ a * exp(b * as.numeric(x)), start = list(a=mean(y,na.rm=T),b=0), control = list(maxiter = 500),trace=TRUE)
      aht.result$a[agent_index] = coef(mod)[1]
      aht.result$b[agent_index] = coef(mod)[2]
      fitted=coef(mod)[1] * exp(coef(mod)[2] *x_index_day)
      aht.result[agent_index, 4:dim(aht.result)[2]] =fitted
    }
    
    
  }
  ##pdf(file=paste("current fit","pdf",sep=""))
  
  ##dev.off()
  ##aht.result is for each agent, each day fit value
  ##aht.fit is for everyday fit for all agent
  ### Calculate number of answered calls
  ## this time we consider per day
  aht.fit = NULL
  for (day_index2 in 1:length(considered_days)) {
    selected_days2 = considered_days[day_index2]
    temp = data[which(format(data$date_received, "%Y-%m-%d") == selected_days2),]
    if (length(temp$date_received) > 0) 
      {
      temp$count = 1
      num = tapply(temp$counting, temp$agent_number, sum)
      na.row = which(is.na(num))
      if (length(na.row) > 0) {
        num = num[-na.row]
      }
      agent = names(num)
      ## today, which agent works restored in agent!!
      ## for now, only work on these agents,anyway, each day we only give a fit result in the end
      agent.aht.data = data.frame(agent = agent, num = as.numeric(num), fit.aht = 0)
      col=3+day_index2
      ##col = which(colnames(aht.result)==considered_days[day_index2])
      xx = unlist(lapply(agent.aht.data$agent, function(x) which(as.character(aht.result$agent) == as.character(x))))
      agent.aht.data$fit.aht = aht.result[xx,col]
      agent.aht.data$num[is.na(agent.aht.data$fit.aht)]=0
      agent.aht.data$fit.aht[is.na(agent.aht.data$fit.aht)]=0
      
      aht.fit = c(aht.fit, sum(agent.aht.data$num * agent.aht.data$fit.aht) / sum(agent.aht.data$num))
    } else {
      aht.fit = c(aht.fit, NA)
    }
 
  }
  return(aht.fit)
}
fitting_AHT_callnumber=function(data,considered_days)
{
  ab.row = which(is.na(data$answered))
  if (length(ab.row) > 0) {
    data = data[-ab.row,]
  }
  ## per agent using agent_id
  agent_id = unique(data$agent_number)
  temp = data.frame(answered = data$answered, transfer = data$transfer, hangup = data$hangup)
  ##temp is the only matrix we needed with relevant attributes
  y = apply(temp, 1, function(x) min(x[2], x[3], na.rm = T))
  ## using y because y here is the fitting plot's y value,here is the minimal time between transfer or hangup since either means an end
  data$service_dur = as.numeric(difftime(as.POSIXct(y), data$answered, units = "secs"))
  day_level=as.character(considered_days)
  data$date = format(data$date_received, "%Y-%m-%d")
  data$date=factor(data$date,levels=day_level)
  data$month = as.numeric(format(data$date_received, "%m"))
  ##dd = tapply(data$service_dur, list(data$agent_number, data$month), mean)
  
  ## here the dd is mean the service_duration as an initial matrix used in the fit
  
  dd=tapply(data$service_dur,list(data$agent_number,data$date),mean)
  ##NA represent this agent on this day have no calls!!
  
  ##for(index in 1:dim(data)[1])
  ##{
  ##data$date_index[index]=which(as.character(considered_days)%in%format(data$date_received[index], "%Y-%m-%d")  )
  ##}
  
  ## step 2 data with 12 attributes including data_index for fitting x!! (method 1: using the day as x value)
  
  aht.result = data.frame(agent = rownames(dd), a = 0, b = 0)
  aht.result = cbind(aht.result, dd)
  ## aht.result restore the value with dim[agents, a,b,dd]
  ## this is used to become the initial value(mean value): vector
  
  data$counting=1
  ##Max_Day=tapply(data$date_index,data$agent_number,max)
  
  
  for(agent_index in 1:dim(dd)[1])
  {
    data.agent=data[which(data$agent_number== rownames(dd)[agent_index]),]
    ## this data.agent is the data can be managed based on each agent_index
    Num_of_Call=tapply(data.agent$counting,data.agent$date,sum)
    Num_of_Day=length(unique(data.agent$date))
    x=as.numeric(data.agent$date)
    y=data.agent$service_dur
    x_index_day=seq(1:length(considered_days))
    ##xname=dd[agent_index,] 
    if (Num_of_Day==0) 
      ##No calls at all
    {
      aht.result$a[agent_index] = aht.result$b[agent_index] = NA
      aht.result[agent_index,4:dim(aht.result)[2]] = NA
    }else if(Num_of_Day<=10)
      ## At least work for 30 days and above considering that they have a pattern of learning!! decreasing time
    {
      aht.result$a[agent_index] = aht.result$b[agent_index] = NA
      fitted = mean(y,na.rm=T)
      aht.result[agent_index,4:dim(aht.result)[2]] = as.numeric(fitted)
      aht.result[agent_index,(3+as.numeric(which(is.na(Num_of_Call))))]=NA
      
    }else{
      mod <- nls(as.numeric(y) ~ a * exp(b * as.numeric(x)), start = list(a=mean(y,na.rm=T),b=0), control = list(maxiter = 500),trace=TRUE)
      aht.result$a[agent_index] = coef(mod)[1]
      aht.result$b[agent_index] = coef(mod)[2]
      fitted=coef(mod)[1] * exp(coef(mod)[2] *x_index_day)
      aht.result[agent_index, 4:dim(aht.result)[2]] =fitted
    }
    
    
  }
  ##pdf(file=paste("current fit","pdf",sep=""))
  
  ##dev.off()
  ##aht.result is for each agent, each day fit value
  ##aht.fit is for everyday fit for all agent
  ### Calculate number of answered calls
  ## this time we consider per day
  aht.fit = NULL
  for (day_index2 in 1:length(considered_days)) {
    selected_days2 = considered_days[day_index2]
    temp = data[which(format(data$date_received, "%Y-%m-%d") == selected_days2),]
    if (length(temp$date_received) > 0) 
    {
      temp$count = 1
      num = tapply(temp$counting, temp$agent_number, sum)
      na.row = which(is.na(num))
      if (length(na.row) > 0) {
        num = num[-na.row]
      }
      agent = names(num)
      ## today, which agent works restored in agent!!
      ## for now, only work on these agents,anyway, each day we only give a fit result in the end
      agent.aht.data = data.frame(agent = agent, num = as.numeric(num), fit.aht = 0)
      col=3+day_index2
      ##col = which(colnames(aht.result)==considered_days[day_index2])
      xx = unlist(lapply(agent.aht.data$agent, function(x) which(as.character(aht.result$agent) == as.character(x))))
      agent.aht.data$fit.aht = aht.result[xx,col]
      agent.aht.data$num[is.na(agent.aht.data$fit.aht)]=0
      agent.aht.data$fit.aht[is.na(agent.aht.data$fit.aht)]=0
      
      aht.fit = c(aht.fit, sum(agent.aht.data$num * agent.aht.data$fit.aht) / sum(agent.aht.data$num))
    } else {
      aht.fit = c(aht.fit, NA)
    }
    
  }
  return(aht.fit)
}
calculate_aht_per_day=function(temp.r,considered_days)
{
  real_aht_per_day=NULL
  ab.row = which(is.na(temp.r$answered))
  if (length(ab.row) > 0) {
    temp.r = temp.r[-ab.row,]
  }
  temp = data.frame(answered = temp.r$answered, transfer = temp.r$transfer, hangup = temp.r$hangup)
  y = apply(temp, 1, function(x) min(x[2], x[3], na.rm = T))
  temp.r$service_dur = as.numeric(difftime(as.POSIXct(y), temp.r$answered, units = "secs"))
  for (day_index3 in 1:length(considered_days))
  {
    selected_days3 = considered_days[day_index3]
    temp2 = temp.r[which(format(temp.r$date_received, "%Y-%m-%d") == selected_days3),]
    real_aht_per_day=c(real_aht_per_day,mean(temp2$service_dur))
  }
  return (real_aht_per_day)
}
arrival_analysis_intra_year=function(skill)
{
  aa=NULL
  intra_year=NULL
  for(monthindex in 1:length(selected_months))
  {aa=call.data[which(months(call.data$date_received)%in%selected_months[monthindex]&call.data$queue_name%in%skill),]
  intra_year=c(intra_year,dim(aa)[1])
  }
  ##intra_year is the number of calls arrived in 12 months seperately
return(intra_year)
}