# 简介
数据来自以色列一个银行呼叫中心公布的来电记录。地址见：[http://ie.technion.ac.il/serveng/callcenterdata/index.html](http://ie.technion.ac.il/serveng/callcenterdata/index.html)  
我们基于这个案例研究如何分析排队系统问题，从而指导业务运作，最优化运营中人员的排班调度。  

# 分析过程
## 0. 数据清理
数据查看的过程中，需要根据情况反复做多次处理，过程较为繁琐。具体清理过程见代码。    
清理后得到两部分数据: 
call.data.tidy和bad.data    
bad.data是清理数据时被去掉的数据，包含了异常情况和错误值。我们可以对异常业务情况做分析，从中发现对业务有用的信息。  
这里仅对排班问题进行研究，因此异常数据的分析略过。  
## 1. 查看业务量
### 1.1 查看每天的呼叫量      
![日呼叫量](https://github.com/oDoraemon/schedule_op/blob/master/img/01_call_day_arrival.png?raw=true)        
由于周五/周六是以色列的周末，银行不上班，因此呼叫量很少。   
从平滑曲线可以看出，日均呼叫量约在1200左右，曲线比较稳定。 

### 1.2 查看呼叫量的月度分布  
由于存在大小月，单独看月累计总量可能影响分布。 因此同时查看每月日均呼叫量。  
![月呼叫量分布](https://github.com/oDoraemon/schedule_op/blob/master/img/02_call_month_arrival.png?raw=true)  
从图中可以看到，不同的月份业务量有变动。大概可以分为1/4/8/9月份, 2/3/5/6/7月份, 8/11/12月份三个档次，分别对应业务淡季，业务正常，业务高峰的情况。  
我们取业务正常情况下的5/6/7月份数据作为代表，进行研究。

### 1.3 查看呼叫量在一天内的分布
![业务量均值-小时分布(5-7月份)](https://github.com/oDoraemon/schedule_op/blob/master/img/03_call_hour_arrival.png?raw=true)  

从图中可以看到，正常上班时间(9:00-18:00)属于忙时。 同时在我们印象中，客户可能会选择在中午休息时间打电话来进行咨询的预设并不成立。 
检查一下不同工作日是否会影响业务量的分布。
![工作日业务量均值-小时分布(5-7月份)](https://github.com/oDoraemon/schedule_op/blob/master/img/04_dow_hour_arrival.png?raw=true)    
工作日的下午高峰时间在分布上有差异, 但差异并不大。 取均值可以有效代表各工作日。

### 1.4 计算呼叫中心每小时服务量
	# 客服服务一个用户平均需要的时间mean.ser_time + 1分钟缓冲时间
	call.data.sample$ser_hour <- factor(
	                                str_split(call.data.sample$ser_start, ':', simplify = TRUE)[,1],
	                                levels=c(0:23))
	service.data.sample <- call.data.sample %>% filter(server != 'NO_SERVER')
	mean.ser_time <- mean(ser.data.sample$ser_time)
	SERVICE.RATE <- floor(3600/(mean.ser_time + 60)) # 1小时能能服务的服务次数  
   
根据以上逻辑，得到SERVICE.RATE=14, 即1个客服1小时约能处理14个电话。  

## 2. 查看当前的排队现状
根据指标，我们将服务水平分为3档，0s-30s内排队-正常,  30s-90s内排队-可接受, 90s+排队-不可接受。  

	# 0,1为正常，2为可接受，3为不可接受
	call.data.sample$q_type <- sapply(call.data.sample$q_time, FUN = function(x) {
	                                      if (x == 0) return(0)
	                                      else if (x <= 30)  return(1)
	                                      else if (x <= 90)  return(2)
	                                      else return(3)
	                            })  
	
	call.day.queue <- call.data.sample %>% group_by(date, q_type) %>% summarise(cnt=n())
	call.day.queue.mean <- call.day.queue %>% group_by(q_type) %>% summarise(avg=mean(cnt))
	call.day.queue.mean$percent <- call.day.queue.mean$avg/sum(call.day.queue.mean$avg)  

	> call.day.queue.mean
	# A tibble: 4 x 3
	  q_type   avg percent
	   <dbl> <dbl>   <dbl>
	1      0  522.   0.328
	2      1  314.   0.197
	3      2  348.   0.218
	4      3  409.   0.257

从结果可以看到，不可接受的排队情况占比高达25.7%，比例过高，至少需要先优化到10%以内。  

## 3. 针对性排班
排队系统只能针对是否需要排队给出概率。因此我们先将不需要排队的概率提高到0.4，调整后会如何影响情况(3-不可接受) 还不确定，在优化方案上线后，收集数据再做调整。  
  
	GOAL.PROB <- 0.4
	# C erlang算法
	servers.needed <- numeric(24) # 24小时需要的值班人数
	for (i in 1:24) {
	  INIT.PROB <- 1
	  while(INIT.PROB > GOAL.PROB) {
	    servers.needed[i] <- servers.needed[i] + 1
	    INIT.PROB <- C_erlang(c = servers.needed[i],
	                          r = call.hour.arr$avg_arr[i]/SERVICE.RATE)
	  }
	}

	> servers.needed  # 各个小时需要值班的人数，最高峰需要13人值班
	[1]  1  1  1  1  1  1  2  5  9 11 13 11 10 11 12 12 11 10  8  7  6  7  6  5

假如员工正常工作时长为8小时，结合算法结果，制定我们的排班计划表:

![排班计划表](https://github.com/oDoraemon/schedule_op/blob/master/img/05_schedule_solution.png?raw=true)  
5个班次加起来总共需要18个客服人员，可以基本满足业务需求。  
因为就呼叫中心的情况来说，客服不一定需要on site上班，因此可以存在1个班次的人员提前下班并支援晚班高峰。  
上午10点高峰期压力会比较大，视具体情况再决定是否安排人员。 

理想服务能力VS平均每小时业务量：
![理想服务能力VS平均每小时业务量](https://github.com/oDoraemon/schedule_op/blob/master/img/06_service_capability.png?raw=true)  
从图中可以看出，在理想情况下(用户依次顺序接入时)，排班方案的服务能力是足够的。  
但因为存在高并发的场景，所以会存在用户需要排队等待的情况。
用户排队的概率是否过高，排队时长是否可接受，则需要进一步的统计数据支撑，根据结果再进行排班的优化。

# 最后
实际场景中，我们很大可能没有完整的历史数据可以参考。  
因此实际应用中，如果我们开始要针对排班最优化，通常是先记录一段时间的来电/排队数据，根据短期的数据给出排班计划。之后再根据主观经验中的忙闲季节，决定是否需要增加人员。，并根据实际情况不断做进一步的调整与优化人员配置。
