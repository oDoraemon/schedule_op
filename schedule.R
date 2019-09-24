library(lubridate)
library(ggplot2)
library(queueing)
library(lpSolve)
library(stringr)
save_plot <- function(g, file, type='png', width=920, height=500) {
  png(file=file, h = height, width = width)
  plot(g)
  dev.off()
}

options(digits=4)
all.data <- data.frame()
for (i in 1:12) {
  filename <- paste('call_center_', str_pad(i, 2, pad='0'), '.txt', sep='')
  input.data <- read.table(filename,
                          header=TRUE,
                          colClasses = c('character', 'character', 'character',
                                         'character', 'character', 'character',
                                         'character','character', 'integer',
                                         'character', 'character', 'integer',
                                         'character', 'character','character',
                                         'integer','character'))
  all.data <- rbind(all.data, input.data)
}
# print(summary(input.data))

# vru.line: 6个VRU，从01-06，每个VRU有1-16条线路
# call_id
# customer_id, 0代表系统无法识别的用户
# priority: 0,1表示未识别用户或常规用户，2表示优先用户
# type: 电话业务类型。使用阿拉伯文简称
#   - PS: 常规业务
#   - PE: 英语常规业务
#   - IN: 网络询问
#   - NE: 股票交易活动
#   - NW: 潜在客户咨询信息
#   - TT: 用户留言要求回拨，但是回拨的时候线路忙导致用户等待
# vru_entry: 用户进入VRU的时间
# vru_exit: 离开VRU的时间，离开的原因包括：获得服务，进队列等待，挂断。
# vru_time: 在vru中花费的时间
# q_start: 进入等待队列的时间。没进入等待则时间为00:00:00
# q_exit: 队列用户得到服务或挂断
# q_time: 排队所花的时间
# Outcome: AGENT-得到服务, HANG-被挂断, PHANTOM-不明电话
# ser_start: 服务开始的时间
# ser_exit: 结束服务的时间
# Server: 服务电话的名称。如果未提供服务，则为NO_SERVER。

# 0. 数据清洗
# -0.1. vru_time >= 0
# -0.2. q_time >= 0
# -0.3. outcome去掉PHANTOM
# -0.4. ser_time >= 0
# -0.5. 日期date列规范化
# -0.6. 添加dow(day of week)列
# -0.7. 添加entry_hour, 统计每小时服务访问量
# -0.8. 添加ser_hour, 统计每小时服务量
# -0.9. 添加month
all.nrow <- nrow(all.data)
call.data <- all.data %>%
              filter(vru_time > 0, q_time >= 0, ser_time >= 0, outcome != 'PHANTOM')
call.data$date <- ymd(paste('19', call.data$date, sep=''))
call.data$dow <- factor(weekdays(call.data$date),
                        levels=c('星期一',	'星期二',	'星期三',	'星期四',	'星期五',	'星期六',	'星期日'))
call.data$entry_hour <- str_split(call.data$vru_entry, ':', simplify = TRUE)[,1]
call.data$entry_hour <- factor(call.data$entry_hour, levels=as.character(seq(0, 23, 1)))
call.data$ser_hour <- str_split(call.data$ser_start, ':', simplify = TRUE)[,1]
call.data$ser_hour <- factor(call.data$ser_hour, levels=as.character(seq(0, 23, 1)))
call.data$month <- month(call.data$date)
call.data$month <- factor(call.data$month, levels=c(1:12))
# 查看数据被整理后的占比
clean.nrow <- nrow(call.data)
clean.ratio <- round(clean.nrow/all.nrow, 4)
print(paste('clean.ratio:', clean.ratio))

call.data$priority <- factor(call.data$priority, levels=c(0,1,2))
call.data$type <- factor(call.data$type)
call.data$outcome <- factor(call.data$outcome)
call.data$server <- factor(call.data$server)

# 处理异常的 vru时间/queue时间/served时间 记录。
# 这种异常数据需要特别处理
# 约16分钟vru时间
# 约16分钟queue时间
# 1个小时的服务时间视为异常

call.data.tidy <- call.data %>% filter(vru_time < 1000 & q_time < 1000 & ser_time <= 3600)
bad.data <- call.data %>% filter(vru_time >= 1000 | q_time >= 1000 | ser_time > 3600)
print(paste('good.data.ratio:', round(nrow(call.data.tidy)/clean.nrow, 4)))
# 我们的目的是为了排班
# 排班需要的几个数据：1. 服务到达率 2. 提供服务率

# 查看每天的服务呼叫数量
call.day.arrival <- call.data.tidy %>% group_by(date) %>% summarise(cnt=n())
g1 <- ggplot(data=call.day.arrival, aes(x=date, y=cnt)) + 
  labs(title="日呼叫量曲线", x="", y="呼叫量") +
  geom_line(size=1, color='darkgreen') + 
  geom_smooth() + 
  geom_text(aes(x=call.day.arrival$date[10], y=2900), hjust=0,
            label=paste('Max:', round(max(call.day.arrival$cnt)))) +
  geom_text(aes(x=call.day.arrival$date[10], y=2820), hjust=0,
            label=paste('Mean:', round(mean(call.day.arrival$cnt),2))) +
  geom_text(aes(x=call.day.arrival$date[10], y=2740), hjust=0,
            label=paste('Median:', round(median(call.day.arrival$cnt))))
g1
save_plot(g1, "01_call_day_arrival.png")

call.month.arrival <- call.data.tidy %>% group_by(month) %>% summarise(cnt=n())
g2 <- ggplot(data=call.month.arrival, aes(x=month, y=cnt)) + 
  geom_col(fill='darkgreen', alpha=0.7) + 
  labs(title="月累计呼叫量", x="", y="累计呼叫量")

# 因为月份有大小月，可能影响分布，查看均值
call.month.arrival.mean <- call.data %>% group_by(date, month) %>% summarise(cnt=n()) %>% 
                            group_by(month) %>% summarise(avg=mean(cnt))
g3 <- ggplot(data=call.month.arrival.mean, aes(x=month, y=avg)) + 
  geom_col(fill='darkgreen', alpha=0.7) + 
  labs(title="日均呼叫量", x="", y="日均呼叫量")
g23 <- grid.arrange(g2, g3, ncol=2)
save_plot(g23, "02_call_month_arrival.png")

# 选择5-7月的数据进行排班建模
# 周五周六作为周末另外安排
call.data.sample <- call.data.tidy %>% filter(month %in% c(5, 6, 7),
                                         !dow %in% c('星期五', '星期六'))

call.hour.arrival <- call.data.sample %>% group_by(date, entry_hour) %>% summarise(cnt=n()) %>% 
                        group_by(entry_hour) %>% summarise(mean_arrival=round(mean(cnt),2))

g4 <- ggplot(data=call.hour.arrival, aes(x=entry_hour, y=mean_arrival)) + 
  geom_col(fill='darkgreen', alpha=0.7) + 
  labs(title='业务量均值-小时分布(5-7月份)', x="", y='业务量均值')
g4
save_plot(g4, "03_call_hour_arrival.png")

# 上班时间基本都是忙时, 10点是业务高峰期
# 12点/1点并没有预想中的，客户会利用休息时间做咨询的情况
# 从之前的图来看, 数据有周周期性，查看工作日差异性
dow.hour.arrival <- call.data.sample %>% group_by(date, dow, entry_hour) %>% summarise(cnt=n()) %>% 
                  group_by(dow, entry_hour) %>% summarise(mean_arrival=round(mean(cnt), 2))
g5 <- ggplot(data=dow.hour.arrival, aes(x=entry_hour, y=mean_arrival)) + 
  geom_col(fill='darkgreen', alpha=0.7) + 
  labs(title='业务量均值-小时分布(5-7月份)', x="", y='业务量均值') + 
  facet_grid(dow~.)
  g5
save_plot(g5, "04_dow_hour_arrival.png", height=900)
# 每周的下午高峰时间有一点差异,但差异不算很大。取个小时均值作为代表进行研究。


# 计算每小时服务
call.data.sample$ser_hour <- factor(
                                str_split(call.data.sample$ser_start, ':', simplify = TRUE)[,1],
                                levels=c(0:23))
service.data.sample <- call.data.sample %>% filter(server != 'NO_SERVER')
mean.ser_time <- mean(ser.data.sample$ser_time)
SERVICE.RATE <- floor(3600/(mean.ser_time + 60))
# mean.ser_time: 192.2s， 约3分钟
# 设挂完电话后需要1分钟的转接时间，平均服务时间约4分钟
# 1个小时约处理能处理约14个电话

# 查看当前的排队的情况
# 将排队划分为3档，0排/30排-优/90排-可接受/90+排-不可接受
call.data.sample$q_type <- sapply(call.data.sample$q_time, FUN = function(x) {
                                      if (x == 0) return(0)
                                      else if (x <= 30)  return(1)
                                      else if (x <= 90)  return(2)
                                      else return(3)
                            })

call.day.queue <- call.data.sample %>% group_by(date, q_type) %>% summarise(cnt=n())
call.day.queue.mean <- call.day.queue %>% group_by(q_type) %>% summarise(avg=mean(cnt))
call.day.queue.mean$percent <- call.day.queue.mean$avg/sum(call.day.queue.mean$avg)

# 不可接受的排队比例有25.7%，至少需要降低10%。
# 先将不排队的概率提高到0.4, 看实际优化排班后的效果再决定是否继续增加
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
servers.needed

capability <- data.frame(x=factor(c(0:23), levels=c(0:23)), y=servers.needed*SERVICE.RATE)
gx <- g4 + geom_col(data=capability,
             aes(x=x, y=y),
             fill='darkgreen',
             alpha=0.5) + 
 labs(title="理想服务能力VS每小时平均业务量") + 
 geom_tile(aes(x=2, y=180, height=8, width=0.8), fill="darkgreen") + 
 geom_tile(aes(x=2, y=170, height=8, width=0.8), fill="#00640006") + 
 geom_text(aes(x=3.5, y=181), label="小时平均业务量") +
 geom_text(aes(x=3.5, y=171), label="理想服务能力")
save_plot(gx, "06_service_capability.png")

# 从结果可以看到, 理想情况下(用户依次顺序接入)时，服务能力是足够的。
# 考虑到高并发的场景，因此会存在用户等待的情况。
# 至于用户排队的情况是否普遍，排队时长是否可接受，则需要进一步的统计支撑，然后再优化
