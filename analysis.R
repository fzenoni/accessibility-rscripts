library(data.table)

data <- fread('../osrm-application/output.csv')

# Compute average duration for 5 closer destinations
avg <- data[, .(avg = mean(head(sort(V2), 5))), by = V1]
# avg <- avg5[, .(avg = mean(closer5)), by = V1]
setnames(avg, 'V1', 'id')
avg[, avg := avg/60]