data = read.csv("D:/UserFiles/Desktop/Latency Tester 6gou/graph5.csv")

xx = c(1,2,4,8)	#output interval multiplier

for(t in 1:4){

xsamples = 100 			#number of samples in 1 count
dpi = 1600				#mouse sensitivity
pixelgap = 25400 / dpi		#distance of 1 count [μm] (c)
xgap = pixelgap / xsamples 	#sampling gap of x [μm]
interval = 91     		#sensor read interval [μs] (i_s), max 1000 
pinterval = 125 * xx[t]		#mouse output interval [μs] (i_c), max 1000
time = -pinterval+1 		#start time of scan

entries = xsamples * interval * pinterval	#total number of samples
delaylist = numeric(entries)			#storage of s_subIn
index = 1						#index in the storage for substitution

datalength = nrow(data)
sensordata = matrix(nrow = datalength, ncol = 2)	#storage of sum of sensor counts' transition
sensordata[, 1] = (1-2001):(nrow(data)-2001)		#substitute


for(x in xsamples:1){ 			#initial condition of x_lastCount (range: x=pixelgap~pixelgap/100)

	#process sensor output
	for(i in 1:(interval)){		#initial condition of t_sLastRead (range: t=-2000~-2000+interval-1)

		datapos = i 							#start from the beginning 
		scanstick = data[datapos,2]					#slider's position
		currentpixels = ceiling((x * xgap - scanstick) / pixelgap)	#sum of sensor counts at scan's start point
		while(datapos <= datalength){					#scan till the end
			scanstick = data[datapos,2]				#slider's position
			ce = ceiling((x * xgap - scanstick) / pixelgap)	#sum of sensor count
			fl = ce - 1							#sum of sensor count with deadzone
			if(ce < currentpixels){					#if a smaller sum were found
				currentpixels = ce				#substitute
			}
			else if(fl > currentpixels){				#if a larger sum were found
				currentpixels = fl				#substitute
			}
			rangemin = datapos					#the range of time to substitute currentpixels
			rangemax = min(datapos + interval - 1, datalength)
			sensordata[rangemin:rangemax,2] = currentpixels	#substitute
			datapos = datapos + interval				#next sensor read
		}

		#sensordata is ready and now we use it to calculate mouse output
		#process mouse output
		for(j in time:0){		#initial condition of t_cLastRead (range: t=-pinterval+1~0)

			datapos = j + 2001					#data[datapos,1] is now equal to j
			nowmouse = sensordata[datapos,2] 			#sum of counts output by mouse
			minmouse = nowmouse					#record the minimum to check reversal
			while(nowmouse <= minmouse){				#scan to check the timing of reversal
				minmouse = nowmouse				#update minimum
				datapos = datapos + pinterval			#next mouse output
				nowmouse = sensordata[datapos,2]		#substitute next mouse output
			}

			#datapos will be the timing of reversal in sum of sensor counts

			delaylist[index] = datapos - 2001			#substitute l_subin
			index = index + 1						#update index
		}

	}

}

mindelaytime = min(delaylist)
maxdelaytime = max(delaylist)
#as the range of t_stamp1 - t_sLastRead and t_stamp1 - t_cLastReadin the simulation is respectively 
#0~interval-1 and 0~pinterval-1, and the average of them is respectively interval/2 - 0.5 and pinterval/2 - 0.5,
#we should add 0.5 each to the simulation result
avgdelaytime = mean(delaylist) + 0.5 + 0.5
sd = sqrt(var(delaylist))

cat("sensor read interval: ", interval, "\n")
cat("mouse output interval: ", pinterval, "\n")
cat("l_subIn(avg): ", avgdelaytime, "\n")
cat("min: ", mindelaytime, "\n")
cat("max: ", maxdelaytime, "\n")
cat("sd: ", sd, "\n")
correction = -(avgdelaytime - interval/2 - pinterval/2)
cat("-l_subIn+l_add: ", correction, "\n")
cat("-l_subIn+l_add(rounded): ", floor(correction + 0.5), "\n")
cat("\n")


}