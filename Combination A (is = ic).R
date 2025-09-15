data = read.csv("D:/UserFiles/Desktop/Latency Tester 6gou/relative mouse position.csv")

xx = c(1,2,4,8)	#output interval multiplier

for(j in 1:4){

xsamples = 100 			#number of samples in 1 count
dpi = 1600				#mouse sensitivity
pixelgap = 25400 / dpi		#distance of 1 count [μm] (c)
xgap = pixelgap / xsamples 	#sampling gap of x [μm]
interval = 125 * xx[j]		#sensor read and mouse output interval [μs] (i_s, i_c), max 1000
time = -interval+1 		#start time of scan

entries = xsamples * interval	#total number of samples
delaylist = numeric(entries)	#storage of s_subIn
index = 1				#index in the storage for substitution

for(x in xsamples:1){ 		#initial condition of x_lastCount (range: x=pixelgap~pixelgap/100)

	#process sensor output
	for(i in time:0){		#initial condition of t_sLastRead (range: t=-interval+1~0)

		datapos = i + 4001						#data[datapos,1] is now equal to i
		scanstick = data[datapos,2]					#slider's position at next sensor read
		currentpixels = ceiling((x * xgap - scanstick) / pixelgap)	#sum of sensor counts at scan's start point
		minpixels = currentpixels					#record the minimum to check reversal
		while(currentpixels <= minpixels){				#scan to check the timing of reversal
			datapos = datapos + interval				#next sensor read
			scanstick = data[datapos,2]				#slider's position at next sensor read
			ce = ceiling((x * xgap - scanstick) / pixelgap)	#sum of sensor count
			fl = ce - 1							#sum of sensor count with deadzone
			if(ce < currentpixels){					#if a smaller sum were found
				currentpixels = ce				#substitute
				minpixels = ce					#update minimum
			}
			else if(fl > currentpixels){				#if a larger sum were found
				currentpixels = fl				#substitute, this will end the while statement
			}
		}

		#datapos will be the timing of reversal in sum of sensor counts

		delaylist[index] = datapos - 4001			#substitute l_subin
		index = index + 1						#update index
	}
}

mindelaytime = min(delaylist)
maxdelaytime = max(delaylist)
#as the range of t_stamp1 - t_sLastRead in the simulation is 0~interval-1, 
#and the average of it is interval/2 - 0.5, we should add 0.5 to the simulation result
avgdelaytime = mean(delaylist) + 0.5
sd = sqrt(var(delaylist))

cat("sensor read interval: ", interval, "\n")
cat("l_subIn(avg): ", avgdelaytime, "\n")
cat("min: ", mindelaytime, "\n")
cat("max: ", maxdelaytime, "\n")
cat("sd: ", sd, "\n")
correction = -(avgdelaytime - interval/2)		
cat("-l_subIn+l_add: ", correction, "\n")
cat("-l_subIn+l_add(rounded): ", floor(correction + 0.5), "\n")
cat("\n")

}