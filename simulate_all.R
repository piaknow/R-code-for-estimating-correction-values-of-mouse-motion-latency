data = read.csv("D:/UserFiles/Desktop/Latency Tester 6gou/graph5.csv")
#print(data[,])

#高さxは100から1まで
#インターバルは(-インターバル+1)から0まで

xx = c(1,2,4,8)

for(j in 1:4){

xsamples = 100 			#1カウント内のサンプリング数
dpi = 1600
pixelgap = 25400 / dpi		#センサーのカウント（ピクセル）の間隔 [um]
xgap = pixelgap / xsamples 	#xのサンプリング間隔 [um]
interval = 125 * xx[j]			#ポーリング間隔 [us]
time = -interval+1 		#横はここからスキャンをスタート
samples = 0 			#mindelay maxdelayを最初に一度だけ代入するためのトリガー

delaylist = c()			#結果を格納するベクトル型変数

#このプログラムが出すのは「すべての位置とタイミングにおけるマウスの応答時間の理論値の平均」であるので、補正値として使う場合は結果の値から(ポーリング間隔/2)を引くべき
#「センサーの位置」はマウスのセンサーが読み取っている表面の位置を指す。
#「センサーのカウント」はセンサーが実際に読み取っている位置情報を指し、センサーのカウント間隔の整数倍の値をとり、且つ更新頻度がポーリングレートに等しい。
#なお、センサーのフレームレートもポーリングレートに等しいものとする。
for(x in xsamples:1){ 					#1ピクセルの位置を網羅する 100~1 ここを上にオフセットしても結果変わらんはず グラフのX軸（時間）タッチしない

	#マウスの位置を処理する
	for(i in time:0){		#ポーリング間隔内で時間を網羅する -interval+1~0 ここを左にオフセットしても結果変わらんはず グラフのY軸（位置X）タッチする

		#回ってきた位置xに対して、比較対象となる距離を初期化
				#回ってきた位置xに対して、比較対象となる距離を初期化
		datapos = i + 2001				#開始時間 -interval+1から0まで i = -interval+1~0

		currentpixels = ceiling((x * xgap - data[datapos,2]) / pixelgap)	#センサーのカウントのスタート地点
		minpixels = currentpixels
		while(currentpixels <= minpixels){
			datapos = datapos + interval
			scanstick = data[datapos,2]						#スキャンするセンサーの位置を取得
			ce = ceiling((x * xgap - scanstick) / pixelgap)
			fl = ce - 1
			if(ce < currentpixels){				#今よりも小さいピクセル数が見つかった場合
				currentpixels = ce			#それを代入する
				minpixels = ce				#ピクセル数の最小値を更新
			}
			else if(fl > currentpixels){			#今よりも大きいピクセル数が見つかった場合
				currentpixels = fl			#それを代入する
			}
		}

		#↑スキャンした値がminpixelsより大きくなった時間、すなわちセンサーのカウントが切り返されたタイミングがdataposに残される

		delaytime = datapos - 2001	#マウスの遅延時間
		delaylist = c(delaylist, delaytime)
	}
}

mindelaytime = min(delaylist)
maxdelaytime = max(delaylist)
avgdelaytime = mean(delaylist)
sd = sqrt(var(delaylist))

print(avgdelaytime)
print(mindelaytime)
print(maxdelaytime)
print(sd)
correction = -(avgdelaytime - interval/2)
print(correction)
print(floor(correction + 0.5))
cat("\n")

}