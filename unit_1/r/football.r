require("plotrix")
draw_pitch = function(players) {
	grn = "#39A83B"
	xl = c(-50, 50)
	yl = c(0, 66)
	par(mar = c(2,0,0,0))
	plot(0,0, xlim=xl+c(-5, 5), ylim = yl+c(-5, 5), type='n', axes = FALSE, ylab = '', xlab = '', bty='n', asp=1)
	axis(1, seq(xl[1], xl[2], 25))
	rect(xl[1]-5, yl[1]-5, xl[2]+5, yl[2]+5, col = grn, border = 'white')
	rect(xl[1], yl[1], xl[2], yl[2], border = 'white')
	
	draw.arc(rep(xl, each=2), rep(yl, 2), 0.5, deg1=c(0, 270, 90, 180), deg2 = c(90, 360, 180, 270), col = 'white')
	
	
	draw.circle(xl[1]+11, median(yl), 9.15, border = 'white')
	draw.circle(xl[2]-11, median(yl), 9.15, border = 'white')
	draw.circle(median(xl), median(yl), 9.15, border = 'white')
	lines(c(0,0), yl, col='white')
	# penalty boxes
	pby = median(yl) + c(-1, 1)*(40.3/2)
	rect(xl[1], pby[1], xl[1]+16.5, pby[2], col = grn, border = 'white')
	rect(xl[2]-16.5, pby[1], xl[2], pby[2], col = grn, border = 'white')
	# goal boxes
	gby = median(yl) + c(-10, 10)
	rect(xl[1], gby[1], xl[1]+5.5, gby[2], col = grn, border = 'white')
	rect(xl[2]-5.5, gby[1], xl[2], gby[2], col = grn, border = 'white')
	
	if(!missing(players))
		draw_players(players)
}

draw_players = function(locs, ymin = 1, ymax = 65, ...) {
	py = seq(ymin, ymax, length.out = length(locs))
	points(locs, py, pch = 16, col="black", cex=0.4, ...)
}

