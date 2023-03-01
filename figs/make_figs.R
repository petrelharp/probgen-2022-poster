

##########
# diagram of continuous-space model

draw_circle <- function (xy, r, radius_label, ...) {
    xy <- unlist(xy)
    theta <- seq(0, 2*pi, length.out=101)
    polygon(xy[1] + r * cos(theta),
            xy[2] + r * sin(theta), ...)
    if (!missing(radius_label)) {
        segments(x0=xy[1], y0=xy[2], x1=xy[1] + r)
        text(xy[1] + r/2, xy[2], labels=radius_label, pos=3, cex=2)
    }
}

N <- 100
epsilon <- 0.3
sigma <- 0.4
H <- 1
W <- 5/3

set.seed(30)
xy <- data.frame(
            x=W * (runif(N) - 0.5),
            y=H * (runif(N) - 0.5)
        )

xy <- xy[order(xy$x^2 + xy$y^2),]

pdf(file="scales_diagram.pdf", width=5, height=3)
par(mar=c(1,1,1,1))
k <- 61
    plot(xy$x, xy$y, type='n',
         xlim=c(-1, 1) * W/2, ylim=c(-1, 1) * H/2,
         asp=1, xaxt='n', xlab='', yaxt='n', ylab='')
    draw_circle(xy[k,], epsilon, col=adjustcolor("red", 0.5), lwd=2, radius_label=expression(epsilon))
    points(xy[k,1], xy[k,2], cex=2, pch=20)
    in_circle <- setdiff(which((xy$x - xy[k,1])^2 + (xy$y - xy[k,2])^2 < epsilon^2), k)
    arrows(x0=xy$x[in_circle], y0=xy$y[in_circle],
           x1=0.8 * xy[k,1] + 0.2 * xy$x[in_circle],
           y1=0.8 * xy[k,2] + 0.2 * xy$y[in_circle],
           col='red', lwd=2)
    points(xy$x, xy$y, pch=20)
k <- 15
    n <- 12
    draw_circle(xy[k,], sigma, border="blue", col=adjustcolor("blue", 0.25), lwd=2, radius_label=expression(sigma))
    points(xy[k,1], xy[k,2], cex=2, pch=20)
    arrows(
        x0=xy[k,1], y0=xy[k,2],
        x1=xy[k,1] + rnorm(12, sd=sigma),
        y1=xy[k,2] + rnorm(12, sd=sigma),
        col='blue', lwd=2
    )
dev.off()


##########
# stationary distribution for lineages in the PME

xvals <- seq(-10, 0, length.out=1001)

# wavefront
w <- function (x) {
    y <- pmax(0, 1 - exp(x / 2))
    return(y / sum(y))
}

# stationary distribution for lineages
f <- function (x) {
    y <- exp(x) * (1 - exp(x/2))
    return(y / sum(y))
}

pdf(file='pme_dists.pdf', width=5, height=3)
par(mar=c(5,3,1,1)+.1)
    plot(xvals, f(xvals), type='l',
         ylab='', xlab='position', lwd=2)
    lines(xvals, w(xvals), col='red', lwd=2)
    legend("topleft", lty=1, lwd=2, col=c("black", "red"),
           legend=c("lineage", "population"))
dev.off()
