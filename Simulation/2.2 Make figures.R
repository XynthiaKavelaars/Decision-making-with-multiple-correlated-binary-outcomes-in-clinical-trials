#### 1.  Figure 1. Univariate vs. multivariate treatment effect ####
#### 1.1 Functions ####
rotate <- function(x,y,theta, diff.x, diff.y){
  rotated.x <- diff.x+(x-diff.x)*cos(theta*pi/180)+(diff.y-y)*sin(theta*pi/180)
  rotated.y <- diff.y+(x-diff.x)*sin(theta*pi/180)+(y-diff.y)*cos(theta*pi/180)
  return(matrix(c(rotated.x,rotated.y), ncol=2))}

#### 1.2 Settings and parameters ####
par(mar=c(4,4,0,1), mfrow=c(1,1),mgp=c(2.5,1,0), family="Times")
set.seed(2500)

phiT <- c(0.15,0.425,0.425,0.0)
phiC <- c(0.0,0.425,0.425,0.15)

#priorT <- priorC <- c(0.5,0.5,0.5,0.5)
priorT <- priorC <- c(1,1,1,1)

nT <- nC <- 30

post.alphaT <- phiT*nT+priorT
post.alphaC <- phiC*nC+priorC

postT              <- SampleDir(1e7,post.alphaT)
postC              <- SampleDir(1e7,post.alphaC)

#### 1.3 Plot figure ####
setEPS()
postscript("Plots/Figure1.eps", family="Times")

layout(matrix(c(1,2,0,3), nrow=2, ncol=2, byrow=TRUE), heights= c(6,0.75), widths=c(0.75,6))

#### 1.3.1 Marginals endpoint 2 ####
par(mar=c(0.01,0.01,0.01,0.01))

# Rotate densities
rotation.matT2 <- rotate(density(rowSums(postT[,c(1,3)]))$x, 
                         density(rowSums(postT[,c(1,3)]))$y*0.02,90,0,0)
rotation.matC2 <- rotate(density(rowSums(postC[,c(1,3)]))$x, 
                         density(rowSums(postC[,c(1,3)]))$y*0.02,90,0,0)

# Plot densities
plot(NULL, xlim=c(min(min(rotation.matT2[,1]),min(rotation.matC2[,1])), 
                  max(max(rotation.matT2[,1]),max(rotation.matC2[,1]))),
     ylim=c(0,1),xaxs="i", yaxs="i", xaxt="n", yaxt="n", frame.plot=FALSE)

lines(rotation.matT2[,1], (rotation.matT2[,2]), lwd=2)
lines(rotation.matC2[,1], (rotation.matC2[,2]), lwd=2)
#text(x=0.4*min(min(rotation.matT2[,1]),min(rotation.matC2[,1])),
#     y=0.9, labels=expression(theta[2]), srt=90, cex=3.00,
#     family="Times")

#### 1.3.2 Plot bivariate distributions & weighted sum ####
# Plot contour treatment T
contour(table(cut(rowSums(postT[,c(1,2)]),breaks=seq(0,1,0.01)),
              cut(rowSums(postT[,c(1,3)]),breaks=seq(0,1,0.01))), 
        frame.plot=FALSE, nlevels=5,
        xlab=" ",
        xlim=c(0,1),
        ylab=" ",
        ylim=c(0,1),
        drawlabels=FALSE, las=1, lwd=2, xaxs="i", yaxs="i", xaxt="n", yaxt="n", 
        xpd=TRUE)
axis(side=1, labels=FALSE, tck=FALSE, lwd=1.5)
axis(side=2, labels=FALSE, tck=FALSE, lwd=1.5)
text(x=-0.05, y=0.9, labels="Fatigue", xpd=NA, srt=90, family="Times", cex=3)#expression(theta[2]), xpd=NA, family="Times")
text(x=0.87, y=-0.07, labels="Cognition", xpd=NA, family="Times", cex=3)#expression(theta[1]), xpd=NA, family="Times")
text(x=0.675, y=0.725, labels="SRS", family="Times", cex=3)

# Plot contour treatment C
contour(table(cut(rowSums(postC[,c(1,2)]),breaks=seq(0,1,0.01)),
              cut(rowSums(postC[,c(1,3)]),breaks=seq(0,1,0.01))), 
        add=TRUE, drawlabels=FALSE, lwd=2, nlevels=5)
text(x=0.325, y=0.275, labels="WBRT", family="Times", cex=3)
segments(x0=0, y0=1, x1=1, y1=0, lty=2)

# Rotate density weighted sum
rotation.matT <- rotate(density(rowSums(postT[,c(1,2)])+rowSums(postT[,c(1,3)]))$x, 
                        density(rowSums(postT[,c(1,2)])+rowSums(postT[,c(1,3)]))$y*0.02,45,1,0)
rotation.matC <- rotate(density(rowSums(postC[,c(1,2)])+rowSums(postC[,c(1,3)]))$x, 
                        density(rowSums(postC[,c(1,2)])+rowSums(postC[,c(1,3)]))$y*0.02,45,1,0)

# Plot density weighted sum
lines(rotation.matT[,1]-0.2, (rotation.matT[,2]+0.2), lwd=2)
lines(rotation.matC[,1]-0.2, (rotation.matC[,2]+0.2), lwd=2)

#### 1.3.3 Marginals endpoint 1 ####
# Rotate densities endpoint 1
rotation.matT1 <- rotate(density(rowSums(postT[,c(1,2)]))$x, 
                                 density(rowSums(postT[,c(1,2)]))$y*0.02,180,0,0)
rotation.matC1 <- rotate(density(rowSums(postC[,c(1,2)]))$x, 
                         density(rowSums(postC[,c(1,2)]))$y*0.02,180,0,0)

# Plot densities endpoint 1
plot(NULL, 
     ylim=c(min(min(rotation.matT1[,2]),min(rotation.matC1[,2])), 
            max(max(rotation.matT1[,2]),max(rotation.matC1[,2]))),
 xlim=c(0,1), xaxs="i", yaxs="i", xaxt="n", yaxt="n", frame.plot=FALSE)
lines(-1*rotation.matT1[,1], (rotation.matT1[,2]), lwd=2)
lines(-1*rotation.matC1[,1], (rotation.matC1[,2]), lwd=2)
#text(x=0.9,
#     y=0.4*min(min(rotation.matT1[,2]),min(rotation.matC1[,2])),
#     labels="Cognitive impairment", cex=3.00,
#     family="Times")
dev.off()


#### 2.  Figure 2. Superiority regions of various decision rules ####
#### 2.1 Figure 2a Single rule ####
setEPS()
postscript("Plots/Figure2a_Single.eps", family="Times")
par(mgp=c(3.5,1,0), mar=c(6,6,4,2))
plot(NULL,
     xlim=c(-1,1), xaxs="i", xlab=expression(delta[1]), 
     ylim=c(-1,1), yaxs="i", ylab=expression(delta[2]), 
     cex.lab=2.00, cex.axis=2.00, las=1)
abline(h=0,lty=1)
abline(v=0,lty=1)
rect(xleft=0,ybottom=par("usr")[3],xright=par("usr")[2],ytop=par("usr")[4],
     density=10, col = "gray40", lwd=1, border="black")
dev.off()

#### 2.2 Figure 2b Any rule ####
setEPS()
postscript("Plots/Figure2b_Any.eps", family="Times")
par(mgp=c(3.5,1,0), mar=c(6,6,4,2))
plot(NULL,
     xlim=c(-1,1), xaxs="i", xlab=expression(delta[1]), 
     ylim=c(-1,1), yaxs="i", ylab=expression(delta[2]), 
     cex.lab=2.00, cex.axis=2.00, las=1)
rect(xleft=0,ybottom=par("usr")[3],xright=par("usr")[2],ytop=par("usr")[4],
     density=10, angle = 45,col = "gray40", lwd=1, border="black")
rect(xleft=par("usr")[1], ybottom=0,xright=par("usr")[2],ytop=par("usr")[4],
     density=10, angle = -45, col = "gray40", lwd=1, border="black")

abline(h=0,lty=1)
abline(v=0,lty=1)
dev.off()

#### 2.3 Figure 2c All rule ####
setEPS()
postscript("Plots/Figure2c_All.eps", family="Times")
par(mgp=c(3.5,1,0), mar=c(6,6,4,2))
plot(NULL,
     xlim=c(-1,1), xaxs="i", xlab=expression(delta[1]), 
     ylim=c(-1,1), yaxs="i", ylab=expression(delta[2]), 
     cex.lab=2.00, cex.axis=2.00, las=1)
abline(h=0,lty=1)
abline(v=0,lty=1)
rect(xleft=0,ybottom=0,xright=par("usr")[2],ytop=par("usr")[4],density=10, 
     col = "gray40", lwd=1, border="black")
dev.off()

#### 2.4 Figure 2d Compensatory rule ####
weights <- function(a1, delta1){(-1*a1*delta1)/(1-a1)}
delta1 <- seq(-1,1,0.1)
dist <- 0.6

setEPS()
postscript("Plots/Figure2d_Compensatory.eps", family="Times")
par(mgp=c(3.5,1,0), mar=c(6,6,4,2))
plot(NULL,
     xlim=c(-1,1), xaxs="i", xlab=expression(delta[1]), 
     ylim=c(-1,1), yaxs="i", ylab=expression(delta[2]), 
     cex.lab=2.00, cex.axis=2.00, las=1)
abline(h=0,lty=1)
abline(v=0,lty=1)
polygon(x=c(par("usr")[1],par("usr")[2],par("usr")[2],par("usr")[1]),
        y=c(par("usr")[4],par("usr")[3],par("usr")[4],par("usr")[4]),
        density=10, col = "gray40", lwd=1, border="black")
curvedarrow(from=c(-sqrt(2*dist^2),0), to=c(sqrt(2*dist^2),0),
            arr.pos=0.375,segment=c(0.25,0.375), curve=-0.5, dr=1e-3,
            arr.type="triangle", arr.length=0.3, arr.adj=1,
            lwd=2, lty=2, cex=2.00)
curvedarrow(from=c(sqrt(2*dist^2),0), to=c(-sqrt(2*dist^2),0),
            arr.pos=0.875,segment=c(0.75,0.875), curve=0.5, dr=1e-3,
            arr.type="triangle", arr.length=0.3, arr.adj=1,
            lwd=2,lty=2, cex=2.00)
text(x=c(-0.8*dist,-0.6*dist), y=c(0.4*dist,1.25*dist), 
     labels=c(expression(w[1]<w[2]),expression(w[1]>w[2])), pos=c(2,3),
     cex=2.00)
dev.off()




#### 3.  Figure 3. Influence of $\vec{\alpha}^{0}_{j}$ and $n^{0}_{j}$ on the bivariate beta distribution ####
#### 3.1 Functions ####
# C++ function to speed up SampleDir()
cppFunction("NumericVector Callrgamma(int n, double shape, double scale) { 
            return(rgamma(n, shape, scale)); }")

# Sample from Dirichlet distribution
# Function to draw a sample from the posterior Dirichlet distribution of treatment $j$ (i.e. $\vec{\phi}_{j}$). 

# Input:  nDraw:      Number of draws 
#         alpha:      Parameters of the Dirichlet distribution 

# Output: z:          Matrix with four columns and nDraw rows, where each row reflects one draw
#                     Columns ordered as c("phi11","phi10","phi01","phi00")
#                     Rows sum to 1

SampleDir                            <- function (nDraw, alpha){
  k = length(alpha)
  z = array(0, dim = c(nDraw, k))
  for (i in 1:k) {z[, i] = Callrgamma(nDraw, as.double(alpha[i]),1/1.0)}
  s                                  <- rowSums(z)
  z                                  <- apply(z,2,"/",s)
  return(z)}

#### 3.2 Settings and parameters ####
# Sample data
set.seed(2500)
nDraw <- 1e6

#### 3.3.1 Figure 3a Alpha = c(0.01,0.01,0.01,0.01) ####
alpha <- c(0.01,0.01,0.01,0.01)
postT <- SampleDir(nDraw,alpha)
postC <- SampleDir(nDraw,alpha)
delta <- postT-postC


theta_3a <- kde2d(
  x=rowSums(postT[,c(1,2)]),
  y=rowSums(postT[,c(1,3)]),
  n=21,
  lims=c(0,1,0,1))

setEPS()
postscript("Plots/Figure3a_theta.eps", family="Times")
wireframe(x=theta_3a$z, row.values=theta_3a$x,col.values=theta_3a$y,
          xlab=list(expression(theta[j*","*1]),
                    cex=2.50),
          ylab=list(expression(theta[j*","*2]),
                    cex=2.50),
          zlab=" ",
          xlim=c(0,1),
          ylim=c(1,21),
          zlim=c(0,20),
          scales=list(arrows=F,col=1,
                      cex=2.00,
                      x=list(at=seq(0,1,0.5), labels=seq(0,1,0.5)),
                      y=list(at=seq(1,21,10), labels=seq(0,1,0.5)),
                      z=list(draw = FALSE)), 
          par.settings = list(axis.line = list(col = "transparent"),
                              box.3d = list(col=NA)), font="Times")
dev.off()

delta_3a <- kde2d(
  x=rowSums(delta[,c(1,2)]),
  y=rowSums(delta[,c(1,3)]),
  n=21,
  lims=c(-1,1,-1,1))


setEPS()
postscript("Plots/Figure3a_delta.eps", family="Times")
wireframe(x=delta_3a$z, row.values=delta_3a$x,col.values=delta_3a$y,
          xlab=list(expression(delta[1]),
                    cex=2.50),
          ylab=list(expression(delta[2]),
                    cex=2.50),
          zlab=" ",
          xlim=c(-1,1),
          zlim=c(0,20),
          scales=list(arrows=F,col=1,
                      cex=2.00,
                      x=list(at=seq(-1,1,0.5), labels=seq(-1,1,by=0.5)),
                      y=list(at=seq(1,21,5), labels=seq(-1,1,by=0.5)),
                      z=list(draw = FALSE)), 
          par.settings = list(axis.line = list(col = "transparent"),
                              box.3d = list(col=NA)), font="Times")
dev.off()


#### 3.3.2 Figure 3b Alpha = c(0.5,0.5,0.5,0.5) (Jeffreys prior)####
alpha <- c(0.5,0.5,0.5,0.5)
postT <- SampleDir(nDraw,alpha)
postC <- SampleDir(nDraw,alpha)
delta <- postT-postC

theta_3b <- kde2d(
  x=rowSums(postT[,c(1,2)]),
  y=rowSums(postT[,c(1,3)]),
  n=21,
  lims=c(0,1,0,1))

setEPS()
postscript("Plots/Figure3b_theta.eps", family="Times")
wireframe(x=theta_3b$z, row.values=theta_3b$x,col.values=theta_3b$y,
          xlab=list(expression(theta[j*","*1]),
                    cex=2.50),
          ylab=list(expression(theta[j*","*2]),
                    cex=2.50),
          zlab=" ",
          xlim=c(0,1),
          zlim=c(0,20),
          scales=list(arrows=F,col=1,
                      cex=2.00,
                      x=list(at=seq(0,1,0.5), labels=seq(0,1,0.5)),
                      y=list(at=seq(1,21,10), labels=seq(0,1,0.5)),
                      z=list(draw = FALSE)), 
          par.settings = list(axis.line = list(col = "transparent"),
                              box.3d = list(col=NA)), font="Times")
dev.off()

delta_3b <- kde2d(
  x=rowSums(delta[,c(1,2)]),
  y=rowSums(delta[,c(1,3)]),
  n=21,
  lims=c(-1,1,-1,1))

setEPS()
postscript("Plots/Figure3b_delta.eps", family="Times")
wireframe(x=delta_3b$z, row.values=delta_3b$x,col.values=delta_3b$y,
          xlab=list(expression(delta[1]),
                    cex=2.50),
          ylab=list(expression(delta[2]),
                    cex=2.50),
          zlab=" ",
          xlim=c(-1,1),
          zlim=c(0,20),
          scales=list(arrows=F,col=1,
                      cex=2.00,
                      x=list(at=seq(-1,1,0.5), labels=seq(-1,1,by=0.5)),
                      y=list(at=seq(1,21,5), labels=seq(-1,1,by=0.5)),
                      z=list(draw = FALSE)), 
          par.settings = list(axis.line = list(col = "transparent"),
                              box.3d = list(col=NA)), font="Times")
dev.off()

#### 3.3.3 Figure 3c Alpha = c(1,1,1,1) ####
alpha <- c(1,1,1,1)
postT <- SampleDir(nDraw,alpha)
postC <- SampleDir(nDraw,alpha)
delta <- postT-postC

theta_3c <- kde2d(
  x=rowSums(postT[,c(1,2)]),
  y=rowSums(postT[,c(1,3)]),
  n=21,
  lims=c(0,1,0,1))

setEPS()
postscript("Plots/Figure3c_theta.eps", family="Times")
wireframe(x=theta_3c$z, row.values=theta_3c$x,col.values=theta_3c$y,
          xlab=list(expression(theta[j*","*1]),
                    cex=2.50),
          ylab=list(expression(theta[j*","*2]),
                    cex=2.50),
          zlab=" ",
          xlim=c(0,1),
          zlim=c(0,20),
          scales=list(arrows=F,col=1,
                      cex=2.00,
                      x=list(at=seq(0,1,0.5), labels=seq(0,1,0.5)),
                      y=list(at=seq(1,21,10), labels=seq(0,1,0.5)),
                      z=list(draw = FALSE)), 
          par.settings = list(axis.line = list(col = "transparent"),
                              box.3d = list(col=NA)), font="Times")
dev.off()

delta_3c <- kde2d(
  x=rowSums(delta[,c(1,2)]),
  y=rowSums(delta[,c(1,3)]),
  n=21,
  lims=c(-1,1,-1,1))

setEPS()
postscript("Plots/Figure3c_delta.eps", family="Times")
wireframe(x=delta_3c$z, row.values=delta_3c$x,col.values=delta_3c$y,
          xlab=list(expression(delta[1]),
                    cex=2.50),
          ylab=list(expression(delta[2]),
                    cex=2.50),
          zlab=" ",
          xlim=c(-1,1),
          zlim=c(0,20),
          scales=list(arrows=F,col=1,
                      cex=2.00,
                      x=list(at=seq(-1,1,0.5), labels=seq(-1,1,by=0.5)),
                      y=list(at=seq(1,21,5), labels=seq(-1,1,by=0.5)),
                      z=list(draw = FALSE)), 
          par.settings = list(axis.line = list(col = "transparent"),
                              box.3d = list(col=NA)), font="Times")
dev.off()

#### 3.3.4 Figure 3d Alpha = c(5,5,5,5) ####
alpha <- c(5,5,5,5)
postT <- SampleDir(nDraw,alpha)
postC <- SampleDir(nDraw,alpha)
delta <- postT-postC

setEPS()
postscript("Plots/Figure3d_theta.eps", family="Times")
theta_3d <- kde2d(
  x=rowSums(postT[,c(1,2)]),
  y=rowSums(postT[,c(1,3)]),
  n=21,
  lims=c(0,1,0,1))

wireframe(x=theta_3d$z, row.values=theta_3d$x,col.values=theta_3d$y,
          xlab=list(expression(theta[j*","*1]),
                    cex=2.50),
          ylab=list(expression(theta[j*","*2]),
                    cex=2.50),
          zlab=" ",
          xlim=c(0,1),
          zlim=c(0,20),
          scales=list(arrows=F,col=1,
                      cex=2.00,
                      x=list(at=seq(0,1,0.5), labels=seq(0,1,0.5)),
                      y=list(at=seq(1,21,10), labels=seq(0,1,0.5)),
                      z=list(draw = FALSE)), 
          par.settings = list(axis.line = list(col = "transparent"),
                              box.3d = list(col=NA)), font="Times")
dev.off()

delta_3d <- kde2d(
  x=rowSums(delta[,c(1,2)]),
  y=rowSums(delta[,c(1,3)]),
  n=21,
  lims=c(-1,1,-1,1))

setEPS()
postscript("Plots/Figure3d_delta.eps", family="Times")
wireframe(x=delta_3d$z, row.values=delta_3d$x,col.values=delta_3d$y,
          xlab=list(expression(delta[1]),
                    cex=2.50),
          ylab=list(expression(delta[2]),
                    cex=2.50),
          zlab=" ",
          xlim=c(-1,1),
          zlim=c(0,20),
          scales=list(arrows=F,col=1,
                      cex=2.00,
                      x=list(at=seq(-1,1,0.5), labels=seq(-1,1,by=0.5)),
                      y=list(at=seq(1,21,5), labels=seq(-1,1,by=0.5)),
                      z=list(draw = FALSE)), 
          par.settings = list(axis.line = list(col = "transparent"),
                              box.3d = list(col=NA)), font="Times")
dev.off()


#### 4. Figure 4. (Section 4) Influence of $\vec{\alpha}^{0}_{j}$ on bivariate beta contour ####
setEPS()
postscript("Plots/Figure4.eps", family="Times")
par(mgp=c(3.5,1,0), mar=c(6,6,4,2))
plot(NULL, 
     xlim=c(0,1), xlab=expression(theta[j*","*1]), xaxs="i",
     ylim=c(0,1), ylab=expression(theta[j*","*2]), yaxs="i", 
     cex.lab=2.00, cex.axis=2.00, las=1)
Arrows(x0=0.6,y0=0.6,x1=0.9,y1=0.9,
       code=2, arr.length=0.2, arr.width=0.2, arr.adj=1, arr.type="triangle", lwd=2, cex=2.00)
Arrows(x0=0.6,y0=0.4,x1=0.9,y1=0.1,
       code=2, arr.length=0.2, arr.width=0.2, arr.adj=1, arr.type="triangle", lwd=2, cex=2.00)
Arrows(x0=0.4,y0=0.6,x1=0.1,y1=0.9,
       code=2, arr.length=0.2, arr.width=0.2, arr.adj=1, arr.type="triangle", lwd=2, cex=2.00)
Arrows(x0=0.4,y0=0.4,x1=0.1,y1=0.1,
       code=2, arr.length=0.2, arr.width=0.2, arr.adj=1, arr.type="triangle", lwd=2, cex=2.00)
segments(x0=par("usr")[1],x1=par("usr")[2],y0=par("usr")[3],y1=par("usr")[4], lty=2)
segments(x0=par("usr")[1],x1=par("usr")[2],y0=par("usr")[4],y1=par("usr")[3], lty=2)
points(x=0.5, y=0.5, pch=19)
text(x=c(0.70,0.70,0.30,0.30), y=c(0.80,0.20,0.80,0.20), pos=c(3,1,3,1),
     label=c(expression(phi[j*","*"11"]%up%" "),
             expression(phi[j*","*"10"]%up%" "),
             expression(phi[j*","*"01"]%up%" "),
             expression(phi[j*","*"00"]%up%" ")),
     cex=2.00)
dev.off()




#### 3.  Figure 3. (Section 4) Influence of $\vec{\alpha}^{0}_{j}$ and $n^{0}_{j}$ on the bivariate beta distribution ####

delta_3d1 <- kde2d(
  x=rowSums(delta[,c(1,2)]),
  y=rowSums(delta[,c(1,3)]),
  n=21,
  lims=c(-1,1,-1,1))


png("Plots/Figure3d_delta1.png", width=300, height=300, res=75)
wireframe(x=delta_3d1$z, row.values=delta_3d1$x,col.values=delta_3d1$y,
          xlab=list(expression(delta[1]),
                    cex=1),
          ylab=list(expression(delta[2]),
                    cex=1),
          zlab=" ",
          xlim=c(-1,1),
          zlim=c(0,20),
          scales=list(arrows=F,col=1,
                      cex=1,
                      x=list(at=seq(-1,1,0.5), labels=seq(-1,1,by=0.5)),
                      y=list(at=seq(1,21,5), labels=seq(-1,1,by=0.5)),
                      z=list(draw = FALSE)), 
          par.settings = list(axis.line = list(col = "transparent"),
                              box.3d = list(col=NA)), font="Times")
dev.off()



#### Figure 5. Posterior distribution vs. correlation ####

# True probabilities: negative correlation
phiE_neg <- c(0.15,0.40,0.40,0.05)
phiC_neg <- rev(phiE_neg)
PostA_neg <- SampleDir(1e7, phiE_neg * 20)
PostB_neg <- SampleDir(1e7, phiC_neg * 20)
Delta_neg <- cbind(rowSums(PostA_neg[,c(1,2)])-rowSums(PostB_neg[,c(1,2)]),
                   rowSums(PostA_neg[,c(1,3)])-rowSums(PostB_neg[,c(1,3)]))

# True probabilities: zero correlation
phiE_zero <- c(0.30,0.25,0.25,0.20)
phiC_zero <- rev(phiE_zero)
PostA_zero <- SampleDir(1e7, phiE_zero * 20)
PostB_zero <- SampleDir(1e7, phiC_zero * 20)
Delta_zero <- cbind(rowSums(PostA_zero[,c(1,2)])-rowSums(PostB_zero[,c(1,2)]),
                   rowSums(PostA_zero[,c(1,3)])-rowSums(PostB_zero[,c(1,3)]))

# True probabilities: positive correlation
phiE_pos <- c(0.45,0.10,0.10,0.35)
phiC_pos <- rev(phiE_pos)
PostA_pos <- SampleDir(1e7, phiE_pos * 20)
PostB_pos <- SampleDir(1e7, phiC_pos * 20)
Delta_pos <- cbind(rowSums(PostA_pos[,c(1,2)])-rowSums(PostB_pos[,c(1,2)]),
                   rowSums(PostA_pos[,c(1,3)])-rowSums(PostB_pos[,c(1,3)]))



setEPS()
postscript("Plots/Fig_correlation.eps", family="Times", height=2, width=6)
layout(matrix(c(1,2,3), nrow=1, ncol=3, byrow=TRUE))
par(mar=c(3.75,3,2.75,0.5), mgp = c(1.75, 0.75, 0))
# Negative correlation
contour(x=seq(-0.995,1,0.005),
        y=seq(-0.995,1,0.005),
        z=table(cut(Delta_neg[,1],breaks=seq(-1,1,0.005)),
                cut(Delta_neg[,2],breaks=seq(-1,1,0.005))),
        drawlabels=FALSE, lwd=1, nlevels=5, frame.plot=FALSE, 
        main="Negative correlation", #xlab=expression(theta[1]),ylab=expression(theta[2]), 
        family="Times", las=1, cex.lab=1.25, cex.main=1.25,
        xaxt="n", yaxt="n", xaxs="i", yaxs="i")

abline(h=0, lty=2)
abline(v=0, lty=2)
segments(x0=-1, x1=1, y0=1, y1=-1, lty=2)
axis(1, labels=c("-1.0","0.0","1.0"), at=c(-0.995,0,1), family="Times", cex=3)
axis(2, labels=c("-1.0","0.0","1.0"), at=c(-0.995,0,1), family="Times", cex=3, las=1)
title(xlab=expression(delta[1]), ylab=expression(delta[2]))

# Zero correlation
contour(x=seq(-0.995,1,0.005),
        y=seq(-0.995,1,0.005),
        z=table(cut(Delta_zero[,1],breaks=seq(-1,1,0.005)),
                cut(Delta_zero[,2],breaks=seq(-1,1,0.005))),
        drawlabels=FALSE, lwd=1, nlevels=5, frame.plot=FALSE, 
        main="Zero correlation", #xlab=expression(theta[1]),ylab=expression(theta[2]), 
        family="Times", las=1, cex.lab=1.25, cex.main=1.25, 
        xaxt="n", yaxt="n", xaxs="i", yaxs="i")

abline(h=0, lty=2)
abline(v=0, lty=2)
segments(x0=-1, x1=1, y0=1, y1=-1, lty=2)
axis(1, labels=c("-1.0","0.0","1.0"), at=c(-0.995,0,1), family="Times", cex=3)
axis(2, labels=c("-1.0","0.0","1.0"), at=c(-0.995,0,1), family="Times", cex=3, las=1)
title(xlab=expression(delta[1]), ylab=expression(delta[2]))

# Positive correlation
contour(x=seq(-0.995,1,0.005),
        y=seq(-0.995,1,0.005),
        z=table(cut(Delta_pos[,1],breaks=seq(-1,1,0.005)),
                cut(Delta_pos[,2],breaks=seq(-1,1,0.005))),
        drawlabels=FALSE, lwd=1, nlevels=5, frame.plot=FALSE, 
        main="Positive correlation", #xlab=expression(theta[1]),ylab=expression(theta[2]), 
        family="Times", las=1, cex.lab=1.25, cex.main=1.25,
        xaxt="n", yaxt="n", xaxs="i", yaxs="i")

abline(h=0, lty=2)
abline(v=0, lty=2)
segments(x0=-1, x1=1, y0=1, y1=-1, lty=2)
axis(1, labels=c("-1.0","0.0","1.0"), at=c(-0.995,0,1), family="Times", cex=3)
axis(2, labels=c("-1.0","0.0","1.0"), at=c(-0.995,0,1), family="Times", cex=3, las=1)
title(xlab=expression(delta[1]), ylab=expression(delta[2]))

dev.off()


