# miscellaneousPlots package: Non Common plots --------

#' @importFrom plotrix draw.ellipse
#' @importFrom Arrows Arrows
#' @importFrom grDevices integral frac
#' @importFrom graphics box plot points par abline polygon text lines
#' @importFrom utils installed.packages
#' @title Non Common plots.
#'
#' @description Build non common plots: equations, flow charts, etc, related to stock assessment, populations dynamics and management strategy evaluation.
#' @aliases miscellaneousPlots-package miscellaneousPlots
#' @docType package
#' @author Josymar Torrejon-Magallanes <ejosymart@@gmail.com>
#' @details Package: miscellaneousPlots
#' @details Type: Package
#' @details Build non common plots: equations, flow charts, etc, related to stock assessment, populations dynamics and management strategy evaluation.
#'
#' @examples
#' #See examples for functions gnomonic() and gnomonicStochastic().

NULL
#' Equations Population Dynamic and Stock Assessment
#'
#' Shows some equations related to Population Dynamic and Stock Assessment.
#' @export
eqPDSA <- function(){
  par(mar = c(0,0,0,0), yaxs = "i", xaxs = "i")
  plot(1,1, type = "n", xlim = c(0, 21), ylim = c(0, 20))
  box()
  text(y = 19, x = 10.5, "Equations for Population Dynamics and Stock Assessment", cex = 2, font = 4)
  text(y = 17, x = 5, expression(paste(S[2] == S[1] + (A + G) - (C + M))), cex = 1.5)
  text(y = 15, x = 5, expression(paste(B[t+1] == B[t] + r %.% B[t] %.% (1- frac(B[t], K)) - q %.% E[t] %.% B[t])), cex = 1.5)
  text(y = 13, x = 5, expression(paste(N[list(a,y)] ==  N[list(a-1, y-1)] %.% e^{-(M[a-1] + F[list(a-1,y-1)])})), cex = 1.5)
  text(y = 11, x = 5, expression(paste(C[list(a,y)] ==  frac(F[list(a,y)], F[list(a,y)]+M[a]) %.% N[list(a,y)] %.% (1- e^{-(M[a] + F[list(a,y)])}))), cex = 1.5)
  text(y = 9, x = 5, expression(paste(Sel[list(a,y)] == frac(F[list(a,y)],sum(F[list(a,y)], a)))), cex = 1.5)
  text(y = 7, x = 5, expression(paste(C(l, paste(Delta, "t")) == q(l, paste(Delta, "t")), s(l), E(paste(Delta, "t")), N(l, paste(Delta, "t")))), cex = 1.5)
  text(y = 5, x = 5, expression(paste(q(l, t, E, f) == f(size))), cex = 1.5)
  text(y = 3, x = 5, expression(paste(R == alpha %.% SSB %.% e^{-beta %.% SSB})), cex = 1.5)
  text(y = 1, x = 5, expression(paste(R == frac(alpha %.% SSB, beta + SSB))), cex = 1.5)

  text(y = 17, x = 15, expression(paste(L[infinity] == L[t] %.% (1-e^{(-k %.% (age - t[0]))}) + epsilon[i])), cex = 1.5, srt = 0)
  text(y = 15, x = 15, expression(paste(W[infinity] == W[t] %.% (1-e^{(-k %.% (age - t[0]))})^3) + epsilon[i]) , cex = 1.5, srt = 0)
  text(y = 13, x = 15, expression(paste(W[i] == a %.% L[i]^b  %.% e^epsilon)), cex = 1.5)
  text(y = 11, x = 15, expression(paste(P[mat](CL) == frac(1, 1+e^{-(alpha + beta%.%CL)}))), cex = 1.5)
  text(y = 9, x = 15, expression(paste(P[list(l, l+1)] == integral(frac(1, beta[g]^{alpha[l]}%.%Gamma(alpha[l])) %.% x^{alpha[l-1]} %.% e^{frac(-x, beta[g])}, l+l[1], l+l[2]) %.% dx)), cex = 1.5)
  text(y = 7, x = 15, expression(paste(frac(Y, R)) == F%.%e^{-M%.%(t[c] - t[r])}%.%W[infinity] %.% paste((frac(1, Z) - frac(paste(3,S), (Z+K)) + frac(paste(3, S^2), (Z+ paste(2,K))) - frac(S^3,(Z+paste(3,K)))))), cex = 1.3)
  text(y = 5, x = 15, expression(paste(Delta[i] == Delta[1] %.% alpha %.% (alpha - 1)^{(i-2)}, ",   ", i>1)), cex = 1.5)
  text(y = 3, x = 15, expression(paste(-lnL(paste(theta[j] ," | ", X^obs)) == prod(paste(frac(1, sigma[i]*sqrt(2*pi)) %.% e^{frac((ln(X[i]^obs)-ln(hat(X[i])))^2, 2*sigma^2)}), n, i == 1))), cex = 1.5)
  text(y = 1, x = 15, expression(paste(Pr, paste("{", A, "|", B, "}")) == frac(paste(Pr, "{", B, "|", A, "}" %.% Pr, "{", A ,"}"), paste(Pr, "{", B,"}"))), cex = 1.5)
  return(invisible())
}

#' FLBEIA flowchart
#'
#' Show all the Management Strategy Evaluation processes.
#'
#' @export
FLEBIAflowchart <- function(){
  par(yaxs = "i", xaxs = "i", mar = c(0.1,0.1,0.1,0.1))
  plot(x = c(-20, 20), y = c(-40, 25), type="n", main="",
       axes = FALSE, xlab = "", ylab = "")
  abline(h = 0, lty = 2)
  polygon(x = c(-20, -20, 20, 20),
          y = c(-40, 0, 0, -40), col = "grey90")
  box()
  #Operating model
  text(-10, 22.5, "Operating Model (OM)", font = 2, cex = 1.5)
  draw.ellipse(-12, 15, col = "grey90", a = 6, b = 4,
               angle = 0, segment = c(0,360))
  text(-12, 15, "Fish Stock(s)", font = 2,  cex = 1.25)
  draw.ellipse(12, 15, col = "grey90", a = 6, b = 4,
               angle = 0, segment = c(0,360))
  text(12, 15, "Fleet(s)", font = 2,  cex = 1.25)
  draw.ellipse(0, 5, col = "grey90", a = 6, b = 4,
               angle = 0, segment = c(0,360))
  text(0, 5, "Covariates", font = 2, cex = 1.25)
  Arrows(x0 = 6.5, y0 = 17, x1 = -6, y1 = 17, lwd = 1.5, arr.type = "triangle")
  Arrows(x0 = -6.5, y0 = 13, x1 = 6, y1 = 13, lwd = 1.5, arr.type = "triangle")
  Arrows(x0 = -10, y0 = 10.5, x1 = -6, y1 = 7.5, lwd = 1.5, arr.type = "triangle")
  Arrows(x0 = -6, y0 = 7.5, x1 = -10, y1 = 10.5, lwd = 1.5, arr.type = "triangle")
  Arrows(x0 = 10, y0 = 10.5, x1 = 6, y1 = 7.5, lwd = 1.5, arr.type = "triangle")
  Arrows(x0 = 6, y0 = 7.5, x1 = 10, y1 = 10.5, lwd = 1.5, arr.type = "triangle")
  text(0, 12, "Catch", font = 4)
  text(0, 18, "Effort", font = 4)
  #Management Procedure
  text(-7, -37.5, "Management Procedure (MP)", font = 2, cex = 1.5)
  text(15, -37.5, "From\nGarcia et al. (2017)", font = 2, cex = 0.75)
  draw.ellipse(0, -5, col = "white", a = 6, b = 4,
               angle = 0, segment = c(0,360))
  text(0, -5, "Data", font = 2, cex = 1.5)
  draw.ellipse(0, -17.5, col = "white", a = 6, b = 4,
               angle = 0, segment = c(0,360))
  text(0, -17.5, "Perceived\nSystem", font = 2, cex = 1.5)
  draw.ellipse(0, -30, col = "white", a = 6, b = 4,
               angle = 0, segment = c(0,360))
  text(0, -30, "Advice", font = 2, cex = 1.5)
  Arrows(x0 = 0, y0 = -9, x1 = 0, y1 = -12.25, lwd = 1.5, arr.type = "triangle")
  Arrows(x0 = 0, y0 = -21.5, x1 = 0, y1 = -24.75, lwd = 1.5, arr.type = "triangle")
  points(-15, 9, pch = 18, cex = 1.5)
  lines(c(-15, -15), c(9, -5), lwd = 3)
  Arrows(x0 = -15, y0 = -5, x1 = -7, y1 = -5, lwd = 3, arr.type = "triangle")
  text(-11, -2, "Observation\nModel", font = 4)
  text(-3.5, -11, "Assessment\nModel", font = 4)
  text(-3.5, -23.5, "Harvest\nControl Rule", font = 4)
  lines(c(6, 15), c(-30, -30), lwd = 3)
  Arrows(x0 = 15, y0 = -30, x1 = 15, y1 = 7, lwd = 3, arr.type = "triangle")
  text(16, -12, "Implementation Model", font = 4, srt = 90)
}
