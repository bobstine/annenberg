### wavesim

## source the adaptive estimator

#--- load the wavelet library

library(waveslim)

#--- load code to get the empirical bayes estimator

library(ebayesthresh)

#---  generate the test signals

blocks <- function(n, s= 3.65949) {
	tau = c(0.1, 0.13, 0.15, 0.23, 0.25, 0.4, 0.44, 0.65, 0.76, 0.78, 0.81);
	h = s*c(4, -5, 3, -4, 5, -4.2, 2.1, 4.3, -3.1, 2.1, -4.2);
	x = (0 : (n-1))/n;
	outer(x,tau,function(a,b){ (1+sign(a-b))/2 }) %*% h
	}
		
bumps <- function(n, s=10.5174) {
	tau = c(0.1, 0.13, 0.15, 0.23, 0.25, 0.4, 0.44, 0.65, 0.76, 0.78, 0.81);
	h = s * c(4, 5, 3, 4, 5, 4.2, 2.1, 4.3, 3.1, 5.1, 4.2);
	w = c(5, 5, 6, 10, 10, 30, 10, 10, 5, 8, 5)/1000;
	x = (0 : (n-1))/n;
	f <- function(a) { sum(1+1/((abs(a-tau)/w)^4)) }	f(x)
	}
	
heaviSine <- function(n, s= 2.35641) {
	x = (0:(n-1))/n;
    s * (4 * sin(4 * pi * x) - sign(x - 0.3) - sign(0.72 - x))
    }
    
doppler <- function(n, s=24.21583) {
	eps = 0.05;
	x = (0:(n-1))/n;
	s * sqrt(x * (1-x)) * sin(2 * pi * (1+eps)/(x+eps))
	}

