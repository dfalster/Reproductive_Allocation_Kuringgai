
make_RAS_shapes <- function(){


	michaelis_menton <- function(x, A50, Amax=1){
		Amax * x / (x+A50)
	}


	RAS <- list()

	RAS[["Big bang"]] <- function(x, mat=0.5){
		y <- x*0 + 1
		y[x < mat] <- 0
		y
	}

	RAS[["Partial bang"]] <- function(x, mat=0.5, RAinit=0.5, RAmax=0.7, A50=0.05*mat){
		y <- RAinit + michaelis_menton(x-mat, A50=A50)*(RAmax - RAinit)
		y[x < mat] <- 0
		y[y > 1] <- 1
		y
	}

	RAS[["Asymptotic"]] <- function(x, ...){
		 RAS[["Partial bang"]](x, RAinit=0,...)
	}


	RAS[["Gradual - indeterminate"]] <- function(x, mat=0.5, A50=2*mat,  ...){
		RAS[["Partial bang"]](x, RAinit=0, mat=mat, A50=A50, ...)
	}

	RAS[["Gradual - determinate"]] <- function(x, mat=0.5, A50=0.5,  ...){
		RAS[["Partial bang"]](x, RAinit=0, RAmax=2, mat=mat, A50=A50, ...)
	}

	# based on formula for a parabola (x-h)^2 = 4p(y-k), with vertex (h,k)
	RAS[["Declining"]] <- function(x, mat=0.5, h=0.65, k=0.6, p=-0.1){
		y <- k + (x^2 - 2*h*x +h^2)/(4*p)
		y[x < mat] <- 0
		y
	}

	RAS
}
