# Lagrange calculator

# define lagrangian


lg <- expression(x^(1/2)+y**(1/2)-z*(18*x+2*y-72))

#first derivatives
derx <- (D(lg, 'x')
dery <-D(lg,'y')
derz <- D(lg,'z')
         