#include <stdio.h>
#include <stdlib.h>
#include <math.h>

/* This routine calculates the weights for reweighting.
   
   Arguments come as:  n p m data h_mix hvals w 
   The parameters n, p, m are just numbers.
   Here is a layout of how the other arrays look.
   DATA :      x_{1,1,1}       ------
               x_{1,1,2}             |
                 ...                 |
               x_{1,1,p}             |
               x_{1,2,1}             |
	       x_{1,2,2              |
		 ...                 |---- This is for Markov Chain 1.
	       x_{1,2,p}             |
                 ...                 |   
	         ...                 |
	       x_{1,n,1}             |
               x_{1,n,2}             |
                 ...                 |
               x_{1,n,p}       ------

               x_{2,1,1}       ------
               x_{2,1,2}             |
		 ...                 |
	       x_{2,1,p}             |
               x_{2,2,1}             |
	       x_{2,2,2              |
		 ...                 |---- This is for Markov Chain 2. 
	       x_{2,2,p}             |
                 ...                 |   
	         ...                 |
	       x_{2,n,1}             |
               x_{2,n,2}             |
                 ...                 |
               x_{2,n,p}       ------
		 
                .....
               x_{m,1,1}       ------
               x_{m,1,2}             |
		 ...                 |
	       x_{m,1,p}             |
               x_{m,2,1}             |
	       x_{m,2,2              |
		 ...                 |---- This is for Markov Chain m.
	       x_{m,2,p}             |
                 ...                 |   
	         ...                 |
	       x_{m,n,1}             |
               x_{m,n,2}             |
                 ...                 |
               x_{m,n,p}       ------

The whole data forms one long array and we have to address entries appropriately.		 

Now, remember we are using Normal Priors. For other priors, this would be different.
    HVALS:       Mean     Variance
               -------   ---------
               v_{1,1,1} v_{1,1,2}       -----
               v_{1,2,1} v_{1,2,2}            |
		 ............                 |----  For Markov Chain 1.
		 ...........                  |
               v_{1,p,1} v_{1,p,2}       -----

               v_{2,1,1} v_{2,1,2}       -----
               v_{2,2,1} v_{2,2,2}            |
		 ............                 |----  For Markov Chain 2.
		 ...........                  |
               v_{2,p,1} v_{2,p,2}       -----

		 ...........

               v_{m,1,1} v_{m,1,2}       -----
               v_{m,2,1} v_{m,2,2}            |
		 ............                 |----  For Markov Chain m.
		 ...........                  |
               v_{m,p,1} v_{m,p,2}       -----
		 
Again, the whole thing is laid out as a single array.

*/
  
calc_weights(n, p, m, data, hvals, hmix, wts)
     double *data, *hvals, *hmix, *wts;
     int *n, *p, *m;
{
  int i, j;
  double sum, alpha, beta;
  
  sum = 0.0;
  for (i = 0; i < (*n) * (*m); i++) {
    alpha = data[i * (*p) + 10];  /* Alpha */
    beta = data[i * (*p) + 11];   /* Beta */
    
    wts[i] = hmix[i] * pow(beta*hvals[2], hvals[1] - 1.0)
      * hvals[2] *
	exp(-alpha/hvals[0]-beta*hvals[2]-gamma(hvals[1]));
    sum += wts[i];
  }
  
  /* Normalize the weights */
  for (i = 0; i < (*n) * (*m); i++)
    wts[i] = wts[i]/sum;
}
