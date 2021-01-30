#include <stdio.h>
#include <stdlib.h>
#include <math.h>

/* This routine calculates the value of the mixture distribution at each
   of the data points.  By  doing this once for all, we save in having
   to compute this everytime. In addition, we return the reciprocals, since
   that is what is used mostly in the computations. 
   
   Arguments come as:  n p m data c ohvals h 
   The parameters n, p, m are just numbers. C is an array of length m. H is an array
   of length n*m.
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
    OHVALS:      Mean     Variance
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

h_mix(n, p, m, data, c, ohvals, h)
  double *data, *c, *ohvals, *h;
  int *n, *p, *m;
{
  int i, j, k, offset;
  double mean, var, stval, sum, prod, alpha, beta;
  
  
  for (i = 0; i < (*n) * (*m);i++) {
    for (k = 0, sum = 0.0; k < *m; k++) {   /* loops over the # of mc's */
      for (j = 0, prod = c[k]; j < *p; j++) { /* loops over each coordinate of the pt.*/
        offset = 2 * (k * (*p) + j);       /* offset into the array of hyper-vals */
        mean = ohvals[offset];         /* we got the mean */
        var = ohvals[offset + 1];      /* the next one is the variance */
        stval = data[i * (*p) + j] - mean;  /* subtract the mean from the point */
        prod *= exp(-0.5 * stval * stval / var) / sqrt(var); 
	/* The normal density sans things that cancel off anyway */
      }
      sum += prod;
    }
    
    h[i] = (*m) / sum;  /* reciprocal value of h_mix at that data point */
  }
}
