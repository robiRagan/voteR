/* ITERATE

	This function manages the iterations of the program as well as the order in which various functions are called.  Presumably, they will be loaded in R
	outside this function.

   NOTE:
   F	 is the frequency distribution on perm (i.e., the number of people with each of the preference orders listed left to right in perm).

   VERSION: This version of iterate (no suffix) is faster than version 2.  This version skips the evaluation of a normative criteria (transitivity or IIA)
   if an earlier normative criteria was violated (like Pareto or transitivity for IIA).  Version 2 continues on if there is a contradition so we can have
   a more accurate picture of the probabiliyt of violating Pareto, transitivity, and IIA sepearately.

   INPUT:
   perm	 is an Af x A matrix of individual preferences, ordered most preferred (left) to least preferred (right).
   N	 is the number of voters.
   T	 is the number of iterations in the simulation.
   dt	 is the distribution type: dt=0 for IC; dt=1 for IAC.

*/


#include <Rcpp.h>
using namespace Rcpp;

/* ---------------------------------------- PREFERENCE DISTRIBUTIONS ---------------------------------------------------- */

/* ICC: IMPARTIAL CULTURE CONDITION */

// [[Rcpp::export]]

	IntegerVector icc(IntegerMatrix P, int N) {
			int n = P.nrow();					/* P is the permutation matrix, perm.  n is the number of strict linear orders, A! */
			IntegerVector F(n,0) ;				/* F is vector, length n, that records the number of individuals with the respective row preference in P. */
			NumericVector bins(n) ;				/* bins records the upper bound of each bin, which must be between 0 and 1 */
		    double ran ;
			double h = 1/ (double) n;

		    /* create n bins in the [0,1] interval */
			for(int j=0; j < n; j++){
			  bins(j) = h*(j+1);
			};

			/* randomly draw N individuals and put them in the appropriate bin for F */
			for(int i=0; i < N; i++){			/* for each individual in that vector */
		      ran = R::runif(0,1);				/* randomly draw a number between 0 and 1 inclusive */
		      if( ran <= bins(0) ){
				F(0)++;							/* increase F in the first position if you found a match in the first bin */
			  }
			  else{
		  	    for(int j=1; j < n; j++){		/* go through the bins and see which bin the number belongs to */
		    	  if( (ran <= bins(j)) & (ran > bins(j-1)) ){
					F(j)++;						/* increase F in the j position if you found a match with bin j */
				  }
			    };
		  	  }
			};

	return F;
	}

/* IAC: IMPARTIAL ANONYMOUS CULTURE CONDITION */

// [[Rcpp::export]]


	Rcpp::IntegerVector iac(IntegerMatrix P, int N)
	{
	   int n = P.nrow();					/* P is the permutation matrix, perm.  n is the number of strict linear orders, A! */
	   Rcpp::NumericVector p(n-1);
  	   IntegerVector F(n,0) ;				/* F is vector, length n, that records the number of individuals with the respective row preference in P. */
	   double ran ;

    /* Generate a probability distribution, q, of each of the n orderings using the broken stick method */
       /* randomly draw a vector of numbers, p, and sort */
       p = runif(n-1);						/* randomly draw a vector of numbers between 0 and 1 */
	   std::sort(p.begin(), p.end());		/* sort the numbers in ascending order */

	/* Randomly draw N individuals and put them in the appropriate bin for F (following the broken stick rules) */
	   for(int i=0; i < N; i++){			/* for each individual */
	     ran = R::runif(0,1);				/* randomly draw a number (scalar) between 0 and 1 inclusive */
	     if( ran <= p(0) ){					/* first element match */
			F(0)++;							/* increase F in the first position if you found a match in the first bin */
		 }
		 else{
			if( ran > p(n-2) ){			/* last element match */
			  F(n-1)++;
			}
			else{
			   for(int j=1; j < n-1; j++){
			     if( (ran <= p(j)) & (ran > p(j-1)) ){		/* middle element match */
				   F(j)++;
				 }
		  	   };
			}
  	     }
	   };

	return(F);
	}

/* WEIGHTED PREFERENCE DISTRIBUTION, dt=2, (ASSUMES A=3) */

// [[Rcpp::export]]

	IntegerVector weighted_pref(IntegerMatrix P, int N, NumericVector V) {
			int n = P.nrow();					/* P is the permutation matrix, perm.  n is the number of strict linear orders, A! */
			IntegerVector F(n,0) ;				/* F is vector, length n, that records the number of individuals with the respective row preference in P. */
			NumericVector bins(n) ;				/* bins records the upper bound of each bin, which must be between 0 and 1 */
		    double ran ;
            //double h = 1/ (double) n;       //This was commented out because it looks like h is never used. Compiler was throwing warning. //RR//

  			if ( V.length() != n ){
			  Rcpp::stop("The length of the weight vector is not equal to A!");
			}

		    /* create n bins in the [0,1] interval: which are cummulative additions of each weight */
		    bins(0) = V(0);
			for(int j=1; j < n; j++){
			  bins(j) = V(j) + bins(j-1);
			};

			/* randomly draw N individuals and put them in the appropriate bin for F */
			for(int i=0; i < N; i++){			/* for each individual in that vector */
		      ran = R::runif(0,1);				/* randomly draw a number between 0 and 1 inclusive */
		      if( ran <= bins(0) ){
				F(0)++;							/* increase F in the first position if you found a match in the first bin */
			  }
			  else{
		  	    for(int j=1; j < n; j++){		/* go through the bins and see which bin the number belongs to */
		    	  if( (ran <= bins(j)) & (ran > bins(j-1)) ){
					F(j)++;						/* increase F in the j position if you found a match with bin j */
				  }
			    };
		  	  }
			};

	return F;
	}


/* --------------------------------------------- PREFERENCE AGGREGATION RULES ----------------------------------------------- */

/* PLURALITY RANKING RULE */

// [[Rcpp::export]]

	Rcpp::IntegerMatrix prr(Rcpp::IntegerVector F, Rcpp::IntegerMatrix P) {
		int n = P.nrow();						/* P is the permutation matrix, perm.  n is the number of strict linear orders, A! */
		int A = P.ncol();						/* P is the permutation matrix, perm.  A is the number of alternatives */
		IntegerVector V(A,0);					/* vector of first place votes for each alternative (a row index) */
		IntegerMatrix soc(A, A);				/* social preference between each binary pair, starting contents -9 (i.e., missing) */
		std::fill(soc.begin(), soc.end(), -9);
		soc.fill_diag(0);						/* diagonal elements indifferent (because they are same alternatives) */

	    for (int i = 0; i < n; i++){ 			/* go through each row of preference profile P */
		  V( P(i,0) ) += F(i);					/* add the total number of first place votes in V */
		}

		/* assign pairwise social preferences based on plurality votes */
	    for (int i = 0; i < A; i++){
  	      for (int j = 0; j < A; j++){
			if( V(i) > V(j) ){
			  soc(i,j)= 1;
			}
			if( V(j) > V(i) ){
			  soc(j,i)= 1;
			}
			if( V(i) == V(j) ){
			  soc(i,j)= 0;
			}
		  }
		}


	return soc;
	}


/* ANTI-PLURALITY RANKING RULE */

// [[Rcpp::export]]

	Rcpp::IntegerMatrix aprr(Rcpp::IntegerVector F, Rcpp::IntegerMatrix P) {
		int n = P.nrow();						/* P is the permutation matrix, perm.  n is the number of strict linear orders, A! */
		int A = P.ncol();						/* P is the permutation matrix, perm.  A is the number of alternatives */
		IntegerVector V(A,0);					/* vector of first place votes for each alternative (a row index) */
		IntegerMatrix soc(A, A);				/* social preference between each binary pair, starting contents -9 (i.e., missing) */
		std::fill(soc.begin(), soc.end(), -9);
		soc.fill_diag(0);						/* diagonal elements indifferent (because they are same alternatives) */

	    for (int i = 0; i < n; i++){ 			/* go through each row of preference profile P */
		  V( P(i,A-1) ) += F(i);				/* add the total number of last place votes in V */
		}

		/* assign pairwise social preferences based on last-place votes (more votes, less preferred) */
	    for (int i = 0; i < A; i++){
  	      for (int j = 0; j < A; j++){
			if( V(i) < V(j) ){
			  soc(i,j)= 1;
			}
			if( V(j) < V(i) ){
			  soc(j,i)= 1;
			}
			if( V(i) == V(j) ){
			  soc(i,j)= 0;
			}
		  }
		}

	return soc;
	}


/* BORDA COUNT */

// [[Rcpp::export]]

	Rcpp::IntegerMatrix borda(Rcpp::IntegerVector F, Rcpp::IntegerMatrix P) {
		int n = P.nrow();						/* P is the permutation matrix, perm.  n is the number of strict linear orders, A! */
		int A = P.ncol();						/* P is the permutation matrix, perm.  A is the number of alternatives */
     	IntegerVector BC(A, 0);					/* borda count for candidate i */
		IntegerMatrix soc(A, A);				/* social preference between each binary pair, starting contents -9 (i.e., missing) */
		std::fill(soc.begin(), soc.end(), -9);
		soc.fill_diag(0);						/* diagonal elements indifferent (because they are same alternatives) */
     	int points;								/* the borda counts assigned to a particular level of preference (1st, 2nd, etc.) */

		/* assign borda count to each candidate (candidates names are row indeces in BC) */
     	for (int i = 0; i < n; i++){ 			/* for each row */
          points = A;							/* start with the maximum number of Borda points when starting each row */
          for (int j = 0; j < A; j++){			/* go through the columns and determine the borda points at each level of preference */
	    	BC( P(i,j) ) += points * F(i);
   	    	points--;
          };
   	    };


		/* assign pairwise social preferences based on Borda Count */
	    for (int i = 0; i < A; i++){
  	      for (int j = 0; j < A; j++){
			if( BC(i) > BC(j) ){
			  soc(i,j)= 1;
			}
			if( BC(j) > BC(i) ){
			  soc(j,i)= 1;
			}
			if( BC(i) == BC(j) ){
			  soc(i,j)= 0;
			}
		  }
		}

	return soc;
	}


/* MEAN WITH GATE (USED IN NANSON).  TAKES AVERAGE OF SECOND COLUMN IN AN Ax2 MATRIX */

// [[Rcpp::export]]
	double avg_w_gate(IntegerMatrix X, IntegerVector G) {
		int A = X.nrow();						/* X is the Ax2 matrix. G is a vector, length A. */
		double sums=0;

        for (int j = 0; j < A; j++){			/* go through the rows of X */
		  if(G(j) == 1){
 		    sums += X(j,1);
		  }
		}

	return (sums / sum(G));
	}


/* NANSON BORDA. RETURNS MATRIX OF ALTERNATIVES AND THEIR BORDA COUNTS */

// [[Rcpp::export]]

	Rcpp::IntegerMatrix nborda(IntegerVector F, IntegerMatrix P, IntegerVector G) {
		int n = P.nrow();						/* P is the permutation matrix, perm.  n is the number of strict linear orders, A! */
		int A = P.ncol();						/* P is the permutation matrix, perm.  A is the number of alternatives */
     	IntegerMatrix BC1(A, 2);				/* borda count for alternative i: column 0 = alternative name, column 1 = borda score */
		std::fill(BC1.begin(), BC1.end(), 0);
     	int points;								/* the borda counts assigned to a particular level of preference (1st, 2nd, etc.) */

     	for (int i = 0; i < A; i++){
		  BC1(i,0) = P(0,i);					/* put the alternative names in column 0 */
		}

		/* assign borda count to each candidate (candidates names are row indeces in BC) */
     	for (int i = 0; i < n; i++){ 			/* for each row in P */
          points = A;							/* start with the maximum number of Borda points when starting each row */
          for (int j = 0; j < A; j++){			/* go through the columns and determine the borda points at each level of preference */
			for (int k = 0; k < A; k++){		/* go through the indeces of BC1 to find a matching name */
			  /* if the alternative name matches P(i,j) and the gate for P(i,j) is open (i.e., this alternative is still in the election), then ... */
			  if( (BC1(k,0) == P(i,j)) & (G( P(i,j) ) == 1)){
	 		    BC1(k,1) += points * F(i);
   	    	    points--;
			  }
			};
          };
   	    };


	return BC1;
	}


/* NANSON */

// [[Rcpp::export]]

	Rcpp::IntegerMatrix nanson(Rcpp::IntegerVector F, Rcpp::IntegerMatrix perm) {
		IntegerMatrix P(clone(perm));
		int A = P.ncol();						/* A is the number of alternatives.  P is the permutation matrix, perm.   */
		IntegerMatrix soc(A, A);				/* social preference between each binary pair, starting contents -9 (i.e., missing) */
		std::fill(soc.begin(), soc.end(), -9);
		soc.fill_diag(0);						/* diagonal elements indifferent (because they are same alternatives) */
		double m;								/* the mean of the borda count scores */
		IntegerMatrix BC(A,2);				 	/* borda count for alternative i: column 0 = alternative name, column 1 = borda score */
     	IntegerVector relim(A, A);				/* relim records the round an alt was eliminated with the vector index used as the alt name (initially A) */
    	IntegerVector G(A, 1);					/* G is a gate length A. G = 1 if alternative at that index is included in BC; 0= if eliminated  */
		int LessA = 0;

		/* run borda count and calcualte mean of scores */
		int r = 1;								/* r is the round, used to keep track of round that each alternative is eliminated */
		do {									/* repeat until only one alternative remains */
		  BC = nborda(F,P,G);
		  m = avg_w_gate(BC, G);				/* calculate the average borda count values (avg of column 1) for alternatives with open gate (col2==1) */
	      for (int i = 0; i < A; i++){			/* walk through each alternative */
		    if(BC(i,1) <= m){					/* for alternatives AT OR BELOW the mean borda score, we */

			  /* if not previously eliminated: record the round eliminated (value) in relim for each alternative (vector index), */
			  if( G(BC(i,0)) == 1 ){
			    relim( BC(i,0) ) = r;
			  }
		 	  G(BC(i,0)) = 0;					/* then mark the alternative as eliminated in the gate for the next round, */
			  LessA++;							/* and keep track of the number of alternatives eliminated so we know when to stop. */

		    }
		  };
		r++;
		} while(A - LessA > 1);					/* if more than one alternative remains, we do it again */


		/* assign pairwise social preferences based on larger r (i.e., later round eliminated) */
	    for (int i = 0; i < A; i++){
  	      for (int j = i; j < A; j++){
			if( relim(i) > relim(j) ){
			  soc(i,j)= 1;
			}
			if( relim(j) > relim(i) ){
			  soc(j,i)= 1;
			}
			if( relim(i) == relim(j) ){
			  soc(i,j)= 0;
			  soc(j,i)= 0;
			}
		  };
		};

	return soc;
	}



/* HARE PLURALITY (USED BY HARE RULE). RETURNS THE PLURALITY VOTE FOR EACH ALTERNATIVE (note: P cannot shrink) */

// [[Rcpp::export]]

	Rcpp::IntegerVector hplurality(IntegerVector F, IntegerMatrix P) {
		int n = P.nrow();						/* P is the permutation matrix, perm.  n is the number of strict linear orders, A! */
		int A = P.ncol();						/* P is the permutation matrix, perm.  A is the number of alternatives */
		IntegerVector V(A,0);					/* vector of first place votes for each alternative (a row index) */

	    for (int i = 0; i < n; i++){ 			/* go through each row of preference profile P */
		  V( P(i,0) ) += F(i);					/* add the total number of first place votes in V */
		}

	return V;
	}

/* MOVE_BOTTOM (USED BY HARE RULE). MOVES ALTERNATIVE "a" TO THE BOTTOM OF EACH PREFERENCE ORDER */

// [[Rcpp::export]]

	Rcpp::IntegerMatrix move_bottom(IntegerMatrix P, int a) {
		int n = P.nrow();						/* P is the permutation matrix, perm.  n is the number of strict linear orders, A! */
		int A = P.ncol();						/* P is the permutation matrix, perm.  A is the number of alternatives */
		IntegerMatrix P2(n, A);					/* new permutation matrix */
		int s=0;								/* s=0 indicates straight copy, s=1 indicates copy one column to the right */

		/* walk through P and copy alternatives into P2 that are not a (moving everything less preferred to "a" up one spot) */
     	for (int i = 0; i < n; i++){ 			/* for each row */
     	s=0;
          for (int j = 0; j < A-1; j++){		/* go through the columns and determine if the alternative in that (i,j) is a */
			if(P(i,j) == a){					/* as soon as we hit alternative a, we will copy next preference down the list (column to the right) */
			  s=1;
			}
			P2(i,j) = P(i,j+s);
		  };
		P2(i,A-1) = a;							/* finally put "a" in least preferred position, before going to a new row */
		};


	return P2;
	}

/* SECOND_SMALLEST (USED BY HARE RULE). IDENTIFIES THE SECOND SMALLEST INTEGER IN A VECTOR OF INTEGERS */

// [[Rcpp::export]]
	int second_smallest(IntegerVector x) {
		int n = x.length() ;
		IntegerVector y = clone(x);
		std::sort(y.begin(), y.end());
		int second = y(0);

  	    for (int i = 0; i < n; i++){
		  if(y(i) >  second){
			  second = y(i);
			  break;
	  	  }
		}

	return second;
    }


/* HARE RULE */

// [[Rcpp::export]]

	Rcpp::IntegerMatrix hare(Rcpp::IntegerVector F, Rcpp::IntegerMatrix perm) {
		IntegerMatrix P(clone(perm));
		int A = P.ncol();						/* A is the number of alternatives.  P is the permutation matrix, perm.   */
		IntegerMatrix soc(A, A);				/* social preference between each binary pair, starting contents -9 (i.e., missing) */
		std::fill(soc.begin(), soc.end(), -9);
		soc.fill_diag(0);						/* diagonal elements indifferent (because they are same alternatives) */
	    IntegerVector V2(A);						/* records the plurality vote in a given round */
		int m;									/* the min plurality vote in a given round */
     	IntegerVector elim(A, A);				/* records the round an alt was eliminated with the vector index used as the alt name */

		/* run plurality rule and determine alternative with least number of votes */
		int r = 1;								/* r is the round, used to keep track of the round that each alternative is eliminated */
		int j = 0;								/* j keeps track of how many alternatives have been eliminated (i.e., moved to bottom) */
		do {
		  V2 = hplurality(F,P);					/* determine plurality votes for each alternative */
	      if(r==1){
			m = min(V2);						/* in round 1, determine the alternative with the smallest plurality vote */
		  }
		  else{
			m = second_smallest(V2); 			/* in later rounds, determine the alternative with second smallest vote (because those eliminate in r1 will have 0 votes) */
		  }
  	      for (int i = 0; i < A; i++){			/* walk through each alternative */
		    if(V2(i) == m){						/* if the alternative has the least number of votes, we */
		  	  elim(i) = r;						/* record the round eliminated (value), */
			  P = move_bottom(P, i);			/* then move that alternative to the bottom of the preference profile */
			  j++;
		    }
		  };
		r++;
		} while(j < A-1);							/* stop when only one alternative remains */


		/* assign pairwise social preferences based on larger r (i.e., later round eliminated) */
	    for (int i = 0; i < A; i++){
  	      for (int j = i; j < A; j++){
			if( elim(i) > elim(j) ){
			  soc(i,j)= 1;
			}
			if( elim(j) > elim(i) ){
			  soc(j,i)= 1;
			}
			if( elim(i) == elim(j) ){
			  soc(i,j)= 0;
			  soc(j,i)= 0;
			}
		  };
		};

	return soc;
	}

/* HEAD-TO_HEAD (USED BY PAIRWISE MAJORITY RULE & COPELAND) */

// [[Rcpp::export]]

	int head_to_head(IntegerVector F, IntegerMatrix P, int x, int y) {
		int n = P.nrow();						/* P is the permutation matrix, perm.  n is the number of strict linear orders, A! */
		int A = P.ncol();						/* P is the permutation matrix, perm.  A is the number of alternatives */
		IntegerVector V(2,0);					/* vector of votes for x (index 0) versus y (index 1) */

	    for (int i = 0; i < n; i++){
  	      for (int j = 0; j < A; j++){
			if(P(i,j) == x){					/* if we arrive at x first (i.e. further left in P) we will assign x the votes in that row */
			  V(0) += F(i);
			  break;
			}
			if(P(i,j) == y){					/* if we arrive at y first (i.e. further left in P) we will assign y the votes in that row */
			  V(1) += F(i);
			  break;
			}
		  };
		};

		if( V(0) > V(1) ){
		  return x;
		}
		if( V(0) < V(1) ){
		  return y;
		}
		if( V(0) == V(1) ){
		  return -8;
		}
		return false; // This was added to remove a warning it should never be reached becuase the if()s above cover all cases. //RR//
	}

/* PAIRWISE MAJORITY RULE */

// [[Rcpp::export]]

	Rcpp::IntegerMatrix pair_maj_rule(Rcpp::IntegerVector F, Rcpp::IntegerMatrix P) {
		int A = P.ncol();						/* P is the permutation matrix, perm.  A is the number of alternatives */
		IntegerMatrix soc(A, A);				/* social preference between each binary pair, starting contents -9 (i.e., missing) */
		std::fill(soc.begin(), soc.end(), -9);
		soc.fill_diag(0);						/* diagonal elements indifferent (because they are same alternatives) */
		int winner;

		/* compare each pair of alternatives pairwise head-to-head in pairwise majority rule contests */
	    for (int i = 0; i < A-1; i++){
  	      for (int j = i+1; j < A; j++){
			winner = head_to_head(F,P,i,j);
			if (winner == i){
			  soc(i,j)= 1;
			}
			else{
			  if (winner == j){
			    soc(j,i)= 1;
			  }
			  else{
			    soc(i,j)= 0;
			    soc(j,i)= 0;
		  	  }
			}
		  };
		};

	return soc;
	}

/* ROWSUMS (USED BY COPELAND) */

// [[Rcpp::export]]

  IntegerVector rowSumsC(IntegerMatrix x) {
    int nrow = x.nrow();
    int ncol = x.ncol();
    IntegerVector out(nrow);

    for (int i = 0; i < nrow; i++) {
      double total = 0;
      for (int j = 0; j < ncol; j++) {
        total += x(i, j);
      }
      out[i] = total;
    }
    return out;
  }

/* COPELAND */

// [[Rcpp::export]]

Rcpp::IntegerMatrix copeland(Rcpp::IntegerVector F, Rcpp::IntegerMatrix P) {
		// int n = P.nrow();						/* P is the permutation matrix, perm.  n is the number of strict linear orders, A! */ //Is this n, used? //R//
		int A = P.ncol();						/* P is the permutation matrix, perm.  A is the number of alternatives */
		IntegerMatrix soc(A, A);				/* social preference between each binary pair, returned */
		IntegerMatrix socp(clone(soc));			/* tournament matrix which is just 1's and 0's */
		std::fill(soc.begin(), soc.end(), -9);	/* make off diagonal elements in soc  -9 for missing */
		soc.fill_diag(0);						/* diagonal elements indifferent (because they are same alternatives) */
		std::fill(socp.begin(), socp.end(), 0);	/* make all elements in socp 0 */
		IntegerVector V(A);						/* total number of pairwise wins for alternative i */
		int winner;

		/* create tournament matrix by comparing each pair of alternatives head-to-head */
	    for (int i = 0; i < A-1; i++){
  	      for (int j = i+1; j < A; j++){
			winner = head_to_head(F,P,i,j);

			if (winner == i){
			  socp(i,j)= 1;
			}
			if (winner == j){
			  socp(j,i)= 1;
			}
			if (winner == -8){					/* i and j tie */
			  socp(i,j)= .5;                    // I think these are going to be turned into a zero b/c you're going from a double, `.5` to an integer in the IntegerMatrix socp //R//
			  socp(j,i)= .5;                        // Maybe that's what he wanted to do? //R//
			}
		  };
		};

		/* sum the rows of the tournament matrix */
		V = rowSumsC(socp);

		/* assign pairwise social preferences based on larger V (larger row sum) */
	    for (int i = 0; i < A; i++){
  	      for (int j = i+1; j < A; j++){
			if( V(i) > V(j) ){
			  soc(i,j)= 1;
			}
			if( V(j) > V(i) ){
			  soc(j,i)= 1;
			}
			if( V(i) == V(j) ){
			  soc(i,j)= 0;
			  soc(j,i)= 0;
			}
		  };
		};

return soc;
}

/* HEAD-TO-HEAD_B (USED BY SCHULZE). RETURNS THE WINNER IN A HEAD-TO-HEAD CONTEST AND THE NUMBER OF VOTES FOR THE WINNER */

// [[Rcpp::export]]

	Rcpp::List head_to_headB(IntegerVector F, IntegerMatrix P, int x, int y) {
		int n = P.nrow();						/* P is the permutation matrix, perm.  n is the number of strict linear orders, A! */
		int A = P.ncol();						/* P is the permutation matrix, perm.  A is the number of alternatives */
		IntegerVector V(2,0);					/* vector of votes for x (index 0) versus y (index 1) */
		int win;								/* name of the winning alternative (number 0 to A-1) */
		int votes_winner;						/* the number of votes for the winner */
		List ret;

	    for (int i = 0; i < n; i++){
  	      for (int j = 0; j < A; j++){
			if(P(i,j) == x){					/* if we arrive at x first (i.e. further left in P) we will assign x the votes in that row */
			  V(0) += F(i);
			  break;
			}
			if(P(i,j) == y){					/* if we arrive at y first (i.e. further left in P) we will assign y the votes in that row */
			  V(1) += F(i);
			  break;
			}
		  };
		};

		if( V(0) > V(1) ){
		  win = x;
		  votes_winner = V(0);
		}
		if( V(0) < V(1) ){
		  win = y;
		  votes_winner = V(1);
		}
		if( V(0) == V(1) ){
		  win = -8;
		  votes_winner = V(0);
		}

	ret["winner"] = win;
	ret["votes_for_winner"] = votes_winner;
	return(ret);
	}


/* SCHULZE */

// [[Rcpp::export]]

Rcpp::IntegerMatrix schulze(Rcpp::IntegerVector F, Rcpp::IntegerMatrix P) {
		int A = P.ncol();						/* P is the permutation matrix, perm.  A is the number of alternatives */
		IntegerMatrix schulze_mat(A, A);		/* number of votes for winner for each binary pair (0 for ties or losers) */
		std::fill(schulze_mat.begin(), schulze_mat.end(), 0);	/* make all elements in schulze 0 */
		IntegerMatrix soc(A,A);					/* social preference matrix (output of function) */
		std::fill(soc.begin(), soc.end(), -9);	/* make off diagonal elements in soc  -9 for missing */
		soc.fill_diag(0);						/* diagonal elements indifferent (because they are same alternatives) */
		List out;

		/* create stage 1 of beat-paths matrix by comparing each pair of alternatives head-to-head and recording number of votes for winner */
	    for (int i = 0; i < A-1; i++){
  	      for (int j = i+1; j < A; j++){
			out = head_to_headB(F,P,i,j);

			if (as<int>(out["winner"]) == i){
			  schulze_mat(i,j)= as<int>(out["votes_for_winner"]);
			}
			if (as<int>(out["winner"]) == j){
			  schulze_mat(j,i)= as<int>(out["votes_for_winner"]);
			}
		  };
		};

		/* Find strongest of the weakest paths between each pair of alternatives */
    	for (int i = 0; i < A; i++){
          for (int j = 0; j < A; j++){
            if (i != j) {
                for (int k = 0; k < A; k++){
                    if ((i != k) && (j != k)){
                        schulze_mat(j, k) = (std::max)(schulze_mat(j, k), (std::min)(schulze_mat(j, i), schulze_mat(i, k)));
                    }
                };
            } else{
                if (i == j){                            //Removed extraneous parantheses ((i == j)) //R//
                    schulze_mat(i, j) = 0;
                }
            }
          };
    	};

		/* assign pairwise social preferences based on schulze matrix (larger value more preferred) */
	    for (int i = 0; i < A-1; i++){
  	      for (int j = i+1; j < A; j++){
			if (schulze_mat(i,j) > schulze_mat(j,i)){
			  soc(i,j)= 1;
			}
			else{
			  if (schulze_mat(i,j) < schulze_mat(j,i)){
			    soc(j,i)= 1;
			  }
			  else{
			    soc(i,j)= 0;
			    soc(j,i)= 0;
		  	  }
			}
		  };
		};

 	return soc;
}


/* VOTE */

// [[Rcpp::export]]

	Rcpp::IntegerMatrix vote(Rcpp::IntegerVector F, Rcpp::IntegerMatrix P, int v) {
		int A = P.ncol();						/* P is the permutation matrix, perm.  A is the number of alternatives */
		IntegerMatrix socc(A, A);				/* social preference between each binary pair, starting contents -9 (i.e., missing) */
		std::fill(socc.begin(), socc.end(), -9);
		socc.fill_diag(0);						/* diagonal elements indifferent (because they are same alternatives) */

	    if(v < 1 || v > 8){
	    Rcpp::stop("Error: You must use a vote rule with a number between 1 and 8.");
 	    }

		switch(v){
		   case 1:
		   	socc=prr(F, P);						/* plurality ranking rule */
		   	break;
		   case 2:
		   	socc=aprr(F, P);					/* anti-plurality ranking rule */
		   	break;
		   case 3:
		   	socc=borda(F, P);					/* borda count */
		   	break;
		   case 4:
		   	socc=nanson(F, P);					/* nanson's rule */
		   	break;
		   case 5:
		   	socc=hare(F, P);					/* hare's rule */
		   	break;
		   case 6:
		   	socc=pair_maj_rule(F, P);			/* pairwise majority rule */
		   	break;
		   case 7:
		   	socc=copeland(F, P);				/* Copeland's rule */
		   	break;
		   case 8:
		   	socc=schulze(F, P);					/* Schulze Method */
		   	break;
		}

	return socc;
	}


/* ------------------------------------------- ARROW'S CRITERIA ---------------------------------------------------- */


/* PARETO */

// [[Rcpp::export]]
	int pareto_eval(Rcpp::IntegerVector F,
						   Rcpp::IntegerMatrix P,
						   Rcpp::IntegerMatrix soc
	                   ) {
		int n = P.nrow();						/* P is the permutation matrix, perm.  n is the number of strict linear orders, A! */
		int A = P.ncol();						/* P is the permutation matrix, perm.  A is the number of alternatives */
		int N = sum(F);							/* N is the number of voters */
		Rcpp::IntegerMatrix count(A, A) ;		/* count is the total number of individuals who prefer alternative row to alternative column. */
        int k ;
        int contradiction = 0;

        for(int i=0; i < n; i++) {       		/* for each strict linear preference order */
          for (int j=0; j < A-1; j++){    		/* consider each alternative j (A-1 because we will compare j at A-2 to k at A-1 */
            k = j+1;
            do {
               count( P(i, j) , P(i, k) ) += F(i);
               k++;
            } while (k < A);
          };
        };

        /* We now count the number of votes on each binary pair */
        for(int j = 0; j < A; j++) {
          for (k = 0; k < A; k++) {
            if (count(j, k) == N) {				/* if this condition is satisfied, we have a weakly Pareto preferred alternative xPy */

				// If that Pareto relation contradicts social preferences, we note the contradiction and end the function.
				if(soc(j, k) != 1){
                  contradiction=1;
                  goto end_function;
				}
            }
          };
        };

    	end_function:

		return contradiction ;
	}

/* TRANSITIVITY */

// [[Rcpp::export]]
  int transitivity_eval(Rcpp::IntegerMatrix soc
	                   ) {
  int A = soc.nrow() ;							/* A is the number of alternatives */
  Rcpp::List ret ;
  int contradiction = 0;

      for(int i=0; i < A; i++) {         		/* row index for first alternative in transitive condition */
         for(int j=0; j < A; j++) {         	/* column index for second alternative in transitive condition */
            if(soc(i,j) >= 0) {					/* if a>= b, i.e. P, R, or I, then ... */
              for(int k=0; k < A; k++) {  		/* for all possible c (the column index for third alternative) */
                 if(soc(j,k) >= 0) {			/* check whether if b>=c. If it is, we need to test for transitivity ... */

					switch(soc(i,k)) {
					  case -9 :
						contradiction=1;
						goto end_function;
					  case 0 :
					    if( (soc(i,j) != 0) | (soc(j,k) != 0) ){
						  contradiction=1;
					      goto end_function;
					    }
						break;
					  case 1 :
					    if( (soc(i,j) != 1) & (soc(j,k) != 1) ){
						  contradiction=1;
					      goto end_function;
						}
					  	break;
					return 0;
					}							/* close switch */

                  };    						/* close if (j,k) */
              };       							/* close for k */
            };         							/* close if (i,j) */
         };            							/* close for j */
      };               							/* close for i */

    end_function:
	return contradiction;						/* identified a three-tuple in an intransitive relationship (using goto) */
}

/* IIA SWITCH (USED BY IIA) */

// [[Rcpp::export]]

	Rcpp::IntegerMatrix iia_switch(IntegerMatrix perm, int a, int s
								) {
		IntegerMatrix P(clone(perm));
		int n = P.nrow();						/* P is the permutation matrix; n is the number of strict linear orders, A! */
		int A = P.ncol();						/* A is the number of alternatives */
		int coln=-99, t;						/* coln is the column location of the alternative we are moving, t is it's target location */

        for(int i=0; i < n; i++) {       		/* for each row in P (i.e., each strict linear ordering) */
          	/* 1- find the column where a is located; call this "coln" */
          	for (int j=0; j < A; j++){
			  if(P(i,j) == a) coln = j;
		  	}

		  	/* 2- record the column to which a is moving; call this t */
		  	t = coln - s;						/* (we subtract because items to the left are more preferred) */
          	  if(t < 0) t=0;					/* we won't move beyond the extremes */
			  if(t > A-1) t=A-1;				/* A-1 because indexes start with 0 */

          	/* 3- replace "t" with "a" and move everything between "t" and "a" to the right or left, depending on sign of s
          	(note: we already know a is the name at one extreme so we don't have to save it in a temp) */
			if(s > 0){							/* if we are moving a up the preference list */
	          	for(int j=coln; j > t; j--){    /* move everything between "t" and "a" to the right */
				  P(i,j) = P(i,j-1);
			  	}
	          	P(i,t) = a;						/* now save a, the alternative name being switched, to location t */
			}
			if(s < 0){							/* if we are moving a up the preference list */
	          	for(int j=coln; j < t; j++){    /* move everything between "t" and "a" to the right */
				  P(i,j) = P(i,j+1);
			  	}
	          	P(i,t) = a;						/* now save a, the alternative name being switched, to location t */
			}
		}										/* close for i loop */

	   	  return(P) ;
	}

/* IIA SWITCH_BL (USED BY IIA) */

// [[Rcpp::export]]

	Rcpp::IntegerMatrix iia_switch_bl(IntegerMatrix perm, int a, int bl, int s
								) {
		IntegerMatrix P(clone(perm));
		int n = P.nrow();						/* P is the permutation matrix; n is the number of strict linear orders, A! */
		int A = P.ncol();						/* A is the number of alternatives */
		int coln=-99, blcoln=-99;				/* coln is the col location of the moving alternative, blcoln is the col location of the blocking alternative bl */
		int t;									/* t is the target column of the moving alternative */

        for(int i=0; i < n; i++) {       		/* for each row in P (i.e., each strict linear ordering) */
          	/* 1- find the column where a is located (call this "coln"); and find the column where bl is located (call this "blcoln")  */
          	for (int j=0; j < A; j++){
			  if(P(i,j) == a) coln = j;
			  if(P(i,j) == bl) blcoln = j;
		  	}

		  	/* 2- record the column to which a is moving; call this t */
		  	t = coln - s;						/* (we subtract because items to the left are more preferred) */
          	if(t < 0) t=0;						/* we won't move beyond the extremes */
			if(t > A-1) t=A-1;					/*   A-1 because indexes start with 0 */
			if((t <= blcoln) & (blcoln < coln)) t= blcoln+1; /* we also don't want to move beyond the blocking alternative */
			if((coln < blcoln) & (blcoln <= t)) t= blcoln-1;

          	/* 3- replace "t" with "a" and move everything between "t" and "a" to the right or left, depending on sign of s
          	(note: we already know a is the name at one extreme so we don't have to save it in a temp) */
			if(s > 0){							/* if we are moving a up the preference list */
	          	for(int j=coln; j > t; j--){    /* move everything between "t" and "a" to the right */
				  P(i,j) = P(i,j-1);
			  	}
	          	P(i,t) = a;						/* now save a, the alternative name being switched, to location t */
			}
			if(s < 0){							/* if we are moving a up the preference list */
	          	for(int j=coln; j < t; j++){    /* move everything between "t" and "a" to the right */
				  P(i,j) = P(i,j+1);
			  	}
	          	P(i,t) = a;						/* now save a, the alternative name being switched, to location t */
			}
		}										/* close for i loop */

	   	  return(P) ;
	}


/* IIA COMPARE (USED BY IIA) */

// [[Rcpp::export]]

	int iia_compare(IntegerMatrix soc1, IntegerMatrix soc2, int x, int y
								) {
		int A = soc1.nrow() ;							/* A is the number of alternatives */
		int contradiction = 0;

		if ( (x > (A-1)) | (y > (A-1)) ){
			Rcpp::stop("An index in iia_compare is out of bounds (likely due to arrays originating at 1, rather than 0).");
		}
		if ( (soc1.nrow() != soc2.nrow()) |  (soc1.ncol() != soc2.ncol())){
			Rcpp::stop("The social preference matrices compared in iia_compare are not the same size.");
		}
		if( (soc1(x,y) != soc2(x,y)) | (soc1(y,x) != soc2(y,x)) ){
		  contradiction = 1;
		}

	return contradiction ;
	}

/* IIA: INDEPENDENCE OF IRRELEVANT ALTERNATIVES */

// [[Rcpp::export]]

	int iia(IntegerVector F,
		   	   IntegerMatrix perm,
		   	   IntegerMatrix socP,
		   	   int voteRule
		   	   ) {

	  IntegerMatrix P(clone(perm));
	  IntegerMatrix socP2(clone(socP));
	  int n = P.nrow();			/* P is the permutation matrix; n is the number of strict linear orders, A! */
	  int A = P.ncol();			/* A is the number of alternatives */
	  IntegerMatrix P2(n, A) ;	/* P2 is the permutation matrix after alternatives are switched in ind preference orders. */
	  int max_s = A-1;			/* s is the number of switches for a single irrelevant alternatives */
	  int k, bl;
	  int contradiction=0;

	/* go through each alternative and all of its switches */
	for(int i=0; i < A; i++){				/* each unordered pair {i,j} of alternatives takes a turn as the exhalted pair */
	  for(int j=i+1; j < A; j++){
	    for(k=0; k < A; k++){				/* for all the single alternative switches, k is the alternative being switched */

		  /* for k that are the third alternative, move k up 1, up 2, down 1, down 2, checking for an IIA violation each time. */
	      if((k!=i) & (k!=j)){				/* switched alternative cannot be one of the alternatives in the exalted pair */
			for(int s= -max_s; s <= max_s; s++){				/* the number of switches (must calculate for both positive and negative) */
		      if(s != 0){										/* skipping a non-case for speed */
		        P2 = iia_switch(perm, k, s);					/* create profile with alternative k switched s steps in preference order */
	    	    socP2 = vote(F, P2, voteRule);					/* vote on switched alternatives */
	    	    contradiction = iia_compare(socP, socP2, i, j);	/* compare exalted pairs in two social preferences */
 	    	    if(contradiction== 1){							/* if they are not the same, we end the iia function */
 	    	      goto end_function;
 	    	    }
		  	  }								/* close if s */
			};								/* close for s */
	      }									/* close if k */

		  /* for k that are one of the elements in the exalted pair, make sure we do not cross over the other element of the pair as we move k up or down,
		     checking for an IIA violation each time. */
		  else{
	        if((k==i) & (k!=j)){bl = j;}						/* designate mover and blocker:  k is the alternative being moved, bl is the blocker */
	        if((k!=i) & (k==j)){bl = i;}

			for(int s= -max_s; s <= max_s; s++){				/* the number of switches (must calculate for both positive and negative) */
		      /* MUST CHANGE THE FOLLOWING LINE if A>3 */
		      if((s == -2) | ( s== 2)){							/* NOTE: for A=3, s=-2 equivalent to s=-1 AND s=1 equivalent to s=2. s=0 is not a case */
		        P2 = iia_switch_bl(perm, k, bl, s);				/* create profile with alternative k switched s steps in preference order, but does not pass bl */
	    	    socP2 = vote(F, P2, voteRule);					/* vote on switched alternatives */
	    	    contradiction = iia_compare(socP, socP2, i, j);	/* compare exalted pairs in two social preferences */
 	    	    if(contradiction== 1){							/* if they are not the same, we end the iia function */
 	    	      goto end_function;
 	    	    }
		  	  }								/* close if s */
			};								/* close for s */

	      }									/* close else */

	    };									/* close for k */
	  };	    							/* close for j */
	};										/* close for i */


	end_function:
	return contradiction;					/* identified a violation of iia for this preference profile */
	}


/* ITERATE: THE PARENT FUNCTION */

// [[Rcpp::export]]

	Rcpp::List iterate(IntegerMatrix perm, int N, int T, int dt, NumericVector V) {
	  int n = perm.nrow();						/* perm is the permutation matrix; n is the number of strict linear orders, A! */
	  int A = perm.ncol();						/* A is the number of alternatives */
  	  IntegerVector F(n,0) ;					/* F is vector, length n, that records the number of individuals with the respective row preference in P. */
	  IntegerMatrix soc_prr(A, A);				/* social preference created by plurality ranking rule (-9 = missing) */
	  IntegerMatrix soc_aprr(A, A);				/* social preference created by anti-plurality ranking rule (-9 = missing) */
	  IntegerMatrix soc_borda(A, A);			/* social preference created by borda count (-9 = missing) */
	  IntegerMatrix soc_nanson(A, A);			/* social preference created by nanson's rule (-9 = missing) */
	  IntegerMatrix soc_hare(A, A);				/* social preference created by hare's rule (-9 = missing) */
	  IntegerMatrix soc_pmr(A, A);				/* social preference created by pairwise majority rule (-9 = missing) */
	  IntegerMatrix soc_copeland(A, A);			/* social preference created by copeland's rule (-9 = missing) */
	  IntegerMatrix soc_schulze(A, A);			/* social preference created by the Schulze method (-9 = missing) */

	  /* These vectors indicate whether there is a contradiction with Pareto (column 0), Transitivity (column 1), or IIA (column 2) in a given trial */
	  IntegerVector contra_prr(3,0);			/* cell = 1 if prr contradicts a criterion; = 0 otherwise */
	  IntegerVector contra_aprr(3,0);			/* cell = 1 if aprr contradicts a criterion; = 0 otherwise */
	  IntegerVector contra_borda(3,0);			/* cell = 1 if borda contradicts a criterion; = 0 otherwise */
	  IntegerVector contra_nanson(3,0);			/* cell = 1 if nanson contradicts a criterion; = 0 otherwise */
	  IntegerVector contra_hare(3,0);			/* cell = 1 if hare contradicts a criterion; = 0 otherwise */
	  IntegerVector contra_pmr(3,0);			/* cell = 1 if pairwise maj rule contradicts a criterion; = 0 otherwise */
	  IntegerVector contra_copeland(3,0);		/* cell = 1 if copeland rule contradicts a criterion; = 0 otherwise */
	  IntegerVector contra_schulze(3,0);		/* cell = 1 if schulze method contradicts a criterion; = 0 otherwise */

	  /* "count" tallies the total number of contradictions across trials: Pareto (column 0), Transitivity (column 1), and IIA (column 2) */
	  IntegerVector count_prr(3,0);
	  IntegerVector count_aprr(3,0);
	  IntegerVector count_borda(3,0);
	  IntegerVector count_nanson(3,0);
	  IntegerVector count_hare(3,0);
	  IntegerVector count_pmr(3,0);
	  IntegerVector count_copeland(3,0);
	  IntegerVector count_schulze(3,0);

	  List ret;

 	  if(dt < 0 || dt > 2 || A != 3){
	    Rcpp::stop("Error: It must be the case that A=3 and dt = 0, 1, or 2 (note: distribution type (dt) =0 if IC; =1 if IAC; = 2 if weighted).");
 	  }

	/* --- BEGIN TRIAL LOOP: TRIAL t IN THE PROBABILITY EXPERIMENT --- */

	 for(int t=1; t <= T; t++){

		/* RESET COUNTERS: EACH TRIAL */
		   for(int i=0; i < 3; i++){						/* reseting vectors indicating a contradition on a criterion (for 3 criteria) */
			 contra_prr(i) = 0;
			 contra_aprr(i) = 0;
	  		 contra_borda(i) = 0;
	  		 contra_nanson(i) = 0;
	  		 contra_hare(i) = 0;
	  		 contra_pmr(i) = 0;
	  		 contra_copeland(i) = 0;
	  		 contra_schulze(i) = 0;
		   };

		/* DRAW PREFERENCES */
     	   if(dt == 0) {									/* if IC preferences requested, draw IC on perm */
		     F=icc(perm, N);
		   }
    	   if(dt == 1) {									/* if IAC preferences requested, draw IAC on perm */
		     F=iac(perm, N);
		   }
    	   if(dt == 2) {									/* if weighted preferences requested, draw preferences using weights V */
		     F=weighted_pref(perm, N, V);
		   }

		/* VOTE USING VARIOUS PARs */
		   soc_prr=prr(F, perm);							/* plurality ranking rule */
		   soc_aprr=aprr(F, perm);							/* anti-plurality ranking rule */
		   soc_borda=borda(F, perm);						/* borda count */
		   soc_nanson=nanson(F, perm);						/* nanson's rule */
		   soc_hare=hare(F, perm);							/* hare's rule */
		   soc_pmr=pair_maj_rule(F, perm);					/* pairwise majority rule */
		   soc_copeland=copeland(F, perm);					/* copeland's rule */
		   soc_schulze=schulze(F, perm);					/* schulze's method */

		/* PARETO CONDITION */
		   contra_prr(0) = pareto_eval(F, perm, soc_prr);
		   contra_aprr(0) = pareto_eval(F, perm, soc_aprr);
		   contra_borda(0) = pareto_eval(F, perm, soc_borda);
		   contra_nanson(0) = pareto_eval(F, perm, soc_nanson);
		   contra_hare(0) = pareto_eval(F, perm, soc_hare);
		   contra_pmr(0) = pareto_eval(F, perm, soc_pmr);
		   contra_copeland(0) = pareto_eval(F, perm, soc_copeland);
		   contra_schulze(0) = pareto_eval(F, perm, soc_schulze);

		/* TRANSITIVITY CONDITION */
		   if(contra_prr(0)==0){
			   contra_prr(1) = transitivity_eval(soc_prr);
		   }
		   if(contra_aprr(0)==0){
			   contra_aprr(1) = transitivity_eval(soc_aprr);
		   }
		   if(contra_borda(0)==0){
			   contra_borda(1) = transitivity_eval(soc_borda);
		   }
		   if(contra_nanson(0)==0){
			   contra_nanson(1) = transitivity_eval(soc_nanson);
		   }
		   if(contra_hare(0)==0){
			   contra_hare(1) = transitivity_eval(soc_hare);
		   }
		   if(contra_pmr(0)==0){
			   contra_pmr(1) = transitivity_eval(soc_pmr);
		   }
		   if(contra_copeland(0)==0){
			   contra_copeland(1) = transitivity_eval(soc_copeland);
		   }
		   if(contra_schulze(0)==0){
			   contra_schulze(1) = transitivity_eval(soc_schulze);
		   }

		/* IIA CONDITION */
		   if( sum(contra_prr)==0){
			   contra_prr(2) = iia(F, perm, soc_prr, 1);
		   }
		   if( sum(contra_aprr)==0){
			   contra_aprr(2) = iia(F, perm, soc_aprr, 2);
		   }
		   if( sum(contra_borda)==0){
			   contra_borda(2) = iia(F, perm, soc_borda, 3);
		   }
		   if( sum(contra_nanson)==0){
			   contra_nanson(2) = iia(F, perm, soc_nanson, 4);
		   }
		   if( sum(contra_hare)==0){
			   contra_hare(2) = iia(F, perm, soc_hare, 5);
		   }
		   if( sum(contra_pmr)==0){
			   contra_pmr(2) = iia(F, perm, soc_pmr, 6);
		   }
		   if( sum(contra_copeland)==0){
			   contra_copeland(2) = iia(F, perm, soc_copeland, 7);
		   }
		   if( sum(contra_schulze)==0){
			   contra_schulze(2) = iia(F, perm, soc_schulze, 8);
		   }

		/* UPDATE COUNTERS (i.e. total number of violations by criteria) */
		   for(int i=0; i < 3; i++){						/* reseting vectors indicating a contradition on a criterion (for 3 criteria) */
		     count_prr(i) = count_prr(i) + contra_prr(i) ;
			 count_aprr(i) = count_aprr(i) + contra_aprr(i) ;
			 count_borda(i) = count_borda(i) + contra_borda(i) ;
		     count_nanson(i) = count_nanson(i) + contra_nanson(i) ;
		     count_hare(i) = count_hare(i) + contra_hare(i) ;
		     count_pmr(i) = count_pmr(i) + contra_pmr(i) ;
		     count_copeland(i) = count_copeland(i) + contra_copeland(i) ;
		     count_schulze(i) = count_schulze(i) + contra_schulze(i) ;
		   };

	 };
	/* --- END TRIAL LOOP --- */

	ret["dt"] = dt;
	ret["F"] = F;
	ret["perm"] = perm;
	ret["count_prr"] = count_prr;
	ret["count_aprr"] = count_aprr;
	ret["count_borda"] = count_borda;
	ret["count_nanson"] = count_nanson;
	ret["count_hare"] = count_hare;
	ret["count_pmr"] = count_pmr;
	ret["count_copeland"] = count_copeland;
	ret["count_schulze"] = count_schulze;

	return(ret);
	}


