# Test that the ArmaMarkov function matches
# the Rloop function for a simple example.

Rloopfunc <- function(m_TR, a_P) {
  # throughout the number of cycles
  for (t in 1:(nrow(m_TR) - 1)) {
 # estimate the Markov trace for cycle the next cycle (t + 1)
    m_TR[t + 1,] <-
      m_TR[t,] %*% a_P[, , t]
  }
  # return the trace
  m_TR
}

# create example array function
makeTransProbArray <- function(simDim = 4){
  # need the random number stream to be consistent- fix at seed = 100
  set.seed(100)
  # create an array from slicing this matrix
  array(
    data = #matrix(
      runif(simDim  * simDim * 100),
      #byrow = T,
      #nrow = simDim,
      #ncol = simDim
   # ),
    dim = c(simDim, simDim, 100)
  )
}

# create example markov trace function
makeMarkovTrace <- function(simDim = 4) {
  # make empty trace
  m_TR <- matrix(data = 0,
                 nrow = 100,
                 ncol = simDim)
  # initialise trace
  m_TR[1,]  <- c(1, rep(0, simDim - 1))
  # return trace
  m_TR

}



#==================================================#
# EQUAL
#==================================================#
test_that("ArmaMarkov equals BaseRloop", {
  # test three different symmetric matrix dimensions:
  for(s in c(3,12,21)){
    # in each case test that the two approaches get equal answers
    testthat::expect_identical(
    ArmaTDMarkovLoop(m_TR = makeMarkovTrace(s), a_P = makeTransProbArray(s)),
    Rloopfunc(m_TR = makeMarkovTrace(s), a_P = makeTransProbArray(s))
  ) # end expect equal
  }
}) # end testthat.



#==================================================#
# FASTER
#==================================================#
test_that("ArmaMarkov is faster than BaseRloop", {
  # test three different symmetric matrix dimensions:
  for(s in c(3,12,21)){
    mb <- microbenchmark::microbenchmark(
      Cpp = ArmaTDMarkovLoop(m_TR = makeMarkovTrace(s), a_P = makeTransProbArray(s)),
      Rloop = Rloopfunc(m_TR = makeMarkovTrace(s), a_P = makeTransProbArray(s))
    )


    # in each case test that the ArmaMarkov function is faster
    # Mean
    testthat::expect_false(
      mean(mb[mb$expr == "Cpp" ,"time"]) > mean(mb[mb$expr == "Rloop" ,"time"])
      ) # end expect equal

    # Median
    testthat::expect_false(
      median(mb[mb$expr == "Cpp" ,"time"]) > median(mb[mb$expr == "Rloop" ,"time"])
    ) # end expect equal

  }
}) # end testthat.


