// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

// we only include RcppArmadillo.h which pulls Rcpp.h in for us
#include "RcppArmadillo.h"

// via the depends attribute we tell Rcpp to create hooks for
// RcppArmadillo so that the build process will know what to do
//
// [[Rcpp::depends(RcppArmadillo)]]
//
// [[Rcpp::export]]
arma::mat ArmaTDMarkovLoop(arma::mat m_TR, arma::cube& a_P )
    {
        int rows = m_TR.n_rows;

        for(int i = 1; i < rows; i++){
            m_TR.row(i) = m_TR.row(i-1) * a_P.slice(i-1);
        }

        return m_TR;
    }
