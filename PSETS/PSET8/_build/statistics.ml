(*
                          CS 51 Problem Set
                 Simulation of an Infectious Disease

                          Statistics-keeping

  We keep a set of counters to track the total number of people in
  various states with respect to the infection.
 *)

open Counter ;;
  
let time = new counter ;;
let susceptible = new counter ;;
let infected = new counter ;;
let recovered = new counter ;;
let deceased = new counter ;;

