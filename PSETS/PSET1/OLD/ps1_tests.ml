{\rtf1\ansi\ansicpg1252\cocoartf2639
\cocoatextscaling0\cocoaplatform0{\fonttbl\f0\fswiss\fcharset0 Helvetica;}
{\colortbl;\red255\green255\blue255;}
{\*\expandedcolortbl;;}
\margl1440\margr1440\vieww11520\viewh8400\viewkind0
\pard\tx720\tx1440\tx2160\tx2880\tx3600\tx4320\tx5040\tx5760\tx6480\tx7200\tx7920\tx8640\pardirnatural\partightenfactor0

\f0\fs24 \cf0 (*\
                         CS 51 Problem Set 1\
                Core Functional Programming -- Testing\
*)                           \
\
open Ps1 ;;\
\
(* The Absbook module contains simple functions for unit testing:\
   `unit_test` and `unit_test_within`. *)\
open CS51Utils ;;\
open Absbook ;;\
  \
let nonincreasing_test () =\
  unit_test (nonincreasing [])\
            "nonincreasing empty";\
  unit_test (nonincreasing [7])\
            "nonincreasing single";\
  unit_test (nonincreasing [4; 4; 4])\
            "nonincreasing repeat";\
  unit_test (not (nonincreasing [2; 1; 2]))\
            "nonincreasing inc at start";\
  unit_test (nonincreasing [2; 2; 1])\
            "nonincreasing dups";\
  unit_test (nonincreasing [9; 8; 7; 6; 5; 5; 5; 4; 4; ~-2])\
            "nonincreasing long with neg";\
  unit_test (not (nonincreasing [9; 8; 7; 6; 7; 5; 5; 5; 5; 4; 3]))\
            "nonincreasing long inc at mid" ;;\
  \
let test_all () =\
  nonincreasing_test () ;;\
\
let _ = test_all () ;;}