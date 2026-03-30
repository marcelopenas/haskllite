#!/bin/bash

ghc --make main.hs -o haskllite && rm ./*.o ./*.hi
