#!/bin/bash

ghc --make main.hs -o haskllite && rm ./*.o ./*.o-boot ./*.hi ./*.hi-boot ./Parser/*.o ./Parser/*.o-boot ./Parser/*.hi ./Parser/*.hi-boot
