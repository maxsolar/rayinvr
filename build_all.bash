#!/bin/bash
#
# This script should make rayinvr and all other programs
#
# Scott Pearse / email: scott.pearse@gmail.com / web: http://www.linkedin.com/in/scottpearse

echo -e "\n*************************************************************************\nThis script will make rayinvr and all the programs in all the directories\nYou will receive compilation warnings on stdout\n*************************************************************************\n"

\rm */*.o >& /dev/null

cd    rayinvr;make rayinvr;make xrayinvr
cd ../rayplot;make rayplot 2>& /dev/null;make xrayplot
cd ../pltsyn ;make pltsyn ;make xpltsyn 
cd ../vmodel ;make vmodel ;make xvmodel 
cd ../tramp  ;make tramp  ;make xtramp  

# Misc has no make file

cd ../misc
for i in $(ls *.f); do echo -e "f77 $i -o $(echo $i | awk -F . '{print "../bin/"$1}')";f77 $i -o $(echo $i | awk -F . '{print "../bin/"$1}'); done

# Finish up, remove objects

echo -e "\n*************************************************************************\nCleaning up\n"

\rm */*.o >& /dev/null

echo -e "Finished!\nCompiled binaries are in ./bin\n*************************************************************************\n\n"
