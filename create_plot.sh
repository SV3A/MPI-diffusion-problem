#!/bin/bash
# This script generates a plot using Gnuplot

# Filenames
plot_data_file="diff_final.dat"
plot_flname="diff.ps"

# Gnuplot command
plot_cmd="set term post eps color solid; set out \"$plot_flname\"; sp \"$plot_data_file\" u 1:2:3 w l"

ls | grep "$plot_data_file" &> /dev/null

if [ $? -eq 0 ] 
then
  # Create the plot using gnuplot
  gnuplot -e "$plot_cmd"

  if [ $? -ne 0 ] 
  then
    echo 'Gnuplot not happy! Is it even installed?'
    exit 1
  fi

  # Try to open the plot
  case $(uname -s) in
    Linux*)
      xdg-open "$plot_flname"
      if [ $? -ne 0 ] 
      then
        echo 'Hmm, try opening the plot yourself!'
        exit 1
        fi;;
      Darwin*) open "$plot_flname";;
      *)
        echo 'I cannot open plots for you on this machine, sorry.';;
    esac

  else
    echo -e "Data file for plotting \"$plot_data_file\" not found"
    exit 1
fi

exit 0
