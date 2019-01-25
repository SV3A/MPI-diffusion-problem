#!/bin/bash
# This script provides a small testing interface for verifying the Fortran
# program

# Global variables
cline_arg_test=$1
test_type=""

function main {
  # Check cmdline arugments
  case $cline_arg_test in
    restart)
      test_type='restart';;
    time)
      test_type='time';;
    *)
      echo 'Usage: ./test_utility.sh [test-type]'
      echo -e '\nTest types:\n  restart\ttest restart function'
      echo -e '  time   \ttime program'
      exit 0;;
  esac

  show_info "[TEST] Running $test_type test...\n"

  case $test_type in
    restart)
      test_restart;;
    time)
      test_time;;
    *)
      exit 1;;
  esac

  show_info '\n[TEST] Finished test'
  exit 0
}

function test_time {
  rm -f testresult_time.txt

  # Clean and build Fortran program
  clean_build

  show_warn '\n[TEST] Starting timing'

  # Number of procs. to test with
  nps=(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)

  for i in ${nps[@]}; do
    rm -f temp_time.txt
    show_info "[TEST] Testing with ${i} core(s)"

    # Time program with for each number of procs. 10 times
    for j in {1..10}; do
      mpirun -np ${i} ./bin | grep -Po "(?<=Wall.time.was:\s).+\s" | \
        tee -a temp_time.txt
      done

    # Calculate average
    awk -v pn=${i} '{s += $1}; END  {print pn,s/NR}' \
      temp_time.txt >> testresult_time.txt

    show_info '\n[TEST] Avgs found until now:'
    cat testresult_time.txt
  done

  # Clean
  rm -f temp_time.txt
}

function test_restart {
  echo 'Not available now...'
  #  # Clean and build Fortran program
  #  clean_build
  #
  #  # Remove restart file if it exists
  #  ls | grep "restart.input" &> /dev/null
  #  if [ $? -eq 0 ] 
  #  then
  #    show_warn '\n[TEST] Found restart file - deleting it'
  #    rm restart.input
  #  fi
  #
  #  # Remove abort file if it exists
  #  ls | grep "abort.command" &> /dev/null
  #  if [ $? -eq 0 ] 
  #  then
  #    show_warn '\n[TEST] Found abort file - deleting it'
  #    rm abort.command
  #  fi
  #
  #  # Run program
  #  show_info '\n[TEST] Running program without stop'
  #  ./bin &> /dev/null
  #
  #  # Check if diagnostics file was created
  #  ls | grep "diag.dat" &> /dev/null
  #  if [ $? -ne 0 ] 
  #  then
  #    show_error '\n[TEST] For some reason diagnostics file was not created - terminating'
  #    exit 1
  #  fi
  #
  #  # Store diagnostics file
  #  mv diag.dat diag_firstrun.dat
  #
  #  # Create abort command and run again
  #  show_info '[TEST] Writing abort file'
  #  touch abort.command
  #  show_info '[TEST] Running program with stop:'
  #  ./bin
  #  rm abort.command
  #
  #  # Restart execution
  #  show_info '\n[TEST] Resuming program:'
  #  'yes' | ./bin
  #
  #  show_info '[TEST] Results:'
  #  echo -e 'Result from continuous run:'
  #  cat diag_firstrun.dat
  #  echo -e '\nResult from restarted run:'
  #  cat diag.dat
  #
  #  # Clean up
  #  rm diag_firstrun.dat
}

function clean_build {
  # Clean and build Fortran program
  show_warn '[TEST] Cleaning build:'
  make clean

  show_info '\n[TEST] Building:'
  make

  # Check if build succeded
  if [ $? -eq 0 ] 
  then
    show_info '\n[TEST] Build succeded'
  else
    show_error '\n[TEST] Build failed - terminating'
    exit 1
  fi
}

# Functions for colored std-output
function show_info {
  echo -e "\033[1;32m$@\033[0m"
}

function show_warn {
  echo -e "\033[1;33m$@\033[m" 1>&2
}

function show_error {
  echo -e "\033[1;31m$@\033[m" 1>&2
}

# Run main
main $@
