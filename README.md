# Options and input processing for Fortran

options.f90 is a Fortran module for defining and parsing command-line options
and input parameters for Fortran programs. Its design is inspired by Python's
optparse module. Some features:

   - Allows definition of options/parameters (real, integer, logical, string,
     and flags) corresponding to natural Fortran types, with names and
     descriptions

   - Automates reading option values from the command line and input parameters
     from file

   - Supports long options (e.g. --myopt) and short options (-m), as well as
     combining several short options (e.g. -v -c is the same as -vc)

   - Performs rigorous input validation & other error checking

   - Supports default values for all option types, and lower & upper bounds on
     numerical options

   - Can print nice descriptions of available options, with line wrapping as
     necessary

   - Designed to be easily extended with new option types

   - Well-tested and carefully coded (much of the code has been formally
     verified)

   - Easy to use

For more info, see http://infty.net/options/options.html

