#!/bin/bash

TEST=../src/test
if [ ! -f $TEST ]; then
    echo "Error: test executable not found."
    echo "Be sure to run 'make' in the source directory before testing."
    exit 1
fi


check_output()
# Check to see if a string pattern matches the output using grep
# If it doesn't, script exits.
{
    filename='test.out'
    pattern=$2
    testname=$3
    result=`grep -E "$pattern" "$filename"`
    if [ -z "$result" ]; then
	echo "Error: $testname failed"
	exit 1
    fi
}

check_param()
# Check to see if the value of a particular parameter is correct.
# Inputs listed below
{
    filename=$1
    parameter=$2
    value_regexp=$3
    testname=$4
    found=$5
    # Check output from print_option_values
    line1=`grep -E "^${parameter}: +${value_regexp}\$" "$filename"`
    if [ -z "$line1" ]; then
	echo "Error: $testname failed (print_option_values)"
	exit 1
    fi
    # Check output from get_option
    line1=`grep -E "^ get_option\('${parameter}'\): +${value_regexp}\$" "$filename"`
    if [ -z "$line1" ]; then
	echo "Error: $testname failed (get_option)"
	exit 1
    fi
    # Check option_found
    # If user passes $5, it should be 'T' or 'F' as desired.
    if [ ! -z "$found" ]; then
	line1=`grep -E "^ option_found\('${parameter}'\): +${found}\$" "$filename"`
    else
	line1=`grep -E "^ option_found\('${parameter}'\): +T\$" "$filename"`
    fi
    if [ -z "$line1" ]; then
	echo "Error: $testname failed (option_found)"
	exit 1
    fi
    # Option_found works well, not going to check the non-found options
}

check_args()
{
    expected_args=""
    filename=$1
    testname=$2
    expected_args=""
    shift; shift
    while [ $# -ne 0 ]; do
	arg=$1
	expected_args="$expected_args $arg"
	shift
    done

    # Found arguments
    found_args=""
    do_read="F"
    while read line; do
	 if [ "$line" == "Arguments:" ]; then
	     do_read="T"
	     continue
	 fi
	 if [ "$do_read" == "T" ]; then
	     if [ ! -z "$line" ]; then
		 found_args="$found_args $line"
	     fi
	 fi
    done < "$filename"

    if [ ! "$expected_args" == "$found_args" ]; then
	echo "Error: $testname failed (check_args)"
	exit 1
    fi
}


# Required option
tn=1
name="test$tn (required parameter 1)"
$TEST &> test.out
check_output 'test.out' 'missing required parameter' "$name"
echo "$name passed"

# Required option
tn=`expr $tn + 1`
name="test$tn (required parameter 2)"
$TEST --intr 3 &> test.out
check_param 'test.out' 'intr' '3' "$name"
echo "$name passed"

# Invalid option
tn=`expr $tn + 1`
name="test$tn (invalid option 1)"
$TEST --gobble &> test.out
check_output 'test.out' 'unknown option' "$name"
echo "$name passed"

# Invalid option
tn=`expr $tn + 1`
name="test$tn (invalid option 2)"
$TEST -z &> test.out
check_output 'test.out' 'unknown option' "$name"
echo "$name passed"

# Invalid option
tn=`expr $tn + 1`
name="test$tn (invalid option 3)"
$TEST -r=123 &> test.out
check_output 'test.out' 'requires an argument' "$name"
echo "$name passed"

# Note: We include the required parameter --intr below, but it doesn't affect
# the tests

# Integer option value
tn=`expr $tn + 1`
name="test$tn (integer value 1)"
$TEST --intr 0 --n1 123 &> test.out
check_param 'test.out' 'n1' '123' "$name"
echo "$name passed"

# Integer option value (w/ abbreviation)
tn=`expr $tn + 1`
name="test$tn (integer value 2)"
$TEST --intr 0 -a 123 &> test.out
check_param 'test.out' 'n1' '123' "$name"
echo "$name passed"

# Negative integer option value
tn=`expr $tn + 1`
name="test$tn (integer value 3)"
$TEST --intr 0 --n1 -123 &> test.out
check_param 'test.out' 'n1' '-123' "$name"
echo "$name passed"

# Integer in bounds
tn=`expr $tn + 1`
name="test$tn (integer bounds 1)"
$TEST --intr 0 --n2 10 &> test.out
check_param 'test.out' 'n2' '10' "$name"
echo "$name passed"

# Integer at upper bound
tn=`expr $tn + 1`
name="test$tn (integer bounds 2)"
$TEST --intr 0 --n2 12 &> test.out
check_param 'test.out' 'n2' '12' "$name"
echo "$name passed"

# Integer at lower bound
tn=`expr $tn + 1`
name="test$tn (integer bounds 3)"
$TEST --intr 0 --n2 -10 &> test.out
check_param 'test.out' 'n2' '-10' "$name"
echo "$name passed"

# Integer out of bounds
tn=`expr $tn + 1`
name="test$tn (integer bounds 4)"
$TEST --intr 0 --n2 -100 &> test.out
check_output 'test.out' 'out of range' "$name"
echo "$name passed"

# Integer out of bounds
tn=`expr $tn + 1`
name="test$tn (integer bounds 4)"
$TEST --intr 0 --n2 +100 &> test.out
check_output 'test.out' 'out of range' "$name"
echo "$name passed"

# Invalid integer
tn=`expr $tn + 1`
name="test$tn (invalid integer)"
$TEST --intr 0 --n2 1E10 &> test.out
check_output 'test.out' 'not a valid integer' "$name"
echo "$name passed"

# Logical value
tn=`expr $tn + 1`
name="test$tn (logical value 1)"
$TEST --intr 0 --logical1 .false. &> test.out
check_param 'test.out' 'logical1' 'F' "$name"
echo "$name passed"

# Logical value
tn=`expr $tn + 1`
name="test$tn (logical value 2)"
$TEST --intr 0 -l .true. &> test.out
check_param 'test.out' 'logical1' 'T' "$name"
echo "$name passed"

# Logical value
tn=`expr $tn + 1`
name="test$tn (logical value 3)"
$TEST --intr 0 --logical1 f &> test.out
check_param 'test.out' 'logical1' 'F' "$name"
echo "$name passed"

# Logical value
tn=`expr $tn + 1`
name="test$tn (logical value 4)"
$TEST --intr 0 -l t &> test.out
check_param 'test.out' 'logical1' 'T' "$name"
echo "$name passed"

# Logical value
tn=`expr $tn + 1`
name="test$tn (logical value 5)"
$TEST --intr 0 -l F &> test.out
check_param 'test.out' 'logical1' 'F' "$name"
echo "$name passed"

# Logical value
tn=`expr $tn + 1`
name="test$tn (logical value 6)"
$TEST --intr 0 --logical1 T &> test.out
check_param 'test.out' 'logical1' 'T' "$name"
echo "$name passed"

# Real value
tn=`expr $tn + 1`
name="test$tn (real value 1)"
$TEST --intr 0 --real1 1234 &> test.out
check_param 'test.out' 'real1' '1.23400+E\+003' "$name"
echo "$name passed"

# Real value
tn=`expr $tn + 1`
name="test$tn (real value 2)"
$TEST --intr 0 --real1 +1 &> test.out
check_param 'test.out' 'real1' '1.00+E\+000' "$name"
echo "$name passed"

# Real value
tn=`expr $tn + 1`
name="test$tn (real value 3)"
$TEST --intr 0 --real1 -0 &> test.out
# Surprisingly, gfortran keeps the (-) sign in front of 0. The Intel compiler
# does not.
check_param 'test.out' 'real1' '[- ]0.00+E\+000' "$name"
echo "$name passed"

# Real value 
tn=`expr $tn + 1`
name="test$tn (real value 4)"
$TEST --intr 0 --real1 1234. &> test.out
check_param 'test.out' 'real1' '1.23400+E\+003' "$name"
echo "$name passed"

# Real value 
tn=`expr $tn + 1`
name="test$tn (real value 5)"
$TEST --intr 0 --real1 1234.0 &> test.out
check_param 'test.out' 'real1' '1.23400+E\+003' "$name"
echo "$name passed"

# Real value 
tn=`expr $tn + 1`
name="test$tn (real value 6)"
$TEST --intr 0 --real1 1234.0E10 &> test.out
check_param 'test.out' 'real1' '1.23400+E\+013' "$name"
echo "$name passed"

# Real value 
tn=`expr $tn + 1`
name="test$tn (real value 7)"
$TEST --intr 0 --real1=.1234E-10 &> test.out
check_param 'test.out' 'real1' '1.23400+E-011' "$name"
echo "$name passed"

# Real value 
tn=`expr $tn + 1`
name="test$tn (real value 8)"
$TEST --intr 0 --real1 -.1234E-10 &> test.out
check_param 'test.out' 'real1' '-1.23400+E-011' "$name"
echo "$name passed"

# Real value 
tn=`expr $tn + 1`
name="test$tn (real value 9)"
$TEST --intr 0 --real1 +1234.5678E+111 &> test.out
check_param 'test.out' 'real1' '1.23456780+E\+114' "$name"
echo "$name passed"

# Real value 
tn=`expr $tn + 1`
name="test$tn (real value 9)"
$TEST --intr 0 --real1 +.5678E+111 &> test.out
check_param 'test.out' 'real1' '5.6780+E\+110' "$name"
echo "$name passed"

# Real value (abbreviation)
tn=`expr $tn + 1`
name="test$tn (real value 10)"
$TEST --intr 0 -r +1234.5678E+111 &> test.out
check_param 'test.out' 'real1' '1.23456780+E\+114' "$name"
echo "$name passed"

# Real value (invalid)
tn=`expr $tn + 1`
name="test$tn (invalid real 1)"
$TEST --intr 0 -r +1234.5678EE+111 &> test.out
check_output 'test.out' 'not a valid real number' "$name"
echo "$name passed"

# Real value (invalid)
tn=`expr $tn + 1`
name="test$tn (invalid real 2)"
$TEST --intr 0 -r "+1234.5678d+111" &> test.out
check_output 'test.out' 'not a valid real number' "$name"
echo "$name passed"

# Real value (invalid)
tn=`expr $tn + 1`
name="test$tn (invalid real 3)"
$TEST --intr 0 -r "+1234..5678E+111" &> test.out
check_output 'test.out' 'not a valid real number' "$name"
echo "$name passed"

# Real value (invalid)
tn=`expr $tn + 1`
name="test$tn (invalid real 4)"
$TEST --intr 0 -r "+12 34.5678E+111" &> test.out
check_output 'test.out' 'not a valid real number' "$name"
echo "$name passed"

# Real value (invalid)
tn=`expr $tn + 1`
name="test$tn (invalid real 5)"
$TEST --intr 0 -r "+12 34.5678E+111" &> test.out
check_output 'test.out' 'not a valid real number' "$name"
echo "$name passed"

# Real value (invalid)
tn=`expr $tn + 1`
name="test$tn (invalid real 6)"
$TEST --intr 0 -r "12,,34" &> test.out
check_output 'test.out' 'not a valid real number' "$name"
echo "$name passed"

# Real bounds
tn=`expr $tn + 1`
name="test$tn (real bounds 1)"
$TEST --intr 0 --real2=+1.0E-300 &> test.out
check_param 'test.out' 'real2' '1.00+E-300' "$name"
echo "$name passed"

# Real bounds
tn=`expr $tn + 1`
name="test$tn (real bounds 2)"
$TEST --intr 0 --real2=-1.0E-300 &> test.out
check_output 'test.out' 'out of range' "$name"
echo "$name passed"

# Real bounds
tn=`expr $tn + 1`
name="test$tn (real bounds 3)"
$TEST --intr 0 --real2=+99.999999 &> test.out
check_param 'test.out' 'real2' '9.9999999[0-9]+E\+001' "$name"
echo "$name passed"

# Real bounds
tn=`expr $tn + 1`
name="test$tn (real bounds 4)"
$TEST --intr 0 --real2=100.000000001 &> test.out
check_output 'test.out' 'out of range' "$name"
echo "$name passed"

# String value
tn=`expr $tn + 1`
name="test$tn (string value 1)"
$TEST --intr 0 --str1=' abc def 123 456 789 0`~!@#$%^& *()' &> test.out
check_param 'test.out' 'str1' '" abc def 123 456 789 0`~!@#\$%\^& \*\(\)"' "$name"
echo "$name passed"

# String value
tn=`expr $tn + 1`
name="test$tn (string value 2)"
$TEST --intr 0 --str1 ' abc def 123 456 789 0`~!@#$%^& *()' &> test.out
check_param 'test.out' 'str1' '" abc def 123 456 789 0`~!@#\$%\^& \*\(\)"' "$name"
echo "$name passed"

# String value
tn=`expr $tn + 1`
name="test$tn (string value 3)"
$TEST --intr 0 -s ' abc def 123 456 789 0`~!@#$%^& *()' &> test.out
check_param 'test.out' 'str1' '" abc def 123 456 789 0`~!@#\$%\^& \*\(\)"' "$name"
echo "$name passed"

# Flag
tn=`expr $tn + 1`
name="test$tn (flag value 1)"
$TEST --intr 0 &> test.out
check_param 'test.out' 'flag1' 'F' "$name" 'F'
echo "$name passed"

# Flag
tn=`expr $tn + 1`
name="test$tn (flag value 2)"
$TEST --intr 0 --flag1 &> test.out
check_param 'test.out' 'flag1' 'T' "$name"
echo "$name passed"

# Flag
tn=`expr $tn + 1`
name="test$tn (flag value 3)"
$TEST --intr 0 -f &> test.out
check_param 'test.out' 'flag1' 'T' "$name"
echo "$name passed"

# Flag
tn=`expr $tn + 1`
name="test$tn (flag value 5)"
$TEST --intr 0 --flag1=.false. &> test.out
check_param 'test.out' 'flag1' 'F' "$name"
echo "$name passed"

# Flag
tn=`expr $tn + 1`
name="test$tn (flag value 6)"
$TEST --intr 0 --flag1=f &> test.out
check_param 'test.out' 'flag1' 'F' "$name"
echo "$name passed"


# Mixed parameters
tn=`expr $tn + 1`
name="test$tn (mixed parameters 1)"
$TEST -f --str1 'chocolate chip' --intr 10 -r 33.7E2   &> test.out
check_param 'test.out' 'flag1' 'T' "$name [flag1]"
check_param 'test.out' 'str1' '"chocolate chip"' "$name [str1]"
check_param 'test.out' 'intr' '10' "$name [intr]"
check_param 'test.out' 'real1' '3.370+E\+003' "$name [real1]"
echo "$name passed"

# Mixed parameters
tn=`expr $tn + 1`
name="test$tn (mixed parameters 2)"
$TEST --str1 'chocolate chip' --intr 10 -r 33.7E2 -a 1 --n2 -9 -l .true. --flag1=.false. --real2=.33 &> test.out
check_param 'test.out' 'str1' '"chocolate chip"' "$name [str1]"
check_param 'test.out' 'intr' '10' "$name [intr]"
check_param 'test.out' 'real1' '3.370+E\+003' "$name [real1]"
check_param 'test.out' 'n1' '1' "$name [n1]"
check_param 'test.out' 'n2' '-9' "$name [n2]"
check_param 'test.out' 'logical1' 'T' "$name [n2]"
check_param 'test.out' 'flag1' 'F' "$name [flag1]"
check_param 'test.out' 'real2' '3.300+E-001' "$name [real2]"
echo "$name passed"


# Mixed parameters + arguments
tn=`expr $tn + 1`
name="test$tn (parameters + arguments)"
$TEST --str1 'chocolate chip' --intr 10 'arg1 is here' -r 33.7E2 +33.33 -a 1 --n2 -9 -l .true. --flag1=.false. --real2=.33 -1.23 hello &> test.out
check_param 'test.out' 'str1' '"chocolate chip"' "$name [str1]"
check_param 'test.out' 'intr' '10' "$name [intr]"
check_param 'test.out' 'real1' '3.370+E\+003' "$name [real1]"
check_param 'test.out' 'n1' '1' "$name [n1]"
check_param 'test.out' 'n2' '-9' "$name [n2]"
check_param 'test.out' 'logical1' 'T' "$name [n2]"
check_param 'test.out' 'flag1' 'F' "$name [flag1]"
check_param 'test.out' 'real2' '3.300+E-001' "$name [real2]"
check_args 'test.out' "$name" '"arg1 is here"' '"+33.33"' '"-1.23"' '"hello"'
echo "$name passed"

# Duplicate parameters
tn=`expr $tn + 1`
name="test$tn (duplicate parameters 1)"
$TEST --intr 10 -r 33.7E2 +33.33 --intr 11 &> test.out
check_output 'test.out' 'tried to set option "intr" twice' "$name"
echo "$name passed"

# Duplicate parameters
tn=`expr $tn + 1`
name="test$tn (duplicate parameters 2)"
$TEST --intr 10 -r 33.7E2 +33.33 --real1 11 &> test.out
check_output 'test.out' 'tried to set option "real1" twice' "$name"
echo "$name passed"

# Duplicate parameters
tn=`expr $tn + 1`
name="test$tn (duplicate parameters 3)"
$TEST --intr 10 --real1 33.7E2 +33.33 -r 112 &> test.out
check_output 'test.out' 'tried to set option "real1" twice' "$name"
echo "$name passed"
