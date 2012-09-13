! Test program for options.f90
! http://infty.us/options/options.html
!
! Copyright (c) 2012 Christopher N. Gilbreth
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.

program testopts
  use options
  implicit none

  type(options_t), save :: opts
  integer :: ierr, n1, n2, q
  logical :: l1, h, flag1
  real(8) :: r1, r2
  character(len=1024) :: str1

  call define_flag(opts,'help','h',description="Print this help message.")
  call define_option_integer(opts,"n1",16,abbrev='a',description="An integer option. &
       &This one has no min or max values defined.")
  call define_option_integer(opts,"n2",16,abbrev='b',min=-10,max=12,&
       description="Another integer option. This one has a min value of -10 and&
       & a max value of 12 defined.")
  call define_option_logical(opts,"logical1",.false.,abbrev='l',&
       description="This is a logical option. Logical options take default values&
       & (here .false.) but, as one would expect, do not accept minimum or maximum&
       & values. Here are some characters to test line breaks:"//new_line('a')//&
       new_line('a')//"  line1"//new_line('a')//"  line2"//new_line('a')//&
       "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabc&
       &defghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuv&
       &wxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmno&
       &pqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefgh&
       &ijklmnopqrstuvwxyz1234567890-=!@#$%^&*()_+[]\;',./{}|:<>?1234567890-=!@&
       &#$%^&*()_+[]\;',./{}|:<>?")
  call define_option_real(opts,"real1",3.1415926535897931d0,abbrev='r',&
       description="A real-valued option, with default value 3.1415926535897931.")
  call define_option_real(opts,"real2",2.7182818284590451d0,min=0.d0,max=100.d0,&
       description="This is another real-valued option, with default value&
       & 2.7182818284590451. In this case the min value is 0.0, and the max &
       &value is 100. No abbreviation is defined.")
  call define_option_string(opts,'str1','(none)',abbrev='s',description=new_line('a')&
       //new_line('a')//" This option is string-valued, with default value &
       &'(none)'. The description begins with two new lines, followed by a space,&
       & and ends with an additional newline. The abbreviation is -s."//new_line('a'))
  call define_flag(opts,'flag1','f',description="  This is a flag. Description&
       & begins with two spaces.")
  call define_option_integer(opts,'intr',7,abbrev='q',&
       description="This is a required option, intended to test check_required_opts.",&
       required=.true.)

  call process_command_line(opts,ierr)
  if (ierr .ne. 0) stop "Try using -h for more info."
  if (option_found(opts,"help")) then
     call print_options(opts)
     stop
  end if

  write (*,'(a)') "* Test of check_required_opts:"
  call check_required_options(opts,ierr)
  if (ierr .ne. 0) stop "Try using -h for more info."
  write (*,*) ""

  write (*,'(a)') "* Test of print_option_values:"
  call print_option_values(opts)
  write (*,*) ""

  write (*,'(a)') "* Test of get_option:"
  call get_flag(opts,'help',h)
  call get_option_integer(opts,"n1",n1)
  call get_option_integer(opts,"n2",n2)
  call get_option_logical(opts,"logical1",l1)
  call get_option_real(opts,"real1",r1)
  call get_option_real(opts,"real2",r2)
  call get_option_string(opts,'str1',str1)
  call get_flag(opts,'flag1',flag1)
  call get_option_integer(opts,"intr",q)
  write (*,'(1x,a,l1)') "get_option('help'): ", h
  write (*,'(1x,a,i0)') "get_option('n1'): ", n1
  write (*,'(1x,a,i0)') "get_option('n2'): ", n2
  write (*,'(1x,a,l1)') "get_option('logical1'): ", l1
  write (*,'(1x,a,es22.14e3)') "get_option('real1'): ", r1
  write (*,'(1x,a,es22.14e3)') "get_option('real2'): ", r2
  write (*,'(1x,3a)') "get_option('str1'): """, trim(str1), '"'
  write (*,'(1x,a,l1)') "get_option('flag1'): ", flag1
  write (*,'(1x,a,i0)') "get_option('intr'): ", q

  write (*,*) ""
  write (*,'(a)') "* Test of opt_found:"
  write (*,'(1x,a,l1)') "option_found('help'): ", option_found(opts,'help')
  write (*,'(1x,a,l1)') "option_found('n1'): ", option_found(opts,'n1')
  write (*,'(1x,a,l1)') "option_found('n2'): ", option_found(opts,'n2')
  write (*,'(1x,a,l1)') "option_found('logical1'): ", option_found(opts,'logical1')
  write (*,'(1x,a,l1)') "option_found('real1'): ", option_found(opts,'real1')
  write (*,'(1x,a,l1)') "option_found('real2'): ", option_found(opts,'real2')
  write (*,'(1x,a,l1)') "option_found('str1'): ", option_found(opts,'str1')
  write (*,'(1x,a,l1)') "option_found('flag1'): ", option_found(opts,'flag1')
  write (*,'(1x,a,l1)') "option_found('intr'): ", option_found(opts,'intr')

  write (*,*) ""
  write (*,'(a)') "* Test of get_arg and get_num_args (via print_args):"
  call print_args(opts)
end program testopts
