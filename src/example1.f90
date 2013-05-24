! example1.f90: Simple example with --help, integer, real, and flag options.
!
! From: options.f90: Module for options processing
! http://infty.net/options/options.html
! v0.8b2
!
! Copyright (c) 2009, 2012 Christopher N. Gilbreth
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

program example1
  use options
  implicit none

  type(options_t), save :: opts
  integer, target :: nt
  real(8), target :: beta
  logical, target :: fast

  call define_help_flag(opts,print_help)
  call define_option_integer(opts,"Nt",16,description="Number of time slices",var=nt)
  call define_option_real(opts,"beta",0.d0,abbrev="b",min=1.d0,max=10.d0,&
          description="Inverse temperature",required=.true.,var=beta)
  call define_flag(opts,"fast",abbrev="f",description="Do it the fast way! &
      &This option has a longer description in order to demonstrate line wrapping.",&
      var=fast)

  call process_command_line(opts)
  call check_required_options(opts)

  write (*,'(a,i0)') "Value of Nt: ", nt
  write (*,'(a,es10.3)') "Value of beta: ", beta
  write (*,'(a,l2)') "Value of --fast: ", fast

contains

  subroutine print_help(opts)
    type(options_t), intent(in) :: opts
    write (*,'(a)') "example1: Compute some useful things."
    write (*,'(a)') "Usage: example1 [options]"
    write (*,'(a)') ""
    write (*,'(a)') "Options:"
    call print_options(opts)
  end subroutine print_help

end program example1
