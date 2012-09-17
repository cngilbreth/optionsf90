program example1
  use options
  implicit none

  type(options_t), save :: opts
  integer :: ierr, nt
  real(8) :: beta

  call define_flag(opts,"help",abbrev="h",description="Print this help message.")
  call define_option_integer(opts,"Nt",16,description="Number of time slices")
  call define_option_real(opts,"beta",0.d0,abbrev="b",min=1.d0,max=10.d0,&
          description="Inverse temperature",required=.true.)
  call define_flag(opts,"fast",abbrev="f",description="Do it the fast way! &
      &This option has a longer description in order to demonstrate line wrapping.")

  call process_command_line(opts,ierr)
  if (ierr .ne. 0) then
     write (*,'(a)') "Try using -h for more info."
     stop
  end if
  if (option_found(opts,"help")) then
     write (*,'(a)') "example1: Compute some useful things."
     write (*,'(a)') "Usage: example1 [options]"
     write (*,'(a)') ""
     call print_options(opts)
     stop
  end if
  call check_required_options(opts,ierr)
  if (ierr .ne. 0) then
     write (*,'(a)') "Try using -h for more info."
     stop
  end if

  ! Do the calculations ...
  call get_option_integer(opts,"Nt",nt)
  call get_option_real(opts,"beta",beta)
  write (*,'(a,i0)') "Value of Nt: ", nt
  write (*,'(a,es10.3)') "Value of beta: ", beta
  ! Etc
end program example1
