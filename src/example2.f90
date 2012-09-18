program example2
  use options
  implicit none

  type(options_t), save :: opts
  character(len=128) :: inputfile
  integer :: ierr, nt, nargs
  real(8) :: beta

  ! Define options
  call define_help_option(opts,print_help)
  call define_flag(opts,"fast",abbrev='f',&
       description="Do it the fast way!",group="cmdline")
  call define_option_integer(opts,"Nt",16,description="(integer) Number of time slices",&
       group="inputfile")
  call define_option_real(opts,"beta",0.d0,min=1.d0,max=10.d0,required=.true.,&
          description="(real) Inverse temperature",group="inputfile")

  ! Process command line
  call process_command_line(opts,ierr)
  if (ierr .ne. 0) stop
  call get_num_args(opts,nargs)
  if (nargs .ne. 1) then
     write (*,'(a)') "Error: expected exactly 1 argument for the input file."
     write (*,'(a)') "Try using -h for more info."
     stop
  end if

  ! Read input file
  call get_arg(opts,1,inputfile,ierr)
  if (ierr .ne. 0) stop
  call process_input_file(opts,inputfile,ierr,group='inputfile')
  if (ierr .ne. 0) stop

  ! Check options
  call check_required_options(opts,ierr)
  if (ierr .ne. 0) stop

  ! Do the calculations ...
  call get_option_integer(opts,"Nt",nt)
  call get_option_real(opts,"beta",beta)
  write (*,'(a,i0)') "Value of Nt: ", nt
  write (*,'(a,es10.3)') "Value of beta: ", beta

contains

  subroutine print_help(opts)
    implicit none
    type(options_t), intent(in) :: opts
    write (*,'(a)') "example2: Compute some useful things."
    write (*,'(a)') "Usage: example2 [options] <input file>"
    write (*,'(a)') ""
    write (*,'(a)') "Command line options:"
    call print_options(opts,group="cmdline")
    write (*,'(a)') ""
    write (*,'(a)') "Input file parameters:"
    call print_options(opts,group="inputfile",style="file")
  end subroutine print_help

end program example2
