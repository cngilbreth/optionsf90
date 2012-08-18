program testopts
  use options
  implicit none

  type(options_t), save :: opts
  integer :: ierr

  call define_option_integer(opts,"Nt",16,description="Number of time slices. And just to show you &
       & I can do it, I'm inserting a really long string here. Yep!!")
  call define_option_real(opts,"ssdi",.false.,abbrev='s',&
       description="Same-spin particles don't interact. xxxxxxxxxxxxxxxxxxxxxxxxxx&
       &xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx&
       &xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")
  call define_option_logical(opts,"beta",0.d0,abbrev='b',&
       description="xyz"//NEW_LINE('a')//'zwkq blah blah blah zergh zergh'//NEW_LINE('a')//'zerbh bloop&
       & bloop bloop blup blup blup ulp ulp ulp top top top bottom bottom bottom right&
       & right right left left left'//NEW_LINE('a'))
  call define_option_string(opts,'str','(none)',abbrev='x',description=new_line('a')//" Hello, world.")
  call define_option_string(opts,'str1','(none)',abbrev='y',description="str1")

  !call define_command(opts,'print',description="Print stuff",allowed_opts=[""])

  call process_command_line(opts,ierr)
  !call get_arg(opts,1,cmd,'Must specify a command ("help" for more info).')
  !call check_command_opts(opts,cmd)

  !if (cmd == "help") then
  !   call print_info(opts)
  !end if

  call print_options(opts)
  !call print_args(opts,6)
  call print_option_values(opts)
  call print_args(opts)
end program testopts
