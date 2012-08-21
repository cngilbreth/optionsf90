program testopts
  use options
  implicit none

  type(options_t), save :: opts
  integer :: ierr

  call define_option_integer(opts,"Nt",16,description="Number of time slices. And just to show you &
       & I can do it, I'm inserting a really long string here. Yep!!")
  call define_option_logical(opts,"ssdi",.false.,abbrev='s',&
       description="Same-spin particles don't interact. yyyyyyyyyyyyyyyyyyyyyyyyyy&
       &yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy&
       &yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy")
  call define_option_real(opts,"beta",0.d0,abbrev='b',&
       description="xyz"//NEW_LINE('a')//'zwkq blah blah blah zergh zergh'//NEW_LINE('a')//'zerbh bloop&
       & bloop bloop blup blup blup ulp ulp ulp top top top bottom bottom bottom right&
       & right right left left left'//NEW_LINE('a'))
  call define_option_string(opts,'str','(none)',abbrev='x',description=new_line('a')//" Hello, world.")
  call define_option_string(opts,'str1','(none)',abbrev='y',description="str1")
  call define_flag(opts,'flag','f',description="This is a flag")

  call process_command_line(opts,ierr)
  if (ierr .ne. 0) stop "Try using -h for more info."
  call print_options(opts)
  call print_option_values(opts)
  call print_args(opts)
end program testopts
