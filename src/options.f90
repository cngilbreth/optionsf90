! options.f90: Module for options processing
! http://infty.net/options/options.html
! v0.8.1
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

module options
  use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
  implicit none
  private

  ! Can customize these parameters as desired:

  ! String lengths
  integer, parameter, public :: opt_len = 256
  integer, parameter, public :: descr_len = 2048
  ! Max number of options & positional arguments
  integer, parameter :: maxopts = 32
  integer, parameter :: maxargs = 32
  ! For formatting output
  integer, parameter :: name_column  = 3
  integer, parameter :: descr_column = 30
  integer, parameter :: max_column   = 90
  ! Kind for real options
  integer, parameter :: rk = selected_real_kind(p=15)
  ! Kind for integer options
  integer, parameter :: ik = kind(1)

  ! The following shouldn't be changed unless you really know what you're doing
  character(len=2), parameter :: crlf = achar(13)//achar(10) ! newline characters
  character(len=2), parameter :: blank = ' '//achar(9)  ! space or tab
  character(len=8), parameter :: name_char_excludes = '''" !#=:$'

  ! ** Option data types *******************************************************

  integer, parameter :: T_NONE=0, T_INTEGER=1, T_LOGICAL=2, T_FLAG=3, T_REAL=4,&
       T_STRING=5

  ! ** Option type *************************************************************

  type opt_t
     character (len=opt_len)   :: name
     character (len=1)         :: abbrev
     character (len=descr_len) :: descr
     logical :: required, found
     integer :: dtype
     character(len=opt_len) :: group

     ! All types: the value as a string
     character(len=opt_len) :: str

     ! For integer options
     integer(ik) :: ival
     integer(ik) :: imin, imax
     integer(ik), pointer :: ivar

     ! For logical options and flags
     logical :: lval
     logical, pointer :: lvar

     ! For real options
     real(rk) :: rval
     real(rk) :: rmin, rmax
     real(rk), pointer :: rvar

     ! For string/character options
     character (len=opt_len)   :: cval
     character (len=opt_len), pointer :: cvar
  end type opt_t

  ! Predicate:
  ! Defined(opt) :=
  !   Defined(name,abbrev,descr,required,found,dtype,group)
  !   .and. (dtype == T_INTEGER ==> Defined(ival,imin,imax,ivar))
  !   .and. (dtype == T_REAL ==> Defined(rval,rmin,rmax,rvar))
  !   .and. (dtype == T_LOGICAL ==> Defined(lval,lvar))
  !   .and. (dtype == T_STRING ==> Defined(cval,cvar))
  !   .and. (dtype == T_FLAG ==> Defined(lval,lvar))
  !
  ! For any intrinsic Fortran type, Defined means that the value has been
  ! explicitly set in the program either to a valid value (either a default
  ! value or an input value).
  !
  ! For names, Defined(name) ==> is_name(name)
  ! Abbreviations: Defined(a) ==> (a == ' ' .or. is_abbrev_char(a))

  ! ** Main options structure **************************************************

  type options_t
     private
     ! Options
     integer :: nopts = 0
     type(opt_t) :: opts(maxopts)
     ! Positional arguments
     integer :: nargs = 0
     character(len=opt_len) :: args(maxargs)

     ! Help routine
     procedure(helpr), pointer, nopass :: help_routine => null()
  end type options_t

  abstract interface
     subroutine helpr(opts)
       import
       type(options_t), intent(in) :: opts
     end subroutine helpr
  end interface

  ! Invariants:
  ! 1. nopts ≤ maxopts
  ! 2. Defined(opts(i)) ∀ i ∈ {1,...,nopts}
  ! 3. opts(i)%name .ne. opts(j)%name ∀ i,j ∈ {1,...,nopts}
  ! 4. (opts(i)%abbrev .ne. opts(j)%abbrev) .or. opts(i)%abbrev == ' '
  !      ∀ i,j ∈ {1,...,nopts}

  public :: options_t


  ! ** Public interface functions **********************************************

  public :: define_option_integer, define_option_real, define_option_logical, &
       define_option_string, define_flag, define_help_flag
  public :: get_option_integer, get_option_real, get_option_logical, &
       get_option_string, get_flag
  public :: print_option, print_options, print_option_values, print_args
  public :: option_found, check_required_options
  public :: process_command_line, process_input_file
  public :: get_arg, get_num_args

  ! The following are useful
  public :: is_real, is_integer, is_logical

  ! ****************************************************************************

contains

  ! ** REAL OPTIONS ********************************************************** !

  subroutine define_option_real(opts,name,default,min,max,abbrev,required,description,group,var)
    ! Status: Proved
    ! Postcondition: Defined(opt)
    implicit none
    type(options_t),  target, intent(inout) :: opts
    character(len=*), intent(in) :: name
    real(rk),         intent(in) :: default
    real(rk),         optional, intent(in) :: min, max
    character(len=*), optional, intent(in) :: abbrev
    logical,          optional, intent(in) :: required
    character(len=*), optional, intent(in) :: description
    character(len=*), optional, intent(in) :: group
    real(rk),         optional, target :: var

    type(opt_t), pointer :: opt

    opt => new_opt(opts,name,abbrev,required,description,group)

    opt%dtype = T_REAL
    opt%rval  = default
    opt%rmin  = -huge(1._rk)
    opt%rmax  = huge(1._rk)

    if (present(min)) opt%rmin = min
    if (present(max)) opt%rmax = max
    if (present(var)) opt%rvar => var
  end subroutine define_option_real


  subroutine get_option_real(opts,name,val)
    ! Status: reviewed
    implicit none
    type(options_t), target, intent(in) :: opts
    character(len=*), intent(in) :: name
    real(rk), intent(out) :: val

    type(opt_t), pointer :: opt

    opt => find_opt(opts,name=name,dtype=T_REAL)
    val = opt%rval
  end subroutine get_option_real


  subroutine set_value_real(opt,valstr,ierr)
    ! Status: proved
    ! Defined(opt) .and. ierr == 0
    !     ==>  IsReal(valstr) .and. Defined(opt%rval) and InBounds(opt%rval)
    implicit none
    type(opt_t), intent(inout) :: opt
    character(len=*), intent(in) :: valstr
    integer, intent(out) :: ierr

    integer :: ios

    ierr = 1
    if (.not. is_real(valstr)) then
       write (error_unit,'(3a)') "Error: parameter ", trim(valstr), " is not a valid real number."
       return
    end if
    read(valstr,*,iostat=ios) opt%rval
    if (ios .ne. 0) then
       write (error_unit,'(3a)') "Error: couldn't convert ", trim(valstr), " to a real number."
       return
    end if
    if (opt%rval < opt%rmin .or. opt%rval > opt%rmax) then
       write (error_unit,'(3a)') 'Error: value for option "', trim(opt%name), '" out of range.'
       write (error_unit,'(3(a,es15.8))') "Value: ", opt%rval, ", min: ", opt%rmin, ", max: ", opt%rmax
       return
    end if
    if (associated(opt%rvar)) then
       opt%rvar = opt%rval
    end if
    ierr = 0
  end subroutine set_value_real


  subroutine print_value_real(opt,unit)
    ! Status: reviewed
    implicit none
    type(opt_t), intent(in) :: opt
    integer, optional, intent(in) :: unit

    integer :: m_unit

    m_unit = output_unit
    if (present(unit)) m_unit = unit
    write (m_unit,'(2a,t30,es22.14e3)') trim(opt%name), ': ', opt%rval
  end subroutine print_value_real


  ! ** INTEGER OPTIONS ******************************************************* !


  subroutine define_option_integer(opts,name,default,min,max,abbrev,required,description,group,var)
    ! Status: reviewed
    implicit none
    type(options_t), target,  intent(inout) :: opts
    character(len=*), intent(in) :: name
    integer(ik),      intent(in) :: default
    integer(ik),      optional, intent(in) :: min, max
    character(len=*), optional, intent(in) :: abbrev
    logical,          optional, intent(in) :: required
    character(len=*), optional, intent(in) :: description
    character(len=*), optional, intent(in) :: group
    integer,          optional, target     :: var

    type(opt_t), pointer :: opt

    opt => new_opt(opts,name,abbrev,required,description,group)

    opt%dtype  = T_INTEGER
    opt%ival   = default
    opt%imin   = -huge(1_ik)
    opt%imax   = huge(1_ik)

    if (present(min)) opt%imin = min
    if (present(max)) opt%imax = max
    if (present(var)) opt%ivar => var
  end subroutine define_option_integer


  subroutine get_option_integer(opts,name,val)
    ! Status: reviewed
    implicit none
    type(options_t),  target, intent(in) :: opts
    character(len=*), intent(in)  :: name
    integer(ik),      intent(out) :: val

    type(opt_t), pointer :: opt

    opt => find_opt(opts,name=name,dtype=T_INTEGER)
    val = opt%ival
  end subroutine get_option_integer


  subroutine set_value_integer(opt,valstr,ierr)
    ! Status: Proved
    ! Defined(opt) .and. ierr == 0
    !     ==>  Defined(opt%ival) .and. IsInteger(valstr) .and. InBounds(opt%ival)
    implicit none
    type(opt_t), intent(inout) :: opt
    character(len=*), intent(in) :: valstr
    integer, intent(out) :: ierr

    integer :: ios

    ierr = 1
    if (.not. is_integer(valstr)) then
       write (error_unit,'(3a)') "Error: parameter ", trim(valstr), " is not a valid integer."
       ! ierr == 1 .and. .not.  is_integer(valstr)
       return
    end if
    read(valstr,*,iostat=ios) opt%ival
    if (ios .ne. 0) then
       write (error_unit,'(3a)') "Error: couldn't convert ", trim(valstr), " to an integer&
            & (may be too large)."
       ! ierr == 1 .and. .not. valid(opt%ival)
       return
    end if
    ! valid(opt%ival) .and. is_integer(valstr)
    if (opt%ival < opt%imin .or. opt%ival > opt%imax) then
       write (error_unit,'(3a)') 'Error: value for option "', trim(opt%name), '" out of range.'
       write (error_unit,'(3(a,i0))') "Value: ", opt%ival, ", min: ", opt%imin, ", max: ", opt%imax
       ! ierr == 1 .and. valid(opt%ival) .and. is_integer(valstr) .and. OutOfBounds(opts%ival)
       return
    end if
    ! opt%ival >= opt%imin .and. opt%ival <= opt%imax
    ! valid(opt%ival) .and. is_integer(valstr) .and. InBounds(opt%ival)
    if (associated(opt%ivar)) then
       opt%ivar = opt%ival
    end if
    ierr = 0
  end subroutine set_value_integer


  subroutine print_value_integer(opt,unit)
    ! Status: reviewed
    implicit none
    type(opt_t), intent(in) :: opt
    integer, optional, intent(in) :: unit

    integer :: m_unit

    m_unit = output_unit
    if (present(unit)) m_unit = unit
    write (m_unit,'(2a,t30,i0)') trim(opt%name), ': ', opt%ival
  end subroutine print_value_integer


  ! ** LOGICAL OPTIONS ******************************************************* !


  subroutine define_option_logical(opts,name,default,abbrev,required,description,group,var)
    ! Status: reviewed
    implicit none
    type(options_t),  target, intent(inout) :: opts
    character(len=*), intent(in) :: name
    logical,          intent(in) :: default
    character(len=*), optional, intent(in) :: abbrev
    logical,          optional, intent(in) :: required
    character(len=*), optional, intent(in) :: description
    character(len=*), optional, intent(in) :: group
    logical,          optional, target     :: var

    type(opt_t), pointer :: opt

    opt => new_opt(opts,name,abbrev,required,description,group)

    opt%dtype = T_LOGICAL
    opt%lval  = default
    if (present(var)) opt%lvar => var
  end subroutine define_option_logical


  subroutine get_option_logical(opts,name,val)
    ! Status: reviewed
    implicit none
    type(options_t),  target, intent(in) :: opts
    character(len=*), intent(in)  :: name
    logical,          intent(out) :: val

    type(opt_t), pointer :: opt

    opt => find_opt(opts,name=name,dtype=T_LOGICAL)
    val = opt%lval
  end subroutine get_option_logical


  subroutine set_value_logical(opt,valstr,ierr)
    ! Status: reviewed
    implicit none
    type(opt_t), intent(inout) :: opt
    character(len=*), intent(in) :: valstr
    integer, intent(out) :: ierr

    integer :: ios

    ierr = 1
    if (.not. is_logical(valstr)) then
       write (error_unit,'(3a)') "Error: parameter ", trim(valstr), " is not a valid logical value."
       return
    end if
    read(valstr,*,iostat=ios) opt%lval
    if (ios .ne. 0) then
       write (error_unit,'(3a)') "Error: couldn't convert ", trim(valstr), " to a logical value."
       return
    end if
    if (associated(opt%lvar)) then
       opt%lvar = opt%lval
    end if
    ierr = 0
  end subroutine set_value_logical


  subroutine print_value_logical(opt,unit)
    ! Status: reviewed
    implicit none
    type(opt_t), intent(in) :: opt
    integer, optional, intent(in) :: unit

    integer :: m_unit

    m_unit = output_unit
    if (present(unit)) m_unit = unit
    write (m_unit,'(2a,t30,l1)') trim(opt%name), ': ', opt%lval
  end subroutine print_value_logical


  ! ** STRING OPTIONS ******************************************************** !


  subroutine define_option_string(opts,name,default,abbrev,required,description,group,var)
    ! Status: reviewed
    implicit none
    type(options_t),  target, intent(inout) :: opts
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: default
    character(len=*), optional, intent(in) :: abbrev
    logical,          optional, intent(in) :: required
    character(len=*), optional, intent(in) :: description
    character(len=*), optional, intent(in) :: group
    character(len=opt_len), optional, target :: var

    type(opt_t), pointer :: opt

    opt => new_opt(opts,name,abbrev,required,description,group)

    opt%dtype = T_STRING
    opt%cval  = default
    if (present(var)) opt%cvar => var
  end subroutine define_option_string


  subroutine get_option_string(opts,name,val)
    ! Status: reviewed
    implicit none
    type(options_t),   target, intent(in) :: opts
    character(len=*), intent(in)  :: name
    character(len=*), intent(out) :: val

    type(opt_t), pointer :: opt

    opt => find_opt(opts,name=name,dtype=T_STRING)
    if (len_trim(opt%cval) > len(val)) then
       write (error_unit,'(a)') "get_option_string: string too long. &
            &Need to supply more storage space when calling this routine."
       stop
    end if
    val = opt%cval
  end subroutine get_option_string


  subroutine set_value_string(opt,valstr,ierr)
    ! Status: reviewed
    implicit none
    type(opt_t), intent(inout) :: opt
    character(len=*), intent(in) :: valstr
    integer, intent(out) :: ierr

    ierr = 1
    if (len_trim(valstr) > len(opt%cval)) then
       write (error_unit,'(a)') "set_value_string: value too long. Need to &
            &increase parameter opt_len in options.f90."
       return
    end if
    opt%cval = valstr
    if (associated(opt%cvar)) then
       opt%cvar = opt%cval
    end if
    ierr = 0
  end subroutine set_value_string


  subroutine print_value_string(opt,unit)
    ! Status: reviewed
    implicit none
    type(opt_t), intent(in) :: opt
    integer, optional, intent(in) :: unit

    integer :: m_unit

    m_unit = output_unit
    if (present(unit)) m_unit = unit
    write (m_unit,'(2a,t30,3a)') trim(opt%name), ': ', '"', trim(opt%cval), '"'
  end subroutine print_value_string


  ! ** FLAGS ***************************************************************** !


  subroutine define_flag(opts,name,abbrev,description,group,var)
    ! Status: reviewed
    implicit none
    type(options_t),  target, intent(inout) :: opts
    character(len=*), intent(in) :: name
    character(len=*), optional, intent(in) :: abbrev
    character(len=*), optional, intent(in) :: description
    character(len=*), optional, intent(in) :: group
    logical,          optional, target     :: var

    type(opt_t), pointer :: opt

    opt => new_opt(opts,name,abbrev,required=.false.,description=description,group=group)

    opt%dtype = T_FLAG
    opt%lval  = .false.
    if (present(var)) then
       opt%lvar => var
       opt%lvar = opt%lval
    end if
  end subroutine define_flag


  subroutine get_flag(opts,name,val)
    ! Status: reviewed
    implicit none
    type(options_t),  target, intent(in) :: opts
    character(len=*), intent(in)  :: name
    logical,          intent(out) :: val

    type(opt_t), pointer :: opt

    opt => find_opt(opts,name=name,dtype=T_FLAG)
    val = opt%lval
  end subroutine get_flag



  ! ** ROUTINES WHICH CALL THE SPECIALIZED ROUTINES ABOVE ******************** !


  subroutine print_option_values(opts,unit)
    ! Status: proved, assuming unit is valid for writing.
    implicit none
    type(options_t), target, intent(in) :: opts
    integer, optional, intent(in) :: unit

    type(opt_t), pointer :: opt
    integer :: iopt, m_unit

    m_unit = output_unit
    if (present(unit)) m_unit = unit
    write (m_unit,'(a)') "Option values: "

    do iopt=1,opts%nopts
       opt => opts%opts(iopt)
       select case (opt%dtype)
       case (T_INTEGER)
          call print_value_integer(opt,unit)
       case (T_REAL)
          call print_value_real(opt,unit)
       case (T_LOGICAL)
          call print_value_logical(opt,unit)
       case (T_FLAG)
          call print_value_logical(opt,unit)
       case (T_STRING)
          call print_value_string(opt,unit)
       case default
          stop "Error in print_option_values(): invalid type"
       end select
    end do
  end subroutine print_option_values


  subroutine set_opt(opt,valstr,overwrite,ierr)
    ! Set the value of an option, given a string representing the value.
    ! If the option has already been set, error out. Otherwise, mark the option
    ! as having been found and set opt%str.
    ! Note: opt must be a properly initialized/defined option structure.
    ! Status: reviewed
    ! ierr .ne. 0 ==> IsValid(valstr,opt)
    implicit none
    type(opt_t),      intent(inout) :: opt
    character(len=*), intent(in) :: valstr
    character(len=*), intent(in) :: overwrite
    integer, intent(out) :: ierr

    ierr = 1
    select case (upcase(overwrite))
    case ("ERROR")
       if (opt%found) then
          write (error_unit,'(3a)') 'Error: tried to set option "', trim(opt%name), '" twice.'
          return
       end if
    case ("YES")
    case ("NO")
       if (opt%found) then
          ierr = 0
          return
       end if
    case default
       write (error_unit,'(a)') "Error: invalid value for overwrite policy ('", trim(overwrite), "')"
       stop
    end select

    opt%found = .true.

    select case (opt%dtype)
    case (T_INTEGER)
       call set_value_integer(opt,valstr,ierr)
    case (T_REAL)
       call set_value_real(opt,valstr,ierr)
    case (T_LOGICAL)
       call set_value_logical(opt,valstr,ierr)
    case (T_FLAG)
       call set_value_logical(opt,valstr,ierr)
    case (T_STRING)
       call set_value_string(opt,valstr,ierr)
    case default
          stop "Error in set_opt(): invalid type"
    end select
    opt%str = valstr
  end subroutine set_opt


  ! ** GENERAL ROUTINES ********************************************************


  subroutine define_help_flag(opts,help_routine,group)
    implicit none
    type(options_t), target, intent(inout) :: opts
    interface
       subroutine help_routine(opts)
         import
         type(options_t), intent(in) :: opts
       end subroutine help_routine
    end interface
    character(len=*), optional, intent(in) :: group

    call define_flag(opts,"help",abbrev='h',description="Print this help message.",&
         group=group)
    opts%help_routine => help_routine
  end subroutine define_help_flag


  logical function is_logical(str)
    ! Return .true. if str represents a logical value.
    ! Status: proved
    implicit none
    character(len=*), intent(in) :: str

    character(len=len(str)) :: ustr

    ustr = upcase(str)
    is_logical = ustr == 'T' .or. ustr == 'F' .or. ustr == '.TRUE.' &
         .or. ustr == '.FALSE.' .or. ustr == 'TRUE' .or. ustr == 'FALSE'
  end function is_logical


  logical function is_integer(str)
    ! Return .true. if str represents an integer, i.e. has the form
    !   [+-]i_1...i_{n_i}
    ! where n_i > 0 and i_k are digits (0-9).
    ! Status: proved
    implicit none
    character(len=*), intent(in) :: str

    integer :: lt,pm,ni

    lt = len_trim(str)
    is_integer = .false.
    pm = count_leading_set(str,"+-")
    if (pm > 1) return
    if (pm == lt) return
    ni = count_leading_set(str(pm+1:lt),'0123456789')
    if (pm + ni .ne. lt) return
    is_integer = .true.
  end function is_integer


  logical function is_real(str)
    ! Return .true. if str represents a real number, i.e. has one of the forms
    !  (i)   [+-]i_1...i_{n_i}  where n_i > 0
    !  (ii)  [+-]i_1...i_{n_i}.d_1...d_{n_d} where n_i > 0 or n_d > 0
    !  (iii) [+-]i_1...i_{n_i}(E|e)[+-]e_1...e_{n_e} where n_i > 0, n_e > 0
    !  (iv)  [+-]i_1...i_ni.d_1...d_{n_d}(E|e)[+-]e_1...e_{n_e}
    !         where (n_i > 0 or n_d > 0) and n_e > 0.
    ! Here i_k, d_k and e_k are digits (0-9).
    ! Status: proved
    implicit none
    character(len=*), intent(in) :: str

    integer :: lt,pm,ni,dot,nd,ee,pm2,ne

    lt = len_trim(str)

    ! Notes:
    !   1. if x = count_leading_set(str(lb+1:ub),set) and ub > lb then it's
    !      guaranteed that lb + x <= ub. Therefore either lb + x < ub or
    !      lb + x == ub.
    !   2. if x = count_leading_set(str(lb+1:ub),set), then str(lb+1:lb+x) ∈ set.

    is_real = .false.
    pm = count_leading_set(str,"+-")
    if (pm > 1) return
    if (pm == lt) return
    ! pm < lt, str(1:pm) ∈ '+-'
    ni = count_leading_set(str(pm+1:lt),'0123456789')
    ! pm+ni ≤ lt, str(pm+1:pm+ni) ∈ '0123456789'
    if (pm + ni == lt) then
       if (ni > 0) is_real = .true. ! Case (i)
       return
    end if
    ! pm+ni < lt
    dot = count_leading_set(str(pm+ni+1:lt),'.')
    ! pm+ni+dot ≤ lt, str(pm+ni+1:pm+ni+dot) ∈ '.'
    if (dot .gt. 1) return
    if (pm+ni+dot == lt) then
       if (ni > 0) is_real = .true. ! Case (ii), n_d = 0
       return
    end if
    ! pm+ni+dot < lt
    nd = count_leading_set(str(pm+ni+dot+1:lt),'0123456789')
    if (ni == 0 .and. nd == 0) return
    ! pm+ni+dot+nd ≤ lt, str(pm+ni+dot+1:pm+ni+dot+nd) ∈ '0123456789', (ni > 0 or nd > 0)
    if (pm+ni+dot+nd == lt) then
       is_real = .true. ! Case (ii)
       return
    end if
    ! etc.
    ee = count_leading_set(str(pm+ni+dot+nd+1:lt),'Ee')
    if (ee .ne. 1) return
    if (pm+ni+dot+nd+ee == lt) return
    pm2 = count_leading_set(str(pm+ni+dot+nd+ee+1:lt),'+-')
    if (pm+ni+dot+nd+ee+pm2 == lt) return
    ne = count_leading_set(str(pm+ni+dot+nd+ee+pm2+1:lt),'0123456789')
    if (ne == 0) return
    if (pm+ni+dot+nd+ee+pm2+ne .ne. lt) return
    is_real = .true. ! Case (iii) or (iv)
  end function is_real


  function option_found(opts,name)
    ! Status: proved
    implicit none
    type(options_t), target,  intent(in) :: opts
    character(len=*), intent(in) :: name
    logical :: option_found

    type(opt_t), pointer :: opt

    opt => find_opt(opts,name)
    option_found = opt%found
  end function option_found


  function new_opt(opts,name,abbrev,required,description,group) result(opt)
    ! Allocate and append a new option to the end of the options list, and
    ! initialize the generic fields which apply to all data types.
    ! On exit: Defined(name,abbrev,descr,required,found,ivar,lvar,cvar,rvar)
    ! Status: proved
    implicit none
    type(options_t), target, intent(inout) :: opts
    character(len=*), intent(in) :: name
    character(len=*), optional, intent(in) :: abbrev
    logical,          optional, intent(in) :: required
    character(len=*), optional, intent(in) :: description
    character(len=*), optional, intent(in) :: group
    ! Return value
    type(opt_t), pointer :: opt

    integer :: iopt

    if (len(name) == 0) stop "Error: empty name for option."
    if (.not. is_name(name)) then
       write (error_unit,*) "Error: invalid option name: ", trim(name)
       stop
    end if
    ! is_name(name)
    if (present(abbrev)) then
       if (.not. (len(abbrev) == 1 .and. is_abbrev_char(abbrev))) then
          write (error_unit,*) "Error: invalid option abbreviation: '", abbrev, "'"
          stop
       end if
    end if
    ! present(abbrev) ==> is_abbrev_char(abbrev)

    do iopt=1,opts%nopts
       call check_name(opts%opts(iopt),name,abbrev)
    end do
    ! name .ne. opt(i)%name for i=1,..,opts%nopts
    opts%nopts = opts%nopts + 1
    if (opts%nopts > maxopts) stop "Error: Need to increase maxopts parameter in options.f90."

    opt => opts%opts(opts%nopts)
    ! Initialize generic fields
    opt%name = name
    opt%abbrev = ' '
    opt%descr = ''
    opt%required = .false.
    opt%found = .false.
    opt%dtype = T_NONE
    opt%group = ''
    nullify(opt%ivar)
    nullify(opt%lvar)
    nullify(opt%rvar)
    nullify(opt%cvar)
    if (present(abbrev)) opt%abbrev = abbrev
    if (present(description)) opt%descr = description
    if (present(group)) opt%group = group
    if (present(required)) opt%required = required
    ! Defined(name,abbrev,descr,required,found)
  end function new_opt


  subroutine print_options(opts,unit,group,style)
    ! Status: ok
    implicit none
    type(options_t), target, intent(in) :: opts
    integer, optional, intent(in) :: unit
    character(len=*), optional, intent(in) :: group, style

    integer :: m_unit, iopt
    type(opt_t), pointer :: opt

    m_unit = output_unit
    if (present(unit)) m_unit = unit

    if (opts%nopts > 0) then
       do iopt=1,opts%nopts
          opt => opts%opts(iopt)
          if (present(group)) then
             if (opt%group .ne. group) cycle
          end if
          call print_option(opts,opt%name,unit,style)
       end do
    end if
  end subroutine print_options


  subroutine print_option(opts,name,unit,style)
    implicit none
    type(options_t), target, intent(in) :: opts
    character(len=*),  intent(in) :: name
    integer, optional, intent(in) :: unit
    character(len=*), optional, intent(in) :: style

    integer, parameter :: format_buf_size = 4*descr_len + 2*opt_len
    character(len=format_buf_size) :: buf
    character(2*opt_len) :: synopsis
    type(opt_t), pointer :: opt
    integer :: m_unit

    m_unit = output_unit
    if (present(unit)) m_unit = unit

    opt => find_opt(opts,name)
    if (opt%dtype == T_FLAG) then
       call format_flag_synopsis(synopsis,opt%name,opt%abbrev,style)
    else
       call format_val_synposis(synopsis,opt%name,opt%abbrev,style)
    end if
    call format_opt(buf,synopsis,opt%descr)
    write (m_unit,'(a)',advance='no') trim(buf)
  end subroutine print_option


  subroutine format_val_synposis(buf,name,abbrev,style)
    implicit none
    character(len=*), intent(out) :: buf
    character(len=*), intent(in) :: name, abbrev
    character(len=*), optional, intent(in) :: style

    character(len=16) :: m_style

    m_style = "cmdline"
    if (present(style)) m_style = style

    if (abbrev == ' ') then
       if (2*len_trim(name) + 3 > len(buf)) then
          write (error_unit,'(a)') "Error in format_val_synopsis: need to increase buffer size"
          stop
       end if
       if (upcase(m_style) == "CMDLINE") then
          buf = "--" // trim(name) // "=" // upcase(trim(name))
       else if (upcase(m_style) == "FILE") then
          buf = trim(name)
       else
          stop "Error: invalid formatting style."
       end if
    else
       if (3*len_trim(name) + len_trim(abbrev) + 7 > len(buf)) then
          write (error_unit,'(a)') "Error in format_val_synopsis: need to increase buffer size"
          stop
       end if
       if (upcase(m_style) == "CMDLINE") then
          buf = "-" // trim(abbrev) // " " // upcase(trim(name)) &
               // ", " // "--" // trim(name) // "=" // &
               upcase(trim(name))
       else if (upcase(m_style) == "FILE") then
          buf = trim(name)//" (-"//trim(abbrev)//")"
       else
          stop "Error: invalid formatting style."
       end if
    end if
  end subroutine format_val_synposis


  subroutine format_flag_synopsis(buf,name,abbrev,style)
    implicit none
    character(len=*) :: buf
    character(len=*), intent(in) :: name, abbrev
    character(len=*), optional, intent(in) :: style

    character(len=16) :: m_style

    m_style = "cmdline"
    if (present(style)) m_style = style

    if (abbrev == ' ') then
       if (len_trim(name) + 2 > len(buf)) then
          write (error_unit,'(a)') "Error in format_flag_synopsis: need to increase buffer size"
          stop
       end if
       if (upcase(m_style) == "CMDLINE") then
          buf = "--" // trim(name)
       else if (upcase(m_style) == "FILE") then
          buf = trim(name)
       else
          stop "Error: invalid formatting style."
       end if
    else
       if (len_trim(name) + len_trim(abbrev) + 5 > len(buf)) then
          write (error_unit,'(a)') "Error in format_flag_synopsis: need to increase buffer size"
          stop
       end if
       if (upcase(m_style) == "CMDLINE") then
          buf = "-" // trim(abbrev) // ", " // "--" // trim(name)
       else if (upcase(m_style) == "FILE") then
          buf = trim(name)//" (-"//trim(abbrev)//")"
       else
          stop "Error: invalid formatting style."
       end if
    end if
  end subroutine format_flag_synopsis


  function upcase(string) result(upper)
    ! From http://www.star.le.ac.uk/~cgp/fortran.html
    ! Status: reviewed
    character(len=*), intent(in) :: string
    character(len=len(string)) :: upper

    integer :: j

    do j=1,len(string)
       if(string(j:j) >= "a" .and. string(j:j) <= "z") then
          upper(j:j) = achar(iachar(string(j:j)) - 32)
       else
          upper(j:j) = string(j:j)
       end if
    end do
  end function upcase


  function is_name_char(c)
    ! This function determines which characters are allowed in option names.
    ! At minimum, must exclude quotation marks and '='.
    ! Status: reviewed
    implicit none
    character(len=1), intent(in) :: c
    logical :: is_name_char

    integer :: ic

    ic = iachar(c)
    is_name_char = (ic >= 37 .and. ic <= 126) .and. .not. in(c,name_char_excludes)
    ! TODO: Allow extended ascii?
  end function is_name_char


  function is_name(str)
    ! Names must be nonempty and consist only of name characters.
    ! Status: proved
    implicit none
    character(len=*), intent(in) :: str
    logical :: is_name

    integer :: i

    if (len_trim(str) .eq. 0) then
       is_name = .false.
       return
    end if
    is_name = .true.
    do i=1,len_trim(str)
       is_name = is_name .and. is_name_char(str(i:i))
    end do
  end function is_name


  function is_abbrev_char(c)
    ! This function determines which characters are allowed as abbreviations.
    ! Status: reviewed
    implicit none
    character(len=1), intent(in) :: c
    logical :: is_abbrev_char

    ! Allow all name characters except numbers and '.', '-' and '+' (these must
    ! be excluded to allow numeric arguments)
    is_abbrev_char = is_name_char(c) .and. (c < '0' .or. c > '9') .and. c .ne. '.' &
         .and. c .ne. '+' .and. c .ne. '-'
  end function is_abbrev_char


  subroutine format_opt(buf,name,descr)
    ! Format the name/synopsis and description of an option in a readable
    ! format, inserting line breaks in the description as necessary.
    implicit none
    character(len=*), intent(out) :: buf
    character(len=*), intent(in) :: name, descr

    integer :: bufidx, last_blank_didx, last_blank_bufidx, col, didx

    !--<name>-------------------<description...>
    !---------------------------<description cont...>
    ! or
    !--<..........name............>
    !---------------------------<description...>
    !---------------------------<description cont...>

    ! Write <name> and advance to the description column, on the next line if
    ! necessary
    if (name_column + len_trim(name) + 1 + descr_column > len(buf)) then
       write (error_unit,'(a)') "format_opt: need to increase format_buf_size in options.f90."
       stop
    end if
    buf = ' '
    buf(name_column:) = trim(name)
    bufidx = name_column + len_trim(name)
    if (bufidx >= descr_column) then
       buf(bufidx:bufidx) = NEW_LINE('a')
       bufidx = bufidx + descr_column
    else
       bufidx = descr_column
    end if
    col = descr_column

    ! Write <description>, wrapping lines as necessary. Notes:
    ! 1. Each non-blank character is printed exactly once
    ! 2. Line wrapping occurs at spaces if possible
    ! 3. Newlines are honored
    ! 4. All characters are printed between col=descr_column and max_column,
    !    inclusive
    didx = 1
    last_blank_didx = 0
    do while (didx <= len_trim(descr) .and. bufidx <= len(buf))
       ! descr(didx:didx): Next charcter to print
       ! descr_column <= col <= max_column + 1
       ! last_blank_didx == 0 .or. last_blank_didx >= beginning_of_line
       ! last_blank_didx == 0 .or. descr(last_blank_didx) == ' '
       ! last_blank_didx == 0 .or. descr(last_blank_bufidx) == ' '
       if (descr(didx:didx) .ne. new_line('a')) then
          if (descr(didx:didx) == ' ') then
             last_blank_didx = didx
             last_blank_bufidx = bufidx
          end if
          if (col <= max_column) then
             ! place character
             buf(bufidx:bufidx) = descr(didx:didx)
             didx = didx + 1
             bufidx = bufidx + 1
             col = col + 1
          else ! col > max_column
             ! Wrap line:
             if (last_blank_didx > 0) then
                ! Back up to last blank
                buf(last_blank_bufidx:bufidx) = ' '
                bufidx = last_blank_bufidx
                didx = last_blank_didx+1
             else
                ! No blanks: back up one and hyphenate
                didx = didx - 1
                buf(bufidx-1:bufidx-1) = '-'
             end if
             go to 100
          end if
       else ! descr(didx:didx) .eq. new_line('a')
          didx = didx + 1
          go to 100
       end if
       cycle
       ! Linebreak
100    buf(bufidx:bufidx) = new_line('a')
       bufidx = bufidx + descr_column
       last_blank_didx = 0
       col = descr_column
    end do

    if (bufidx > len(buf)) then
       write (error_unit,'(a)') "format_opt: need to increase format_buf_size in options.f90."
       stop
    end if
    buf(bufidx:bufidx) = new_line('a')
  end subroutine format_opt


  subroutine print_args(opts,unit)
    ! Status: reviewed
    implicit none
    type(options_t), target, intent(in) :: opts
    integer, optional, intent(in) :: unit

    integer :: iarg, m_unit, ierr, nargs
    character(len=opt_len) :: arg

    m_unit = output_unit
    if (present(unit)) m_unit = unit

    write (m_unit,'(a)') 'Arguments: '
    call get_num_args(opts,nargs)
    do iarg=1,nargs
       call get_arg(opts,iarg,arg,ierr)
       if (ierr .ne. 0) stop "Error in print_args"
       write (m_unit,'(3a)') '"', trim(arg), '"'
    end do
  end subroutine print_args


  function find_opt(opts,name,abbrev,dtype,assert,group) result(opt)
    ! Find the struct for an existing option by either name or abbreviation.
    !
    ! 1. If dtype is present, will also assert that the datatype is the same as
    !    that passed in.
    ! 2. By default, program will terminate if the option can't be found. If
    !    assert is present and .false., the function will instead return a null
    !    pointer.
    ! Status: Reviewed
    !
    ! Precondition: 1 <= i <= opts%nopts ==> Defined(opts%opts(i))
    ! Postcondition: associated(opt) ==> Defined(opt)
    implicit none
    type(options_t),  target, intent(in) :: opts
    character(len=*), optional, intent(in) :: name
    character(len=1), optional, intent(in) :: abbrev
    integer,          optional, intent(in) :: dtype
    logical,          optional, intent(in) :: assert
    character(len=*), optional, intent(in) :: group

    type(opt_t), pointer :: opt
    integer :: iopt
    logical :: m_assert, found

    if (.not. present(name) .and. .not. present(abbrev)) then
       stop "find_opt(): must be called with either name or abbrev."
    else if (present(name) .and. present(abbrev)) then
       stop "find_opt(): should not be called with both name and abbrev."
    end if
    if (present(assert)) then
       m_assert = assert
    else
       m_assert = .true.
    end if

    found = .false.
    do iopt=1,opts%nopts
       ! Loop invariant: found == .false.
       opt => opts%opts(iopt)
       if (present(group)) then
          if (opt%group .ne. group) cycle
       end if
       if (present(name)) then
          if (opt%name == name) found = .true.
       else if (present(abbrev)) then
          if (opt%abbrev == abbrev) found = .true.
       end if
       ! found <==> opt%name == name .or. (present(abbrev) .and. opt%abbrev == abbrev)
       !            .and. opt%group == group
       if (found .and. present(dtype)) then
          if (dtype .ne. opt%dtype) then
             write (error_unit,'(a,a)') "find_opt(): Datatype doesn't match for option: ", trim(name)
             stop
          end if
       end if
       ! found <==> [opt%name == name .or. (present(abbrev) .and. opt%abbrev == abbrev)]
       !            .and. (present(dtype) --> opt%dtype == dtype) .and. opt%group == group
       if (found) return
       ! found == .false.
    end do

    nullify(opt)
    if (m_assert) then
       if (present(name)) then
          write (error_unit,'(3a)',advance='no') "find_opt(): Option ", trim(name), " doesn't exist"
       else if (present(abbrev)) then
          write (error_unit,'(3a)',advance='no') "find_opt(): Option ", trim(abbrev), " doesn't exist"
       end if
       if (present(group)) then
          write (error_unit,'(a,a)') " in group ", trim(group)
       else
          write (error_unit,'(a)') ""
       end if
       stop
    end if
  end function find_opt


  subroutine store_arg(opts,str)
    ! Status: proved
    implicit none
    type(options_t), target, intent(inout) :: opts
    character(len=*), intent(in) :: str

    if (opts%nargs == maxargs) stop "Error: need to increase maxargs in options.f90"
    opts%nargs = opts%nargs + 1
    opts%args(opts%nargs) = str
  end subroutine store_arg


  subroutine getarg_check(iarg,buf,status)
    ! Status: Proved
    ! status == 0 ==> Defined(buf)
    implicit none
    integer, intent(in) :: iarg
    character(len=*), intent(out) :: buf
    integer, intent(out) :: status

    integer :: l

    if (iarg > command_argument_count()) stop "Error in getarg_check"
    buf = ''
    call get_command_argument(iarg,buf,status=status,length=l)
    if (status .ne. 0) then
       ! If the value was truncated or the retrieval fails, status .ne. 0.
       ! Otherwise status == 0.
       write (error_unit,'(a,i0,a,i0)') "Error retrieving command argument ", &
            iarg, " status: ", status
       if (l > len(buf)) then
          write (error_unit,'(a,i0,a)') "Error: command argument ", iarg, &
               " too long. May need to increase opt_len in options.f90."
       end if
       return
    end if
  end subroutine getarg_check


  subroutine process_command_line(opts,ierr,group)
    ! Process the command-line arguments for the program, detecting options
    ! passed and storing their values in the options structure.
    ! If group is passed, only options with this group name are processed. All
    ! others are considered as non-existent.
    ! If the command-line arguments are invalid (options which don't exist,
    ! etc.), then ierr .ne. 0 is returned.
    ! If an internal buffer is too short, a message is printed and the program
    ! stops.
    ! If the command-line arguments are processed succesfully, ierr=0 is
    ! returned.
    !
    ! The command line has the form
    !
    ! <program_name> <term1> ... <termN>
    !
    ! Where <term> is any of:
    !
    !  (1)  -<f1><f2>...<fN>
    !         where each <fi> is a 1-character abbreviation for a flag
    !  (2)  -<f1><f2>...<fN><opt> <value>
    !         where <opt> is an option abbreviation, and <value> is the next term
    !  (3)  --<flag_name>
    !  (4)  --<flag_name>=<value>
    !  (5)  --<opt_name> <value>
    !         where <value> is the next term
    !  (6)  --<opt_name>=<value>
    !  (7)  -- (double dashes)
    !       (If this appears by itself, all subsequent terms are considered
    !       arguments.)
    !  (8)  <numeric>, stored as an argument. E.g. -10, 1.23, -.23, 1E10, +0.34
    !  (9)  term which does not begin with a dash (stored as an argument)
    !  (10) - (A single dash by itself is allowed as an argument)
    !
    ! If a term begins with - or -- but doesn't fall into one of the above
    ! categories, it is considered an error, and ierr .ne. 0 is returned.
    ! Status: reviewed
    implicit none
    type(options_t),  target, intent(inout) :: opts
    integer, optional, intent(out) :: ierr
    character(len=*), optional, intent(in) :: group

    type(opt_t), pointer :: opt
    character(len=opt_len) :: buf, name, val
    character(len=1) :: eqlc, a
    integer :: iarg, max, j, mierr
    logical :: help

    max = command_argument_count()

    buf = ''
    iarg = 0
    do while (iarg < max)
       iarg = iarg + 1
       call getarg_check(iarg,buf,mierr)
       if (mierr .ne. 0) goto 99
       if (buf(1:1) == '-' .and. is_abbrev_char(buf(2:2))) then
          ! Short options: Case (1) or (2)
          do j=2,len_trim(buf)
             ! Find the option struct
             a = buf(j:j)
             opt => find_opt(opts,abbrev=a,assert=.false.,group=group)
             if (.not. associated(opt)) then
                write (error_unit,'(3a)',advance='no') 'Error: unknown option "-', buf(j:j), '"'
                if (present(group)) then
                   write (error_unit,'(3a)') ' in group "', trim(group), '"'
                else
                   write (error_unit,'(a)') ''
                end if
                mierr = 1
                goto 99
             end if
             ! associated(opt) => Defined(opt)
             ! Get the option value
             if (opt%dtype == T_FLAG) then
                ! Flags: -<abbrev>, no value allowed
                val = ".true."
                ! IsFlag(opt) .and. Defined(val)
             else
                ! Non-flag options: -<abbrev> <value>
                iarg = iarg + 1
                if (j .ne. len_trim(buf) .or. iarg > max) then
                   write (error_unit,'(3a)') 'Error: Option "-', buf(j:j), '" requires an argument.'
                   mierr = 1
                   goto 99
                end if
                ! j == len_trim(buf) .and. iarg <= max
                call getarg_check(iarg,val,mierr)
                if (mierr .ne. 0) goto 99
                ! mierr .eq. 0 => Defined(val) (may be blank)
                ! j == len_trim(buf) .and. Defined(val)
             end if
             ! Defined(val) .and. (IsFlag(opt) .or. (j == len_trim(buf))) .and. Defined(opt)
             ! Set the option value
             call set_opt(opt,val,"error",mierr)
             if (mierr .ne. 0) goto 99
             ! IsValid(val,opt)
          end do
       else if (buf(1:2) == '--' .and. is_name_char(buf(3:3))) then
          ! Long options
          call parse_long_option(buf,name,eqlc,val,mierr)
          if (mierr .ne. 0) then
             write (error_unit,'(2a)') 'Error: invalid option string "', trim(buf), '"'
             goto 99
          end if
          ! IsNameString(name) .and. eqlc ∈ " =" .and. (eqlc == '=' ==> Defined(val))
          ! Find option
          opt => find_opt(opts,name=name,assert=.false.,group=group)
          if (.not. associated(opt)) then
             write (error_unit,'(3a)',advance='no') 'Error: unknown option "--', trim(name), '"'
             if (present(group)) then
                write (error_unit,'(3a)') ' in group "', trim(group), '"'
             else
                write (error_unit,'(a)') ''
             end if
             mierr = 1
             goto 99
          end if
          ! Defined(opt)
          ! Get the option value
          if (opt%dtype == T_FLAG) then
             ! IsFlag(opt)
             if (eqlc .eq. ' ') then
                ! Case (3) --<flag_name>
                val = '.true.'
             else
                ! eqlc == '=', so Defined(val)
                ! Case (4) --<flag_name>=<value>
                ! (val is already set, do nothing)
             end if
             ! Defined(val)
          else
             if (eqlc == ' ') then
                ! Case (5) --<opt_name> <value>
                iarg = iarg + 1
                if (iarg > max) then
                   write (error_unit,'(3a)') 'Error: option "--', trim(name), '" requires an argument.'
                   mierr = 1
                   goto 99
                end if
                call getarg_check(iarg,val,mierr)
                if (mierr .ne. 0) goto 99
                ! Defined(val)
             else
                ! eqlc == '='. so Defined(val)
                ! Case (6) --<opt_name>=<value>
                ! (val is already set, do nothing)
             end if
             ! Defined(val)
          end if
          ! Defined(opt) .and. Defined(val)
          ! Set the option value
          call set_opt(opt,val,"error",mierr)
          if (mierr .ne. 0) goto 99
          ! IsValid(val,opt) .and. ValueSet(val,opt)
       else if (buf == "--") then
          ! Case (7) Stop processing options
          do while (iarg < max)
             iarg = iarg + 1
             call getarg_check(iarg,buf,mierr)
             if (mierr .ne. 0) goto 99
             call store_arg(opts,buf)
          end do
       else if (buf(1:1) == '-' .and. in(buf(2:2), '-0123456789.')) then
          ! Case (8) Numeric
          if (.not. is_real(buf)) then
             ! Note is_integer(buf) ==> is_real(buf), so we just check the latter
             write (error_unit,'(3a)') 'Error: expected a numeric argument: "', trim(buf), '"'
             mierr = 1
             goto 99
          end if
          call store_arg(opts,buf)
       else if (buf(1:1) .ne. '-') then
          ! Case (9) Doesn't start with a dash
          call store_arg(opts,buf)
       else if (buf == '-') then
          ! Case (10) Just a dash
          call store_arg(opts,buf)
       else
          ! Anything else
          write (error_unit,'(3a)') 'Error: invalid argument: "', trim(buf), '"'
          mierr = 1
          goto 99
       end if
    end do
    mierr = 0
    if (associated(opts%help_routine)) then
       call get_flag(opts,'help',help)
       if (help) then
          call opts%help_routine(opts)
          mierr = 3
       end if
    end if
    if (present(ierr)) ierr = mierr
    return
99  if (associated(opts%help_routine)) then
       write (error_unit,'(a)') "Try using -h for more info."
    end if
    if (.not. present(ierr)) then
       stop
    else
       ierr = mierr
    end if
  end subroutine process_command_line


  subroutine check_required_options(opts,ierr)
    ! Status: proved
    implicit none
    type(options_t),  target, intent(in) :: opts
    integer, optional, intent(out) :: ierr

    type(opt_t), pointer :: opt
    integer :: iopt, mierr

    mierr = 0
    do iopt=1,opts%nopts
       opt => opts%opts(iopt)
       if (opt%required .and. .not. opt%found) then
          write (error_unit,'(3a)') 'Error: missing required parameter: "', &
               trim(opt%name), '"'
          mierr = 2
          if (associated(opts%help_routine)) then
             write (error_unit,'(a)') "Try using -h for more info."
          end if
          goto 99
       end if
    end do
99  if (present(ierr)) then
       ierr = mierr
    else
       if (mierr .ne. 0) stop
    end if
  end subroutine check_required_options


  subroutine parse_long_option(str,name,eqlc,val,ierr)
    ! Check and parse a long option of the form (i) "--<name>" or (ii)
    ! "--<name>=<val>".
    ! Input:
    !   str: String of the form above, possibly with trailing spaces.
    !        Here <name> must consist of characters for which is_name_char()
    !        is true, and must have at least one such character.
    ! Output:
    !   name:  The name of the option.
    !   eqlc:  For case (i), eqlc == ' '. For case (ii), eqlc == '='.
    !   val:   For case (i), val == ''. For case (ii), if val is a quoted string,
    !          the text between the quotes; otherwise, all characters between the
    !          '=' sign and the end of the string, excluding trailing spaces.
    !   ierr:  Upon success, 0. If the string is not formatted correctly, 1.
    ! On exit:
    !   ierr .eq. 0 ==> IsNameString(name) .and. eqlc ∈ " =" .and.
    !    (eqlc == '=' ==> HasValue(val))
    implicit none
    character(len=*), intent(in) :: str
    character(len=*), intent(out) :: name
    character(len=1), intent(out) :: eqlc
    character(len=*), intent(out) :: val
    integer, intent(out) :: ierr

    integer :: name_end, val_begin, val_end, lt

    lt = len_trim(str)
    ierr = 1
    val = ""
    eqlc = ' '

    ! Ensure there's at least one valid character for the name
    if (lt < 3) return
    if (str(1:2) .ne. "--") return
    if (.not. is_name_char(str(3:3))) return

    ! Parse name
    name_end = 2 + count_leading(str(3:),is_name_char)
    name = str(3:name_end)

    ! Parse '=' sign.
    if (name_end == lt) then
       ierr = 0
       return
    end if
    eqlc = str(name_end+1:name_end+1)
    if (eqlc .ne. '=') return

    ! Parse value
    val_begin = name_end+2
    val_end = lt
    if (val_begin > val_end) then
       ! Empty value
       ierr = 0
       return
    end if
    call unquote(str(val_begin:val_end),val,ierr)
  end subroutine parse_long_option


  subroutine unquote(str,val,ierr)
    ! Unquote a string, if appropriate.
    ! Input:
    !   str:  A string.
    !   val:  Unquoted version of str, as appropriate (see code).
    !   ierr: If str is processed successfully, 0. If not, 1.
    ! Status: proved
    implicit none
    character(len=*), intent(in) :: str
    character(len=*), intent(out) :: val
    integer, intent(out) :: ierr

    integer :: lt

    lt = len_trim(str)
    ierr = 1
    val = ''

    ! Case (i): length = 0
    if (lt == 0) then
       ierr = 0
       return
    end if

    ! Case (ii): length = 1. Must not be a quote
    if (lt == 1) then
       if (is_quote(str)) then
          return
       else
          val = str(1:1)
          ierr = 0
          return
       end if
    end if

    ! Case (iii): length > 1. If str begins with a quote, we try to unquote,
    ! flagging an error if the last nonblank character does not match the initial
    ! quote. If str does not begin with a quote, return str itself. (We do not
    ! skip leading spaces to unquote.)
    if (is_quote(str(1:1))) then
       if (str(lt:lt) .eq. str(1:1)) then
          ierr = 0
          val = str(2:lt-1)
       end if
    else
       val = str
       ierr = 0
    end if
  end subroutine unquote


  logical function is_quote(c)
    implicit none
    character(len=1), intent(in) :: c

    is_quote = c == '"' .or. c == "'"
  end function is_quote


  function count_leading(str,p) result(count)
    ! Count the number of leading characters in str for which the predicate p is
    ! true.
    ! Status: proved
    implicit none
    character(len=*), intent(in) :: str
    interface
       logical function p(c)
         character(len=1), intent(in) :: c
       end function p
    end interface
    integer :: count, i

    count = 0
    do i=1,len(str)
       if (p(str(i:i))) then
          count = count + 1
       else
          exit
       end if
    end do
  end function count_leading


  logical function in(c,set)
    ! Determine whether the character c is in the set 'set'.
    ! Status: proved
    implicit none
    character(len=1), intent(in) :: c
    character(len=*), intent(in) :: set

    integer :: i

    in = .false.
    do i=1,len(set)
       in = c == set(i:i)
       if (in) return
    end do
  end function in


  function count_leading_set(str,set) result(count)
    ! Count the number of leading characters in str which are contained in the
    ! set 'set'.
    ! Status: proved
    implicit none
    character(len=*), intent(in) :: str,set
    integer :: count, i

    count = 0
    do i=1,len(str)
       if (in(str(i:i),set)) then
          count = count + 1
       else
          exit
       end if
    end do
  end function count_leading_set


  subroutine check_name(opt,name,abbrev)
    ! Status: proved
    ! Postcondition: opt%name .ne. name .and. (present(abbrev) ==> opt%abbrev .ne. abbrev)
    implicit none
    type(opt_t),      intent(in) :: opt
    character(len=*), intent(in) :: name
    character(len=1), optional, intent(in) :: abbrev

    if (opt%name == name) then
       write (error_unit,'(3a)') 'Error: duplicate definition of option "', &
            trim(name), '"'
       stop
    end if
    if (present(abbrev)) then
       if (opt%abbrev == abbrev) then
          write (error_unit,'(3a)') 'Error: duplicate definition of option "-', &
               trim(abbrev), '"'
          stop
       end if
    end if
  end subroutine check_name


  subroutine get_num_args(opts,num)
    implicit none
    type(options_t), target, intent(in) :: opts
    integer, intent(out) :: num

    num = opts%nargs
  end subroutine get_num_args


  subroutine get_arg(opts,index,str,ierr)
    implicit none
    type(options_t), intent(in) :: opts
    integer, intent(in) :: index
    character(len=*), intent(out) :: str
    integer, intent(out) :: ierr

    ierr = 0
    if (index > opts%nargs .or. index < 1) then
       ierr = 1
       return
    end if
    str = opts%args(index)
  end subroutine get_arg


  ! ** INPUT FILE PROCESSING ************************************************* !


  subroutine process_input_file(opts,filename,ierr,group,overwrite,delim_char,comment_chars)
    ! Read input options from file
    ! Input/output:
    !   opts: Input options structure. Input file is taken from opts%arg1. On
    !         exit, fields are updated according to values in the file.
    ! Output:
    !   ierr:  0 upon success, nonzero if an error occured
    ! Notes:
    !   Blank lines and comments
    use iso_fortran_env, only: input_unit
    implicit none
    type(options_t), target, intent(inout) :: opts
    character(len=*), intent(in) :: filename
    integer, intent(out)   :: ierr
    character(len=*), optional, intent(in) :: group, overwrite
    character(len=1), optional, intent(in) :: delim_char
    character(len=2), optional, intent(in) :: comment_chars

    type(opt_t), pointer :: opt
    integer :: unit, idx, nchar, status
    character(len=1024) :: val, name
    character :: c

    character(len=1) :: m_delim_char
    character(len=2) :: m_comment_chars
    character(len=8) :: m_overwrite

    m_delim_char = ':'
    m_comment_chars = '#!'
    m_overwrite = "ERROR"
    if (present(delim_char)) m_delim_char = delim_char
    if (present(comment_chars)) m_comment_chars = comment_chars
    if (present(overwrite)) m_overwrite = overwrite

    if (is_name_char(m_delim_char)) then
       stop "Error: invalid delim character -- must not be a name character."
    end if
    do idx=1,2
       if (is_name_char(m_comment_chars(idx:idx))) then
          stop "Error: invalid comment character -- must not be a name character."
       end if
    end do

    ierr = 1
    unit=10
    open(unit,file=filename,access='stream',status='old')

    idx = 1
    status = 0
    do while (status .eq. 0)
       ! Skip blanks and newlines
       call skip_chars(unit,blank//crlf,idx,nchar,status)
       if (status .ne. 0) exit
       ! Skip comment lines
       call peek(unit,idx,c,status)
       if (status .ne. 0) exit
       if (in(c,m_comment_chars)) then
          call skip_until_char(unit,crlf,idx,nchar,status)
          cycle
       end if

       ! Ok, at beginning of non-blank line, read option & value
       call parse_opt(unit,m_delim_char,m_comment_chars,idx,name,val,status)
       if (status .ne. 0) exit

       ! Lookup & set the option
       opt => find_opt(opts,name=name,assert=.false.,group=group)
       if (.not. associated(opt)) then
          write (error_unit,'(3a)',advance='no') 'Error: unknown option "', trim(name), '"'
          if (present(group)) then
             write (error_unit,'(3a)') ' in group "', trim(group), '"'
          else
             write (error_unit,'(a)') ''
          end if
          return
       end if
       call set_opt(opt,val,m_overwrite,status)
       if (status .ne. 0) return

       ! Go to end of line
       call skip_until_char(unit,crlf,idx,nchar,status)
    end do
    if (status > 0) then
       ierr = status
    else
       ierr = 0
    end if
  end subroutine process_input_file


  subroutine parse_opt(unit,delim_char,comment_chars,idx,name,val,ierr)
    ! Parse an option from an input file, allowing for comments at the end of the line.
    ! Input:
    !   str: string in the form
    !          <name><delim_char><value>[<comment_char><...>]
    !        where blanks may appear between any of the elements above. If
    !        <value> is a quoted string, only the content between the quotes is
    !        returned. Quoted strings may extend over several lines.
    !   delim_char:     Character to separate names from values (usually ':' or '=')
    !   comment_chars:  Characters considered to begin comments (usually '#' or '!')
    ! Input/output:
    !   idx:  Current position in file. Incremented for each character read.
    ! Output:
    !   name: The name of the option
    !   val:  The value
    !   ierr: Upon success, 0. Otherwise 1.
    implicit none
    integer,          intent(in) :: unit
    character(len=1), intent(in) :: delim_char
    character(len=*), intent(in) :: comment_chars
    integer,          intent(inout) :: idx
    character(len=*), intent(out) :: name, val
    integer,          intent(out) :: ierr

    character(len=len(comment_chars)+2) :: end_chars
    integer :: nchar, ios, beg

    character :: c

    ierr = 1
    val = ''
    name = ''
    beg = idx

    call skip_chars(unit,blank,idx,nchar,ios)
    if (ios .ne. 0) return

    call read_while(unit,is_name_char,idx,name,nchar,ios)
    if (nchar .eq. 0) then
       write (error_unit,'(a,i0,a)') 'Error: expected option name at position ', &
            idx, ' of input file'
       return
    end if
    if (ios < 0) then
       write (error_unit,'(3a)') 'Error: unexpected end of file after "', &
            trim(name), '" in input file (expected option value)'
       return
    end if
    if (ios > 0) return

    call skip_chars(unit,blank,idx,nchar,ios)
    if (ios < 0) then
       write (error_unit,'(3a)') 'Error: unexpected end of file found while&
            &processing option "', trim(name), '" in input file'
       return
    end if
    if (ios > 0) return

    call skip_chars(unit,delim_char,idx,nchar,ios)
    if (nchar .eq. 0) then
       write (error_unit,'(3a,i0,a)',advance='no') &
            "Error: expected '", delim_char, ''' at position ', idx, ' of input file'
       if (len_trim(name) > 0) &
            write (error_unit,'(3a)',advance='no') ' (after "', trim(name), '")'
       write (error_unit,'(a)') ''
       return
    end if

    call skip_chars(unit,blank,idx,nchar,ios)
    if (ios > 0) return
    if (ios < 0) then
       ! End of file -- ok
       ierr = ios
       return
    end if

    call peek(unit,idx,c,ios)
    if (ios .ne. 0) stop "Unexpected error in parse_opt" ! Expected a character here
    if (c == '"' .or. c == "'") then
       ! End of the value is the next occurence of the same quote -- no exceptions
       idx = idx + 1
       call read_until_char(unit,c,idx,val,nchar,ios)
       if (ios > 0) return
       if (ios < 0) then
          write (error_unit,'(2a)') 'Error: unterminated quote found while reading&
               & value of option "', trim(name), '"'
       end if
       idx = idx + 1
    else
       ! No quote: read until end of line, comment character, or end of file
       end_chars = comment_chars//crlf
       call read_until_char(unit,end_chars,idx,val,nchar,ios)
       if (ios > 0) return
    end if
    ierr = ios
  end subroutine parse_opt


  subroutine peek(unit,idx,c,ios)
    implicit none
    integer, intent(in) :: unit
    integer, intent(inout) :: idx
    character, intent(out) :: c
    integer, intent(out) :: ios

    read (unit,pos=idx,iostat=ios) c
  end subroutine peek


  subroutine read_while(unit,p,idx,buf,nchar,ios)
    implicit none
    integer, intent(in) :: unit
    interface
       logical function p(c)
         character(len=1), intent(in) :: c
       end function p
    end interface
    integer, intent(inout) :: idx
    character(len=*), intent(out) :: buf
    integer, intent(out) :: nchar, ios

    character :: c

    nchar = 0
    read (unit,pos=idx,iostat=ios) c
    do while (ios .eq. 0 .and. p(c))
       nchar = nchar + 1
       buf(nchar:nchar) = c
       idx = idx + 1
       read (unit,pos=idx,iostat=ios) c
    end do
  end subroutine read_while


  subroutine read_until_char(unit,chars,idx,buf,nchar,ios)
    implicit none
    integer, intent(in) :: unit
    character(len=*), intent(in) :: chars
    integer, intent(inout) :: idx
    character(len=*), intent(out) :: buf
    integer, intent(out) :: nchar, ios

    character :: c

    nchar = 0
    read (unit,pos=idx,iostat=ios) c
    do while (ios .eq. 0 .and. .not. in(c,chars))
       nchar = nchar + 1
       buf(nchar:nchar) = c
       idx = idx + 1
       read (unit,pos=idx,iostat=ios) c
    end do
  end subroutine read_until_char


  subroutine skip_until_char(unit,chars,idx,nchar,ios)
    implicit none
    integer, intent(in) :: unit
    character(len=*), intent(in) :: chars
    integer, intent(inout) :: idx
    integer, intent(out) :: nchar, ios

    character :: c

    nchar = 0
    read (unit,pos=idx,iostat=ios) c
    do while (ios .eq. 0 .and. .not. in(c,chars))
       nchar = nchar + 1
       idx = idx + 1
       read (unit,pos=idx,iostat=ios) c
    end do
  end subroutine skip_until_char


  subroutine skip_chars(unit,chars,idx,nchar,ios)
    ! Postconditions:
    !   Let beg := value of 'idx' on entry. Then
    !     i.   The characters at positions beg, beg+1, ..., idx-1 are in 'chars',
    !     ii.  nchar = the number of characters skipped
    !                = idx - beg,
    !     iii. If ios .eq. 0, the character at position 'idx' is not in 'chars'.
    ! Status: proved
    implicit none
    integer, intent(in) :: unit
    character(len=*), intent(in) :: chars
    integer, intent(inout) :: idx
    integer, intent(out) :: nchar, ios

    character :: c

    nchar = 0
    read (unit,pos=idx,iostat=ios) c
    ! beg := idx
    ! 1. in(file[beg:idx-1],chars)
    ! 2. (ios .eq. 0) --> c = file[idx]
    do while (ios .eq. 0 .and. in(c,chars))
       ! a. in(file[idx:idx],chars)
       !    ==> in(file[beg:idx],chars)
       nchar = nchar + 1
       idx = idx + 1
       ! 1. in(file[beg:idx-1],chars)
       read (unit,pos=idx,iostat=ios) c
       ! 2. (ios .eq. 0) --> c = file[idx]
    end do
    ! 1. in(file[beg:idx-1],chars)
    ! 2. ios .ne. 0 .or. in(c,chars)
  end subroutine skip_chars


end module options
