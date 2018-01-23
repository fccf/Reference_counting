module object
  use reference_counting
  implicit none

  type,extends(rcobj_t) :: obj_t
    integer,allocatable :: arr(:)
  contains
    procedure :: allocate
    procedure :: deallocate
    procedure :: print
  end type obj_t

contains
  !=============================================================================
  subroutine allocate(this,n)
    class(obj_t), intent(inout) :: this
    integer, intent(in) :: n

    allocate(this%arr(n))
    this%arr = 0
    call this%incref()

  end subroutine allocate
  !=============================================================================
  subroutine deallocate(this)
    class(obj_t), intent(inout) :: this

    call this%decref()
    if(this%hasref()) return
    deallocate(this%arr)

  end subroutine deallocate
  !=============================================================================
  subroutine print(this)
    class(obj_t), intent(inout) :: this

    if(allocated(this%arr)) then
      write(unit=6, fmt=*) this%arr
    else
      error stop "Unallocated object"
    endif

  end subroutine print
end module object
