module reference_counting
  implicit none

  !=============================================================================
  !! reference counting object
  type rcobj_t
    integer :: refcount = 0
  contains
    procedure :: incref
    procedure :: decref
    procedure :: hasref
  end type rcobj_t

contains
  !=============================================================================
  subroutine incref(this)
    class(rcobj_t), intent(inout) :: this

    this%refcount = this%refcount + 1

  end subroutine incref
  !=============================================================================
  subroutine decref(this)
    class(rcobj_t), intent(inout) :: this

    this%refcount = this%refcount - 1

  end subroutine decref
  !=============================================================================
  pure function hasref(this)
    class(rcobj_t), intent(in) :: this
    logical :: hasref
    hasref = (this%refcount>0)
  end function hasref
  !=============================================================================

end module reference_counting
