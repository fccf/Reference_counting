program test_refcount
  use object
  implicit none

  type(obj_t),target :: obj
  class(obj_t),pointer :: ptr1 => null()
  class(obj_t),pointer :: ptr2 => null()

  integer :: n= 5

  call obj%allocate(n)
  call obj%print()

  ptr1 => obj;call obj%incref()
  ptr2 => obj;call obj%incref()
  call deallocate(obj)

  call ptr1%print()
  call deallocate(ptr1)

  call ptr2%print()
  call deallocate(ptr2)

  print*, ptr2%hasref()


end program test_refcount
