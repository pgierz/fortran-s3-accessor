module fortran_s3_accessor
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello
    print *, "Hello, fortran-s3-accessor!"
  end subroutine say_hello
end module fortran_s3_accessor
