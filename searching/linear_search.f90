function linear_search_existence(length, array, number) result(res)
    implicit none
    integer, intent(in) :: length
    integer, intent(in) :: array(length)
    integer, intent(in) :: number
    logical :: res
    integer :: i

    res = .false.
    do i = 1, length
        if (array(i) == number) then
            res = .true.
            exit
        end if
    end do
end function linear_search_existence

function linear_search_index(length, array, number) result(res)
    implicit none
    integer, intent(in) :: length
    integer, intent(in) :: array(length)
    integer, intent(in) :: number
    integer :: res
    integer :: i

    res = -1
    do i = 1, length
        if (array(i) == number) then
            res = i
            exit
        end if
    end do
end function linear_search_index

subroutine  linear_search_all(length, array, number)
    implicit none
    integer, intent(in) :: length
    integer, intent(in) :: array(length)
    integer, intent(in) :: number
    integer :: i

    do i = 1, length
        if (array(i) == number) then
            print *, i
        end if
    end do
end subroutine  linear_search_all

program linear_search
    implicit none
    integer :: array(10)
    integer :: number
    logical :: linear_search_existence
    integer :: linear_search_index

    array = [8, 2, 8, 4, 5, 6, 7, 8, 9, 8]
    number = 8

    print *, "Existence:", linear_search_existence(size(array), array, number)
    print *, "Index:", linear_search_index(size(array), array, number)
    print *, "All:"

    call linear_search_all(size(array), array, number)
end program linear_search