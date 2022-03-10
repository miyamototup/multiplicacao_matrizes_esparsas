module subrotinas
implicit none
integer :: ncol,linicial,lfinal,ntotal
integer,allocatable :: colunas(:),linhas(:)
double precision, allocatable :: elementos(:),u(:),v1(:),v2(:),b(:),A(:,:)
double precision ::tp1,tp2

contains

subroutine lc(k,p,n,l,c)
implicit none
integer :: i=0,j=1,k,l,m=0,n,p
integer,allocatable,intent(inout) :: c(:)
m=k
allocate(b(k))
open(unit=1,file='dados.csv')
do i=1,n

if (i<l) then
read(1,*)
end if
if (i>=l) then
read(1,*)b
c(j:k)=b
j=j+m
k=k+m
end if
end do
deallocate(b)
close(1)
end subroutine

subroutine ll(k,p,n,l,c)
implicit none
integer :: i=0,j=1,k,l,m=0,n,p
integer,allocatable,intent(inout) :: c(:)
m=k
allocate(b(k))
open(unit=1,file='dados.csv')
do i=1,n
if (i<l) then
read(1,*)
end if
if (i>=l) then
read(1,*)b
c(j:k)=b
j=j+m
k=k+m
end if
end do
deallocate(b)
close(1)
end subroutine


subroutine le(k,p,n,l,c)
implicit none
integer :: i=0,j=1,k,l,m=0,n,p
double precision,allocatable,intent(inout) :: c(:)
m=k
allocate(b(k))
open(unit=1,file='dados.csv')
do i=1,n
if (i<l) then
read(1,*)
end if
if (i>=l) then
read(1,*)b
c(j:k)=b
j=j+m
k=k+m
end if
end do
deallocate(b)
close(1)
end subroutine

subroutine m1(lin,col,el,ale,r)
implicit none
double precision,allocatable,intent(inout) :: r(:)
integer,allocatable,intent(in) :: lin(:),col(:)
double precision,allocatable,intent(in) :: el(:), ale(:)
integer :: i,j 
call cpu_time(tp1)
do j=1,496
r(lin(col(j):col(j+1)-1)) = &
r(lin(col(j):col(j+1)-1))+el(col(j):col(j+1)-1)*ale(j)
end do
call cpu_time(tp2)
end subroutine

subroutine m2(lin,col,el,ale,r,a)
implicit none
double precision,allocatable,intent(inout) :: a(:,:),r(:)
integer,allocatable,intent(in) :: lin(:),col(:)
double precision,allocatable,intent(in) :: el(:), ale(:)
integer :: i,j,k,n 
k=0
do i=1,496
j=1
n=col(i+1)-col(i)
if (n/=0) then
do while (j<=n)
a(lin(j+k),i)=el(j+k)
j=j+1
end do
k=k+j-1
end if
end do
call cpu_time(tp1)
r=matmul(a,ale)
call cpu_time(tp2)
end subroutine


end module
