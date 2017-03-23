subroutine moveparticle(n,particle,ex,q,me,ith)
    !use field
    use Global
    use moduleElectron
    use moduleIon
    implicit none
    
    integer :: i,n1,n,ith
    real(8) :: ex(nx+2)
    real(8) :: extemp,x1,x2,me
    real(8) :: particle(4,n)
    real(8) :: q
    
    do i=1,n
        n1=ceiling(particle(1,i)/dx)
        x1=n1-particle(1,i)/dx
        x2=1.0-x1
        
        extemp=x1*Ex(n1+1)+x2*Ex(n1) !CIC of field/force on particles, ex(n+1) means the force on gird (n)
        
        particle(2,i)=particle(2,i)+extemp*q*dt/me
        particle(1,i)=particle(1,i)+particle(2,i)*dt
    
    end do

    return
    end subroutine
    
!subroutine setv(n,particle,ex,q,me) !move v of dt/2
!    use Global
!    use moduleElectron
!    use moduleIon
!    !use field
!    implicit none
    
!    integer :: i,n,n1
!    real(8) :: particle(4,n)
!    real(8) :: ex(nx),extemp,me
!    real(8) :: x1,x2,q
    
!    do i=1,n
!       n1=ceiling(particle(1,i)/dx)
!       x1=n1-particle(1,i)/dx
!       x2=dx-x1
!       extemp=x1/dx*Ex(n1)+x2/dx*Ex(n1-1)
       
!       particle(2,i)=particle(2,i)-0.5*extemp*q*dt/me  
!    end do
!    return
!end subroutine