subroutine fieldcalculate(ith)
    use Global
    use moduleElectron
    use moduleIon
    use field
    implicit none
    integer ith,i,j
    
    call poissonsolver(nx)
    
    do i=3,nx
        ex(i)=-(phi(i)-phi(i-2))/(2*dx)
    end do
    ex(2)=-(phi(2)-phi0)/(2.0*dx)
    ex(nx+1)=-(phi1-phi(nx-1))/(2.0*dx)
    ex(1)=2*ex(2)-ex(3)
    ex(nx+2)=2*ex(nx+1)-ex(nx)
    
    return
    end subroutine
    
subroutine poissonsolver(n)
    use Global
    use field
    use moduleElectron
    use moduleIon
    implicit none
    
    integer :: n,i
    !real(8) :: rho(n)
    real(8) :: a(n),b(n),c(n)
    real(8) :: c1(n),d(n),d1(n)

    
    a(1)=0.d0
    a(1:n)=1.d0
    b(1:n)=-2.d0
    c(1:n-1)=1.d0
    c(n)=0.d0
    
    d(1)=rho(1)*index-phi0
    d(n)=rho(n)*index-phi1
    do i=2,n-1
    d(i)=rho(i)*index
    end do
    
    c1(1)=c(1)/(b(1))
    do i=2,n-1
        c1(i)=c(i)/(b(i)-a(i)*c1(i-1))
    end do
    
    d1(1)=d(1)/(b(1))
    do i=2,n
        d1(i)=(d(i)-a(i)*d1(i-1))/(b(i)-a(i)*c1(i-1))
    end do
    
    !results for phi
    phi(n)=d1(n)
    do i=n-1,1,-1
        phi(i)=d1(i)-c1(i)*phi(i+1)
    end do
    
    
    
    return
end subroutine