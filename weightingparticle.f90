subroutine weightingparticle(n,particle,q,flag) !CIC on charge density
    use Global
    use field
    use moduleElectron
    use moduleIon
    implicit none
    
    integer :: i,n,n1,flag
    real(8) :: particle(4,n)
    !real(8) :: rho(nx+1)
    real(8) :: x1,x2,q
    
    if(flag==1)then !electron
    do i=1,n
        n1=ceiling(particle(1,i)/dx)
        x1=dble(n1)-particle(1,i)/dx
        x2=1.0-x1
        if(n1==1)then
            ne(1)=ne(1)+cloudpar*x1/dx
            rho(1)=rho(1)+cloudpar*x1*q/dx
        else  
            ne(n1)=ne(n1)+cloudpar*x1/dx
            ne(n1-1)=ne(n1-1)+cloudpar*x2/dx
            rho(n1)=rho(n1)+cloudpar*x1*q/dx
            rho(n1-1)=rho(n1-1)+cloudpar*x2*q/dx
        end if
    end do
    else !ion
    do i=1,n
        n1=ceiling(particle(1,i)/dx)
        x1=dble(n1)-particle(1,i)/dx
        x2=1.0-x1
        if(n1==1)then
            ni(1)=ni(1)+cloudpar*x1/dx
            rho(1)=rho(1)+cloudpar*x1*q/dx
        else  
            ni(n1)=ni(n1)+cloudpar*x1/dx
            ni(n1-1)=ni(n1-1)+cloudpar*x2/dx
            rho(n1)=rho(n1)+cloudpar*x1*q/dx
            rho(n1-1)=rho(n1-1)+cloudpar*x2*q/dx
        end if
    end do    
        
    end if
    return    
end subroutine