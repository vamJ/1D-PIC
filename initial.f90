subroutine initialize()
    use Global
    use moduleElectron
    use moduleIon
    implicit none
    
    integer :: i
    real(8) :: VxTemp,VyTemp,VzTemp,rr
    
    call init_random_seed
    do i=1,nelectron
        call maxwellian1(vxtemp,vytemp,vztemp,vet)
        electron(2,i)=vxtemp
        electron(3,i)=vytemp
        electron(4,i)=vztemp
        
    end do
    
    do i=1,nion
        call maxwellian1(vxtemp,vytemp,vztemp,vit)
        ion(2,i)=vxtemp
        ion(3,i)=vytemp
        ion(4,i)=vztemp
        
    end do
    
    do i=1,nelectron
        call RANDOM_NUMBER(rr)
        electron(1,i)=xleft+rr*xright
        ion(1,i)=electron(1,i)
    end do
    
    return
    
end subroutine
    
    
subroutine init_random_seed()
      implicit none
      
      INTEGER :: i, n, clock
      INTEGER,ALLOCATABLE :: seed(:)

      CALL RANDOM_SEED(size = n)
      ALLOCATE(seed(n))

      CALL SYSTEM_CLOCK(COUNT=clock)

      seed = clock + 37 * (/ (i - 1, i = 1, n) /)
      CALL RANDOM_SEED(PUT = seed)

      DEALLOCATE(seed)
end
    
subroutine maxwellian1(vxtemp,vytemp,vztemp,vt)
    use Global
    use moduleElectron
    use moduleIon
    use field
    implicit none
    
    integer :: i,flag,n
    real(8) :: Rs,R1,R2,Rx,Rr
    real(8) :: costheta,sintheta
    real(8) :: vxtemp,vytemp,vztemp,vt
    
    flag=1
    
        
        call RANDOM_NUMBER(Rr)
10      call RANDOM_NUMBER(Rs)
        call RANDOM_NUMBER(r1)
        call RANDOM_NUMBER(r2)
        if((r1*r1+r2*r2)>1.d0) then
            go to 10
        end if
        
        costheta=(r1*r1-r2*r2)/(r1*r1+r2*r2)
        if (r1>0.5d0) then
            sintheta=2.d0*r1*r2/(r1*r1+r2*r2)
        else 
            sintheta=-2.d0*r1*r2/(r1*r1+r2*r2)
        end if
        
       if(flag==1) then 
        vxtemp=vt*dsqrt(-2.0*log(1.d0-Rs))*costheta
        flag=2
        go to 10
       else 
        vytemp=vt*dsqrt(-2.0*log(1.d0-Rs))*costheta
        vztemp=vt*dsqrt(-2.0*log(1.d0-Rs))*sintheta
        flag=1
        
       end if
   
        

    end subroutine
    
subroutine maxwellian2(vxtemp,vytemp,vztemp,vt)
    use Global
    use moduleElectron
    use moduleIon
    use field
    implicit none
    
    integer :: i,flag,n
    real(8) :: Rs,Rx,Rr,Rtheta
    real(8) :: costheta,sintheta
    real(8) :: vxtemp,vytemp,vztemp,vt
    
    call RANDOM_NUMBER(Rr)
    call RANDOM_NUMBER(Rs)
    call RANDOM_NUMBER(Rtheta)
    
    vxtemp=vt*(-1.d0+2.0*Rs)
    vytemp=vt*dsqrt(1-(-1+2.0*Rs)**2)*dcos(2.0*pi*Rtheta)
    vztemp=vt*dsqrt(1-(-1+2.0*Rs)**2)*dsin(2.0*pi*Rtheta)

end subroutine