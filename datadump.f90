subroutine datadump(step)
    use moduleelectron
    use global
    use moduleion
    use field
    use fileIO
    implicit none
    
    integer :: step,i
    Character(len=10) NC
    character(30) name
    
    
    phi_ave(:)=phi_ave(:)+phi(:)/step_dump
    ne_ave(:)=ne_ave(:)+ne(:)/step_dump
    ni_ave(:)=ni_ave(:)+ni(:)/step_dump
    rho_ave(:)=rho_ave(:)+rho(:)/step_dump
    ex_ave(:)=ex_ave(:)+ex(:)/step_dump
    
    
    if (mod(step,step_dump)==0)then
        
        write(NC,'(I4)') Step/step_dump
        
        name="electron x"//trim(adjustl(NC))
        call dump1d(name,nelectron,electron(1,1:nelectron))
        name="electron vx"//trim(adjustl(NC))
        call dump1d(name,nelectron,electron(2,1:nelectron))
       ! name="electron vy"//trim(adjustl(NC))
        !call dump1d(name,nelectron,electron(3,1:nelectron))
        !name="electron vz"//trim(adjustl(NC))
        !call dump1d(name,nelectron,electron(4,1:nelectron))
        name="ion x"//trim(adjustl(NC))
        call dump1d(name,nion,ion(1,1:nion))
        name="ion vx"//trim(adjustl(NC))
        call dump1d(name,nion,ion(2,1:nion))
        !name="ion vy"//trim(adjustl(NC))
       ! call dump1d(name,nion,ion(3,1:nion))
        !name="ion vz"//trim(adjustl(NC))
        !call dump1d(name,nion,ion(4,1:nion))
        name="potential"//trim(adjustl(NC))
        call dump1d(name,nx,phi(1:nx))
        name="potential_ave"//trim(adjustl(NC))
        call dump1d(name,nx,phi_ave(1:nx))
        name="rho"//trim(adjustl(NC))
        call dump1d(name,nx,rho(1:nx))
        name="rho_ave"//trim(adjustl(NC))
        call dump1d(name,nx,rho_ave(1:nx))
        name="ex"//trim(adjustl(NC))
        call dump1d(name,nx+2,ex(1:nx+2))
        name="ex_ave"//trim(adjustl(NC))
        call dump1d(name,nx+2,ex_ave(1:nx+2))
        name="ne"//trim(adjustl(NC)) 
        call dump1d(name,nx,ne(1:nx))
        name="ne_ave"//trim(adjustl(NC))
        call dump1d(name,nx,ne_ave(1:nx))
        name="ni"//trim(adjustl(NC)) 
        call dump1d(name,nx,ni(1:nx))
        name="ni_ave"//trim(adjustl(NC))
        call dump1d(name,nx,ni_ave(1:nx))
        
        do i=1,nelectron
            vth=vth+dsqrt(electron(2,i)*electron(2,i))
        end do
        vth=vth/nelectron
        
        
        write(*,*) "phi_ave(100)=",phi_ave(100)
        !write(*,*) "vth=",vth,"m/s"
        write(*,*) "energyelectroninject=",energyelectroninject/(step_dump*dt),"w"
        write(*,*) "energyioninject=",energyioninject/(step_dump*dt),"w"
        write(*,*) "energyionloss=",energyionloss/(step_dump*dt),"w"
        write(*,*) "energyelectronloss=",energyelectronloss/(step_dump*dt),"w"
        
        if(step==0) then
            open(unit=10,file='parameter.txt')
            write(10,*)"dt=",dt,"s"
            write(10,*)"dx=",dx,"m"
            write(10,*)"lx=",lx,"m"
            write(10,*)"delby lengh=",lambdade,"m"
            write(10,*)"vet=",vet,"m/s"
            write(10,*)"vit=",dt,"m/s"
            write(10,*)"ubohm",ubohm,"m/s"
            write(10,*)"wp=",wp,"rad/s"
            write(10,*)"wp*dt=",wp*dt
            write(10,*)"dx/delby lengh=",dx/lambdade
            write(10,*)"(wp*delby lengh)/vet=",wp*lambdade/vet
            
            close(10)
        end if
        
        
        
        energyelectroninject=0.d0
        energyioninject=0.d0
        energyionloss=0.d0
        energyelectronloss=0.d0
        vth=0.d0
        phi_ave(:)=0.d0
        ne_ave(:)=0.d0
        ni_ave(:)=0.d0
        rho_ave(:)=0.d0
        ex_ave(:)=0.d0
        
        write(*,*) "saving ",step/step_dump
    end if
    return
    
end subroutine