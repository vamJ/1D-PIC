Program main
    use Global
    use FileIO
    use moduleElectron
    use moduleIon
    use field
    implicit none
    
    integer :: step,i
    real(8) :: averageenergy=0.d0
    
    call initialize()
    call weightingparticle(nelectron,electron(1:4,1:nelectron),qelectron,1)
    call weightingparticle(nion,ion(1:4,1:nion),qion,2)
    call fieldcalculate(0)
    !call setv(nelectron,electron(1:4,1:nelectron),ex(1:nx),qelectron,melectron)
    !call setv(nion,ion(1:4,1:nion),ex(1:nx),qion,mion)
    call datadump(0)
    
    write(*,*)"wp*dt=",wp*dt,"vet*dt",vet*dt,"vet=",vet,"m/s"
    write(*,*)"delby length",lambdade,"dx/delby length",dx/lambdade
    write(*,*)"cloudpar*nelectron/plasmadensity/lx=",cloudpar*nelectron/plasmadensity/lx,"lambdadelby*wp/vet=",lambdade*wp/vet
    write(*,*)"vh=",vet/5.0/lambdade
    do step=1,nt
        
        call moveparticle(nelectron,electron(1:4,1:nelectron),ex(1:nx+2),qelectron,melectron,step)
        call moveparticle(nion,ion(1:4,1:nion),ex(1:nx+2),qion,mion,step)
        call boundary(nion,ion(1:4,1:nion),1)
        call boundary(nelectron,electron(1:4,1:nelectron),2)
        !call updatev(nelectron,electron(1:4,1:nelectron),1)
        !call updatev(nion,ion(1:4,1:nion),2)
        
        rho(:)=0.d0
        ne(:)=0.d0
        ni(:)=0.d0
        
        call weightingparticle(nelectron,electron(1:4,1:nelectron),qelectron,1)
        call weightingparticle(nion,ion(1:4,1:nion),qion,2)
        call fieldcalculate(step)
        !write(*,*) "rho(100)=",rho(100),"phi(100)=",phi(100)
        call datadump(step)
        
        !do i=1,nelectron
        !    averageenergy=averageenergy+0.5*melectron*electron(2,i)*electron(2,i)
        !end do
        
        !averageenergy=averageenergy/nelectron/qion
        
        !write(*,*)"averageenergy/Telectron",averageenergy/telectron
        !averageenergy=0.d0
        
    end do
    
    
end program