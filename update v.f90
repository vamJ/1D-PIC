subroutine updatev(n,particle,flag)
    use Global
    use moduleElectron
    use moduleIon
    use field
    implicit none
    
    integer :: i,n,flag
    real(8) :: RR,vxtemp,vytemp,vztemp
    real(8) :: particle(4,n)
    
    call RANDOM_NUMBER(RR)
    
    if(rr<=(vh*dt)) then
    
        if(flag==1) then !electron
            do i=1,n
                call maxwellian1(vxtemp,vytemp,vztemp,vet)
        
                energyelectroninject=energyelectroninject+0.5*cloudpar*melectron*(vxtemp*vxtemp-particle(2,i)*particle(2,i))
        
                particle(2,i)=vxtemp
                particle(3,i)=vytemp
                particle(4,i)=vztemp
            end do
        else !ion
            do i=1,n
                call maxwellian1(vxtemp,vytemp,vztemp,vit)
                
                energyioninject=energyioninject+0.5*cloudpar*mion*(vxtemp*vxtemp-particle(2,i)*particle(2,i))
                
                particle(2,i)=vxtemp
                particle(3,i)=vytemp
                particle(4,i)=vztemp
                
            end do
            end if
    
    end if
    
end subroutine