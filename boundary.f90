subroutine boundary(n,particle,flag)
    use global
    use moduleElectron
    use moduleIon
    use field
    implicit none
    
    
    integer :: i,j,n,flag!flag 1:ion ,flag 2:electron
    integer :: npartemp
    real(8) particle(4,n)
    real(8) :: VxTemp,VyTemp,VzTemp,rr
    
    
    if(flag==1) then !condition:ion
        do i=1,n
            if ((particle(1,i)>xright).or.(particle(1,i)<xleft)) then
                
                energyionloss=energyionloss+0.5*mion*cloudpar*ion(2,i)*ion(2,i)
                
                call maxwellian1(vxtemp,vytemp,vztemp,vit)
                ion(2,i)=vxtemp
                ion(3,i)=vytemp
                ion(4,i)=vztemp
                call RANDOM_NUMBER(rr)
                ion(1,i)=xleft+rr*xright
                
                nelectron=nelectron+1
                call maxwellian1(vxtemp,vytemp,vztemp,vet)
                electron(2,nelectron)=vxtemp
                electron(3,nelectron)=vytemp
                electron(4,nelectron)=vztemp
                electron(1,nelectron)=ion(1,i)
                
                energyelectroninject=energyelectroninject+0.5*melectron*cloudpar*electron(2,nelectron)*electron(2,nelectron)
                energyioninject=energyioninject+0.5*mion*cloudpar*ion(2,i)*ion(2,i)
            end if
            end do
    else !condition:electron
        npartemp=nelectron
        do i=npartemp,1,-1
            if((particle(1,i)>xright).or.(particle(1,i)<xleft))then
                
                energyelectronloss=energyelectronloss+0.5*melectron*cloudpar*electron(2,i)*electron(2,i)!calculate the energy loss on the boudary.
                
                particle(:,i)=particle(:,nelectron)
                nelectron=nelectron-1
                
            end if
        end do
    end if
    return
end subroutine