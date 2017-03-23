module Global
    implicit none
    integer,parameter :: nt=1e7 !number of time step
    integer,parameter :: nparmax=1e7 !number of particles
    integer,parameter :: nspecies=2
    integer,parameter :: nx=199 !number of grid
    real(8),parameter :: k=1.3807d-23
    real(8),parameter :: dx=5.d-4 !in unit of meter 0.5*Ldlby
    real(8),parameter :: Xleft=0.d0
    real(8),parameter :: Xright=xleft+(nx+1)*dx !in unit of meter
    real(8),parameter :: lx=Xright-Xleft
    real(8),parameter :: dt=1.0d-10!in unit of seconds
    integer,parameter :: step_dump=10000
    real(8),parameter :: plasmadensity=5.d14 !in unit of m-3
    real(8),parameter :: Bfield=0.d0
    real(8),parameter :: epsi=1.0/8.8542d-12 !1/epsion0
    real(8),parameter :: phi0=0.d0 !potential on lhs
    real(8),parameter :: phi1=0.d0 !potential on rhs
    real(8),parameter :: index=-1.0/8.8542d-12*dx*dx
    real(8),parameter :: pi=3.1415926
    real(8),parameter :: vh=1.0d8 !heating frequecy, to check if we update the vx
    !real(8),parameter :: cloudpar=1.d8
    real(8),parameter :: wp=dsqrt(1.6022d-19*1.6022d-19/9.1095d-31*epsi*plasmadensity) !plasma frequency
    !real(8),parameter :: wc=qm*B !cyclotron frequency
    

    end module
    
module moduleElectron
    use Global
    implicit none
    integer :: nelectron=1d4
    real(8),parameter :: Qelectron=-1.6022d-19
    real(8),parameter :: Melectron=9.1095d-31
    real(8),parameter :: Telectron=5.2d0 !in unit of eV
    real(8),parameter :: vet=dsqrt(k*11605*Telectron/melectron)
    real(8),parameter :: lambdade=dsqrt(8.8542d-12*Telectron/1.6022d-19/(plasmadensity)) !delby length
    !real(8),parameter :: lambdade=dsqrt(Telectron/(plasmadensity/1.d6))*7.40 !delby length
    real(8),parameter :: cloudpar=plasmadensity*lx/1.d4 !plasmadensity*lx/nelectron
    real(8) :: electron(4,nparmax)=0.d0 !1:position x, 2: vx, 3:vy, 4:vz.
    
    
    end module moduleElectron
    
module moduleIon!H+
    use Global
    use moduleElectron
    implicit none
    integer :: nion=1d4 !plasmadensity*lx/cloudpar
    real(8),parameter :: Qion=1.6022d-19
    real(8),parameter :: Mion=1.0*1864.0*9.1095d-31
    real(8),parameter :: Tion=0.0d0
    real(8),parameter :: vit=dsqrt(k*11605*Tion/Mion)
    real(8) :: ion(4,nparmax)=0.d0 !1:position x, 2: vx, 3:vy, 4:vz.
    real(8) :: ubohm=dsqrt(1.6022d-19*Telectron/mion)
    
end module moduleIon
    
module field
    use global
    implicit none
    real(8) :: ex(nx+2)=0.d0 !including the field on both side
    real(8) :: phi(nx+1)=0.d0
    real(8) :: rho(nx+1)=0.d0
    real(8) :: ne(nx+1)=0.d0
    real(8) :: ni(nx+1)=0.d0
    real(8) :: phi_ave(nx+1)=0.d0,ne_ave(nx+1)=0.d0,ni_ave(nx+1)=0.d0,rho_ave(nx+1)=0.d0,ex_ave(nx+2)
    real(8) :: vth=0.d0
    !real(8) :: energyloss(nt/step_dump)=0.d0,energyinject(nt/step_dump)=0.d0
    real(8) :: energyionloss=0.d0,energyelectronloss=0.d0,energyelectroninject=0.d0,energyioninject=0.d0
    real(8) :: averageenenrgy=0.d0
    
end module
    
