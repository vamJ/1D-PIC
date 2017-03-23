Module FileIO
    implicit none
    contains 
    subroutine Dump1D(Name,Nre,Res1,Res2,Res3,Res4,Res5)
        implicit none
        integer i,Nre
        real(8) :: Res1(Nre)
        real(8),optional :: Res2(Nre),Res3(Nre),Res4(Nre),Res5(Nre)
        character(len=30) name,Filename
        write(filename,*) trim(Name),".dat"
        open (10,file=filename)
        !write(10,*) Nre
        do i=1,Nre
             if(present(Res5)) then
                      write(10,FMt="(5D21.13)") Res1(i),Res2(i),Res3(i),Res4(i),Res5(i)
             else if (present(Res4)) then
                      write(10,FMt="(4D21.13)") Res1(i),Res2(i),Res3(i),Res4(i)
             else if (present(Res3)) then
                      write(10,FMt="(3D21.13)") Res1(i),Res2(i),Res3(i)
             else if (present(Res2)) then
                      !write(10,FMt="(2D21.13)") Res1(Nre),Res2(Nre)
                      write(10,*) Res1(i),Res2(i)
             else
                      !write(10,FMt="(D21.13)") Res1(i)  
                      write(10,*) i,Res1(i)
             end if                 
        end do
        close(10)  
        return
     end subroutine Dump1D
    
    subroutine Dump2D(Name,Nx,Ny,Res)
        implicit none
        integer i,j,Nx,Ny
        real(8) :: Res(Nx,Ny)
        character(len=30) name,Filename
        logical alive
        write(filename,*) trim(Name),".dat"
        Write(*,*) "Saving ",trim(Name)," Please wait..."
        open (10,file=filename)
  !      write(10,*) Nx,Ny
        do i=1,Ny
               Write(10,FMt="(D21.13\)")   (Res(j,i),j=1,Nx)
               Write(10,*)  
        end do
        close(10)
        Write(*,*) "Save ",trim(Name),"Complete!"  
        return
    end subroutine Dump2D
    
end module FileIO