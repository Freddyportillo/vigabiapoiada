program viga_biapoiada_dinarray

    implicit none
    
        real, dimension (2) :: apx
        real, dimension (2,2) :: cc
        real :: lvig 
        integer :: nforces, nele, ii
        real, allocatable, dimension (:,:) :: Fx
        real, allocatable, dimension (:) :: le
        
        print*, 'insira a posição do apio 1 e 2 (m)'
        read*, apx
        print*, 'insira o comprimento da viga'
        read*, lvig
        print*, 'insira a quantidade de forcas a serem aplicadas'
        read*, nforces
        allocate (Fx(nforces,2))
        print*, 'insira a posicao das forcas (x,F)'
        read*, Fx
        deallocate (Fx)
        nele = nforces+1
        
!         gerando malha   
        allocate (le(nele))
        
        
        do ii = 1, nele
            if (ii == 1) then
            le(ii) = Fx(ii,1)
            else
            le(ii) = Fx(ii-1,1)
            end if
        end do
        deallocate(le)
        
        print*, le
            
        
        
    end program
