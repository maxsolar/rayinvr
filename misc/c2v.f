      program c2v
c-----------------------------------------------------------------------
c Convert a column-oriented "c.in" velocity model into a rayinvr 
c v.in-formatted velocity model. Use "v2c" to convert from a rayinvr
c v.in-formatted file to a column-formatted file.
c-----------------------------------------------------------------------
      parameter (pcmax=100,player=25)
      real*4 x(player,pcmax),z(player,pcmax),xv1(player,pcmax),
     &     xv2(player,pcmax),v1(player,pcmax),
     &     v2(player,pcmax)
      integer numz(pcmax),numv1(pcmax),numv2(pcmax),
     &     nz(player),nv1(player),nv2(player),
     &     invz(player,pcmax),invv1(player,pcmax),invv2(player,pcmax)
      character*80 string
      character*130 ifile,ofile
c-----------------------------------------------------------------------
      write(*,*) 'Enter input file name [<return> for "c.in"]'
      read(*,1030) ifile
 1030 format(a130)
      if(ifile.eq.'') ifile='c.in'
      write(*,*) 'Enter output file name [<return> for "v.in"]'
      read(*,1030) ofile
      if(ofile.eq.'') ofile='v.in'
      open(7,file=ifile,status='old')
      open(8,file=ofile,status='unknown')
c-----------------------------------------------------------------------
      read(7,80) string
 80   format(a80)
      if(string(1:1).ne.'b'.and.string(1:1).ne.'B') then
         write(*,*) '* Error in file: c.in'
         write(*,*) '  First character of first line must be "B"'
         stop
      end if
      layer=1
      nz(layer)=1
 10   continue
      read(7,80,end=998) string
      if(string(1:1).eq.'b'.or.string(1:1).eq.'B') then
         nz(layer)=nz(layer)-1
         layer=layer+1
         nz(layer)=1
         goto 10
      elseif(string(1:1).eq.'v'.or.string(1:1).eq.'V') then
         goto 1000
      end if
      read(string,*) x(layer,nz(layer)),z(layer,nz(layer)),
     &     invz(layer,nz(layer))
      nz(layer)=nz(layer)+1
      goto 10
      
 1000 continue
      nz(layer)=nz(layer)-1
      xmax=-9e9
      do k=1,nz(layer)
         xmax=max(xmax,x(layer,k))
      end do
      write(*,*) 'xmax = ',xmax
      layer=1
      nv1(layer)=1
      nv2(layer)=1
 11   continue
      read(7,80,end=999) string
      if(string(1:1).eq.'v'.or.string(1:1).eq.'V') then
         nv1(layer)=nv1(layer)-1
         nv2(layer)=nv2(layer)-1
         layer=layer+1
         nv1(layer)=1
         nv2(layer)=1
         goto 11
      end if
      read(string,*) xz,vt1,iv1,vt2,iv2
      if(vt1.gt.0.) then
         xv1(layer,nv1(layer))=xz
         v1(layer,nv1(layer))=vt1
         invv1(layer,nv1(layer))=iv1
         nv1(layer)=nv1(layer)+1
      end if
      if(vt2.gt.0.) then
         xv2(layer,nv2(layer))=xz
         v2(layer,nv2(layer))=vt2
         invv2(layer,nv2(layer))=iv2
         nv2(layer)=nv2(layer)+1
      end if
      goto 11

 998  continue
      write(*,*) '* Error in file: "c.in:'
      write(*,*) '  Short file'

 999  continue
      nv1(layer)=nv1(layer)-1
      nv2(layer)=nv2(layer)-1

      write(*,*) 'Number of layers = ',layer

c Write out rayinvr v.in-formatted file

      do i=1,layer
         mz=nz(i)/10
         mv1=nv1(i)/10
         mv2=nv2(i)/10
         nremz=mod(nz(i),10)
         nremv1=mod(nv1(i),10)
         nremv2=mod(nv2(i),10)
c-----------------------------------------------------------------------   
c write out Z values
c
         if(mz.ge.1) then
            do ii=1,mz
               numz(ii)=10
            end do
         end if
         if(nremz.gt.0) then
            mz=mz+1
            numz(mz)=nremz
         end if
         n1=1
         n2=numz(1)
         do j=1,mz
            icont=1
            write(8,100) i,(x(i,nn),nn=n1,n2)
            if(j.eq.mz) icont=0
            write(8,100) icont,(z(i,nn),nn=n1,n2)
            write(8,101) (invz(i,nn),nn=n1,n2)
            if(j.lt.mz) then
               n1=n2+1
               n2=n2+numz(j+1)
            end if
         end do
c         
c write out v1 values
c
         if(mv1.ge.1) then
            do ii=1,mv1
               numv1(ii)=10
            end do
         end if
         if(nremv1.gt.0) then
            mv1=mv1+1
            numv1(mv1)=nremv1
         end if
         n1=1
         n2=numv1(1)
         do j=1,mv1
            icont=1
            if(j.eq.mv1) icont=0
            write(8,100) i,(xv1(i,nn),nn=n1,n2)
            write(8,100) icont,(v1(i,nn),nn=n1,n2)
            write(8,101) (invv1(i,nn),nn=n1,n2)
            if(j.lt.mv1) then
               n1=n2+1
               n2=n2+numv1(j+1)
            end if
         end do
         if(mv1.eq.0) then
            write(8,100) i,xmax
            write(8,100) 0,0.
            write(8,101) 0
         end if
c         
c write out v2 values
c
         if(mv2.ge.1) then
            do ii=1,mv2
               numv2(ii)=10
            end do
         end if
         if(nremv2.gt.0) then
            mv2=mv2+1
            numv2(mv2)=nremv2
         end if
         n1=1
         n2=numv2(1)
         do j=1,mv2
            icont=1
            if(j.eq.mv2) icont=0
            write(8,100) i,(xv2(i,nn),nn=n1,n2)
            write(8,100) icont,(v2(i,nn),nn=n1,n2)
            write(8,101) (invv2(i,nn),nn=n1,n2)
            if(j.lt.mv2) then
               n1=n2+1
               n2=n2+numv2(j+1)
            end if
         end do
         if(mv2.eq.0) then
            write(8,100) i,xmax
            write(8,100) 0,0.
            write(8,101) 0
         end if

      end do
c-----------------------------------------------------------------------
      mz=nz(i)/10
      nremz=mod(nz(i),10)
      if(mz.ge.1) then
         do ii=1,mz
            numz(ii)=10
         end do
      end if
      if(nremz.gt.0) then
         mz=mz+1
         numz(mz)=nremz
      end if
      n1=1
      n2=numz(1)
      do j=1,mz
         icont=1
         write(8,100) i,(x(i,nn),nn=n1,n2)
         if(j.eq.mz) icont=0
         write(8,100) icont,(z(i,nn),nn=n1,n2)
         if(j.lt.mz) then
            n1=n2+1
            n2=n2+numz(j+1)
         end if
      end do
c-----------------------------------------------------------------------
 100  format(I2, 1X, 10F8.3)
 101  format(3X, 10I8)

      stop
      end
