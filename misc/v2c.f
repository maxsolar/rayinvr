      program c2v
c-----------------------------------------------------------------------
c Convert rayinvr v.in-formatted velocity model to a column-oriented 
c "c.in" velocity model. Use "c2v" to convert from column-oriented
c file to a rayinvr v.in-formatted file.
c-----------------------------------------------------------------------
      include 'v2c.par'
      include 'v2c.com'
      character*130 ifile,ofile
      common /ivar/ ivarv,ivarz
      data nzed/pncntr*1/,nvel/pinvel*1/
c-----------------------------------------------------------------------
      write(*,*) 'Enter input file name [<return> for "v.in"]'
      read(*,1030) ifile
 1030 format(a130)
      if(ifile.eq.'') ifile='v.in'
      write(*,*) 'Enter output file name [<return> for "c.in"]'
      read(*,1030) ofile
      if(ofile.eq.'') ofile='c.in'
      open(7,file=ofile,status='unknown')
      open(8,file=ifile,status='old')
c-----------------------------------------------------------------------
c Get xmin and xmax from v.in  

      call xminmax(ncont,xmin,xmax)
      if(xmin.eq.xmax) then
         xmin=0.
      end if
      write(*,*) 'xmin,xmax = ',xmin,xmax
      call readv(ncont,irec)
      call calmod(ncont,xmin,xmax)

c Write out boundaries

      do i=1,nlayer+1
         write(7,11) i
 11      format('B',1x,i2)
         j=1
 10      continue
         write(7,12) xm(i,j),zm(i,j),ivarz(i,j)
 12      format(f8.3,f8.3,1x,i2)
         if(xm(i,j+1).gt.xm(i,j)) then
            j=j+1
            goto 10
         end if
      end do

c Write out velocities

      do i=1,nlayer
         write(7,13) i
 13      format('V',1x,i2)
         j1=1
         j2=1
 20      continue
         if(xvel(i,j1,1).eq.xvel(i,j2,2)) then
            write(7,14) xvel(i,j1,1),vf(i,j1,1),ivarv(i,j1,1),
     &           vf(i,j2,2),ivarv(i,j2,2)
            if(xvel(i,j1+1,1).gt.xvel(i,j1,1).or.
     &           xvel(i,j2+1,2).gt.xvel(i,j2,2)) then
               if(xvel(i,j1+1,1).gt.xvel(i,j1,1)) then
                  j1=j1+1
               end if
               if(xvel(i,j2+1,2).gt.xvel(i,j2,2)) then
                  j2=j2+1
               end if
               goto 20
            end if
         elseif(xvel(i,j1,1).lt.xvel(i,j2,2)) then
            write(7,14) xvel(i,j1,1),vf(i,j1,1),ivarv(i,j1,1),0,0
            if(xvel(i,j1+1,1).gt.xvel(i,j1,1)) then
               j1=j1+1
               goto 20
            end if
         elseif(xvel(i,j1,1).gt.xvel(i,j2,2)) then
            write(7,14) xvel(i,j2,2),0,0,vf(i,j2,2),ivarv(i,j2,2)
            if(xvel(i,j2+1,2).gt.xvel(i,j2,2)) then
               j2=j2+1
               goto 20
            end if
         end if
 14      format(f8.3,f8.3,1x,i2,f8.3,1x,i2)
      end do

      close(8)

      stop
      end

c
c     ----------------------------------------------------------------
c
      subroutine xminmax(ncont,xmin,xmax)
c
      include 'v2c.par'
      include 'v2c.com'
c
c Read in rayinvr v.in file
c
      xmin=9.e10
      xmax=-9.e10
      ncont=1
      nrzmax=ppcntr/10
      nrvmax=ppvel/10
      do 202 icont=1,player+1
         nrz=1
         j1=1
         j2=10
11       if(nrz.gt.nrzmax) go to 211
         read(8,15,end=999) ilyr,(xm(icont,j),j=j1,j2)
         read(8,15,end=999) icnt,(zm(icont,j),j=j1,j2)
         read(8,245,end=99) 
15       format(i2,1x,10f8.3)
245      format(' ')
         nrz=nrz+1
         if(icnt.ne.1) go to 211
         j1=j1+10
         j2=j2+10
         go to 11
211      continue
         xmin=min(xmin,xm(icont,1))
         xmax=max(xmax,xm(icont,1))
         do j=2,j2
            if(xm(icont,j).gt.xm(icont,j-1)) then
               xmax=max(xmax,xm(icont,j))
            end if
         end do

         nrv=1
         j1=1
         j2=10
21       if(nrv.gt.nrvmax) go to 311
         read(8,15,end=999) ilyr,(xvel(icont,j,1),j=j1,j2)
         read(8,15,end=999) icnt,(vf(icont,j,1),j=j1,j2)
         read(8,245,end=999) 
         nrv=nrv+1
         if(icnt.ne.1) go to 311
         j1=j1+10
         j2=j2+10
         go to 21
311      continue

         nrv=1
         j1=1
         j2=10
31       if(nrv.gt.nrvmax) go to 411
         read(8,15,end=999) ilyr,(xvel(icont,j,2),j=j1,j2)
         read(8,15,end=999) icnt,(vf(icont,j,2),j=j1,j2)
         read(8,245,end=999) 
         nrv=nrv+1
         if(icnt.ne.1) go to 411
         j1=j1+10
         j2=j2+10
         go to 31
411      continue
         ncont=ncont+1
 202     continue    
c                 
99    nlayer=ncont-1
      rewind(8)
      return
 999  continue
      write(*,*) '* ERROR in FORTRAN routine xminmax'
      return
      end

c
c     ----------------------------------------------------------------
c
      subroutine readv(ncont,irec)
c
      include 'v2c.par'
      include 'v2c.com'
      common /ivar/ ivarv,ivarz
c
c Read in rayinvr v.in file
c
      irec=3
      ncont=1
      nrzmax=ppcntr/10
      nrvmax=ppvel/10
      do 202 icont=1,player+1
         nrz=1
         j1=1
         j2=10
11       if(nrz.gt.nrzmax) go to 211
         read(8,15,end=999) ilyr,(xm(icont,j),j=j1,j2)
         read(8,15,end=999) icnt,(zm(icont,j),j=j1,j2)
         read(8,235,end=99) (ivarz(icont,j),j=j1,j2)
15       format(i2,1x,10f8.3)
235      format(3x,10i8)
         nrz=nrz+1
         if(icnt.ne.1) go to 211
         j1=j1+10
         j2=j2+10
         go to 11
211      continue
         irec=irec+1
         nrv=1
         j1=1
         j2=10
21       if(nrv.gt.nrvmax) go to 311
         read(8,15,end=999) ilyr,(xvel(icont,j,1),j=j1,j2)
         read(8,15,end=999) icnt,(vf(icont,j,1),j=j1,j2)
         read(8,235,end=999) (ivarv(icont,j,1),j=j1,j2)
         nrv=nrv+1
         if(icnt.ne.1) go to 311
         j1=j1+10
         j2=j2+10
         go to 21
311      continue
         irec=irec+1
         nrv=1
         j1=1
         j2=10
31       if(nrv.gt.nrvmax) go to 411
         read(8,15,end=999) ilyr,(xvel(icont,j,2),j=j1,j2)
         read(8,15,end=999) icnt,(vf(icont,j,2),j=j1,j2)
         read(8,235,end=999) (ivarv(icont,j,2),j=j1,j2)
         nrv=nrv+1
         if(icnt.ne.1) go to 411
         j1=j1+10
         j2=j2+10
         go to 31
411      continue
         irec=irec+1
         ncont=ncont+1
 202     continue    
c                 
99    nlayer=ncont-1
      

      return
 999  continue
      write(*,*) '* ERROR in FORTRAN routine ridumppar'
      return
      end

c
c     ----------------------------------------------------------------
c
c     version 1.3  Aug 1992
c
      subroutine calmod(ncont,xmin,xmax)
c
c     calculate model parameters now for use later in program
c
c
      include 'v2c.par'
      real xa(2*(ppcntr+ppvel))
      include 'v2c.com'

      do i=1,player
         do j=1,ptrap
            do k=1,4
               vm(i,j,k)=0.
            end do
         end do
      end do

c
      do 10 i=1,ncont
         nzed(i)=1
         do 20 j=1,ppcntr
            if(abs(xm(i,j)-xmax).lt..0001) go to 30
            nzed(i)=nzed(i)+1
20       continue
30       if(nzed(i).gt.1) then
           do 40 j=1,nzed(i)-1
              if(xm(i,j).ge.xm(i,j+1)) go to 999
40         continue
           if(abs(xm(i,1)-xmin).gt..001.or.
     +        abs(xm(i,nzed(i))-xmax).gt..001) go to 999
         else
            xm(i,1)=xmax
         end if
10    continue
      do 11 i=1,nlayer
         nvel(i,1)=1
         do 21 j=1,ppvel
            if(abs(xvel(i,j,1)-xmax).lt..0001) go to 31
            nvel(i,1)=nvel(i,1)+1
21       continue
31       if(nvel(i,1).gt.1) then
           do 41 j=1,nvel(i,1)-1
              if(xvel(i,j,1).ge.xvel(i,j+1,1)) go to 999
41         continue
           if(abs(xvel(i,1,1)-xmin).gt..001.or.abs(xvel(i,nvel(i,1),1)-
     +        xmax).gt..001) go to 999
         else
           if(vf(i,1,1).gt.0.) then
             xvel(i,1,1)=xmax
           else
             if(i.eq.1) then
               go to 999
             else
               nvel(i,1)=0
             end if
           end if
         end if
11    continue
      do 12 i=1,nlayer
         nvel(i,2)=1
         do 22 j=1,ppvel
            if(abs(xvel(i,j,2)-xmax).lt..0001) go to 32
            nvel(i,2)=nvel(i,2)+1
22       continue
32       if(nvel(i,2).gt.1) then
           do 42 j=1,nvel(i,2)-1
              if(xvel(i,j,2).ge.xvel(i,j+1,2)) go to 999
42         continue
           if(abs(xvel(i,1,2)-xmin).gt..001.or.abs(xvel(i,nvel(i,2),2)-
     +        xmax).gt..001) go to 999
         else
           if(vf(i,1,2).gt.0.) then
             xvel(i,1,2)=xmax
           else
             nvel(i,2)=0
           end if
         end if
12    continue
c
      do 50 i=1,nlayer
         xa(1)=xmin
         xa(2)=xmax
         ib=2
         ih=ib
         do 60 j=1,nzed(i)
            do 61 k=1,ih
               if(abs(xm(i,j)-xa(k)).lt..005) go to 60
61          continue
            ib=ib+1
            xa(ib)=xm(i,j)
60       continue
         ih=ib
         do 70 j=1,nzed(i+1)
            do 80 k=1,ih
               if(abs(xm(i+1,j)-xa(k)).lt..005) go to 70
80          continue
            ib=ib+1
            xa(ib)=xm(i+1,j)
70       continue
         ih=ib
         if(nvel(i,1).gt.0) then
           il=i
           is=1
         else
           if(nvel(i-1,2).gt.0) then
             il=i-1
             is=2
           else
             il=i-1
             is=1
           end if
         end if
         do 71 j=1,nvel(il,is)
            do 81 k=1,ih
               if(abs(xvel(il,j,is)-xa(k)).lt..005) go to 71
81          continue
            ib=ib+1
            xa(ib)=xvel(il,j,is)
71       continue
         if(nvel(i,2).gt.0) then
           ih=ib
           do 72 j=1,nvel(i,2)
              do 82 k=1,ih
                 if(abs(xvel(i,j,2)-xa(k)).lt..005) go to 72
82            continue
              ib=ib+1
              xa(ib)=xvel(i,j,2)
72         continue
         end if
c
         if(ib.gt.(ptrap+1)) then
           write(6,5) i
5          format(/'***  maximum number of blocks in layer ',
     +       i2,' exceeded  ***'/)
           write(*,*) '    number of blocks requested = ',ib
           return
         end if
c
         call sort(xa,ib)
c
         nblk(i)=ib-1
         do 90 j=1,nblk(i)
            xbnd(i,j,1)=xa(j)
            xbnd(i,j,2)=xa(j+1)
90       continue
c
50    continue
c
c     calculate slopes and intercepts of each block boundary
c
      do 100 i=1,nlayer
         do 110 j=1,nblk(i)
            xbndc=xbnd(i,j,1)+.001
            if(nzed(i).gt.1) then
              do 120 k=1,nzed(i)-1
                 if(xbndc.ge.xm(i,k).and.xbndc.le.xm(i,k+1)) then
                   dx=xm(i,k+1)-xm(i,k)
                   c1=(xm(i,k+1)-xbnd(i,j,1))/dx
                   c2=(xbnd(i,j,1)-xm(i,k))/dx
                   z1=c1*zm(i,k)+c2*zm(i,k+1)
                   c1=(xm(i,k+1)-xbnd(i,j,2))/dx
                   c2=(xbnd(i,j,2)-xm(i,k))/dx
                   z2=c1*zm(i,k)+c2*zm(i,k+1)
                   go to 130
                 end if
  120         continue
            else
              z1=zm(i,1)
              z2=zm(i,1) 
            end if
130         s(i,j,1)=(z2-z1)/(xbnd(i,j,2)-xbnd(i,j,1))
            b(i,j,1)=z1-s(i,j,1)*xbnd(i,j,1)
            if(nzed(i+1).gt.1) then
              do 140 k=1,nzed(i+1)-1
                 if(xbndc.ge.xm(i+1,k).and.xbndc.le.
     +             xm(i+1,k+1)) then
                   dx=xm(i+1,k+1)-xm(i+1,k)
                   c1=(xm(i+1,k+1)-xbnd(i,j,1))/dx
                   c2=(xbnd(i,j,1)-xm(i+1,k))/dx
                   z3=c1*zm(i+1,k)+c2*zm(i+1,k+1)
                   c1=(xm(i+1,k+1)-xbnd(i,j,2))/dx
                   c2=(xbnd(i,j,2)-xm(i+1,k))/dx
                   z4=c1*zm(i+1,k)+c2*zm(i+1,k+1)
                   go to 150
                 end if
140           continue
            else
              z3=zm(i+1,1)
              z4=zm(i+1,1) 
            end if
150         s(i,j,2)=(z4-z3)/(xbnd(i,j,2)-xbnd(i,j,1))
            b(i,j,2)=z3-s(i,j,2)*xbnd(i,j,1)
c                 
c           check for layer pinchouts 
c                 
            ivg(i,j)=1
            if(abs(z3-z1).lt..0005) ivg(i,j)=2 
            if(abs(z4-z2).lt..0005) ivg(i,j)=3 
            if(abs(z3-z1).lt..0005.and.abs(z4-z2).lt..0005) ivg(i,j)=-1
c
110      continue 
100   continue    
c
c     assign velocities to each model block
c
      do 160 i=1,nlayer 
c
         if(nvel(i,1).eq.0) then
           do 161 j=i-1,1,-1
              if(nvel(j,2).gt.0) then
                ig=j
                jg=2
                n1g=nvel(j,2)
                go to 162
              end if
              if(nvel(j,1).gt.0) then
                ig=j
                jg=1
                n1g=nvel(j,1)
                go to 162
              end if
161        continue
         else
           ig=i
           jg=1
           n1g=nvel(i,1)
         end if
c
162      if(n1g.gt.1.and.nvel(i,2).gt.1) ivcase=1
         if(n1g.gt.1.and.nvel(i,2).eq.1) ivcase=2
         if(n1g.eq.1.and.nvel(i,2).gt.1) ivcase=3
         if(n1g.eq.1.and.nvel(i,2).eq.1) ivcase=4
         if(n1g.gt.1.and.nvel(i,2).eq.0) ivcase=5
         if(n1g.eq.1.and.nvel(i,2).eq.0) ivcase=6
c
         do 170 j=1,nblk(i)
c
            if(ivg(i,j).eq.-1) go to 170
c
            xbndcl=xbnd(i,j,1)+.001
            xbndcr=xbnd(i,j,2)-.001
c
            go to (1001,1002,1003,1004,1005,1006), ivcase
c
1001        do 180 k=1,n1g-1
               if(xbndcl.ge.xvel(ig,k,jg).and.xbndcl.le.xvel(ig,k+1,jg)) 
     +              then
                  dxx=xvel(ig,k+1,jg)-xvel(ig,k,jg)
                  c1=xvel(ig,k+1,jg)-xbnd(i,j,1)
                  c2=xbnd(i,j,1)-xvel(ig,k,jg)
                  vm(i,j,1)=(c1*vf(ig,k,jg)+c2*vf(ig,k+1,jg))/dxx
                  if(ig.ne.i) vm(i,j,1)=vm(i,j,1)+.001
                  go to 1811
               end if
 180        continue
c
1811        do 1812 k=1,n1g-1
               if(xbndcr.ge.xvel(ig,k,jg).and.xbndcr.le.xvel(ig,k+1,jg)) 
     +           then
                 dxx=xvel(ig,k+1,jg)-xvel(ig,k,jg)
                 c1=xvel(ig,k+1,jg)-xbnd(i,j,2)
                 c2=xbnd(i,j,2)-xvel(ig,k,jg)
                 vm(i,j,2)=(c1*vf(ig,k,jg)+c2*vf(ig,k+1,jg))/dxx
                 if(ig.ne.i) vm(i,j,2)=vm(i,j,2)+.001
                 go to 181
               end if
1812        continue
c
181         do 182 k=1,nvel(i,2)-1
               if(xbndcl.ge.xvel(i,k,2).and.xbndcl.le.xvel(i,k+1,2)) 
     +           then
                 dxx=xvel(i,k+1,2)-xvel(i,k,2)   
                 c1=xvel(i,k+1,2)-xbnd(i,j,1)
                 c2=xbnd(i,j,1)-xvel(i,k,2)
                 if(ivg(i,j).ne.2) then
                   vm(i,j,3)=(c1*vf(i,k,2)+c2*vf(i,k+1,2))/dxx
                 else
                   vm(i,j,3)=vm(i,j,1)
                 end if 
                 go to 187
               end if
182         continue
c
187         do 1822 k=1,nvel(i,2)-1
               if(xbndcr.ge.xvel(i,k,2).and.xbndcr.le.xvel(i,k+1,2)) 
     +           then
                 dxx=xvel(i,k+1,2)-xvel(i,k,2)   
                 c1=xvel(i,k+1,2)-xbnd(i,j,2)
                 c2=xbnd(i,j,2)-xvel(i,k,2)
                 if(ivg(i,j).ne.3) then
                   vm(i,j,4)=(c1*vf(i,k,2)+c2*vf(i,k+1,2))/dxx
                 else
                   vm(i,j,4)=vm(i,j,2) 
                 end if
                 go to 171
               end if
1822        continue
c    
1002        do 183 k=1,n1g-1
               if(xbndcl.ge.xvel(ig,k,jg).and.xbndcl.le.xvel(ig,k+1,jg)) 
     +           then
                 dxx=xvel(ig,k+1,jg)-xvel(ig,k,jg)   
                 c1=xvel(ig,k+1,jg)-xbnd(i,j,1)
                 c2=xbnd(i,j,1)-xvel(ig,k,jg)
                 vm(i,j,1)=(c1*vf(ig,k,jg)+c2*vf(ig,k+1,jg))/dxx
                 if(ig.ne.i) vm(i,j,1)=vm(i,j,1)+.001
                 go to 1833
               end if
183         continue
c
1833        do 1832 k=1,n1g-1
               if(xbndcr.ge.xvel(ig,k,jg).and.xbndcr.le.xvel(ig,k+1,jg)) 
     +           then
                 dxx=xvel(ig,k+1,jg)-xvel(ig,k,jg)   
                 c1=xvel(ig,k+1,jg)-xbnd(i,j,2)
                 c2=xbnd(i,j,2)-xvel(ig,k,jg)
                 vm(i,j,2)=(c1*vf(ig,k,jg)+c2*vf(ig,k+1,jg))/dxx
                 if(ig.ne.i) vm(i,j,2)=vm(i,j,2)+.001
                 go to 184
               end if
1832        continue
c
184         vm(i,j,3)=vf(i,1,2)
            if(ivg(i,j).eq.2) vm(i,j,3)=vm(i,j,1)
            vm(i,j,4)=vf(i,1,2)
            if(ivg(i,j).eq.3) vm(i,j,4)=vm(i,j,2)
            go to 171
c
1003        vm(i,j,1)=vf(ig,1,jg)
            vm(i,j,2)=vf(ig,1,jg)
            if(ig.ne.i) then
              vm(i,j,1)=vm(i,j,1)+.001
              vm(i,j,2)=vm(i,j,2)+.001
            end if
c
            do 185 k=1,nvel(i,2)-1
               if(xbndcl.ge.xvel(i,k,2).and.xbndcl.le.xvel(i,k+1,2)) 
     +           then
                 dxx=xvel(i,k+1,2)-xvel(i,k,2)   
                 c1=xvel(i,k+1,2)-xbnd(i,j,1)
                 c2=xbnd(i,j,1)-xvel(i,k,2)
                 if(ivg(i,j).ne.2) then
                   vm(i,j,3)=(c1*vf(i,k,2)+c2*vf(i,k+1,2))/dxx
                 else
                   vm(i,j,3)=vm(i,j,1)
                 end if
                 go to 188
               end if
185         continue
c
188         do 1851 k=1,nvel(i,2)-1
               if(xbndcr.ge.xvel(i,k,2).and.xbndcr.le.xvel(i,k+1,2)) 
     +           then
                 dxx=xvel(i,k+1,2)-xvel(i,k,2)   
                 c1=xvel(i,k+1,2)-xbnd(i,j,2)
                 c2=xbnd(i,j,2)-xvel(i,k,2)
                 if(ivg(i,j).ne.3) then
                   vm(i,j,4)=(c1*vf(i,k,2)+c2*vf(i,k+1,2))/dxx
                 else
                   vm(i,j,4)=vm(i,j,2)
                 end if
                 go to 171
               end if
1851        continue
c
1004        vm(i,j,1)=vf(ig,1,jg)
            vm(i,j,2)=vf(ig,1,jg) 
            if(ig.ne.i) then
              vm(i,j,1)=vm(i,j,1)+.001
              vm(i,j,2)=vm(i,j,2)+.001
            end if
c
            vm(i,j,3)=vf(i,1,2)
            if(ivg(i,j).eq.2) vm(i,j,3)=vm(i,j,1)
            vm(i,j,4)=vf(i,1,2)
            if(ivg(i,j).eq.3) vm(i,j,4)=vm(i,j,2)
            go to 171
c
1005        do 186 k=1,n1g-1
               if(xbndcl.ge.xvel(ig,k,jg).and.xbndcl.le.xvel(ig,k+1,jg)) 
     +           then
                 dxx=xvel(ig,k+1,jg)-xvel(ig,k,jg)   
                 c1=xvel(ig,k+1,jg)-xbnd(i,j,1)
                 c2=xbnd(i,j,1)-xvel(ig,k,jg)
                 vm(i,j,1)=(c1*vf(ig,k,jg)+c2*vf(ig,k+1,jg))/dxx
                 if(ig.ne.i) vm(i,j,1)=vm(i,j,1)+.001
                 vm(i,j,3)=vm(i,j,1)
                 go to 1861
               end if
186         continue
c
1861        do 1862 k=1,n1g-1
               if(xbndcr.ge.xvel(ig,k,jg).and.xbndcr.le.xvel(ig,k+1,jg)) 
     +           then
                 dxx=xvel(ig,k+1,jg)-xvel(ig,k,jg)   
                 c1=xvel(ig,k+1,jg)-xbnd(i,j,2)
                 c2=xbnd(i,j,2)-xvel(ig,k,jg)
                 vm(i,j,2)=(c1*vf(ig,k,jg)+c2*vf(ig,k+1,jg))/dxx
                 if(ig.ne.i) vm(i,j,2)=vm(i,j,2)+.001
                 vm(i,j,4)=vm(i,j,2)                        
                 go to 171
               end if
1862        continue
c
1006        vm(i,j,1)=vf(ig,1,jg)
            if(ig.ne.i) vm(i,j,1)=vm(i,j,1)+.001
            vm(i,j,2)=vm(i,j,1) 
            vm(i,j,3)=vm(i,j,1)
            vm(i,j,4)=vm(i,j,1) 
c
c           calculate velocity coefficients
c
171         s1=s(i,j,1)
            s2=s(i,j,2)
            b1=b(i,j,1)
            b2=b(i,j,2)
            xb1=xbnd(i,j,1)
            xb2=xbnd(i,j,2)
            if(ivg(i,j).eq.2) then
              z3=s(i,j,2)*xb1+b(i,j,2)+.001
              z4=s(i,j,2)*xb2+b(i,j,2)
              s2=(z4-z3)/(xb2-xb1)
              b2=z3-s2*xb1
            end if
            if(ivg(i,j).eq.3) then
              z3=s(i,j,2)*xb1+b(i,j,2)
              z4=s(i,j,2)*xb2+b(i,j,2)+.001
              s2=(z4-z3)/(xb2-xb1)
              b2=z3-s2*xb1
            end if
            v1=vm(i,j,1)
            v2=vm(i,j,2)
            v3=vm(i,j,3)
            v4=vm(i,j,4)
c
            c(i,j,1)=s2*(xb2*v1-xb1*v2)+b2*(v2-v1)-
     +               s1*(xb2*v3-xb1*v4)-b1*(v4-v3)       
            c(i,j,2)=s2*(v2-v1)-s1*(v4-v3)
            c(i,j,3)=-xb2*v1+xb1*v2+xb2*v3-xb1*v4
            c(i,j,4)=-v2+v1+v4-v3
            c(i,j,5)=b2*(xb2*v1-xb1*v2)-b1*(xb2*v3-xb1*v4)
            c(i,j,6)=(s2-s1)*(xb2-xb1)
            c(i,j,7)=(b2-b1)*(xb2-xb1)
            c(i,j,8)=2.*c(i,j,2)*c(i,j,7)
            c(i,j,9)=c(i,j,2)*c(i,j,6)
            c(i,j,10)=c(i,j,4)*c(i,j,7)-c(i,j,3)*c(i,j,6)
            c(i,j,11)=c(i,j,1)*c(i,j,7)-c(i,j,5)*c(i,j,6)
c
            if(ivg(i,j).eq.-1) then
              vm(i,j,1)=0.
              vm(i,j,2)=0.
              vm(i,j,3)=0.
              vm(i,j,4)=0.
              do 172 k=1,11
                 c(i,j,1)=0.
172           continue
            end if
            if(abs(vm(i,j,1)-vm(i,j,2)).le..001.and.abs(vm(i,j,2)-
     +        vm(i,j,3)).le..001.and.abs(vm(i,j,3)-vm(i,j,4)).le..001. 
     +        and.ivg(i,j).ne.-1) ivg(i,j)=0
c
170      continue
160   continue    
c                 
      return      
c                 
999   write(6,900)
900   format(/'***  error in velocity model  ***'/)
      return
      end         
c                 
c     ----------------------------------------------------------------
c                 
      subroutine sort(x,npts)
c                 
c     sort the elements of array x in order of increasing size using
c     a bubble sort technique
c                 
      real x(1) 
      do 10 i=1,npts-1
         iflag=0  
         do 20 j=1,npts-1
            if(x(j).gt.x(j+1)) then
              iflag=1
              xh=x(j)
              x(j)=x(j+1)
              x(j+1)=xh
            end if
20       continue 
         if(iflag.eq.0) return
10     continue   
      return      
      end         
