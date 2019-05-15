      integer nblk(player),ivarv(player,ppvel,2),
     +        nzed(pncntr),ivarz(player,ppcntr),
     +        ivg(player,ptrap),
     +        nvel(player,2)
      real*4 c(player,ptrap,11),s(player,ptrap,2),b(player,ptrap,2),
     +       vm(player,ptrap,4),xbnd(player,ptrap,2),
     +       xm(pncntr,ppcntr),zm(pncntr,ppcntr),
     +       vf(player,ppvel,2),
     +       xvel(player,ppvel,2),
     +       velgrd(pxgrid,pzgrid)
c
      common /blk1/ layer,iblk
      common /blk2/ c,ivg
      common /blk3/ s,b,vm
      common /blk4/ xbnd,nblk,nlayer
      common /blk5/ xm,zm,vf,nzed,nvel,xvel
      common /blk13/ nxg,nzg,dxg,dzg,velgrd
c
