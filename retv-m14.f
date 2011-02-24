      program cab_tap
c
c Version 11/95
c         04/96 test version
c         10/96 modified tap
c
c
      character*80 fileenv,filev1,filev2,fileab,filetp,filetptp
      common/files/fileenv,filev1,filev2,fileab,filetp
      character*8 fnu,fnv,fnw,fnz,opt,xbuf*80,ftr*8
      integer(kind=1) itm(9)
      common/iterms/iturb
 
      data ftr,cr,iftr/'cressman',3.,1/
      data icase/3/
      data idomain/1/
      data xlati,xalph/38.,90./
      data imelt,z0km,z1melt,z2melt/1,0.2,4.,7./
      data isolvt,pmu0init,ct0/1,-999.,0./
      data v00,w00/1.,0.5/
      data ipress,cp0/1,0./
      data fileenv/'snd.dat'/
      data fnu,fnv,fnw,fnz/'FU','FV','W','DBZ'/
      data filev1,filev2,fileab/' ',' ',' '/
      data filetp/'temp.pt'/
      data addu,addv/0.,0./
      data iref/1/
      data iptretv/1/
      data itermx,errmx/10000,0.01/
      data iturb/1/
      data hfac/1./
      data icenter/0/
      data idim/3/
      data itv12,xmv1,ymv1,xmv2,ymv2/86400.,0.,0.,0.,0./
 
  10  continue
 
      call xread(opt,xbuf)
      print*,opt,':',xbuf(1:70)
 
c     if(opt.eq.'dim-tim')read(xbuf,*)icase
      if(opt.eq.'dim')read(xbuf,*)idim
      if(opt.eq.'lat-xax')read(xbuf,*)xlati,xalph
      if(opt.eq.'melt')read(xbuf,*)imelt,z0km,z1melt,z2melt
      if(opt.eq.'filter')read(xbuf,*)ftr,cr,iftr
c     if(opt.eq.'domain')read(xbuf,*)idomain,imin,imax,jmin,jmax,kmax
      if(opt.eq.'thermo')read(xbuf,*)isolvt,pmu0init,ct0
      if(opt.eq.'v0w0')read(xbuf,*)v00,w00
      if(opt.eq.'press')read(xbuf,*)ipress,cp0
      if(opt.eq.'fileenv')read(xbuf,*)fileenv
      if(opt.eq.'uvwz')read(xbuf,*)fnu,fnv,fnw,fnz
      if(opt.eq.'filev1')read(xbuf,*)filev1
      if(opt.eq.'filev2')read(xbuf,*)filev2
      if(opt.eq.'timexy')read(xbuf,*)itv12,xmv1,ymv1,xmv2,ymv2
      if(opt.eq.'timesft')read(xbuf,*)itv12,xmv1,ymv1,xmv2,ymv2
      if(opt.eq.'fileab')read(xbuf,*)fileab
      if(opt.eq.'filetp'.or.opt.eq.'filept')read(xbuf,*)filetp
      if(opt.eq.'adduv')read(xbuf,*)addu,addv
      if(opt.eq.'reflect')read(xbuf,*)iref
      if(opt.eq.'ptretv')read(xbuf,*)iptretv
      if(opt.eq.'itererr')read(xbuf,*)itermx,errmx
      if(opt.eq.'iturb')read(xbuf,*)iturb
      if(opt.eq.'hfac')read(xbuf,*)hfac
      if(opt.eq.'center')read(xbuf,*)icenter
      if(opt.ne.'exit'.and.opt.ne.'run')goto 10
 
      if(iturb.gt.0)iturb=1
      if(iturb.ne.1)iturb=0
      if(icenter.gt.0)icenter=1
      if(icenter.lt.0)icenter=-1
      if(ftr.ne.'leise')ftr='cressman'
 
      if(errmx.le.0.)errmx=0.01
      if(itermx.le.0)itermx=10000
      print*,'filter ',ftr,cr,iftr
 
c     print *,'CASE <1:2D-stat, 2:2D-evol, 3:3D-stat, 4:3D-evol>'
c     read*,icase
      if(idim.ne.2)idim=3
      if(filev2.eq.' ')filev2=filev1
      if(idim.eq.2.and.filev1.eq.filev2)icase=1
      if(idim.eq.2.and.filev1.ne.filev2)icase=2
      if(idim.eq.3.and.filev1.eq.filev2)icase=3
      if(idim.eq.3.and.filev1.ne.filev2)icase=4
      print*,'CASE=',icase
 
c     print *,'DOMAIN <1:full, 2:reduced>, imin,imax,jmin,jmax,kmax'
c     read*,idomain,imin,imax,jmin,jmax,kmax
 
      open(05,file=fileenv,status='unknown',form='formatted')

      iscra=0
      if(fileab.eq.' '.or.fileab.eq.'null')iscra=1
      if(iscra.eq.1)then
         call ltime(time(),itm)
         itm(4)=itm(4)+1
         itm(5)=itm(5)+1
         itm(6)=mod(itm(6),100)
         write(fileab,15)itm(4),itm(5),itm(6),itm(3),itm(2),itm(1)
  15     format('AB_',3i2.2,'_',3i2.2,'.tmp')
      endif
      open(01,file=fileab,status='unknown',form='unformatted')
 
      call cab(icase,xlati,xalph,imelt,z0km,z1melt,z2melt,iref,cr,
     +     ftr,iftr,idomain,imin,imax,jmin,jmax,kmax,fnu,fnv,fnw,fnz,
     +     addu,addv,hfac,itv12,xmv1,ymv1,xmv2,ymv2,icenter)
      print*,'*** E N D   O F   C A B ***'

      if(iptretv.eq.0)goto 60
 
      open(31,file=filetp,status='unknown',form='unformatted')
      do i=80,1,-1
      if(filetp(i:i).ne.' ')goto 50
      enddo
      i=1
  50  continue
      filetptp=filetp(1:i)//'TP'
      open(02,file=filetptp,status='unknown',form='formatted')
      call tap(isolvt,ipress,ct0,cp0,pmu0init,v00,w00,itermx,errmx)
      print*,'*** E N D   O F   T A P ***'
 
  60  continue
      if(iscra.eq.1)close(01,status='delete')
 
      stop
      end
 
      subroutine cab(icase,xlati,xalph,imelt,z0km,z1melt,z2melt,iref,
     +           cr,ftr,iftr,idomain,imin,imax,jmin,jmax,kmax,
     +           fnu,fnv,fnw,fnz,addu,addv,hfac,itv12,xmv1,ymv1,
     +           xmv2,ymv2,icenter)
c    NOTE! Modifying the parameter of dimension if the domain is changed.
      parameter (n1=70,n2=70,n3=36,n3p1=n3+1)
c
      integer   iday(6),ih(6),jh(6),it(n1,n3,14),kk(n3)
      real      u(n1,n2,n3),v(n1,n2,n3),w(n1,n2,n3)
     &         ,z(n1,n2,n3),qp(n1,n2,n3),vp(n1,n2,n3)
     &         ,dtu(n1,n2,n3),dtv(n1,n2,n3),dtw(n1,n2,n3)
     &         ,dtq(n1,n2,n3),xk(n1,n2,n3),xkm(n1,n2,n3)
     &         ,um(n1,n2,n3),vm(n1,n2,n3),wm(n1,n2,n3)
     &         ,dxk(n1,n2,n3),dyk(n1,n2,n3),dzk(n1,n2,n3)
     &         ,sx(n1,n2,n3),sy(n1,n2,n3),sz(n1,n2,n3)
     &         ,ax(n1,n2,n3),ay(n1,n2,n3),az(n1,n2,n3)
     &         ,bx(n1,n2,n3),by(n1,n2,n3),bt(n1,n2,n3),beta(n1,n2,n3)
     &         ,fq(n1,n2,n3),pres(n1,n2,n3),tref(n1,n2,n3)
     &         ,tvel(n1,n2,n3),tm(n1,n2,n3),tmm(n1,n2,n3)
     &         ,teta0(n3p1),tetav0(n3p1),pi0(n3p1),rho0(n3p1)
     &         ,dzt0(n3p1),dzq0(n3p1),g0(n3p1),g1(n3p1)

      dimension shit(n1,n2,n3)
 
      character*8 fnu,fnv,fnw,fnz,fn,ftr
      character*80 fileenv,filev1,filev2,fileab,filetp
      character*4 name,date*12
      common/files/fileenv,filev1,filev2,fileab,filetp
      common/grid/hx,hy,hz,nx,ny,nz,i2d
      common/coord/i0,j0,k0
      common/cedric/reflat,reflon
      common/iterms/iturb
c
c**** cp    = SPECIFIC HEAT AT CONSTANT PRESSURE (10+3 J/kg)
c**** r     = Cp-Cv (10+3 J/kg)
c**** vlh   = LATENT HEAT OF VAPORIZATION (10+6 J/kg)
c**** flh   = LATENT HEAT OF FUSION (10+6 J/kg)
c**** g     = GRAVITATIONAL ACCELERATION (m.s-2)
c**** omega = EARTH ANGULAR VELOCITY (10-3 s-1)
c
      cp=1.005
      r=0.287
      vlh=2.5
      flh=0.334
      slh=vlh+flh
      g=9.81
      omega=0.1458
      conv=3.14159/180.
      spv=-999.
 
      ifilter=1
      if(cr.le.0.)ifilter=0
c
c**** READ THE CONTROL DATA ON DATA_cab
c
      io_env=1
c
      if(icase.lt.1.or.icase.gt.4)then
      print *,' ICASE=',icase,'   !!!! SHOULD BE 1,2,3 or 4 !!!!'
      stop
      endif
      if(icase.eq.1.or.icase.eq.3)then
        nseq=1
      else
        nseq=2
      endif
      if(icase.le.2)then
        xmod2d=0.
      else
        xmod2d=1.
      endif
c
c**** READ THE HEADERS ON THE 3D_WIND_FIELD_FILES FV_*
c
      jxor=0
      jyor=0
      jzor=0
      tps=0.
      dt=0.
      fn=' '
      do iseq=1,nseq
 
      if(iseq.eq.1)then
      call readced(filev1,fn,spv,u,n1,n2,n3,iday,ih,na,nb,nc,h1,h2,h3)
      else if(iseq.eq.2)then
      call readced(filev2,fn,spv,u,n1,n2,n3,iday,ih,na,nb,nc,h1,h2,h3)
      endif
 
      ihx=h1*hfac
      ihy=h2*hfac
      ihz=h3*hfac
c      ifich=10+iseq
c      read(ifich)date,name
c    +               ,ih,ixor,iyor,izor,na,nb,nc,ihx,ihy,ihz
c    +               ,iolat,iolon,iha,ima,isa,iua,iva
       ixor=0
       iyor=0
       izor=0
  111  format(a12,a4,22i7)
       if(icase.le.2)then
         iyor=0
         ihy=0
       endif
       write(date,'(3A2)') iday(3),iday(2),iday(1)
       ihms1=10000*ih(1)+100*ih(2)+ih(3)
       ihms2=10000*ih(4)+100*ih(5)+ih(6)
       print *,' FILE NO',iseq,'  HHMMSS :',ihms1,' -',ihms2
     &        ,'   NX,NY,NZ :',na,nb,nc
       jxor=jxor+ixor/nseq
       jyor=jyor+iyor/nseq
       jzor=jzor+izor/nseq
       t_ks=3.6*float(ih(1))+0.06*float(ih(2))+0.001*float(ih(3))
       tps=tps+t_ks
       if(nseq.eq.1)then
         do i=1,6
         jh(i)=ih(i)
         enddo
         dt=1.e+6
       else
         lt=(iseq-1)*3
         jh(lt+1)=ih(lt+1)
         jh(lt+2)=ih(lt+2)
         jh(lt+3)=ih(lt+3)
         sign=float(2*iseq-3)
         dt=dt+sign*t_ks
c        call hms2s(itv1,itv11)
c        call hms2s(itv2,itv22)
c        dt=(itv22-itv11)/1000.
         dt=itv12/1000.
         print*,itv12,dt,xmv1,ymv1,xmv2,ymv2
       endif
      enddo
c     if(dt.lt.0.)dt=dt+86.4
      xor=float(jxor)/1000.
      yor=float(jyor)/1000.
      zor=float(jzor)/1000.
      tps=tps/float(nseq)
      ihh=(tps/3.6)
      imm=(tps-3.6*float(ihh))/0.06
      ihhmm=100*ihh+imm
      write(name,888)ihhmm
  888 format(i4)
      hx=float(ihx)/1000.
      hy=float(ihy)/1000.
      hz=float(ihz)/1000.
      if(icase.le.2)then
        hy=hx
        cd=0.2*sqrt(hx*hz)
      else
        cd=0.2*(hx*hy*hz)**(1./3.)
      endif
      cd2=cd*cd
c
      clati=cos(conv*xlati)
      slati=sin(conv*xlati)
      calph=cos(conv*xalph)
      salph=sin(conv*xalph)
      cox=+omega*clati*calph
      coy=-omega*clati*salph
      coz=+omega*slati
c
      k1melt=(z1melt-z0km)/hz+1
      k2melt=(z2melt-z0km)/hz+1
      xpmelt=float(k2melt-k1melt+1)
c
      idomain=2
      if(idomain.eq.1)then
c
c  for full domain
c
         imin=1
         imax=na
         nx=na
         jmin=1
         jmax=nb
         if(icase.gt.2)then
            ny=nb
         else
            ny=4
         endif
         kmax=nc
         nz=nc
c        kmax=min(nc,n3)
c        nz=min(nc,n3)
c
         print*,'IMIN,IMAX,JMIN,JMAX,KMAX:',imin,imax,jmin,jmax,kmax
      else
c
c  reduce domain
c
         imin=1
         imax=n1
         jmin=1
         jmax=n2
         kmax=n3

         nx=imax-imin+1
         ny=jmax-jmin+1
         if(icase.gt.2)then
            ny=jmax-jmin+1
         else
            ny=4
         endif
         nz=kmax
         print*,'IMIN,IMAX,JMIN,JMAX,KMAX,NX,NY,NZ:'
     &          ,imin,imax,jmin,jmax,kmax,nx,ny,nz
         xor=xor+float(imin-1)*hx
         yor=yor+float(jmin-1)*hy
      endif
      if(nx.gt.n1.or.ny.gt.n2.or.nz.gt.n3)then
         print*,'!!!! NX,NY,NZ :',nx,ny,nz
     &         ,'GREATER THAN DECLARED N1,N2,N3:',n1,n2,n3,'!!!!'
         print*,'!!!! MODIFY LINE 3 IN CAB.F AND RECOMPILE !!!!'
         stop
      endif
c
c**** OPEN THE FILES
c
      iolat=reflat*100
      iolon=reflon*100
      iha=0
      ima=0
      isa=0
      iua=0
      iva=0
      rewind 01
      write(01)date,name
      write(01)iolat,iolon,iha,ima,isa,iua,iva
      write(01)jh,jxor,jyor,jzor,nx,ny,nz,ihx,ihy,ihz
c
      io_ab=0
      if(io_ab.ne.0)then
      open(77,file='oCAB',status='unknown',form='formatted')
      endif
c
c**** READ THE ENVIRONMENTAL DATA ON (05) ENV_*
c
      do k=1,kmax+1
      read(05,'(6x,6f9.4)',end=1)teta0(k),tetav0(k),pi0(k)
     +               ,rho0(k),dzt0(k),dzq0(k)
      enddo
      goto 2
  1   continue
      print*,'kmax+1=',kmax+1
      call quit('end of file in '//fileenv(1:20))
  2   continue
c
c**** COEFFICIENTS FOR THE PSEUDO-ADIABATIC LAPSE RATES
c
      cap=cp/r
      t00=273.15
      ae=17.2694
      be=35.86
      ag=21.8746
      bg=7.66
      e0=0.61078
      epsi=0.622
      do k=1,kmax+1
         p0=100.*(pi0(k)**cap)
         t0=teta0(k)*pi0(k)
         if(t0.gt.t00)then
            aa=ae
            bb=be
            xlat=vlh*1000.
         else
            if(t0.gt.263.15)then
               ce=(t0-263.15)/10.
               cg=1.-ce
               aa=ae*ce+ag*cg
               bb=be*ce+bg*cg
               xlat=(vlh*ce+slh*cg)*1000.
            else
               aa=ag
               bb=bg
               xlat=slh*1000.
            endif
         endif
         es0=e0*exp(aa*(t0-t00)/(t0-bb))
         b0=aa*(t00-bb)*pi0(k)/((t0-bb)*(t0-bb))
         xx0=(epsi*xlat*es0)/(r*p0*t0)
         yy0=(epsi*xlat)/(cp*t0)
         g0(k)=(1.+xx0)/(1.+xx0*yy0)
         g11=xx0/((1.+xx0*yy0)*(1.+xx0*yy0))
         g12=b0*(1.-yy0)+(xx0*yy0+2.*yy0-1.)/teta0(k)
         g1(k)=g11*g12
         if(io_env.eq.1)then
         print 999,p0,t0-t00,g0(k),g1(k)
  999    format(' P=',f6.2,' T=',f6.2,'    G0=',f7.4,' G1=',f7.4)
         endif
      enddo
c
c**** READ THE 3D_WIND_FIELDS FV_*
c
      print*,' '
      print*,'READ THE 3D_WIND_FIELDS FILES : ',fileV1,fileV2
c
      velm=0.
      xpv=0.
c
c  read ced file
c. liu
c
      call readced(filev1,fnu,spv,u,n1,n2,n3,iday,ih,na,nb,nc,h1,h2,h3)
      call readced(filev1,fnv,spv,v,n1,n2,n3,iday,ih,na,nb,nc,h1,h2,h3)
      call readced(filev1,fnw,spv,w,n1,n2,n3,iday,ih,na,nb,nc,h1,h2,h3)
      call readced(filev1,fnz,spv,z,n1,n2,n3,iday,ih,na,nb,nc,h1,h2,h3)
      call bndfil(u,na,nb,nc,spv,1,3,5)
      call bndfil(v,na,nb,nc,spv,1,3,5)
      call bndfil(w,na,nb,nc,spv,1,3,5)
      call bndfil(z,na,nb,nc,spv,1,3,5)
      call adduv(u,v,na,nb,nc,addu,addv,spv)
      if(xmv1.ne.0..or.ymv1.ne.0.)then
         call sftxy(u,shit,n1,n2,n3,xmv1,ymv1,spv)
         call sftxy(v,shit,n1,n2,n3,xmv1,ymv1,spv)
         call sftxy(w,shit,n1,n2,n3,xmv1,ymv1,spv)
         call sftxy(z,shit,n1,n2,n3,xmv1,ymv1,spv)
      endif
      if(nseq.eq.2)then
      call readced(filev2,fnu,spv,ax,n1,n2,n3,iday,ih,na,nb,nc,h1,h2,h3)
      call readced(filev2,fnv,spv,ay,n1,n2,n3,iday,ih,na,nb,nc,h1,h2,h3)
      call readced(filev2,fnw,spv,az,n1,n2,n3,iday,ih,na,nb,nc,h1,h2,h3)
      call readced(filev2,fnz,spv,bx,n1,n2,n3,iday,ih,na,nb,nc,h1,h2,h3)
      call bndfil(ax,na,nb,nc,spv,1,3,5)
      call bndfil(ay,na,nb,nc,spv,1,3,5)
      call bndfil(az,na,nb,nc,spv,1,3,5)
      call bndfil(bx,na,nb,nc,spv,1,3,5)
      call adduv(ax,ay,na,nb,nc,addu,addv,spv)
      if(xmv2.ne.0..or.ymv2.ne.0.)then
         call sftxy(ax,shit,n1,n2,n3,xmv2,ymv2,spv)
         call sftxy(ay,shit,n1,n2,n3,xmv2,ymv2,spv)
         call sftxy(az,shit,n1,n2,n3,xmv2,ymv2,spv)
         call sftxy(bx,shit,n1,n2,n3,xmv2,ymv2,spv)
      endif
      endif
c
      do k=1,max0(nz,kmax)
         nok=0
         do j=1,nb
            do i=1,na
c
c  input data from 11 (and 12)
c
c           read(11)r1,rr,qp1,vp1,u1,v1,w1
 
            r1=z(i,j,k)
            u1=u(i,j,k)
            v1=v(i,j,k)
            w1=w(i,j,k)
            call getqv(r1,qp1,vp1,teta0,pi0,rho0,k,spv)
            if(iref.eq.0)qp1=0.
            if(nseq.eq.2)then
               r2=bx(i,j,k)
               u2=ax(i,j,k)
               v2=ay(i,j,k)
               w2=az(i,j,k)
               call getqv(r2,qp2,vp2,teta0,pi0,rho0,k,spv)
               if(iref.eq.0)qp2=0.
            else
               r2=r1
               qp2=qp1
               vp2=vp1
               u2=u1
               v2=v1
               w2=w1
            endif
            if(i.ge.imin.and.i.le.imax.and.
     +      j.ge.jmin.and.j.le.jmax.and.k.le.kmax)then
               i1=i-imin+1
               j1=j-jmin+1
               z(i1,j1,k)=-999.
               qp(i1,j1,k)=-999.
               dtq(i1,j1,k)=-999.
               vp(i1,j1,k)=-999.
               u(i1,j1,k)=-999.
               dtu(i1,j1,k)=-999.
               v(i1,j1,k)=-999.
               dtv(i1,j1,k)=-999.
               w(i1,j1,k)=-999.
               dtw(i1,j1,k)=-999.
               tref(i1,j1,k)=0.
               tvel(i1,j1,k)=0.
               if(r1.ne.spv.and.r2.ne.spv.and.
     +         qp1.ne.spv.and.qp2.ne.spv.and.
     +         vp1.ne.spv.and.vp2.ne.spv.and.
     +         u1.ne.spv.and.v1.ne.spv.and.w1.ne.spv.and.
     +         u2.ne.spv.and.v2.ne.spv.and.w2.ne.spv)then
                  nok=nok+1
                  tref(i1,j1,k)=1.
                  if(icenter.eq.0)then
                     z(i1,j1,k)=(r1+r2)/2.
                     qp(i1,j1,k)=(qp1+qp2)/2.
                     vp(i1,j1,k)=(vp1+vp2)/2.
                     u(i1,j1,k)=(u1+u2)/2.
                     v(i1,j1,k)=(v1+v2)/2.
                     w(i1,j1,k)=(w1+w2)/2.
                  else if(icenter.gt.0)then
                     z(i1,j1,k)=r2
                     qp(i1,j1,k)=qp2
                     vp(i1,j1,k)=vp2
                     u(i1,j1,k)=u2
                     v(i1,j1,k)=v2
                     w(i1,j1,k)=w2
                  else if(icenter.lt.0)then
                     z(i1,j1,k)=r1
                     qp(i1,j1,k)=qp1
                     vp(i1,j1,k)=vp1
                     u(i1,j1,k)=u1
                     v(i1,j1,k)=v1
                     w(i1,j1,k)=w1
                  endif
                  tvel(i1,j1,k)=1.
                  dtq(i1,j1,k)=(qp2-qp1)/dt
                  dtu(i1,j1,k)=(u2-u1)/dt
                  dtv(i1,j1,k)=(v2-v1)/dt
                  dtw(i1,j1,k)=(w2-w1)/dt
                  xpv=xpv+1.
                  uu=u(i1,j1,k)*u(i1,j1,k)
                  vv=v(i1,j1,k)*v(i1,j1,k)
                  ww=w(i1,j1,k)*w(i1,j1,k)
                  velm=velm+(uu+vv+ww)
               endif
            endif
            enddo
         enddo
      print*,'LEVEL ',k,' NPTS_OK:',nok
      enddo
      xpv=amax1(1.,xpv)
      print*,' '
      print*,'VELOCITY COMPONENTS'
      print*,'   rms VEL (ms-1) =',sqrt(velm/xpv)
      print*,'   npts=',xpv
c
      if(icase.le.2)then
        do j=2,ny
           do i=1,nx
              do k=1,nz
                 tref(i,j,k)=tref(i,1,k)
                 z(i,j,k)=z(i,1,k)
                 qp(i,j,k)=qp(i,1,k)
                 vp(i,j,k)=vp(i,1,k)
                 tvel(i,j,k)=tvel(i,1,k)
                 u(i,j,k)=u(i,1,k)
                 dtu(i,j,k)=dtu(i,1,k)
                 v(i,j,k)=v(i,1,k)
                 dtv(i,j,k)=dtv(i,1,k)
                 w(i,j,k)=w(i,1,k)
                 dtw(i,j,k)=dtw(i,1,k)
              enddo
           enddo
        enddo
c
      endif
c
c**** EDDY MIXING COEFFICIENT AT (I,J,K)+1/2
c
      print*,'EDDY MIXING COEFFICIENT AT (I,J,K)+1/2'
c
      do k=1,nz
         do j=1,ny
            do i=1,nx
               xk(i,j,k)=-999.
               dxk(i,j,k)=-999.
               dyk(i,j,k)=-999.
               dzk(i,j,k)=-999.
               ax(i,j,k)=-999.
               ay(i,j,k)=-999.
               az(i,j,k)=-999.
               bx(i,j,k)=-999.
               tm(i,j,k)=0.
            enddo
         enddo
      enddo
c
      xkmean=0.
      xpk=0.
      do k=1,nz-1
         k0=k
         do j=1,ny-1
            j0=j
            do i=1,nx-1
               i0=i
               tvelm=xmean(tvel,n1,n2,n3)
               if(tvelm.ge.1.)then
                 tm(i,j,k)=1.
                 um(i,j,k)=xmean(u,n1,n2,n3)
                 vm(i,j,k)=xmean(v,n1,n2,n3)
                 wm(i,j,k)=xmean(w,n1,n2,n3)
                 call deriv(u,dxu,dyu,dzu,n1,n2,n3)
                 call deriv(v,dxv,dyv,dzv,n1,n2,n3)
                 call deriv(w,dxw,dyw,dzw,n1,n2,n3)
                 d1=(dyw+dzv)*(dyw+dzv)
                 d2=(dzu+dxw)*(dzu+dxw)
                 d3=(dxv+dyu)*(dxv+dyu)
                 d4=2.*(dxu*dxu+dyv*dyv+dzw*dzw)
                 xk(i,j,k)=cd2*sqrt(d1+d2+d3+d4)
                 xkmean=xkmean+xk(i,j,k)*xk(i,j,k)
                 xpk=xpk+1.
               endif
            enddo
         enddo
      enddo
c
      xxpk=amax1(1.,xpk)
      print*,'   rms KV (10+3 m2s-1) = ',sqrt(xkmean/xxpk)
      print*,'   npts=',xpk
c
c**** SUBGRID SCALE FORCE AT (I,J,K)+1
c
      print*,'SUBGRID SCALE FORCE AT (I,J,K)+1'
c
      do k=1,nz
         do j=1,ny
            do i=1,nx
               ax(i,j,k)=-999.
               ay(i,j,k)=-999.
               az(i,j,k)=-999.
               pres(i,j,k)=0.
               tmm(i,j,k)=0.
            enddo
         enddo
      enddo
c
      dxkm=0.
      dykm=0.
      dzkm=0.
      xdk=0.
      do k=1,nz-2
         k0=k
         do j=1,ny-2
            j0=j
            do i=1,nx-2
               i0=i
               tvelmm=xmean(tm,n1,n2,n3)
               if(tvelmm.ge.1.)then
                 tmm(i,j,k)=1.
                 pres(i,j,k)=1.
                 xkm(i,j,k)=xmean(xk,n1,n2,n3)
                 call deriv(xk,d1,d2,d3,n1,n2,n3)
                 dxk(i,j,k)=d1
                 dyk(i,j,k)=d2
                 dzk(i,j,k)=d3
                 xdk=xdk+1.
                 dxkm=dxkm+dxk(i,j,k)*dxk(i,j,k)
                 dykm=dykm+dyk(i,j,k)*dyk(i,j,k)
                 dzkm=dzkm+dzk(i,j,k)*dzk(i,j,k)
                 call deriv(um,dxu,dyu,dzu,n1,n2,n3)
                 call deriv(vm,dxv,dyv,dzv,n1,n2,n3)
                 call deriv(wm,dxw,dyw,dzw,n1,n2,n3)
                 d2u=der2(u,n1,n2,n3)
                 d2v=der2(v,n1,n2,n3)
                 d2w=der2(w,n1,n2,n3)
                 ax(i,j,k)=xkm(i,j,k)*d2u+dxk(i,j,k)*(dxu+dxu)
     &                    +dyk(i,j,k)*(dyu+dxv)+dzk(i,j,k)*(dzu+dxw)
                 ay(i,j,k)=xkm(i,j,k)*d2v+dxk(i,j,k)*(dxv+dyu)
     &                    +dyk(i,j,k)*(dyv+dyv)+dzk(i,j,k)*(dzv+dyw)
                 if(icase.le.2)ay(i,j,k)=0.
                 az(i,j,k)=xkm(i,j,k)*d2w+dxk(i,j,k)*(dxw+dzu)
     &                    +dyk(i,j,k)*(dyw+dzv)+dzk(i,j,k)*(dzw+dzw)
               endif
            enddo
         enddo
      enddo
      xxdk=amax1(1.,xdk)
      print *,'    rms DXK,DYK,DZK (ms-1) = '
     &       ,sqrt(dxkm/xxdk),sqrt(dykm/xxdk),sqrt(dzkm/xxdk)
      print *,'    npts=',xdk
c
c**** SUBGRID SCALE FORCE : (I,J,K)+1 -> I,J,K +1/2
c
      print *,' SUBGRID SCALE FORCE : (I,J,K)+1 -> I,J,K +1/2'
c
      call trans(sx,sy,sz,ax,ay,az,beta,pres,n1,n2,n3)
c
c**** PRESSURE GRADIENTS AT (I,J,K)+1/2
c
      print *,' PRESSURE GRADIENTS AT (I,J,K)+1/2'
c
      do k=1,nz
         do j=1,ny
            do i=1,nx
               ax(i,j,k)=-999.
               ay(i,j,k)=-999.
               az(i,j,k)=-999.
               pres(i,j,k)=0.
            enddo
         enddo
      enddo
c
      xaxyz=0.
      axm=0.
      aym=0.
      azm=0.
      do k=1,nz-1
         k0=k
         tv0=(tetav0(k+1)+tetav0(k+2))/2000.
         cona=-1./(cp*tv0)
         do j=1,ny-1
            j0=j
            do i=1,nx-1
               i0=i
               if(tm(i,j,k).ge.1.)then
                 xaxyz=xaxyz+1.
                 pres(i,j,k)=1.
                 dtum=xmean(dtu,n1,n2,n3)
                 dtvm=xmean(dtv,n1,n2,n3)
                 dtwm=xmean(dtw,n1,n2,n3)
                 qm=xmean(qp,n1,n2,n3)
                 vdu=xlag(u,v,w,beta,n1,n2,n3,1)
                 vdv=xlag(u,v,w,beta,n1,n2,n3,2)
                 vdw=xlag(u,v,w,beta,n1,n2,n3,3)
                 ax(i,j,k)=cona*(dtum+vdu-coz*vm(i,j,k)
     &                    +coy*wm(i,j,k)-iturb*sx(i,j,k))
                 axm=axm+ax(i,j,k)*ax(i,j,k)
                 ay(i,j,k)=cona*(dtvm+vdv+coz*um(i,j,k)
     &                    -cox*wm(i,j,k)-iturb*sy(i,j,k))
                 if(icase.le.2)ay(i,j,k)=0.
                 aym=aym+ay(i,j,k)*ay(i,j,k)
                 az(i,j,k)=cona*(dtwm+vdw+g*qm-coy*um(i,j,k)
     &                    +cox*vm(i,j,k)-iturb*sz(i,j,k))
                 azm=azm+az(i,j,k)*az(i,j,k)
               endif
            enddo
         enddo
      enddo
      xxaxyz=amax1(1.,xaxyz)
      print *,'    rms AX,AY,AZ (10-9 m-1) = '
     &       ,sqrt(axm/xxaxyz),sqrt(aym/xxaxyz),sqrt(azm/xxaxyz)
      print *,'    npts=',xaxyz
c
c**** FILTER ON AX,AY,AZ (IF IFILTER=1)
c
      if(ifilter.eq.1)then
        if(ftr.eq.'cressman')then
           do iff=1,iftr
           print *,' Cressman FILTER ON AX'
           call filter(ax,pres,sx,sy,sz,n1,n2,n3,1,cr)
           print *,' Cressman FILTER ON AY'
           call filter(ay,pres,sx,sy,sz,n1,n2,n3,1,cr)
           print *,' Cressman FILTER ON AZ'
           call filter(az,pres,sx,sy,sz,n1,n2,n3,0,cr)
           enddo
        else
           iicr=cr
           print *,' Leise FILTER ON AX'
           call leise(ax,sx,n1,n2,n3,-999.,iicr,iftr)
           print *,' Leise FILTER ON AY'
           call leise(ay,sx,n1,n2,n3,-999.,iicr,iftr)
           print *,' Leise FILTER ON AZ'
           call leise(az,sx,n1,n2,n3,-999.,iicr,iftr)
        endif
      endif
      call getmx2('AX',ax,n1,n2,n3,-999.)
      call getmx2('AY',ay,n1,n2,n3,-999.)
      call getmx2('AZ',az,n1,n2,n3,-999.)
c
c**** REVISED VALUES FOR TM(I,J,K) AT (I,J,K)+1/2
c****                AND TMM(I,J,K) AT (I,J,K)+1
c
      do k=1,nz
         do j=1,ny
            do i=1,nx
               tm(i,j,k)=0.
               tmm(i,j,k)=0.
               if(ax(i,j,k).ne.-999..and.ay(i,j,k).ne.-999..and.
     &            az(i,j,k).ne.-999.)then
                 tm(i,j,k)=1.
               endif
            enddo
         enddo
      enddo
      do k=1,nz-1
         k0=k
         do j=1,ny-1
            j0=j
            do i=1,nx-1
               i0=i
               tam=xmean(tm,n1,n2,n3)
               if(tam.ge.1.)then
                 tmm(i,j,k)=1.
               endif
            enddo
         enddo
      enddo
c
c**** HORIZONTAL TEMPERATURE GRADIENTS AT (I,J,K)+1
c
      print *,' HORIZONTAL TEMPERATURE GRADIENTS AT (I,J,K)+1'
c
      do k=1,nz
         do j=1,ny
            do i=1,nx
               bx(i,j,k)=-999.
               by(i,j,k)=-999.
               pres(i,j,k)=0.
            enddo
         enddo
      enddo
c
      xbxy=0.
      bxm=0.
      bym=0.
      do k=1,nz-2
         k0=k
         t0=teta0(k+2)/1000.
         tv0=tetav0(k+2)/1000.
         conb=cp*t0*tv0/g
         do j=1,ny-2
            j0=j
            do i=1,nx-2
               i0=i
               if(tmm(i,j,k).ge.1.)then
                 xbxy=xbxy+1.
                 pres(i,j,k)=1.
                 call deriv(ax,da,db,dzax,n1,n2,n3)
                 call deriv(ay,da,db,dzay,n1,n2,n3)
                 call deriv(az,dxaz,dyaz,da,n1,n2,n3)
                 bx(i,j,k)=conb*(dzax-dxaz)
                 bxm=bxm+bx(i,j,k)*bx(i,j,k)
                 by(i,j,k)=conb*(dzay-dyaz)
                 if(icase.le.2)by(i,j,k)=0.
                 bym=bym+by(i,j,k)*by(i,j,k)
               endif
            enddo
         enddo
      enddo
      xxbxy=amax1(1.,xbxy)
      print *,'    rms BX,BY (10-3 Km-1) = '
     &       ,sqrt(bxm/xxbxy),sqrt(bym/xxbxy)
      print *,'    npts=',xbxy
c
c**** FILTER ON BX,BY (IF IFILTER=1)
c
      if(ifilter.eq.1)then
        if(ftr.eq.'cressman')then
           do iff=1,iftr
           print *,' Cressman FILTER ON BX'
           call filter(bx,pres,sx,sy,sz,n1,n2,n3,1,cr)
           print *,' Cressman FILTER ON BY'
           call filter(by,pres,sx,sy,sz,n1,n2,n3,0,cr)
           enddo
        else
           iicr=cr
           print *,' Leise FILTER ON BX'
           call leise(bx,sx,n1,n2,n3,-999.,iicr,iftr)
           print *,' Leise FILTER ON BY'
           call leise(by,sx,n1,n2,n3,-999.,iicr,iftr)
        endif
      endif
      call getmx2('BX',bx,n1,n2,n3,-999.)
      call getmx2('BY',by,n1,n2,n3,-999.)
c
c**** PRODUCTION RATE OF PRECIPITATION AT (I,J,K)+1/2
c
      print *,' PRODUCTION RATE OF PRECIPITATION AT (I,J,K)+1/2'
c
      do k=1,nz
         do j=1,ny
            do i=1,nx
               fq(i,j,k)=-999.
               pres(i,j,k)=0.
            enddo
         enddo
      enddo
c
      xfq=0.
      fqm=0.
      do k=1,nz-1
         k0=k
         do j=1,ny-1
            j0=j
            do i=1,nx-1
               i0=i
               if(tm(i,j,k).ge.1.)then
                 pres(i,j,k)=1.
                 trm=xmean(tref,n1,n2,n3)
                 if(trm.ge.1.)then
                   dtqm=xmean(dtq,n1,n2,n3)
                   vdq=xlag(u,v,w,qp,n1,n2,n3,4)
                   dzvq=derz(qp,vp,rho0,n1,n2,n3)
                   fq(i,j,k)=dtqm+vdq+dzvq
                 else
                   fq(i,j,k)=0.
                 endif
                 xfq=xfq+1.
                 fqm=fqm+fq(i,j,k)*fq(i,j,k)
               endif
            enddo
         enddo
      enddo
      xxfq=amax1(1.,xfq)
      print *,'    rms FQ (10-6 s-1) = ',sqrt(fqm/xxfq)
      print *,'    npts=',xfq
c
c**** FILTER ON F(QP) (IF IFILTER=1)
c
      if(ifilter.eq.1)then
        if(ftr.eq.'cressman')then
           do iff=1,iftr
           print *,' Cressman FILTER ON F(QP)'
           call filter(fq,pres,sx,sy,sz,n1,n2,n3,0,cr)
           enddo
        else
           iicr=cr
           print *,' Leise FILTER ON F(QP)'
           call leise(fq,sx,n1,n2,n3,-999.,iicr,iftr)
        endif
      endif
      call getmx2('FQ',fq,n1,n2,n3,-999.)
c
c**** ADVECTION OF TEMPERATURE AT (I,J,K)+1
c
      print *,' ADVECTION OF TEMPERATURE AT (I,J,K)+1'
c
      do k=1,nz
         do j=1,ny
            do i=1,nx
               bt(i,j,k)=-999.
               beta(i,j,k)=-999.
            enddo
         enddo
      enddo
c
      xbt=0.
      btm=0.
      betam=0.
      do k=1,nz-2
         alt=zor+float(k-1)*hz
         k0=k
         temp=teta0(k+2)*pi0(k+2)
         if(temp.ge.273.15)then
           xlat=vlh
         else
           if(temp.le.263.15)then
             xlat=slh
           else
             ce=(temp-263.15)/10.
             cg=1.-ce
             xlat=ce*vlh+cg*slh
           endif
         endif
         t0=teta0(k+2)/1000.
         gpit=g*(1./(cp*pi0(k+2))-1.61*t0/xlat)
         b0=-dzt0(k+2)-0.61*t0*dzq0(k+2)
         b1s=(1.-g0(k+2))*gpit
         b1ns=xlat/(cp*pi0(k+2))-0.61*t0
         bet=-g1(k+2)*gpit
         do j=1,ny-2
            j0=j
            do i=1,nx-2
               i0=i
               pres(i,j,k)=0.
               if(tmm(i,j,k).ge.1.
     &            .and.test(wm,n1,n2,n3).ge.1.
     &            .and.test(fq,n1,n2,n3).ge.1.)then
                 pres(i,j,k)=1.
                 wmm=xmean(wm,n1,n2,n3)
                 bt0=wmm*b0
                 fqm=xmean(fq,n1,n2,n3)
                 if(fqm.lt.-1.)then
c
c**** CASE: UNSATURATED AIR
c
                   bt1=b1ns*fqm
                   beta(i,j,k)=0.
                 else
c
c**** CASE: SATURATED AIR
c
                   bt1=wmm*b1s+t0*fqm
                   beta(i,j,k)=wmm*bet
                 endif
                 bt(i,j,k)=bt0+bt1
c
c**** MELTING LAYER (IF IMELT=1)
c
                 if(imelt.eq.1)then
                   if(k.ge.k1melt.and.k.le.k2melt
     &                .and.tref(i+1,j+1,k2melt+1).ge.1.)then
                     qmelt=qp(i+1,j+1,k2melt+1)/xpmelt
                     vg=vp(i+1,j+1,k2melt+1)
                     bmelt=(flh/cp)*vg*qmelt/hz
                     bt(i,j,k)=bt(i,j,k)+bmelt
                   endif
                 endif
c
                 xbt=xbt+1.
                 btm=btm+bt(i,j,k)*bt(i,j,k)
                 betam=betam+beta(i,j,k)*beta(i,j,k)
c
               endif
c
            enddo
         enddo
      enddo
      xxbt=amax1(1.,xbt)
      print *,'    rms BT(10-3 Ks-1),BETA(10-3 s-1) = '
     &       ,sqrt(btm/xxbt),sqrt(betam/xxbt)
      print *,'    npts=',xbt
c
c**** FILTER ON BT (IF IFILTER=1)
c
      if(ifilter.eq.1)then
        if(ftr.eq.'cressman')then
           do iff=1,iftr
           print *,' Cressman FILTER ON BT'
           call filter(bt,pres,sx,sy,sz,n1,n2,n3,0,cr)
           enddo
        else
           iicr=cr
           print *,' Leise FILTER ON BT'
           call leise(bt,sx,n1,n2,n3,-999.,iicr,iftr)
        endif
      endif
      call getmx2('BT',bt,n1,n2,n3,-999.)
c
c**** OUTPUTS ON AB_*
c
      print *,' '
      print *,' OUTPUTS ON ',fileAB
c
      do k=1,nz
         print *,'  LEVEL ',k
         do j=1,ny
            do i=1,nx
               write(01)u(i,j,k),v(i,j,k),w(i,j,k)
     &                 ,qp(i,j,k),fq(i,j,k)
     &                 ,ax(i,j,k),ay(i,j,k),az(i,j,k)
     &                 ,bx(i,j,k),by(i,j,k),bt(i,j,k)
     &                 ,beta(i,j,k),xkm(i,j,k)
     &                 ,dxk(i,j,k),dyk(i,j,k),dzk(i,j,k)
            enddo
         enddo
      enddo
c
c**** OUTPUTS ON oCAB_* (IF IOUTPUT_AB=1)
c
      if(io_ab.eq.1)then
        print *,' '
        print *,' OUTPUTS ON oCAB'
        write(77,100)fileAB,date,name
  100   format(' AB_FILE:',a100,/,' DATE:',a12,'    HHMM:',a4,///)
c
        do j=1,ny
           print *,'  YZ PLANE NO ',j
c
           do k=1,nz
              do i=1,nx
                 do l=1,14
                    it(i,k,l)=999
                 enddo
                 if(z(i,j,k).gt.-900.)it(i,k,4)=z(i,j,k)
                 if(qp(i,j,k).gt.-900.)it(i,k,13)=(10.*qp(i,j,k))
                 if(vp(i,j,k).gt.-900.)it(i,k,14)=(10.*vp(i,j,k))
                 if(u(i,j,k).gt.-900.)it(i,k,1)=u(i,j,k)
                 if(v(i,j,k).gt.-900.)it(i,k,2)=v(i,j,k)
                 if(w(i,j,k).gt.-900.)it(i,k,3)=(10.*w(i,j,k))
                 if(ax(i,j,k).gt.-900.)it(i,k,5)=ax(i,j,k)
                 if(ay(i,j,k).gt.-900.)it(i,k,6)=ay(i,j,k)
                 if(az(i,j,k).gt.-900.)it(i,k,7)=az(i,j,k)
                 if(fq(i,j,k).gt.-900.)it(i,k,12)=(10.*fq(i,j,k))
                 if(bx(i,j,k).gt.-900.)it(i,k,8)=(100.*bx(i,j,k))
                 if(by(i,j,k).gt.-900.)it(i,k,9)=(100.*by(i,j,k))
                 if(bt(i,j,k).gt.-900.)it(i,k,10)=bt(i,j,k)
                 if(beta(i,j,k).gt.-900.)it(i,k,11)=(100.*beta(i,j,k))
                 do l=1,14
                 if(it(i,k,l).lt.-999.or.it(i,k,l).gt.999)it(i,k,l)=999
                 enddo
              enddo
           enddo
c
           y1=yor+hy*float(j-1)
           y2=y1+hy/2.
           y3=y1+hy
c
           call output(it,kk,y1,n1,n3,1)
           call output(it,kk,y1,n1,n3,2)
           call output(it,kk,y1,n1,n3,3)
           call output(it,kk,y1,n1,n3,4)
           call output(it,kk,y1,n1,n3,13)
           call output(it,kk,y1,n1,n3,14)
           if(j.le.(ny-1))then
             call output(it,kk,y2,n1,n3,5)
             call output(it,kk,y2,n1,n3,6)
             call output(it,kk,y2,n1,n3,7)
             call output(it,kk,y2,n1,n3,12)
             if(j.le.(ny-2))then
               call output(it,kk,y3,n1,n3,8)
               call output(it,kk,y3,n1,n3,9)
               call output(it,kk,y3,n1,n3,10)
               call output(it,kk,y3,n1,n3,11)
             endif
           endif
c
        enddo
      endif
c
      return
 
      end

      subroutine hms2s(ihms,isec)
      ih=ihms/10000
      im=mod(ihms/100,100)
      is=mod(ihms,100)
      isec=ih*3600+im*60+is
      return
      entry s2hms(isec,ihms)
      ih=isec/3600
      im=(isec-ih*3600)/60
      is=mod(isec,60)
      ihms=ih*10000+im*100+is
      return
      end
c
c**** THIS FUNCTION TESTS ARRAY T(N1,N2,N3)
c**** AT ((I,I+1),(J,J+1),(K,K+1) AND RETURNS
c**** ->  0 IF THERE ARE MISSING DATA
c**** ->  1 IF NO DATA IS MISSING
c
      function test(t,n1,n2,n3)
c
      dimension t(n1,n2,n3)
c
      common/coord/i,j,k
c
      test=0.
      do kk=k,k+1
         do jj=j,j+1
            do ii=i,i+1
               if(t(ii,jj,kk).lt.-900.)return
            enddo
         enddo
      enddo
      test=1.
c
      return
      end
c
c**** THIS FUNCTION RETURNS A MEAN VALUE OF ARRAY T(N1,N2,N3)
c**** AT (I+1/2,J+1/2,K+1/2)
c
c
c
c**** THIS FUNCTION RETURNS THE VALUE OF
c**** U.dT/dx+V.dT/dy+W.dT/dz
c**** AT (I+1/2,J+1/2,K+1/2)
c
      function xlag(u,v,w,t,n1,n2,n3,ind)
c
      dimension u(n1,n2,n3),v(n1,n2,n3),w(n1,n2,n3),t(n1,n2,n3)
c
      common/grid/hx,hy,hz,nx,ny,nz,i2d
      common/coord/i,j,k
c
      i1=i
      i2=i+1
      j1=j
      j2=j+1
      k1=k
      k2=k+1
c
      if(ind.ne.4)then
        do ii=i1,i2
           do jj=j1,j2
              do kk=k1,k2
                 if(ind.eq.1)t(ii,jj,kk)=u(ii,jj,kk)
                 if(ind.eq.2)t(ii,jj,kk)=v(ii,jj,kk)
                 if(ind.eq.3)t(ii,jj,kk)=w(ii,jj,kk)
              enddo
           enddo
        enddo
      endif
c
      udt=0.
      do jj=j1,j2
         do kk=k1,k2
            udt=udt+(u(i,jj,kk)+u(i+1,jj,kk))
     &              *(-t(i,jj,kk)+t(i+1,jj,kk))/(8.*hx)
         enddo
      enddo
c
      vdt=0.
      do ii=i1,i2
         do kk=k1,k2
            vdt=vdt+(v(ii,j,kk)+v(ii,j+1,kk))
     &              *(-t(ii,j,kk)+t(ii,j+1,kk))/(8.*hy)
         enddo
      enddo
c
      wdt=0.
      do ii=i1,i2
         do jj=j1,j2
            wdt=wdt+(w(ii,jj,k)+w(ii,jj,k+1))
     &              *(-t(ii,jj,k)+t(ii,jj,k+1))/(8.*hz)
         enddo
      enddo
c
      xlag=udt+vdt+wdt
c
      if(ind.ne.4)then
        do ii=i1,i2
           do jj=j1,j2
              do kk=k1,k2
                 t(ii,jj,kk)=0.
              enddo
           enddo
        enddo
      endif
c
      return
      end
c
c**** THIS ROUTINE CALCULATES THE FIRST-ORDER DERIVATIVES
c**** dT/dx and dT/dy OF ARRAY T(N1,N2,N3)
c**** AT (I+1/2,J+1/2,K+1/2)
c
      subroutine deriv(t,dxt,dyt,dzt,n1,n2,n3)
c
      real t(n1,n2,n3)
c
      common/grid/hx,hy,hz,nx,ny,nz,i2d
      common/coord/i,j,k
c
      dxt=(-t(i,j,k)+t(i+1,j,k)
     &     -t(i,j+1,k)+t(i+1,j+1,k)
     &     -t(i,j,k+1)+t(i+1,j,k+1)
     &     -t(i,j+1,k+1)+t(i+1,j+1,k+1))/(4.*hx)
      dyt=(-t(i,j,k)-t(i+1,j,k)
     &     +t(i,j+1,k)+t(i+1,j+1,k)
     &     -t(i,j,k+1)-t(i+1,j,k+1)
     &     +t(i,j+1,k+1)+t(i+1,j+1,k+1))/(4.*hy)
      dzt=(-t(i,j,k)-t(i+1,j,k)
     &     -t(i,j+1,k)-t(i+1,j+1,k)
     &     +t(i,j,k+1)+t(i+1,j,k+1)
     &     +t(i,j+1,k+1)+t(i+1,j+1,k+1))/(4.*hz)
c
      return
      end
c
c**** THIS FUNCTION CALCULATES THE SECOND-ORDER DERIVATIVE
c**** (d2T/dx2+d2T/dy2+d2T/dz2) OF ARRAY T(N1,N2,N3)
c**** AT (I+1,J+1,K+1)
c
      function der2(t,n1,n2,n3)
c
      dimension t(n1,n2,n3)
c
      common/grid/hx,hy,hz,nx,ny,nz,i2d
      common/coord/i,j,k
c
      dxxt=(t(i,j+1,k+1)-2.*t(i+1,j+1,k+1)
     &                  +t(i+2,j+1,k+1))/(hx*hx)
      dyyt=(t(i+1,j,k+1)-2.*t(i+1,j+1,k+1)
     &                  +t(i+1,j+2,k+1))/(hy*hy)
      dzzt=(t(i+1,j+1,k)-2.*t(i+1,j+1,k+1)
     &                  +t(i+1,j+1,k+2))/(hz*hz)
      der2=dxxt+dyyt+dzzt
c
      return
      end
c
c**** THIS ROUTINE CALCULATES THE VALUES
c**** OF THE ARRAYS SX(N1,N2,N3), SY(NX,NY,NZ), SZ(NX,NY,NZ)
c**** AT (I+1/2,J+1/2,K+1/2) FROM THE VALUES AT (I+1,J+1,K+1)
c
      subroutine trans(sx,sy,sz,ax,ay,az,x,p,n1,n2,n3)
c
      real sx(n1,n2,n3),sy(n1,n2,n3),sz(n1,n2,n3)
     &         ,ax(n1,n2,n3),ay(n1,n2,n3),az(n1,n2,n3)
     &         ,x(n1,n2,n3),p(n1,n2,n3)
c
      common/grid/hx,hy,hz,nx,ny,nz,i2d
c
      xs=0.
      sxm=0.
      sym=0.
      szm=0.
c
      do k=1,nz
         do j=1,ny
            do i=1,nx
               x(i,j,k)=0.
               sx(i,j,k)=0.
               sy(i,j,k)=0.
               sz(i,j,k)=0.
            enddo
         enddo
      enddo
c
      do i=1,nx
         imin=max0(1,i-1)
         imax=i
         do j=1,ny
            jmin=max0(1,j-1)
            jmax=j
            do k=1,nz
               kmin=max0(1,k-1)
               kmax=k
               do ii=imin,imax
                  do jj=jmin,jmax
                     do kk=kmin,kmax
                        xp=p(ii,jj,kk)
                        x(i,j,k)=x(i,j,k)+xp
                        sx(i,j,k)=sx(i,j,k)+xp*ax(ii,jj,kk)
                        sy(i,j,k)=sy(i,j,k)+xp*ay(ii,jj,kk)
                        sz(i,j,k)=sz(i,j,k)+xp*az(ii,jj,kk)
                     enddo
                  enddo
               enddo
            enddo
         enddo
      enddo
c
      do i=1,nx
         do j=1,ny
            do k=1,nz
               if(x(i,j,k).gt.0.)then
                 sx(i,j,k)=sx(i,j,k)/x(i,j,k)
                 sy(i,j,k)=sy(i,j,k)/x(i,j,k)
                 sz(i,j,k)=sz(i,j,k)/x(i,j,k)
                 xs=xs+1.
                 sxm=sxm+sx(i,j,k)*sx(i,j,k)
                 sym=sym+sy(i,j,k)*sy(i,j,k)
                 szm=szm+sz(i,j,k)*sz(i,j,k)
               else
                 sx(i,j,k)=0.
                 sy(i,j,k)=0.
                 sz(i,j,k)=0.
               endif
               x(i,j,k)=0.
               ax(i,j,k)=0.
               ay(i,j,k)=0.
               az(i,j,k)=0.
               p(i,j,k)=0.
            enddo
         enddo
      enddo
c
      print *,'    rms SX,SY,SZ (10-3 ms-2) = '
     &       ,sqrt(sxm/xs),sqrt(sym/xs),sqrt(szm/xs)
      print *,'    npts=',xs
c
      return
      end
c
c**** THIS FUNCTION CALCULATES THE VERTICAL DERIVATIVE
c**** 1/RHO*d(RHO*VP*QP)/dz
c**** AT (I+1/2,J+1/2,K+1/2)
c
      function derz(q,vp,rho0,n1,n2,n3)
c
      dimension q(n1,n2,n3),vp(n1,n2,n3),rho0(40)
c
      common/grid/hx,hy,hz,nx,ny,nz,i2d
      common/coord/i,j,k
c
      r1=rho0(k+1)
      vq1=r1*(vp(i,j,k)*q(i,j,k)
     &        +vp(i+1,j,k)*q(i+1,j,k)
     &        +vp(i,j+1,k)*q(i,j+1,k)
     &        +vp(i+1,j+1,k)*q(i+1,j+1,k))/4.
      r2=rho0(k+2)
      vq2=r2*(vp(i,j,k+1)*q(i,j,k+1)
     &        +vp(i+1,j,k+1)*q(i+1,j,k+1)
     &        +vp(i,j+1,k+1)*q(i,j+1,k+1)
     &        +vp(i+1,j+1,k+1)*q(i+1,j+1,k+1))/4.
c
      rm=(r1+r2)/2.
      derz=(vq2-vq1)/(rm*hz)
c
      return
      end
c
c**** THIS ROUTINE FILTERS ARRAY T(N1,N2,N3)
c**** USING A CRESSMAN  WEIGHTING FUNCTION
c**** (1959,MWR,87,367-374)
c
      subroutine filter(t,p,a,x,s,n1,n2,n3,ind,cr)
c
      real t(n1,n2,n3),p(n1,n2,n3)
     &    ,a(n1,n2,n3),x(n1,n2,n3),s(n1,n2,n3)
c
      common/grid/hx,hy,hz,nx,ny,nz,i2d
      cr2=cr*cr
c
      xp=0.
      tm=0.
c
      do k=1,nz
         do j=1,ny
            do i=1,nx
               a(i,j,k)=0.
               x(i,j,k)=0.
               s(i,j,k)=0.
            enddo
         enddo
      enddo
c
      do k=1,nz
         kmin=max0(1,k-1)
         kmax=min0(nz,k+1)
         do j=1,ny
            jmin=max0(1,j-1)
            jmax=min0(ny,j+1)
            do i=1,nx
               imin=max0(1,i-1)
               imax=min0(nx,i+1)
c
               do kk=kmin,kmax
                  dk2=float((kk-k)*(kk-k))
                  do jj=jmin,jmax
                     dj2=float((jj-j)*(jj-j))
                     do ii=imin,imax
                        di2=float((ii-i)*(ii-i))
c
                        d2=di2+dj2+dk2
                        pds=(cr2-d2)/(cr2+d2)
                        s(ii,jj,kk)=s(ii,jj,kk)+p(i,j,k)
                        x(ii,jj,kk)=x(ii,jj,kk)+p(i,j,k)*pds
                        a(ii,jj,kk)=a(ii,jj,kk)+p(i,j,k)*pds*t(i,j,k)
c
                     enddo
                  enddo
               enddo
c
            enddo
         enddo
      enddo
c
      do k=1,nz
         if(k.eq.1.or.k.eq.nz)then
           sk=2.
         else
           sk=3.
         endif
         do j=1,ny
            if(j.eq.1.or.j.eq.ny)then
              sj=2.
            else
              sj=3.
            endif
            do i=1,nx
               if(i.eq.1.or.i.eq.nx)then
                 si=2.
               else
                 si=3.
               endif
c
               smin=si*sj*sk/2.
               if(s(i,j,k).gt.smin.and.t(i,j,k).gt.-900.)then
                 t(i,j,k)=a(i,j,k)/x(i,j,k)
                 xp=xp+1.
                 tm=tm+t(i,j,k)*t(i,j,k)
               else
                 t(i,j,k)=-999.
               endif
c
               p(i,j,k)=float(ind)*p(i,j,k)
               s(i,j,k)=0.
               x(i,j,k)=0.
               a(i,j,k)=0.
c
            enddo
         enddo
      enddo
c
      xxp=amax1(1.,xp)
      print *,'    rms filtered = ',sqrt(tm/xxp)
      print *,'    npts = ',xp
c
      return
      end
c
c**** THIS ROUTINE PRINT THE RESULTS
c**** ON A FORMATTED FILE oCAB_*
c
      subroutine output(it,kk,y,n1,n3,l)
      integer it(n1,n3,14),kk(n3)
c
      common/grid/hx,hy,hz,nx,ny,nz,i2d
c
      write(77,100)y
  100 format(/////,1x,'VERTICAL XZ PLANE AT Y=',f6.2,'km',//)
c
      go to (1,2,3,4,5,6,7,8,9,10,11,12,13,14),l
c
  1   write(77,101)
  101 format(5x,'WIND COMPONENT U (m/s)',//)
      go to 20
  2   write(77,102)
  102 format(5x,'WIND COMPONENT V (m/s)',//)
      go to 20
  3   write(77,103)
  103 format(5x,'WIND COMPONENT W (dm/s)',//)
      go to 20
  4   write(77,104)
  104 format(5x,'REFLECTIVITY (dBZ)',//)
      go to 20
  5   write(77,105)
  105 format(5x,'AX (10-9 m-1)',//)
      go to 20
  6   write(77,106)
  106 format(5x,'AY (10-9 m-1)',//)
      go to 20
  7   write(77,107)
  107 format(5x,'AZ (10-9 m-1)',//)
      go to 20
  8   write(77,108)
  108 format(5x,'BX (0.01 K/km)',//)
      go to 20
  9   write(77,109)
  109 format(5x,'BY (0.01 K/km)',//)
      go to 20
  10  write(77,110)
  110 format(5x,'BT (10-3 K/s)',//)
      go to 20
  11  write(77,111)
  111 format(5x,'BETA (10-5 s-1)',//)
      go to 20
  12  write(77,112)
  112 format(5x,'F(qp) (10-7 s-1)',//)
      go to 20
  13  write(77,113)
  113 format(5x,'QP (0.1 g/kg)',//)
      go to 20
  14  write(77,114)
  114 format(5x,'VP (dm/s)',//)
c
  20  nk=min0(25,nz)
      xk=24./float(max0(24,nz-1))
      do k=1,25
         kk(k)=(1.+float(k-1)/xk)
      enddo
c
      write(77,200)(kk(k),k=1,nk)
  200 format(/,1x,'i/k ',25i3,/)
      do i=1,nx
         write(77,300)i,(it(i,kk(k),l),k=1,nk)
      enddo
  300 format(1x,i2,2x,25i3)
      write(77,200)(kk(k),k=1,nk)
c
      return
      end
c
c**** THIS ROUTINE DECODES THE REFLECTIVITY (dBZ)
c**** PRECIPITATION CONTENT (g/kg), FALLSPEED (m/s)
c**** CARTESIAN VELOCITY COMPONENTS (m/s)
c**** FROM THE DATA WRITTEN IN THE FV_* FILE
c
      subroutine decod(iwinds,mz1z2,mqv,muvw,re1,re2,qp,vp,u,v,w)
c
      ir1=mz1z2/1000
      if(ir1.ne.999)then
        re1=float(ir1)/10.
      else
        re1=-999.
      endif
      ir2=mz1z2-1000*ir1
      if(ir2.ne.999)then
        re2=float(ir2)/10.
      else
        re2=-999.
      endif
c
      if(mqv.eq.999999)then
        qp=-999.
        vp=-999.
      else
        iqp=mqv/1000
        qp=float(iqp)/100.
        if(qp.lt.0.)qp=-qp
        ivp=mqv-1000*iqp
        vp=float(ivp-500)/50.
        if(vp.gt.0.)vp=-vp
      endif
c
      scale_uv=10./float(iwinds)
      scale_w=20./float(iwinds)
      if(muvw.eq.999999999)then
        u=-999.
        v=-999.
        w=-999.
      else
        iu=muvw/1000000
        u=float(iu-500)/scale_uv
        iv=muvw/1000-1000*iu
        v=float(iv-500)/scale_uv
        iw=muvw-1000000*iu-1000*iv
        if(iw.ne.999)then
          w=float(iw-500)/scale_w
        else
          w=0.
        endif
      endif
c
      return
      end

      subroutine getmx2(fn,p,m,n,l,spv)
      character*(*) fn
      dimension p(m,n,l)
      do k=1,l
      pmin= 1.e20
      pmax=-1.e20
      psum=0.
      npt=0
      ii=0
      do i=1,m
      do j=1,n
      if(p(i,j,k).ne.spv)then
         pmin=min(p(i,j,k),pmin)
         pmax=max(p(i,j,k),pmax)
         psum=psum+p(i,j,k)
         ii=ii+1
      endif
      enddo
      enddo
      if(ii.ne.0)then
         pmean=psum/ii
      else
         pmin=0.
         pmax=0.
         pmean=0.
      endif
      print*,fn,k,ii,pmin,pmax,pmean
      enddo
      return
      end
 

      subroutine tap(isolvt,ipress,ct0,cp0,pm0init,v00,w00,itermx,errmx)
 
c     program tap
c c.liu 10/26/96 moified
c
      parameter(n1=70,n2=70,n3=36,n3p1=n3+1)
c
      integer ih(6),itp(n1,n2,n3,7),kk(n3),itm6(9)
      real u(n1,n2,n3),v(n1,n2,n3),w(n1,n2,n3)
     &    ,vel(n1,n2,n3),fq(n1,n2,n3),pfq(n1,n2,n3)
     &    ,ax(n1,n2,n3),ay(n1,n2,n3),az(n1,n2,n3),xkm(n1,n2,n3)
     &    ,dxk(n1,n2,n3),dyk(n1,n2,n3),dzk(n1,n2,n3)
     &    ,bx(n1,n2,n3),by(n1,n2,n3)
     &    ,bt(n1,n2,n3),beta(n1,n2,n3)
     &    ,pa(n1,n2,n3),pb(n1,n2,n3)
     &    ,pp(n1,n2,n3),pt(n1,n2,n3)
     &    ,xmat(27,n1,n2,n3),vect(n1,n2,n3)
     &    ,t(n1,n2,n3),p(n1,n2,n3),st(n1,n2,n3)
     &    ,xpg(n1,n2,n3),ypg(n1,n2,n3)
     &    ,a(n1,n2,n3),b(n1,n2,n3),c(n1,n2,n3),d(n1,n2,n3)
     &    ,teta0(n3p1),tetav0(n3p1),pi0(n3p1),rho0(n3p1)
     &    ,dzt0(n3p1),dzq0(n3p1)
      equivalence (u(1,1,1),xpg(1,1,1)),(v(1,1,1),ypg(1,1,1))
     &           ,(w(1,1,1),p(1,1,1)),(vel(1,1,1),pp(1,1,1))
     &           ,(pb(1,1,1),pa(1,1,1)),(bx(1,1,1),ax(1,1,1))
     &           ,(by(1,1,1),ay(1,1,1)),(bt(1,1,1),az(1,1,1))
     &           ,(beta(1,1,1),st(1,1,1))
 
c     character path_abs*21,dir*50,fileENV*20
c    &         ,fileAB*100,fileE*100
c    &         ,fileTP*100
c    &         ,suffix*10,date*12
c    &         ,suffixAB*4,suffixTP*4
c
      character*4 date*12,suffixAB,suffixTP,fileTPTP*100
      character*80 fileenv,filev1,filev2,fileab,filetp
      common/files/fileenv,filev1,filev2,fileab,filetp
 
      common/grid/hx,hy,hz,nx,ny,nz,i2d
      common/constraints/iterative,v0,w0,pm0_init,pm1,pm2
     &                                  ,pn0_init,pn2,pn3
      common/matinv/iter,itermax,err,errmax,ibreak,testmax
      common/result/itert,errt,dt,txc,txa,etx
     &              ,tyc,tya,ety,at,tac,taa,eta,et1,et2,et3
      common/resulp/iterp,errp,dp,pxc,pxa,epx
     &              ,pyc,pya,epy,pzc,pza,epz,ep1,ep2,ep3
      common/coord/i0,j0,k0
      common/ident/fileTPTP,date,ihhmmss1,ihhmmss2
c
c**** g = GRAVITATIONAL ACCELERATION (m.s-2)
c**** cp = SPECIFIC HEAT AT CONSTANT PRESSURE (10+3 J.kg-1.K-1)
c**** v0 = MINIMUM VELOCITY AMPLITUDE TO APPLY THERMO. EQ. (m.s-1)
c**** itermax = MAXIMUM NUBER OF ITERATIONS IN ROUTINE 'conjgrad'
c**** errmax = MAXIMUM RELATIVE ERROR FOR MATRIX INVERSION
c
      fileTPTP=fileTP
      g=9.81
      cp=1.005
c     v0=1.
c     w0=0.5
c     itermax=10000
c     errmax=1.e-2
      itermax=itermx
      errmax=errmx
      v0=v00
      w0=w00
      pm0_init=pm0init
 
c     path_abs='/public/doppler/rouf/'
c
c     open(99,file='DATA_tap',status='old',form='formatted')
c
c     read(99,*)dir
c     print *,' WORKING_DIRECTORY : '
c     nchar=1
c     do while (dir(nchar:nchar).ne.' ')
c        nchar=nchar+1
c     end do
c     nchar=nchar-1
c
c     read(99,*)fileENV
c     print *,' ENVIRONMENTAL_FILE : ',fileENV
c
c     read(99,*)suffix
c     print *,' OUTPUT_FILES_SUFFIX : ',suffix
c
c     read(99,*)isolvt
c     print *,' SOLVE(1) or READ+MODIFY(2) TEMP. PERT. :',isolvt
c
c     read(99,*)pm0_init

      iterative=0
      if(isolvt.eq.1)then
        if(abs(pm0_init).lt.1.e-6)then
c  pm0_init = 0
          print *,' THERMODYNAMIC EQ IS NOT TAKEN INTO ACCOUNT'
          print *,' "GAL-CHEN" TEMPERATURE RETRIEVAL'
        else
c  pm0_init <>0
          print *,' THERMODYNAMIC EQ IS TAKEN INTO ACCOUNT'
          if(pm0_init.gt.0.)then
c  pm0_init > 0
             print *,' "COMBINED" TEMPERATURE RETRIEVAL'
             print *,' -> FORCED RETRIEVAL WITH MU0=',pm0_init
          else
c  pm0_init < 0
             print *,' "COMBINED" TEMPERATURE RETRIEVAL'
     +              ,' WITH ITERATIVE DETERMINATION OF MU0'
             print *,' -> ITERATIVE RETRIEVAL'
             iterative=1
          endif
        endif
      endif
c
c     read(99,*)ipress
c     print *,' PRESSURE_RETRIEVAL (y=1,n=0) : ',ipress
c
c     read(99,*)ct0,cp0
c     print *,' TEMPERATURE_AND_PRESSURE_CONSTANTS (in K and Pa) : '
c    &       ,ct0,cp0
c
c     read(99,*)ioutput_tp
c     print *,' LISTING_FOR_TP (y=1,n=0) : ',ioutput_tp
c
c     close(99)
      ioutput_tp=0
c
c     fileAB=path_abs//dir(1:nchar)//'/AB_'//suffix
c     fileTP=path_abs//dir(1:nchar)//'/TP_'//suffix
c     fileE=path_abs//dir(1:nchar)//'/'//fileENV
c
c     open(01,file=fileAB,status='unknown',form='unformatted')
c     open(02,file=fileTP,status='unknown',form='formatted')
c     open(05,file=fileE,status='unknown',form='unformatted')
c     open(66,file='oTAP',status='unknown',form='formatted')
 
      rewind 01
      rewind 02
      rewind 05
      rewind 31
c
      ioutput_tp=0
      io_tp=ioutput_tp
      open(66,file='oTAP',status='unknown',form='formatted')
 
c
c**** OPEN FILE AB_*
c
      print *,' OPEN FILE ',fileAB
c
      read(01)date,suffixAB
      read(01)iolat,iolon,iha,ima,isa,iua,iva
      read(01)ih,ix0,iy0,iz0,nx,ny,nz,ihx,ihy,ihz
      ihhmmss1=10000*ih(1)+100*ih(2)+ih(3)
      ihhmmss2=10000*ih(4)+100*ih(5)+ih(6)
      print *,' DATE :',date
      if(nx.gt.n1.or.ny.gt.n2.or.nz.gt.n3)then
        print *,' !!!! NX,NY,NZ ON',fileAB,' :',nx,ny,nz
     &         ,' GREATER THAN DECLARED N1,N2,N3:',n1,n2,n3
     &         ,' !!!!'
        print *,' !!!! MODIFY LINE 3 IN TAP.F AND RECOMPILE !!!!'
        stop
      endif
      print *,' HHMMSS :',ihhmmss1,' -',ihhmmss2
     &       ,'   NX,NY,NZ : ',nx,ny,nz
      x0=float(ix0)/1000.
      y0=float(iy0)/1000.
      z0=float(iz0)/1000.
      hx=float(ihx)/1000.
      hy=float(ihy)/1000.
      if(ihy.eq.0)then
        hy=hx
        i2d=1
      else
        i2d=0
      endif
      hz=float(ihz)/1000.
c
c****READ ENVIRONMENTAL DATA ON (05) ENV_*
c
      print *,' READ THE ENVIRONMENTAL DATA ON ',fileENV
c
      nlev_env=0
      do k=1,nz+1
         read(05,998,end=999)teta0(k),tetav0(k),pi0(k)
     &                 ,rho0(k),dzt0(k),dzq0(k)
         nlev_env=nlev_env+1
      enddo
  998 format(6x,6f9.4)
  999 print *,'     NB LEVELS :',nlev_env
      if(nlev_env.lt.nz+1)then
        print *,' !!!! ONLY',nlev_env,' LEVELS ON 05:',fileENV,' !!!!'
        print *,' !!!! SHOULD BE >= ',nz+1,' !!!!'
        print *,' !!!! MODIFY PREPENV.F AND START AGAIN !!!!'
        stop
      endif
c
      if(isolvt.eq.1)then
c
c****ISOLVT=1 -> READ THE INPUT (BX,BY,BT,BETA) ON AB_*
c
        print *,' '
        print *,' READ THE INPUT (BX,BY,BT,BETA) ON ',fileAB
        xv=0.
        velm=0.
        xf=0.
        fqm=0.
        xb=0.
        bxm=0.
        bym=0.
        btm=0.
        betm=0.
        xk=0.
        xkmm=0.
        dxkm=0.
        dykm=0.
        dzkm=0.
        do k=1,nz
           nvel=0
           nfq=0
           nb=0
           nkm=0
           do j=1,ny
              do i=1,nx
c
                 u(i,j,k)=-999.
                 v(i,j,k)=-999.
                 w(i,j,k)=-999.
                 vel(i,j,k)=0.1
                 pfq(i,j,k)=0.
                 fq(i,j,k)=-999.
                 pb(i,j,k)=0.
                 pt(i,j,k)=0.
                 bx(i,j,k)=-999.
                 by(i,j,k)=-999.
                 bt(i,j,k)=-999.
                 beta(i,j,k)=-999.
                 xkm(i,j,k)=0.
                 dxk(i,j,k)=0.
                 dyk(i,j,k)=0.
                 dzk(i,j,k)=0.
c
                 read(01)uu,vv,ww,qq,ffq,aax,aay,aaz
     &                  ,bbx,bby,bbt,bet,xxk,d1k,d2k,d3k
c
                 if(uu.gt.-900..and.vv.gt.-900..and.ww.gt.-900.)then
                   nvel=nvel+1
                   u(i,j,k)=uu
                   v(i,j,k)=vv
                   w(i,j,k)=ww
                   vel(i,j,k)=amax1(0.1,sqrt(uu*uu+vv*vv+ww*ww))
                   xv=xv+1.
                   velm=velm+vel(i,j,k)*vel(i,j,k)
                 endif
                 if(ffq.gt.-900.)then
                   nfq=nfq+1
                   fq(i,j,k)=ffq
                   pfq(i,j,k)=1.
                   xf=xf+1.
                   fqm=fqm+fq(i,j,k)*fq(i,j,k)
                 endif
                 if(bbx.gt.-900..and.bby.gt.-900..and.
     &              bbt.gt.-900..and.bet.gt.-900.)then
                   nb=nb+1
                   bx(i,j,k)=bbx
                   by(i,j,k)=bby
                   bt(i,j,k)=bbt
                   beta(i,j,k)=bet
                   pb(i,j,k)=1.
                   xb=xb+1.
                   bxm=bxm+bx(i,j,k)*bx(i,j,k)
                   bym=bym+by(i,j,k)*by(i,j,k)
                   btm=btm+bt(i,j,k)*bt(i,j,k)
                   betam=betam+beta(i,j,k)*beta(i,j,k)
                 endif
                 if(xxk.gt.-900..and.d1k.gt.-900..and.
     &              d2k.gt.-900..and.d3k.gt.-900.)then
                   nkm=nkm+1
                   xkm(i,j,k)=3.*xxk
                   dxk(i,j,k)=3.*d1k
                   dyk(i,j,k)=3.*d2k
                   dzk(i,j,k)=3.*d3k
                   xk=xk+1.
                   xkmm=xkmm+xkm(i,j,k)
                   dxkm=dxkm+dxk(i,j,k)*dxk(i,j,k)
                   dykm=dykm+dyk(i,j,k)*dyk(i,j,k)
                   dzkm=dzkm+dzk(i,j,k)*dzk(i,j,k)
                 endif
c
              enddo
           enddo
           print *,' LEVEL',k,' NPTS_VEL,FQ,B,KM:',nvel,nfq,nb,nkm
        enddo
c
        print *,' '
        rms_vel=sqrt(velm/amax1(1.,xv))
        print *,' rms VEL (ms-1) :',rms_vel,'   NPTS :',xv
        print *,' rms FQ (10-6 s-1) :',sqrt(fqm/xf),'   NPTS :',xf
        rms_bx=sqrt(bxm/amax1(1.,xb))
        rms_by=sqrt(bym/amax1(1.,xb))
        rms_bt=sqrt(btm/amax1(1.,xb))
        print *,' rms BX,BY (10-3 Km-1) :',rms_bx,rms_by
     &         ,'   rms BT (10-3 Ks-1), BETA (10-3 s-1) :'
     &         ,rms_bt,sqrt(betam/xb),'   NPTS :',xb
        print *,' rms XKM (10+3 m2s-1) :',sqrt(xkmm/xk)
     &         ,'   rms DXK,DYK,DZK (ms-1) :'
     &         ,sqrt(dxkm/xk),sqrt(dykm/xk),sqrt(dzkm/xk),'   NPTS :',xk
        print *,' '
c
c****ITERATIVE TEMPERATURE RETRIEVAL
c
        print *,' '
        print *,' ***************************************************'
        print *,' *                                                 *'
        if(iterative.eq.0.and.abs(pm0_init).lt.1.e-6)then
          print *,' "GAL-CHEN" TEMPERATURE RETRIEVAL'
        endif
        if(iterative.eq.0.and.pm0_init.gt.1.e-6)then
          print *,' "COMBINED" TEMPERATURE RETRIEVAL'
     &           ,' WITH FORCED MU0=',pm0_init
        endif
        if(iterative.eq.1)then
          print *,' "COMBINED" TEMPERATURE RETRIEVAL'
     &           ,' WITH ITERATIVE DETERMINATION OF MU0'
        endif
        print *,' *                                                 *'
        print *,' ***************************************************'
c
        write(66,11)fileTPTP,date,ihhmmss1,ihhmmss2
  11    format(' TP_FILE:',a100
     &         ,/,' DATE:',a12,'    HHMMSS:',i6,'-',i6)
c
        testmax=1.4
c
  1     if(iterative.eq.0)then
          write(66,1101)
 1101     format(//,' !!! TEMPERATURE RETRIEVAL WITH FIXED MU0='
     &           ,f6.3,' !!!')
        else
          write(66,1102)testmax
 1102     format(//,' !!! ITERATIVE TEMPERATURE RETRIEVAL'
     &           ,' -> MAX(ETH/ETT,ETT/ETH)<',f4.1,' !!!')
          pm0_init=((rms_bx+rms_by)/2.)/(rms_bt/rms_vel)
          print *,' '
          print *,' '
          print *,' '
          print *,'  INITIAL MU0 : ',pm0_init
          print *,'  MAX(ETH/ETT,ETT/ETH)<',testmax
          print *,' '
        endif
c
        call ajust(pb,pt,bx,by,bt,beta,u,v,w,vel,xkm,dxk,dyk,dzk
     &             ,xmat,vect,t,ct0,a,b,c,d,itp,n1,n2,n3)
        if(ibreak.eq.1)then
          testmax=testmax*testmax
          go to 1
        endif
c
c****RETRIEVED HEAT SOURCE
c
        do k=1,nz
           do j=1,ny
              do i=1,nx
                 st(i,j,k)=-999.
              enddo
           enddo
        enddo
        do k=2,nz-1
           do j=2,ny-1
              do i=2,nx-1
                 if(w(i,j,k).gt.-900.)then
                   ptm=pt(i-1,j,k)+pt(i+1,j,k)
     &                +pt(i,j-1,k)+pt(i,j+1,k)
     &                +pt(i,j,k-1)+pt(i,j,k+1)
                     if(ptm.ge.6)then
                     dxt=(-t(i-1,j,k)+t(i+1,j,k))/(hx+hx)
                     dyt=(-t(i,j-1,k)+t(i,j+1,k))/(hy+hy)
                     dzt=(-t(i,j,k-1)+t(i,j,k+1))/(hz+hz)
                     st(i,j,k)=u(i,j,k)*dxt+v(i,j,k)*dyt
     &                        +w(i,j,k)*(dzt0(k+1)+dzt)
                   endif
                 endif
              enddo
           enddo
        enddo
c
c****OUTPUTS ON (06) oTAP3D
c
        if(ioutput_tp.eq.1)then
          print *,' '
          print *,' OUTPUTS ON (66) : oTAP'
          call outputt(itp,kk,y0,n1,n2,n3)
        endif
c
      else
c
c****ISOLVT=2 -> READ and MODIFY TEMP. PERT. FIELD
c
        print *,' '
        print *,' READ THE TEMP. PERT. ON ',fileTPTP
        write(66,1103)
 1103   format(//,' !!! TEMPERATURE PERTURBATIONS ARE READ !!!',////)
        read(02,111)date,suffixTP
     &             ,iolat,iolon,iha,ima,isa,iua,iva
     &             ,ih,ix0,iy0,iz0,nx,ny,nz,ihx,ihy,ihz
  111   format(a12,a4,22i7)
        xtc1=0.
        tc1m=0.
        xst=0.
        stm=0.
        do k=1,nz
           print *,'   LEVEL',k
           do j=1,ny
              do i=1,nx
                 read(02,222)mfs,mtp,mpg
  222            format(3i6)
c
                 it=mtp/1000
                 if(it.ne.999)then
                   t(i,j,k)=float(it-500)/10.+ct0
                   pt(i,j,k)=1.
                   xtc1=xtc1+1.
                   tc1m=tc1m+t(i,j,k)*t(i,j,k)
                 else
                   t(i,j,k)=-999.
                   pt(i,j,k)=0.
                 endif
                 ifq=mfs/1000
                 ist=mfs-1000*ifq
                 if(ist.ne.999)then
                   st(i,j,k)=float(ist-500)/10.
                   xst=xst+1.
                   stm=stm+st(i,j,k)*st(i,j,k)
                 else
                   st(i,j,k)=-999.
                 endif
c
c****IPRESS=0 -> READ PRES. PERT. FIELD
                 if(ipress.ne.1)then
c
                   ip=mtp-1000*it
                   if(ip.ne.999)then
                     p(i,j,k)=float(ip-500)*10.+cp0
                     pp(i,j,k)=1.
                   else
                     p(i,j,k)=-999.
                     pp(i,j,k)=0.
                   endif
                   ixpg=mpg/1000
                   if(ixpg.ne.999)then
                     xpg(i,j,k)=(ixpg-500.)/10.
                   else
                     xpg(i,j,k)=-999.
                   endif
                   iypg=mpg-1000*ixpg
                    if(iypg.ne.999)then
                     ypg(i,j,k)=(iypg-500.)/10.
                   else
                     ypg(i,j,k)=-999.
                    endif
c
                 endif
c
              enddo
           enddo
        enddo
        print *,' rms Tc1 (K) :',sqrt(tc1m/amax1(1.,xtc1))
     &         ,'   NPTS :',xtc1
        print *,' rms ST (K/h) :',sqrt(stm/amax1(1.,xst))
     &         ,'   NPTS :',xst
        rewind(02)
c
      endif
c
      if(ipress.eq.1)then
c
c****PRESSURE RETRIEVAL
c
        print *,' '
        print *,' *****************************************************'
        print *,' *                                                   *'
        print *,' *          ITERATIVE PRESSURE RETRIEVAL             *'
        print *,' *                                                   *'
        print *,' *****************************************************'
        print *,' '
c
c****READ THE INPUT (AX,AY,AZ) ON AB_*
c
        print *,' READ THE INPUT DATA ON ',fileAB
c
        rewind(01)
        read(01)date,suffixAB
        print *,' DATE : ',date,' HHMMSS :',ihhmmss1,'-',ihhmmss2
        read(01)iolat,iolon,iha,ima,isa,iua,iva
        read(01)ih,ix0,iy0,iz0,nx,ny,nz,ihx,ihy,ihz
        print *,' HHMMSS :',ihhmmss1,' -',ihhmmss2
     &         ,'   NX,NY,NZ : ',nx,ny,nz
        write(66,22)fileTPTP,date,ihhmmss1,ihhmmss2
  22    format(/////,' TP_FILE:',a100
     &         ,/,' DATE:',a12,'    HHMM:',i6,'-',i6)
c
        do k=1,nz
           na=0
           do j=1,ny
              do i=1,nx
c
                 pa(i,j,k)=0.
                 ax(i,j,k)=-999.
                 ay(i,j,k)=-999.
                 az(i,j,k)=-999.
                 pp(i,j,k)=0.
                 pfq(i,j,k)=0.
                 fq(i,j,k)=-999.
c
                 read(01)uu,vv,ww,qq,ffq,aax,aay,aaz
     &                  ,bbx,bby,bbt,bet,xxk,d1k,d2k,d3k
c
                 if(aax.ne.-999..and.aay.ne.-999..and.
     &              aaz.ne.-999.)then
                   na=na+1
                   ax(i,j,k)=aax
                   ay(i,j,k)=aay
                   az(i,j,k)=aaz
                   pa(i,j,k)=1.
                 endif
                 if(ffq.gt.-900.)then
                   fq(i,j,k)=ffq
                   pfq(i,j,k)=1.
                 endif
c
              enddo
           enddo
           print *,'  LEVEL',k,' NPTS_A :',na
        enddo
c
        testmax=1.4
c
        xa=0.
        axm=0.
        aym=0.
        azm=0.
        saxy2=0.
        sazt2=0.
        do k=1,nz-1
           k0=k
           t0=(teta0(k)+teta0(k+1))/2000.
           tv0=(tetav0(k)+tetav0(k+1))/2000.
           cz=g/(cp*t0*tv0)
           do j=1,ny-1
              j0=j
              do i=1,nx-1
                 i0=i
                 ptm=xmean(pt,n1,n2,n3)
                 if(ptm.ge.1.
     &              .and.abs(ax(i,j,k)).ne.999.
     &              .and.abs(ay(i,j,k)).ne.999.
     &              .and.abs(az(i,j,k)).ne.999.)then
                   saxy2=saxy2+ax(i,j,k)*ax(i,j,k)
     &                        +ay(i,j,k)*ay(i,j,k)
                   az(i,j,k)=az(i,j,k)+cz*xmean(t,n1,n2,n3)
                   sazt2=sazt2+az(i,j,k)*az(i,j,k)
                   pa(i,j,k)=1.
                   xa=xa+1.
                   axm=axm+ax(i,j,k)*ax(i,j,k)
                   aym=aym+ay(i,j,k)*ay(i,j,k)
                   azm=azm+az(i,j,k)*az(i,j,k)
                 else
                   ax(i,j,k)=-999.
                   ay(i,j,k)=-999.
                   az(i,j,k)=-999.
                   pa(i,j,k)=0.
                 endif
                enddo
             enddo
        enddo
c
        vqm_ax=sqrt(axm/amax1(1.,xa))
        vqm_ay=sqrt(aym/amax1(1.,xa))
        vqm_az=sqrt(azm/amax1(1.,xa))
        print *,' rms AX,AY,AZ+T (10-9 m-1) : '
     &         ,vqm_ax,vqm_ay,vqm_az
     &         ,' NPTS : ',xa
c
  2     pn0_init=((vqm_ax+vqm_ay)/2.)/vqm_az
        pn2=0.1*pn0_init
        pn3=pn2
        write(66,221)testmax
 221    format(//,' !!!! ITERATIVE PRESSURE RETRIEVAL'
     &         ,' -> MAX(EPV/EPH,EPH/EPV)<',f4.1,' !!!!')
        print *,' '
        print *,' '
        print *,' '
        print *,'  INITIAL nu0 : ',pn0_init
        print *,'  MAX(EPH/EPV,EPV/EPH)<',testmax
        print *,' '
c
        call ajusp(pa,pp,ax,ay,az,xmat,vect,p,cp0,a,b,c,d
     &             ,tetav0,rho0,itp,n1,n2,n3)
        if(ibreak.eq.1)then
          testmax=testmax*testmax
          go to 2
        endif
c
c****OUTPUTS ON (06) oTAP3D
c
        if(ioutput_tp.eq.1)then
          print *,' '
          print *,' OUTPUTS ON (66) : oTAP'
          call outputp(itp,kk,y0,n1,n2,n3)
        endif
c
c****RETRIEVED X AND Y PRESSURE GRADIENTS
c
        do k=1,nz
           do j=1,ny
              do i=1,nx
                 xpg(i,j,k)=-999.
                 ypg(i,j,k)=-999.
              enddo
           enddo
        enddo
        do k=1,nz
           do j=2,ny-1
              do i=2,nx-1
                 ppm=pp(i-1,j-1,k)+pp(i,j-1,k)+pp(i+1,j-1,k)
     &               +pp(i-1,j,k)+pp(i,j,k)+pp(i+1,j,k)
     &               +pp(i-1,j+1,k)+pp(i,j+1,k)+pp(i+1,j+1,k)
                 if(ppm.ge.9)then
                   dxp=(-p(i-1,j-1,k)-p(i-1,j,k)-p(i-1,j+1,k)
     &                  +p(i+1,j-1,k)+p(i+1,j,k)+p(i+1,j+1,k))/(6.*hx)
                   dyp=(-p(i-1,j-1,k)-p(i,j-1,k)-p(i+1,j-1,k)
     &                  +p(i-1,j+1,k)+p(i,j+1,k)+p(i+1,j+1,k))/(6.*hy)
                     xpg(i,j,k)=-(1./rho0(k+1))*dxp
                   ypg(i,j,k)=-(1./rho0(k+1))*dyp
                 endif
              enddo
           enddo
        enddo
c
      endif
c
c****OUTPUTS ON (02) TP_*
c
      print *,' '
      print *,' OUTPUTS ON (02) ',fileTPTP
      null_word=999999
c
      if(i2d.eq.0)then
c
        write(31)date,suffixAB,ih,ix0,iy0,iz0,nx,ny,nz,ihx,ihy,ihz
     +          ,et1,et2,et3,ep1,ep2,ep3,v0,pm0,pm1,pm2
c    +          ,iolat,iolon,iha,ima,isa,iua,iva
        open(32,file='error.dat',form='formatted',status='unknown'
     +         ,access='append')
        write(32,*)
        write(32,'(a)')filetp(1:60)
        write(32,'(a)')filev1(1:60)
        write(32,'(a)')filev2(1:60)
        call ltime(time(),itm6)
        itm6(4)=itm6(4)+1
        itm6(5)=itm6(5)+1
        itm6(6)=mod(itm6(6),100)
        write(32,15)itm6(4),itm6(5),itm6(6),itm6(3),itm6(2),itm6(1)
  15    format(3i2.2,'_',3i2.2)
        write(32,32)nx,ny,nz,v0,pm0,pm1,pm2,et1,et2,et3,ep1,ep2,ep3
        close(32)
  32    format(3i4,f4.1,3f6.3,6f6.2)
 
        write(02,111)date,suffixAB
     &               ,iolat,iolon,iha,ima,isa,iua,iva
     &               ,ih,ix0,iy0,iz0,nx,ny,nz,ihx,ihy,ihz
        do k=1,nz
           print *,'  LEVEL K=',k
           k0=k-1
           do j=1,ny
              j0=j-1
              do i=1,nx
                 i0=i-1
c
                 mtemp=999
                 if(pt(i,j,k).ge.1.)then
                   mtemp=(10.*t(i,j,k)+500.)
                   if(mtemp.lt.1.or.mtemp.gt.999)mtemp=999
                 endif
                 mpres=999
                 if((ipress.eq.1.or.isolvt.eq.2)
     &              .and.pp(i,j,k).ge.1.)then
                   mpres=(p(i,j,k)/10.+500.)
                   if(mpres.lt.1.or.mpres.gt.999)mpres=999
                 endif
                 mtp=1000*mtemp+mpres
c
                 mfq=999
                 if(i0*j0*k0.ne.0)then
                   pfqm=xmean(pfq,n1,n2,n3)
                      if(pfqm.ge.1.)then
                     fqm=xmean(fq,n1,n2,n3)
                     mfq=(10.*fqm+500.)
                     if(mfq.lt.1.or.mfq.gt.999)mfq=999
                   endif
                 endif
                   mst=999
                 if(st(i,j,k).gt.-900.)then
                   mst=(10.*st(i,j,k)+500.)
                   if(mst.lt.1.or.mst.gt.999)mst=999
                 endif
                 mfs=1000*mfq+mst
c
                 mpg=999999
                 if((ipress.eq.1.or.isolvt.eq.2)
     &              .and.xpg(i,j,k).ne.-999.
     &              .and.ypg(i,j,k).ne.-999.)then
                   mxpg=(10.*xpg(i,j,k)+500.)
                   mypg=(10.*ypg(i,j,k)+500.)
                   if(     mxpg.gt.0.and.mxpg.lt.999
     &                .and.mypg.gt.0.and.mypg.lt.999)then
                     mpg=1000*mxpg+mypg
                   endif
                 endif
c
                 write(02,222)mfs,mtp,mpg
 
                 tijk=t(i,j,k)
                 pijk=p(i,j,k)
                 fqijk=fq(i,j,k)
                 stijk=st(i,j,k)
                 xpgijk=xpg(i,j,k)
                 ypgijk=ypg(i,j,k)
                 if(mtemp.eq.999)tijk=-999.
                 if(mpres.eq.999)pijk=-999.
                 if(mfq  .eq.999)fqijk=-999.
                 if(mst  .eq.999)stijk=-999.
                 if(mxpg .eq.999)xpgijk=-999.
                 if(mypg .eq.999)ypgijk=-999.
                 write(31)stijk,fqijk,tijk,pijk,xpgijk,ypgijk
c
              enddo
           enddo
        enddo
c
      else
c
        write(31)date,suffixAB,ih,ix0,0,iz0,nx,1,nz,ihx,0,ihz
     +          ,et1,et2,et3,ep1,ep2,ep3,v0,pm0,pm1,pm2
c    +          ,ioloat,iolon,iha,ima,isa,iua,iva
 
        open(32,file='error.dat',form='formatted',status='unknown'
     +         ,access='append')
        write(32,*)
        write(32,'(a)')filetp(1:60)
        write(32,'(a)')filev1(1:60)
        write(32,'(a)')filev2(1:60)
        call ltime(time(),itm6)
        itm6(4)=itm6(4)+1
        itm6(5)=itm6(5)+1
        itm6(6)=mod(itm6(6),100)
        write(32,15)itm6(4),itm6(5),itm6(6),itm6(3),itm6(2),itm6(1)
        write(32,32)nx,ny,nz,v0,pm0,pm1,pm2,et1,et2,et3,ep1,ep2,ep3
        close(32)
 
        write(02,111)date,suffixAB
     &               ,ioloat,iolon,iha,ima,isa,iua,iva
     &               ,ih,ix0,0,iz0,nx,1,nz,ihx,0,ihz
        do k=1,nz
           k0=k-1
           do i=1,nx
              i0=i-1
c
              mtemp=999
              if(pt(i,2,k)+pt(i,3,k).ge.2.)then
                t2d=(t(i,2,k)+t(i,3,k))/2.
                mtemp=(10.*t2d+500.)
                if(mtemp.le.1.or.mtemp.gt.999)mtemp=999
              endif
              mpres=999
              if(pp(i,2,k)+pp(i,3,k).ge.2.)then
                p2d=(p(i,2,k)+p(i,3,k))/2.
                mpres=(p2d/10.+500.)
                if(mpres.le.1.or.mpres.gt.999)mpres=999
              endif
              mtp=1000*mtemp+mpres
c
              mfq=999
              if(i0*k0.ne.0)then
                j0=2
                pfqm=xmean(pfq,n1,n2,n3)
                if(pfqm.ge.1.)then
                  fqm=xmean(fq,n1,n2,n3)
                    mfq=(10.*fqm+500.)
                  if(mfq.lt.1.or.mfq.gt.999)mfq=999
                endif
              endif
              mst=999
              if(st(i,2,k).gt.-900.
     &           .and.st(i,3,k).gt.-900.)then
                stm=(st(i,2,k)+st(i,3,k))/2.
                mst=(10.*stm+500.)
                if(mst.lt.1.or.mst.gt.999)mst=999
              endif
                  mfs=1000*mfq+mst
c
              mpg=999
              if(xpg(i,2,k).ne.-999..and.xpg(i,3,k).ne.-999..and.
     &           ypg(i,2,k).ne.-999..and.ypg(i,3,k).ne.-999.)then
                xpgm=(xpg(i,2,k)+xpg(i,3,k))/2.
                mxpg=(10.*xpgm+500.)
                ypgm=(ypg(i,2,k)+ypg(i,3,k))/2.
                mypg=(10.*ypgm+500.)
                if(mxpg.gt.0.or.mxpg.lt.999.and.
     &             mypg.gt.0.or.mypg.lt.999)then
                  mpg=1000*mxpg+mypg
                endif
              endif
c
                write(02,222)mfs,mtp,mpg
c
                 tijk=t(i,j,k)
                 pijk=p(i,j,k)
                 fqijk=fq(i,j,k)
                 stijk=st(i,j,k)
                 xpgijk=xpg(i,j,k)
                 ypgijk=ypg(i,j,k)
                 if(mtemp.eq.999)tijk=-999.
                 if(mpres.eq.999)pijk=-999.
                 if(mfq  .eq.999)fqijk=-999.
                 if(mst  .eq.999)stijk=-999.
                 if(mxpg .eq.999)xpgijk=-999.
                 if(mypg .eq.999)ypgijk=-999.
                 write(31)stijk,fqijk,tijk,pijk,xpgijk,ypgijk
 
           enddo
        enddo
c
      endif
c
      return
      end
c
c**** THIS ROUTINE WRITES THE MATRIX AND VECTOR FOR TEMPERATURE RETRIEVAL
c
      subroutine ajust(pb,pt,bx,by,bt,beta,ux,vy,wz,vel
     &                 ,xkm,dxk,dyk,dzk
     &                 ,x,v,t,ct0,a,b,c,d,it,n1,n2,n3)
c
      real ux(n1,n2,n3),vy(n1,n2,n3),wz(n1,n2,n3)
     &    ,vel(n1,n2,n3),xkm(n1,n2,n3)
     &    ,dxk(n1,n2,n3),dyk(n1,n2,n3),dzk(n1,n2,n3)
     &    ,pb(n1,n2,n3),pt(n1,n2,n3)
     &    ,bx(n1,n2,n3),by(n1,n2,n3)
     &    ,bt(n1,n2,n3),beta(n1,n2,n3)
     &    ,x(27,n1,n2,n3),v(n1,n2,n3),t(n1,n2,n3)
     &    ,a(n1,n2,n3),b(n1,n2,n3),c(n1,n2,n3),d(n1,n2,n3)
      integer it(n1,n2,n3,7)
c
      common/grid/hx,hy,hz,nx,ny,nz,i2d
      common/constraints/iterative,v0,w0,pm0_init,pm1,pm2
     &                                  ,pn0_init,pn2,pn3
      common/matinv/iter,itermax,err,errmax,ibreak,testmax
      common/result/itert,errt,dt,txc,txa,etx
     &              ,tyc,tya,ety,at,tac,taa,eta,et1,et2,et3
c
c****PM1 IS SET TO 0.1 (MINIMIZE d/dz WHEN W AND/OR UVW ARE WEAK)IF ITHERMO=1
c****           TO 0.                                            IF ITHERMO=0
c****PM2 IS SET TO 0.01 (NUMERICAL DIFFUSION FOR d2/dx2+d2/dy2+d2/dz2)
c
      if(abs(pm0_init).lt.1.e-6)then
        pm1=0.
      else
        pm1=0.01
      endif
      pm2=0.01
      px=1./hx
      px2=px*px
      py=1./hy
      py2=py*py
      pz=1./hz
      pz2=pz*pz
      pxyz2=px2+py2+pz2
      hxyz2=(hx*hy*hz)**(2./3.)
c
c**** BEGINS THE ITERATIVE RETRIEVAL
c
      ipass=0
      epsi_sup=+999.
  1   ipass=ipass+1
      if(iterative.eq.1)then
        if(ipass.eq.1)pm0=0.
        if(ipass.eq.2)pm0=pm0_init
      else
        pm0=pm0_init
      endif
      print *,' IPASS=',ipass,' MU0=',pm0
c
c**** INITIALIZATIONS
c
      if(ipass.le.3)then
        do k=1,nz
           do j=1,ny
              do i=1,nx
                 t(i,j,k)=0.
              enddo
           enddo
        enddo
      endif
c
      do k=1,nz
         do j=1,ny
            do i=1,nx
               v(i,j,k)=0.
               a(i,j,k)=0.
               b(i,j,k)=0.
               c(i,j,k)=0.
               d(i,j,k)=0.
               do l=1,27
                  x(l,i,j,k)=0.
               enddo
               do n=1,7
                  it(i,j,k,n)=999
               enddo
            enddo
         enddo
      enddo
c
c****MATRIX AND VECTOR FOR TEMPERATURE
c
      print *,' MATRIX AND VECTOR FOR TEMPERATURE'
c
      do k=1,nz-2
         do j=1,ny-2
            do i=1,nx-2
c
               ax=pb(i,j,k)*px/2.
               adx=ax*px/2.
               ay=pb(i,j,k)*py/2.
               ady=ay*py/2.
c
               um=ux(i+1,j+1,k+1)
               vm=vy(i+1,j+1,k+1)
               wm=wz(i+1,j+1,k+1)
               v2=vel(i+1,j+1,k+1)*vel(i+1,j+1,k+1)
               vr=vel(i+1,j+1,k+1)/v0
               vr4=vr*vr*vr*vr
               fv=1./(1.+1./vr4)
               xm0=pb(i,j,k)*pm0*fv/v2
               b1=pz*((-wm+dzk(i,j,k))/2.-pz*xkm(i,j,k))
               b2=py*((-vm+dyk(i,j,k))/2.-py*xkm(i,j,k))
               b3=px*((-um+dxk(i,j,k))/2.-px*xkm(i,j,k))
               b4=2.*(px2+py2+pz2)*xkm(i,j,k)-beta(i,j,k)
               b5=px*((+um-dxk(i,j,k))/2.-px*xkm(i,j,k))
               b6=py*((+vm-dyk(i,j,k))/2.-py*xkm(i,j,k))
               b7=pz*((+wm-dzk(i,j,k))/2.-pz*xkm(i,j,k))
               pb1=xm0*b1
               pb2=xm0*b2
               pb3=xm0*b3
               pb4=xm0*b4
               pb5=xm0*b5
               pb6=xm0*b6
               pb7=xm0*b7
               pb11=xm0*b1*b1
               pb12=xm0*b1*b2
               pb13=xm0*b1*b3
               pb14=xm0*b1*b4
               pb15=xm0*b1*b5
               pb16=xm0*b1*b6
               pb17=xm0*b1*b7
               pb22=xm0*b2*b2
               pb23=xm0*b2*b3
               pb24=xm0*b2*b4
               pb25=xm0*b2*b5
               pb26=xm0*b2*b6
               pb27=xm0*b2*b7
               pb33=xm0*b3*b3
               pb34=xm0*b3*b4
               pb35=xm0*b3*b5
               pb36=xm0*b3*b6
               pb37=xm0*b3*b7
               pb44=xm0*b4*b4
               pb45=xm0*b4*b5
               pb46=xm0*b4*b6
               pb47=xm0*b4*b7
               pb55=xm0*b5*b5
               pb56=xm0*b5*b6
               pb57=xm0*b5*b7
               pb66=xm0*b6*b6
               pb67=xm0*b6*b7
               pb77=xm0*b7*b7
c
               wr=wm/w0
               wr4=wr*wr*wr*wr
               gw=1./(1.+1/wr4)
               hvw=1.-sqrt(fv*gw)
               xm1=pb(i,j,k)*pm1*hvw
               cdz=xm1*pz2
c
               xm2=pb(i,j,k)*pm2*hxyz2
               xm2xx=xm2*px2*px2
               xm2xy=xm2*px2*py2
               xm2xz=xm2*px2*pz2
               xm2yy=xm2*py2*py2
               xm2yz=xm2*py2*pz2
               xm2zz=xm2*pz2*pz2
               xm2xt=xm2*px2*pxyz2
               xm2yt=xm2*py2*pxyz2
               xm2zt=xm2*pz2*pxyz2
               xm2tt=xm2*pxyz2*pxyz2
c
      x(13,i+1,j+1,k)=x(13,i+1,j+1,k)        +pb11+cdz+xm2zz
      x(20,i+1,j+1,k)=x(20,i+1,j+1,k)        +pb12    +xm2yz
      x(21,i+1,j+1,k)=x(21,i+1,j+1,k)        +pb13    +xm2xz
      x(22,i+1,j+1,k)=x(22,i+1,j+1,k)        +pb14-cdz-2.*xm2zt
      x(23,i+1,j+1,k)=x(23,i+1,j+1,k)        +pb15    +xm2xz
      x(24,i+1,j+1,k)=x(24,i+1,j+1,k)        +pb16    +xm2yz
      x(25,i+1,j+1,k)=x(25,i+1,j+1,k)        +pb17    +xm2zz
      v(i+1,j+1,k)=v(i+1,j+1,k)                 +pb1*bt(i,j,k)
c
      x(6,i+1,j,k+1) =x(6,i+1,j,k+1)         +pb12    +xm2yz
      x(13,i+1,j,k+1)=x(13,i+1,j,k+1)    +ady+pb22    +xm2yy
      x(16,i+1,j,k+1)=x(16,i+1,j,k+1)        +pb23    +xm2xy
      x(17,i+1,j,k+1)=x(17,i+1,j,k+1)        +pb24    -2.*xm2yt
      x(18,i+1,j,k+1)=x(18,i+1,j,k+1)        +pb25    +xm2xy
      x(19,i+1,j,k+1)=x(19,i+1,j,k+1)    -ady+pb26    +xm2yy
      x(24,i+1,j,k+1)=x(24,i+1,j,k+1)        +pb27    +xm2yz
      v(i+1,j,k+1)=v(i+1,j,k+1)    -ay*by(i,j,k)+pb2*bt(i,j,k)
c
      x(5,i,j+1,k+1) =x(5,i,j+1,k+1)         +pb13    +xm2xz
      x(10,i,j+1,k+1)=x(10,i,j+1,k+1)        +pb23    +xm2xy
      x(13,i,j+1,k+1)=x(13,i,j+1,k+1)    +adx+pb33    +xm2xx
      x(14,i,j+1,k+1)=x(14,i,j+1,k+1)        +pb34    -2.*xm2xt
      x(15,i,j+1,k+1)=x(15,i,j+1,k+1)    -adx+pb35    +xm2xx
      x(18,i,j+1,k+1)=x(18,i,j+1,k+1)        +pb36    +xm2xy
      x(23,i,j+1,k+1)=x(23,i,j+1,k+1)        +pb37    +xm2xz
      v(i,j+1,k+1)=v(i,j+1,k+1)    -ax*bx(i,j,k)+pb3*bt(i,j,k)
c
      x(4,i+1,j+1,k+1) =x(4,i+1,j+1,k+1)     +pb14-cdz   -2.*xm2zt
      x(9,i+1,j+1,k+1) =x(9,i+1,j+1,k+1)     +pb24       -2.*xm2yt
      x(12,i+1,j+1,k+1)=x(12,i+1,j+1,k+1)    +pb34       -2.*xm2xt
      x(13,i+1,j+1,k+1)=x(13,i+1,j+1,k+1)    +pb44+2.*cdz+4.*xm2tt
      x(14,i+1,j+1,k+1)=x(14,i+1,j+1,k+1)    +pb45       -2.*xm2xt
      x(17,i+1,j+1,k+1)=x(17,i+1,j+1,k+1)    +pb46       -2.*xm2yt
      x(22,i+1,j+1,k+1)=x(22,i+1,j+1,k+1)    +pb47-cdz   -2.*xm2zt
      v(i+1,j+1,k+1)=v(i+1,j+1,k+1)             +pb4*bt(i,j,k)
c
      x(3,i+2,j+1,k+1) =x(3,i+2,j+1,k+1)     +pb15    +xm2xz
      x(8,i+2,j+1,k+1) =x(8,i+2,j+1,k+1)     +pb25    +xm2xy
      x(11,i+2,j+1,k+1)=x(11,i+2,j+1,k+1)-adx+pb35    +xm2xx
      x(12,i+2,j+1,k+1)=x(12,i+2,j+1,k+1)    +pb45    -2.*xm2xt
      x(13,i+2,j+1,k+1)=x(13,i+2,j+1,k+1)+adx+pb55    +xm2xx
      x(16,i+2,j+1,k+1)=x(16,i+2,j+1,k+1)    +pb56    +xm2xy
      x(21,i+2,j+1,k+1)=x(21,i+2,j+1,k+1)    +pb57    +xm2xz
      v(i+2,j+1,k+1)=v(i+2,j+1,k+1)+ax*bx(i,j,k)+pb5*bt(i,j,k)
c
      x(2,i+1,j+2,k+1) =x(2,i+1,j+2,k+1)     +pb16    +xm2yz
      x(7,i+1,j+2,k+1) =x(7,i+1,j+2,k+1) -ady+pb26    +xm2yy
      x(8,i+1,j+2,k+1) =x(8,i+1,j+2,k+1)     +pb36    +xm2xy
      x(9,i+1,j+2,k+1) =x(9,i+1,j+2,k+1)     +pb46    -2.*xm2yt
      x(10,i+1,j+2,k+1)=x(10,i+1,j+2,k+1)    +pb56    +xm2xy
      x(13,i+1,j+2,k+1)=x(13,i+1,j+2,k+1)+ady+pb66    +xm2yy
      x(20,i+1,j+2,k+1)=x(20,i+1,j+2,k+1)    +pb67    +xm2yz
      v(i+1,j+2,k+1)=v(i+1,j+2,k+1)+ay*by(i,j,k)+pb6*bt(i,j,k)
c
      x(1,i+1,j+1,k+2) =x(1,i+1,j+1,k+2)     +pb17    +xm2zz
      x(2,i+1,j+1,k+2) =x(2,i+1,j+1,k+2)     +pb27    +xm2yz
      x(3,i+1,j+1,k+2) =x(3,i+1,j+1,k+2)     +pb37    +xm2xz
      x(4,i+1,j+1,k+2) =x(4,i+1,j+1,k+2)     +pb47-cdz-2.*xm2zt
      x(5,i+1,j+1,k+2) =x(5,i+1,j+1,k+2)     +pb57    +xm2xz
      x(6,i+1,j+1,k+2) =x(6,i+1,j+1,k+2)     +pb67    +xm2yz
      x(13,i+1,j+1,k+2)=x(13,i+1,j+1,k+2)    +pb77+cdz+xm2zz
      v(i+1,j+1,k+2)=v(i+1,j+1,k+2)             +pb7*bt(i,j,k)
c
            enddo
         enddo
      enddo
c
c****MATRIX INVERSION THROUGH CONJUGATE GRADIENT
c
      print *,' MATRIX INVERSION THROUGH CONJUGATE GRADIENT'
c
      call conjgrad(x,v,t,a,b,c,d,25,n1,n2,n3)
      itert=iter
      errt=err
      if(ibreak.eq.1)return
c
c****RMS VALUES AND DEVIATIONS
c
      print *,' '
      print *,' RMS VALUES AND DEVIATION'
c
      do k=1,nz
         do j=1,ny
            do i=1,nx
               if(abs(t(i,j,k)).gt.0.)then
                 pt(i,j,k)=1.
               endif
            enddo
         enddo
      enddo
c
      dt=0.
c
      txc=0.
      txa=0.
      etx=0.
c
      tyc=0.
      tya=0.
      ety=0.
c
      at=0.
      tac=0.
      taa=0.
      eta=0.
c
      do k=1,nz-2
         do j=1,ny-2
            do i=1,nx-2
c
               sb=bx(i,j,k)+by(i,j,k)+bt(i,j,k)
               test=pt(i+1,j+1,k)+pt(i+1,j,k+1)+pt(i,j+1,k+1)
     &              +pt(i+1,j+1,k+1)+pt(i+2,j+1,k+1)+pt(i+1,j+2,k+1)
     &              +pt(i+1,j+2,k+1)+pt(i+1,j+1,k+2)
               if(test.ge.8..and.sb.gt.-900.)then
c
                 um=ux(i+1,j+1,k+1)
                 vm=vy(i+1,j+1,k+1)
                 wm=wz(i+1,j+1,k+1)
                 uvw=vel(i+1,j+1,k+1)
c
                 dt=dt+1.
c
                 txc=txc+bx(i,j,k)*bx(i,j,k)
                 dxt=(-t(i,j+1,k+1)+t(i+2,j+1,k+1))/(2.*hx)
                 txa=txa+dxt*dxt
                 etx=etx+(dxt-bx(i,j,k))*(dxt-bx(i,j,k))
                 it(i+1,j+1,k+1,2)=10.*bx(i,j,k)
                 it(i+1,j+1,k+1,3)=10.*dxt
c
                 tyc=tyc+by(i,j,k)*by(i,j,k)
                 dyt=(-t(i+1,j,k+1)+t(i+1,j+2,k+1))/(2.*hy)
                 tya=tya+dyt*dyt
                 ety=ety+(dyt-by(i,j,k))*(dyt-by(i,j,k))
                 it(i+1,j+1,k+1,4)=10.*by(i,j,k)
                 it(i+1,j+1,k+1,5)=10.*dyt
c
                 dzt=(-t(i+1,j+1,k)+t(i+1,j+1,k+2))/(2.*hz)
                 dx2t=(t(i,j+1,k+1)-2.*t(i+1,j+1,k+1)
     &                             +t(i+2,j+1,k+1))/(hx*hx)
                 dy2t=(t(i+1,j,k+1)-2.*t(i+1,j+1,k+1)
     &                             +t(i+1,j+2,k+1))/(hy*hy)
                 dz2t=(t(i+1,j+1,k)-2.*t(i+1,j+1,k+1)
     &                             +t(i+1,j+1,k+2))/(hz*hz)
                 vdt=(um-dxk(i,j,k))*dxt+(vm-dyk(i,j,k))*dyt
     &                                  +(wm-dzk(i,j,k))*dzt
     &               -xkm(i,j,k)*(dx2t+dy2t+dz2t)
     &               -beta(i,j,k)*t(i+1,j+1,k+1)
                 it(i+1,j+1,k+1,6)=bt(i,j,k)
                 it(i+1,j+1,k+1,7)=vdt
                 if(uvw.gt.v0)then
                   at=at+1.
                   tac=tac+bt(i,j,k)*bt(i,j,k)
                   taa=taa+vdt*vdt
                   eta=eta+(vdt-bt(i,j,k))*(vdt-bt(i,j,k))
                 endif
c
               endif
            enddo
         enddo
      enddo
      if(dt.lt.1.)return
c
      print *,' NPTS BX,BY =',dt
      print *,' rms BX calc,retr,dev : '
     &       ,sqrt(txc/dt),sqrt(txa/dt),sqrt(etx/dt)
     &       ,' ETX=',etx/amax1(0.001,txc)
      et1=etx/amax1(0.001,txc)
      if(i2d.ne.1)then
        print *,' rms BY calc,retr,dev : '
     &         ,sqrt(tyc/dt),sqrt(tya/dt),sqrt(ety/dt)
     &         ,' ETY=',ety/amax1(0.001,tyc)
        et2=ety/amax1(0.001,tyc)
      endif
      print *,' NTPS BT =',at
      if(at.lt.1.)return
      print *,' rms BT calc,retr,dev : '
     &       ,sqrt(tac/at),sqrt(taa/at),sqrt(eta/at)
     &       ,' ETT=',eta/amax1(0.001,tac)
      et3=eta/amax1(0.001,tac)
c
      print *,' '
      if(i2d.eq.1)then
        eth=etx/txc
      else
        eth=(etx/txc+ety/tyc)/2.
      endif
      ett=eta/tac
      epsi=eth/ett
      test=amax1(epsi,1./epsi)
      print *,' IPASS,MU0,ETH,ETT,TEST:',ipass,pm0,eth,ett,test
      print *,' '
      print *,' '
      write(66,100)ipass,pm0,iter,eth,ett,test
  100 format(' PASS',i3,'  MU0=',f6.3,'  NB ITER=',i4
     &       ,'  ETH=',f5.3,'  ETT=',f5.3,'  TEST=',f7.3)
      if(ipass.eq.1)then
        pm0_inf=pm0
        epsi_inf=epsi
        if(iterative.eq.1)go to 1
      endif
      if(ipass.ge.2.and.test.gt.testmax)then
        if(epsi.lt.1..and.epsi.gt.epsi_inf)then
          pm0_inf=pm0
          epsi_inf=epsi
        endif
        if(epsi.gt.1..and.epsi.lt.epsi_sup)then
          pm0_sup=pm0
          epsi_sup=epsi
        endif
        if(epsi_sup.lt.900.)then
          pm0=(pm0_sup*(1.-epsi_inf)+pm0_inf*(epsi_sup-1.))
     &        /(epsi_sup-epsi_inf)
        else
          pm0=pm0/epsi
        endif
        go to 1
      endif
c
      xt=0.
      tmoy=0.
      do k=1,nz
         do j=1,ny
            do i=1,nx
               if(abs(t(i,j,k)).gt.0.)then
                 tmoy=tmoy+t(i,j,k)
                 xt=xt+1.
               endif
            enddo
         enddo
      enddo
      if(xt.gt.0.)tmoy=tmoy/xt
      do k=1,nz
         do j=1,ny
            do i=1,nx
               if(abs(t(i,j,k)).gt.0.)then
                 t(i,j,k)=t(i,j,k)-tmoy+ct0
                 it(i,j,k,1)=10.*t(i,j,k)
                 pt(i,j,k)=1.
               else
                 pt(i,j,k)=0.
               endif
            enddo
         enddo
      enddo
c
      return
      end
c
c**** THIS ROUTINE WRITES THE MATRIX AND VECTOR FOR PRESSURE RETRIEVAL
c
      subroutine ajusp(pa,pp,ax,ay,az,x,v,p,cp0,a,b,c,d
     &                 ,tetav0,rho0,ip,n1,n2,n3)
c
      real ax(n1,n2,n3),ay(n1,n2,n3),az(n1,n2,n3)
     &    ,pa(n1,n2,n3),pp(n1,n2,n3)
     &    ,x(27,n1,n2,n3),v(n1,n2,n3),p(n1,n2,n3)
     &    ,a(n1,n2,n3),b(n1,n2,n3),c(n1,n2,n3),d(n1,n2,n3)
     &    ,tetav0(40),rho0(40)
      integer ip(n1,n2,n3,7)
c
      common/grid/hx,hy,hz,nx,ny,nz,i2d
      common/constraints/iterative,v0,w0,pm0_init,pm1,pm2
     &                                  ,pn0_init,pn2,pn3
      common/matinv/iter,itermax,err,errmax,ibreak,testmax
      common/resulp/iterp,errp,dp,pxc,pxa,epx
     &              ,pyc,pya,epy,pzc,pza,epz,ep1,ep2,ep3
      common/coord/i0,j0,k0
c
      hxyz=(hx*hy*hz)**(2./3.)
c
c**** BEGINS THE ITERATIVE RETRIEVAL
c
      ipass=0
      epsi_sup=+999.
  1   ipass=ipass+1
      if(ipass.eq.1)pn0=0.
      if(ipass.eq.2)pn0=pn0_init
      print *,' IPASS=',ipass,' NU0=',pn0
c
c**** INITIALIZATIONS
c
      if(ipass.le.3)then
        do k=1,nz
           do j=1,ny
              do i=1,nx
                 p(i,j,k)=0.
              enddo
           enddo
        enddo
      endif
c
      do k=1,nz
         do j=1,ny
            do i=1,nx
               v(i,j,k)=0.
               a(i,j,k)=0.
               b(i,j,k)=0.
               c(i,j,k)=0.
               d(i,j,k)=0.
               do l=1,27
                  x(l,i,j,k)=0.
               enddo
               do n=1,7
                  ip(i,j,k,n)=999
               enddo
            enddo
         enddo
      enddo
c
c****MATRIX AND VECTOR FOR PRESSURE
c
      print *,' MATRIX AND VECTOR FOR PRESSURE'
c
      do k=1,nz-1
         do j=1,ny-1
            do i=1,nx-1
               px=pa(i,j,k)/(4.*hx)
               px2=px/(4.*hx)
               py=pa(i,j,k)/(4.*hy)
               py2=py/(4.*hy)
               pz=pa(i,j,k)*pn0/(4.*hz)
               pz2=pz/(4.*hz)
               cdxy=pa(i,j,k)*pn2/(4.*hx*hy)
               cdyz=pa(i,j,k)*pn2/(4.*hy*hz)
               cdzx=pa(i,j,k)*pn2/(4.*hz*hx)
               cd21=+cdxy+cdyz+cdzx
               cd22=-cdxy+cdyz-cdzx
               cd23=-cdxy-cdyz+cdzx
               cd24=+cdxy-cdyz-cdzx
               cd3=pa(i,j,k)*pn3/hxyz
c
               x(14,i,j,k)=x(14,i,j,k)+px2+py2+pz2+cd3+cd21
               x(15,i,j,k)=x(15,i,j,k)-px2+py2+pz2-cd3+cd22
               x(17,i,j,k)=x(17,i,j,k)+px2-py2+pz2-cd3+cd23
               x(18,i,j,k)=x(18,i,j,k)-px2-py2+pz2+cd3+cd24
               x(23,i,j,k)=x(23,i,j,k)+px2+py2-pz2-cd3+cd24
               x(24,i,j,k)=x(24,i,j,k)-px2+py2-pz2+cd3+cd23
               x(26,i,j,k)=x(26,i,j,k)+px2-py2-pz2+cd3+cd22
               x(27,i,j,k)=x(27,i,j,k)-px2-py2-pz2-cd3+cd21
               v(i,j,k)=v(i,j,k)-px*ax(i,j,k)-py*ay(i,j,k)
     &                                       -pz*az(i,j,k)
c
               x(13,i+1,j,k)=x(13,i+1,j,k)-px2+py2+pz2-cd3+cd22
               x(14,i+1,j,k)=x(14,i+1,j,k)+px2+py2+pz2+cd3+cd21
               x(16,i+1,j,k)=x(16,i+1,j,k)-px2-py2+pz2+cd3+cd24
               x(17,i+1,j,k)=x(17,i+1,j,k)+px2-py2+pz2-cd3+cd23
               x(22,i+1,j,k)=x(22,i+1,j,k)-px2+py2-pz2+cd3+cd23
               x(23,i+1,j,k)=x(23,i+1,j,k)+px2+py2-pz2-cd3+cd24
               x(25,i+1,j,k)=x(25,i+1,j,k)-px2-py2-pz2-cd3+cd21
               x(26,i+1,j,k)=x(26,i+1,j,k)+px2-py2-pz2+cd3+cd22
               v(i+1,j,k)=v(i+1,j,k)+px*ax(i,j,k)-py*ay(i,j,k)
     &                                           -pz*az(i,j,k)
c
               x(11,i,j+1,k)=x(11,i,j+1,k)+px2-py2+pz2-cd3+cd23
               x(12,i,j+1,k)=x(12,i,j+1,k)-px2-py2+pz2+cd3+cd24
               x(14,i,j+1,k)=x(14,i,j+1,k)+px2+py2+pz2+cd3+cd21
               x(15,i,j+1,k)=x(15,i,j+1,k)-px2+py2+pz2-cd3+cd22
               x(20,i,j+1,k)=x(20,i,j+1,k)+px2-py2-pz2+cd3+cd22
               x(21,i,j+1,k)=x(21,i,j+1,k)-px2-py2-pz2-cd3+cd21
               x(23,i,j+1,k)=x(23,i,j+1,k)+px2+py2-pz2-cd3+cd24
               x(24,i,j+1,k)=x(24,i,j+1,k)-px2+py2-pz2+cd3+cd23
               v(i,j+1,k)=v(i,j+1,k)-px*ax(i,j,k)+py*ay(i,j,k)
     &                                           -pz*az(i,j,k)
c
               x(10,i+1,j+1,k)=x(10,i+1,j+1,k)-px2-py2+pz2+cd3+cd24
               x(11,i+1,j+1,k)=x(11,i+1,j+1,k)+px2-py2+pz2-cd3+cd23
               x(13,i+1,j+1,k)=x(13,i+1,j+1,k)-px2+py2+pz2-cd3+cd22
               x(14,i+1,j+1,k)=x(14,i+1,j+1,k)+px2+py2+pz2+cd3+cd21
               x(19,i+1,j+1,k)=x(19,i+1,j+1,k)-px2-py2-pz2-cd3+cd21
               x(20,i+1,j+1,k)=x(20,i+1,j+1,k)+px2-py2-pz2+cd3+cd22
               x(22,i+1,j+1,k)=x(22,i+1,j+1,k)-px2+py2-pz2+cd3+cd23
               x(23,i+1,j+1,k)=x(23,i+1,j+1,k)+px2+py2-pz2-cd3+cd24
               v(i+1,j+1,k)=v(i+1,j+1,k)+px*ax(i,j,k)+py*ay(i,j,k)
     &                                               -pz*az(i,j,k)
c
               x(5,i,j,k+1)=x(5,i,j,k+1)+px2+py2-pz2-cd3+cd24
               x(6,i,j,k+1)=x(6,i,j,k+1)-px2+py2-pz2+cd3+cd23
               x(8,i,j,k+1)=x(8,i,j,k+1)+px2-py2-pz2+cd3+cd22
               x(9,i,j,k+1)=x(9,i,j,k+1)-px2-py2-pz2-cd3+cd21
               x(14,i,j,k+1)=x(14,i,j,k+1)+px2+py2+pz2+cd3+cd21
               x(15,i,j,k+1)=x(15,i,j,k+1)-px2+py2+pz2-cd3+cd22
               x(17,i,j,k+1)=x(17,i,j,k+1)+px2-py2+pz2-cd3+cd23
               x(18,i,j,k+1)=x(18,i,j,k+1)-px2-py2+pz2+cd3+cd24
               v(i,j,k+1)=v(i,j,k+1)-px*ax(i,j,k)-py*ay(i,j,k)
     &                                           +pz*az(i,j,k)
c
               x(4,i+1,j,k+1)=x(4,i+1,j,k+1)-px2+py2-pz2+cd3+cd23
               x(5,i+1,j,k+1)=x(5,i+1,j,k+1)+px2+py2-pz2-cd3+cd24
               x(7,i+1,j,k+1)=x(7,i+1,j,k+1)-px2-py2-pz2-cd3+cd21
               x(8,i+1,j,k+1)=x(8,i+1,j,k+1)+px2-py2-pz2+cd3+cd22
               x(13,i+1,j,k+1)=x(13,i+1,j,k+1)-px2+py2+pz2-cd3+cd22
               x(14,i+1,j,k+1)=x(14,i+1,j,k+1)+px2+py2+pz2+cd3+cd21
               x(16,i+1,j,k+1)=x(16,i+1,j,k+1)-px2-py2+pz2+cd3+cd24
               x(17,i+1,j,k+1)=x(17,i+1,j,k+1)+px2-py2+pz2-cd3+cd23
               v(i+1,j,k+1)=v(i+1,j,k+1)+px*ax(i,j,k)-py*ay(i,j,k)
     &                                               +pz*az(i,j,k)
c
               x(2,i,j+1,k+1)=x(2,i,j+1,k+1)+px2-py2-pz2+cd3+cd22
               x(3,i,j+1,k+1)=x(3,i,j+1,k+1)-px2-py2-pz2-cd3+cd21
               x(5,i,j+1,k+1)=x(5,i,j+1,k+1)+px2+py2-pz2-cd3+cd24
               x(6,i,j+1,k+1)=x(6,i,j+1,k+1)-px2+py2-pz2+cd3+cd23
               x(11,i,j+1,k+1)=x(11,i,j+1,k+1)+px2-py2+pz2-cd3+cd23
               x(12,i,j+1,k+1)=x(12,i,j+1,k+1)-px2-py2+pz2+cd3+cd24
               x(14,i,j+1,k+1)=x(14,i,j+1,k+1)+px2+py2+pz2+cd3+cd21
               x(15,i,j+1,k+1)=x(15,i,j+1,k+1)-px2+py2+pz2-cd3+cd22
               v(i,j+1,k+1)=v(i,j+1,k+1)-px*ax(i,j,k)+py*ay(i,j,k)
     &                                               +pz*az(i,j,k)
c
               x(1,i+1,j+1,k+1)=x(1,i+1,j+1,k+1)-px2-py2-pz2-cd3+cd21
               x(2,i+1,j+1,k+1)=x(2,i+1,j+1,k+1)+px2-py2-pz2+cd3+cd22
               x(4,i+1,j+1,k+1)=x(4,i+1,j+1,k+1)-px2+py2-pz2+cd3+cd23
               x(5,i+1,j+1,k+1)=x(5,i+1,j+1,k+1)+px2+py2-pz2-cd3+cd24
               x(10,i+1,j+1,k+1)=x(10,i+1,j+1,k+1)-px2-py2+pz2+cd3+cd24
               x(11,i+1,j+1,k+1)=x(11,i+1,j+1,k+1)+px2-py2+pz2-cd3+cd23
               x(13,i+1,j+1,k+1)=x(13,i+1,j+1,k+1)-px2+py2+pz2-cd3+cd22
               x(14,i+1,j+1,k+1)=x(14,i+1,j+1,k+1)+px2+py2+pz2+cd3+cd21
               v(i+1,j+1,k+1)=v(i+1,j+1,k+1)+px*ax(i,j,k)+py*ay(i,j,k)
     &                                           +pz*az(i,j,k)
c
            enddo
         enddo
      enddo
c
c****MATRIX INVERSION THROUGH CONJUGATE GRADIENT
c
      print *,' MATRIX INVERSION THROUGH CONJUGATE GRADIENT'
c
      call conjgrad(x,v,p,a,b,c,d,27,n1,n2,n3)
      iterp=iter
      errp=err
      if(ibreak.eq.1)return
c
c****RMS VALUES AND DEVIATIONS
c
      print *,' '
      print *,' RMS VALUES AND DEVIATIONS'
c
      do k=1,nz
         do j=1,ny
            do i=1,nx
               if(abs(p(i,j,k)).gt.0.)then
                  pp(i,j,k)=1.
               endif
            enddo
         enddo
      enddo
c
      dp=0.
c
      pxc=0.
      pxa=0.
      epx=0.
c
      pyc=0.
      pya=0.
      epy=0.
c
      pzc=0.
      pza=0.
      epz=0.
c
      do k=1,nz-1
         k0=k
         do j=1,ny-1
            j0=j
            do i=1,nx-1
               i0=i
c
               test=xmean(pp,n1,n2,n3)
               if(test.ge.1.)then
c
                 dp=dp+1.
c
                 pxc=pxc+ax(i,j,k)*ax(i,j,k)
                 dxp=(-p(i,j,k)+p(i+1,j,k)
     &                -p(i,j+1,k)+p(i+1,j+1,k)
     &                -p(i,j,k+1)+p(i+1,j,k+1)
     &                -p(i,j+1,k+1)+p(i+1,j+1,k+1))/(4.*hx)
                 pxa=pxa+dxp*dxp
                 epx=epx+(dxp-ax(i,j,k))*(dxp-ax(i,j,k))
                 ip(i,j,k,2)=ax(i,j,k)/10.
                 ip(i,j,k,3)=dxp/10.
c
                 pyc=pyc+ay(i,j,k)*ay(i,j,k)
                 dyp=(-p(i,j,k)-p(i+1,j,k)
     &                +p(i,j+1,k)+p(i+1,j+1,k)
     &                -p(i,j,k+1)-p(i+1,j,k+1)
     &                +p(i,j+1,k+1)+p(i+1,j+1,k+1))/(4.*hy)
                 pya=pya+dyp*dyp
                 epy=epy+(dyp-ay(i,j,k))*(dyp-ay(i,j,k))
                 ip(i,j,k,4)=ay(i,j,k)/10.
                 ip(i,j,k,5)=dyp/10.
c
                 pzc=pzc+az(i,j,k)*az(i,j,k)
                 dzp=(-p(i,j,k)-p(i+1,j,k)
     &                -p(i,j+1,k)-p(i+1,j+1,k)
     &                +p(i,j,k+1)+p(i+1,j,k+1)
     &                +p(i,j+1,k+1)+p(i+1,j+1,k+1))/(4.*hz)
                 pza=pza+dzp*dzp
                 epz=epz+(dzp-az(i,j,k))*(dzp-az(i,j,k))
                 ip(i,j,k,6)=az(i,j,k)/10.
                 ip(i,j,k,7)=dzp/10.
c
               endif
c
            enddo
         enddo
      enddo
c
      print *,' NPTS AX,AY =',dp
      if(dp.lt.1.)return
      print *,' rms AX calc,retr,dev : '
     &       ,sqrt(pxc/dp),sqrt(pxa/dp),sqrt(epx/dp)
     &       ,' EPX=',epx/amax1(0.001,pxc)
      ep1=epx/amax1(0.001,pxc)
      if(ny.ne.4)then
        print *,' rms AY calc,retr,dev : '
     &         ,sqrt(pyc/dp),sqrt(pya/dp),sqrt(epy/dp)
     &         ,' EPY=',epy/amax1(0.001,pyc)
        ep2=epy/amax1(0.001,pyc)
      endif
      print *,' rms AZ calc,retr,dev : '
     &       ,sqrt(pzc/dp),sqrt(pza/dp),sqrt(epz/dp)
     &       ,' EPZ=',epz/amax1(0.001,pzc)
      ep3=epz/amax1(0.001,pzc)
c
      print *,' '
      if(i2d.eq.1)then
        eph=epx/pxc
      else
        eph=(epx/pxc+epy/pyc)/2.
      endif
      epv=epz/pzc
      epsi=eph/epv
      test=amax1(epsi,1./epsi)
      print *,' IPASS,NU0,EPH,EPV,TEST:',ipass,pn0,eph,epv,test
      print *,' '
      print *,' '
      write(66,100)ipass,pn0,iter,eph,epv,test
  100 format(' PASS',i3,'  NU0=',f6.3,'  NB ITER=',i4
     &       ,'  EPH=',f5.3,'  EPZ=',f5.3,'  TEST=',f7.3)
      if(ipass.eq.1)then
        pn0_inf=pn0
        epsi_inf=epsi
        go to 1
      endif
      if(ipass.ge.2.and.pn0.gt.1.e-10.and.test.gt.testmax)then
        if(epsi.lt.1..and.epsi.gt.epsi_inf)then
          pn0_inf=pn0
          epsi_inf=epsi
        endif
        if(epsi.gt.1..and.epsi.lt.epsi_sup)then
          pn0_sup=pn0
          epsi_sup=epsi
        endif
        if(epsi_sup.lt.900.)then
          pn0=(pn0_sup*(1.-epsi_inf)+pn0_inf*(epsi_sup-1.))
     &        /(epsi_sup-epsi_inf)
        else
          pn0=pn0/epsi
        endif
        go to 1
      endif
c
      xp=0.
      pmoy=0.
      do k=1,nz
         do j=1,ny
            do i=1,nx
               if(abs(p(i,j,k)).ge.0.)then
                 pmoy=pmoy+p(i,j,k)
                 xp=xp+1.
               endif
            enddo
         enddo
      enddo
      if(xp.gt.0.)pmoy=pmoy/xp
      do k=1,nz
         cpr=1.005*tetav0(k+1)/1000.*rho0(k+1)
         do j=1,ny
            do i=1,nx
               if(abs(p(i,j,k)).gt.0.)then
                 p(i,j,k)=cpr*(p(i,j,k)-pmoy)+cp0
                 ip(i,j,k,1)=p(i,j,k)/100.
               endif
            enddo
         enddo
      enddo
c
      return
      end
c
c**** THIS ROUTINE CACULATES S SUCH AS
c**** ( S.X = V )  WITH X=MATRIX, V=VECTOR
c**** THROUGH THE CONJUGATE GRADIENT METHOD
c**** ( HESTENES AND STIEFEL, 1952: J.RECH.NAT.BUR.STNDS,49,409-436)
c
      subroutine conjgrad(x,v,s,xs,g,w,xw,ndiag,n1,n2,n3)
c
      real x(27,n1,n2,n3),v(n1,n2,n3),s(n1,n2,n3)
     &    ,xs(n1,n2,n3),g(n1,n2,n3),w(n1,n2,n3),xw(n1,n2,n3)
c
      common/grid/hx,hy,hz,nx,ny,nz,i2d
      common/matinv/iter,itermax,err,errmax,ibreak,testmax
c
      ibreak=0
c
      vv=sqrt(sp(v,v,n1,n2,n3))
      if(vv.le.0.)then
        print *,' !!! NULL VECTOR !!!'
        return
      endif
      call mvm(x,v,xw,ndiag,n1,n2,n3)
      xv=sp(xw,xw,n1,n2,n3)
      if(xv.le.0.)then
        print *,' !!! NULL MATRIX !!!'
        return
      endif
c
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c!!!!      call mvm(x,s,xs,ndiag,n1,n2,n3)
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c
      do k=1,nz
         do j=1,ny
            do i=1,nx
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c!!!!               g(i,j,k)=xs(i,j,k)-v(i,j,k)
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
               g(i,j,k)=-v(i,j,k)
               w(i,j,k)=g(i,j,k)
            enddo
         enddo
      enddo
c
      iter=0
c
  1   iter=iter+1
c
      call mvm(x,w,xw,ndiag,n1,n2,n3)
      xww=sp(xw,w,n1,n2,n3)
      gw=sp(g,w,n1,n2,n3)
      a=gw/xww
c
      do k=1,nz
         do j=1,ny
            do i=1,nx
               s(i,j,k)=s(i,j,k)-a*w(i,j,k)
            enddo
         enddo
      enddo
c
      call mvm(x,s,xs,ndiag,n1,n2,n3)
c
      do k=1,nz
         do j=1,ny
            do i=1,nx
               g(i,j,k)=xs(i,j,k)-v(i,j,k)
            enddo
         enddo
      enddo
c
      grad=sqrt(sp(g,g,nx,ny,nz))
      err=grad/vv
      if(err.le.errmax)then
        print *,' '
        print *,'  CONJGRAD STOPS AT ITERATION',iter
     &         ,'    RELATIVE ERROR :',err
      else
        if(iter.eq.itermax)then
          ibreak=1
          print *,'  !!!! NO CONVERGENCE AFTER',itermax
     &           ,' ITERATIONS !!!!'
          return
        else
          if(iter.gt.100.and.err.gt.10.)then
            ibreak=1
            print *,'  CONJGRAD DIVERGES AT ITERATION',iter
     &             ,'    RELATIVE ERROR :',err
     &             ,' -> STOP !!!!'
            return
          endif
          if(iter.eq.10*(iter/10))print *,'  ITERATION',iter
     &                             ,'    RELATIVE ERROR :',err
 
          gxw=sp(g,xw,nx,ny,nz)
          b=gxw/xww
          do k=1,nz
             do j=1,ny
                do i=1,nx
                   w(i,j,k)=g(i,j,k)-b*w(i,j,k)
                enddo
             enddo
          enddo
          go to 1
        endif
      endif
c
      return
      end
c
c**** THIS ROUTINE MULTIPLIES VECTOR V BY MATRIX X
c**** AND RETURNS XV=V*X
c
      subroutine mvm(x,v,xv,ndiag,n1,n2,n3)
c
      real x(27,n1,n2,n3),v(n1,n2,n3),xv(n1,n2,n3)
      integer i1(27),i1t(27),i1p(27),i2(27),i2t(27),i2p(27)
     &       ,j1(27),j1t(27),j1p(27),j2(27),j2t(27),j2p(27)
     &       ,k1(27),k1t(27),k1p(27),k2(27),k2t(27),k2p(27)
c
      data i1t/0,0,1,0,0,0,0,1,0,0,2,1
     &         ,0,0,0,1,0,0,0,0,1,0,0,0,0
     &         ,0,0/
      data i2t/0,0,0,0,1,0,0,0,0,1,0,0
     &         ,0,1,2,0,0,1,0,0,0,0,1,0,0
     &         ,0,0/
      data j1t/0,1,0,0,0,0,2,1,1,1,0,0
     &         ,0,0,0,0,0,0,0,1,0,0,0,0,0
     &         ,0,0/
      data j2t/0,0,0,0,0,1,0,0,0,0,0,0
     &         ,0,0,0,1,1,1,2,0,0,0,0,1,0
     &         ,0,0/
      data k1t/2,1,1,1,1,1,0,0,0,0,0,0
     &         ,0,0,0,0,0,0,0,0,0,0,0,0,0
     &         ,0,0/
      data k2t/0,0,0,0,0,0,0,0,0,0,0,0
     &         ,0,0,0,0,0,0,0,1,1,1,1,1,2
     &         ,0,0/
c
      data i1p/1,0,0,1,0,0,1,0,0,1,0,0,1
     &         ,0,0,1,0,0,1,0,0,1,0,0,1,0,0/
      data i2p/0,0,1,0,0,1,0,0,1,0,0,1,0
     &         ,0,1,0,0,1,0,0,1,0,0,1,0,0,1/
      data j1p/1,1,1,0,0,0,0,0,0,1,1,1,0
     &         ,0,0,0,0,0,1,1,1,0,0,0,0,0,0/
      data j2p/0,0,0,0,0,0,1,1,1,0,0,0,0
     &         ,0,0,1,1,1,0,0,0,0,0,0,1,1,1/
      data k1p/1,1,1,1,1,1,1,1,1,0,0,0,0
     &         ,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
      data k2p/0,0,0,0,0,0,0,0,0,0,0,0,0
     &         ,0,0,0,0,0,1,1,1,1,1,1,1,1,1/
c
      common/grid/hx,hy,hz,nx,ny,nz,i2d
c
      if(ndiag.eq.25)then
c
        do l=1,27
           i1(l)=i1t(l)
           i2(l)=i2t(l)
           j1(l)=j1t(l)
           j2(l)=j2t(l)
           k1(l)=k1t(l)
           k2(l)=k2t(l)
        enddo
c
      else
c
        do l=1,27
           i1(l)=i1p(l)
           i2(l)=i2p(l)
           j1(l)=j1p(l)
           j2(l)=j2p(l)
           k1(l)=k1p(l)
           k2(l)=k2p(l)
        enddo
c
      endif
c
      do k=1,nz
         do j=1,ny
            do i=1,nx
               xv(i,j,k)=0.
            enddo
         enddo
      enddo
c
      do l=1,27
         imin=1+i1(l)
         imax=nx-i2(l)
         jmin=1+j1(l)
         jmax=ny-j2(l)
         kmin=1+k1(l)
         kmax=nz-k2(l)
c
         if(l.le.ndiag)then
           do i=imin,imax
              iv=i-i1(l)+i2(l)
              do j=jmin,jmax
                 jv=j-j1(l)+j2(l)
                 do k=kmin,kmax
                      kv=k-k1(l)+k2(l)
                    xv(i,j,k)=xv(i,j,k)+x(l,i,j,k)*v(iv,jv,kv)
                 enddo
              enddo
           enddo
         endif
c
      enddo
c
      return
      end
c
c**** THIS FUNCTION RETURNS THE SCALAR PRODUCT
c**** OF VECTORS U AND V
c
      function sp(u,v,n1,n2,n3)
c
      real u(n1,n2,n3),v(n1,n2,n3)
c
      common/grid/hx,hy,hz,nx,ny,nz,i2d
c
      sp=0.
      do k=1,nz
         do j=1,ny
            do i=1,nx
               sp=sp+u(i,j,k)*v(i,j,k)
            enddo
         enddo
      enddo
c
      return
      end
c
c**** THIS FUNCTION RETURNS A MEAN VALUE OF ARRAY T(NX,NY,NZ)
c**** FOR ((I,I+1),(J,J+1),(K,K+1))
c
      function xmean(t,n1,n2,n3)
c
      real t(n1,n2,n3)
c
      common/coord/i,j,k
c
      xmean=(t(i,j,k)+t(i+1,j,k)+t(i,j+1,k)+t(i,j,k+1)
     &      +t(i+1,j+1,k)+t(i+1,j,k+1)
     &      +t(i,j+1,k+1)+t(i+1,j+1,k+1))/8.
c
      return
      end
c
c**** THIS ROUTINE PRINTS THE RESULTS FOR TEMPERATURE
c**** ON A FORMATTED FILE oTAP_*
c
      subroutine outputt(it,kk,y0,n1,n2,n3)
c
      integer it(n1,n2,n3,7),kk(nz)
      character fileTPTP*100,date*12
c
      common/grid/hx,hy,hz,nx,ny,nz,i2d
      common/result/iter,err,dt,txc,txa,etx,tyc,tya,ety,at,tac,taa,eta
     +              ,et1,et2,et3
      common/constraints/iterative,v0,w0,pm0_init,pm1,pm2
     &                                  ,pn0_init,pn2,pn3
      common/ident/fileTPTP,date,ihhmmss1,ihhmmss2
c
      do k=1,nz
         do j=1,ny
            do i=1,nx
               do l=1,7
                  if(it(i,j,k,l).lt.-99.or.it(i,j,k,l).gt.999)then
                    it(i,j,k,l)=999
                  endif
               enddo
            enddo
         enddo
      enddo
c
      xk=24./float(max0(24,nz-1))
      do k=1,25
         kk(k)=(1.+float(k-1)/xk)
      enddo
c
      write(66,13)
  13  format(//,' d(TETAc1)/dX (K/km)')
      write(66,16)dt,sqrt(txc/dt),sqrt(txa/dt),sqrt(etx/dt),etx/txc
      write(66,14)
  14  format(//,' d(TETAc1)/dY (K/km)')
      write(66,16)dt,sqrt(tyc/dt),sqrt(tya/dt),sqrt(ety/dt)
     &            ,ety/amax1(0.001,tyc)
      write(66,15)
  15  format(//,' ADV + DIFF (TETAc1) (10-3 K/s)')
      aat=amax1(1.,at)
      write(66,16)at,sqrt(tac/aat),sqrt(taa/aat),sqrt(eta/aat)
     &            ,eta/amax1(0.001,tac)
  16  format(' npts=',f7.0,'   rms calc=',f6.3
     &       ,'   rms retr=',f6.3,'   rms diff=',f6.3
     &       ,/,'        ER=',f6.3)
      write(66,17)
  17  format(////////////////////////////////////)
c
      do j=1,ny
         print *,'   YZ PLANE NO',j
c
         do l=1,7
c
            if((j-1)*(j-ny).ne.0.or.l.eq.1)then
              y=y0+float(j-1)*hy
c
              write(66,100)y
  100         format(//////////,' XZ VERTICAL PLANE Y=',f5.1,'km',/)
c
              go to (1,2,3,4,5,6,7),l
c
  1           write(66,101)
  101         format(' RETRIEVED TETAc1 (0.1 K)',//)
              go to 20
  2           write(66,102)
  102         format(' CALCULATED BX (0.1 K/km)',//)
              go to 20
  3           write(66,103)
  103         format(' RETRIEVED d(TETAc1)/dX (0.1 K/km)',//)
              go to 20
  4           write(66,104)
  104         format(' CALCULATED BY (0.1 K/km)',//)
              go to 20
  5           write(66,105)
  105         format(' RETRIEVED d(TETAc1)/dY (0.1 K/km)',//)
              go to 20
  6           write(66,106)
  106         format(' CALCULATED BT (10-3 K/s)',//)
              go to 20
  7           write(66,107)
  107         format(' RETRIEVED (V.D)Tc1-DIFF(Tc1)-BETA*Tc1'
     &               ,' (10-3 K/s)',//)
c
  20              nk=min0(nz,25)
              write(66,200)(kk(k),k=1,nk)
  200         format(/,1x,'i/k ',25i3,/)
              do i=1,nx
                 write(66,300)i,(it(i,j,kk(k),l),k=1,nk)
              enddo
  300         format(1x,i2,2x,25i3)
              write(66,200)(kk(k),k=1,nk)
c
            endif
c
         enddo
c
      enddo
c
      return
      end
c
c**** THIS ROUTINE PRINTS THE RESULTS FOR PRESSURE
c**** ON A FORMATTED FILE oTAP_*
c
      subroutine outputp(ip,kk,y0,n1,n2,n3)
c
      integer ip(n1,n2,n3,7),kk(n3)
      character fileTPTP*100,date*12
c
      common/grid/hx,hy,hz,nx,ny,nz,i2d
      common/constraints/iterative,v0,w0,pm0_init,pm1,pm2
     &                                  ,pn0_init,pn2,pn3
      common/resulp/iter,err,dp,pxc,pxa,epx,pyc,pya,epy,pzc,pza,epz
     +              ,ep1,ep2,ep3
      common/ident/fileTPTP,date,ihhmmss1,ihhmmss2
c
      do k=1,nz
         do j=1,ny
            do i=1,nx
               do l=1,7
                  if(ip(i,j,k,l).lt.-99.or.ip(i,j,k,l).gt.999)then
                    ip(i,j,k,l)=999
                  endif
               enddo
            enddo
         enddo
      enddo
c
      xk=24./float(max0(24,nz-1))
      do k=1,25
         kk(k)=(1.+float(k-1)/xk)
      enddo
c
      write(66,13)
  13  format(//,1x,'d(Pi1)/dX (10-8 m-1)')
      write(66,16)dp,sqrt(pxc/dp)/10.,sqrt(pxa/dp)/10.
     &            ,sqrt(epx/dp)/10.,epx/amax1(0.001,pxc)
      write(66,14)
  14  format(//,1x,'d(Pi1)/dY (10-8 m-1)')
      write(66,16)dp,sqrt(pyc/dp)/10.,sqrt(pya/dp)/10.
     &            ,sqrt(epy/dp)/10.,epy/amax1(0.001,pyc)
      write(66,15)
  15  format(//,1x,'d(Pi1)/dZ (10-8 m-1)')
      write(66,16)dp,sqrt(pzc/dp)/10.,sqrt(pza/dp)/10.
     &            ,sqrt(epz/dp)/10.,epz/amax1(0.001,pzc)
  16  format(1x,'npts=',f7.0,'   rms calc=',f6.3
     &       ,'   rms retr=',f6.3,'   rms diff=',f6.3
     &       ,/,'        ER=',f6.3)
      write(66,17)
  17  format(////////////////////////////////////)
c
      do j=1,ny
c
         do l=1,7
c
            if(j.ne.ny.or.l.eq.1)then
              y=y0+(float(j)-0.5)*hy
              if(l.eq.1)y=y-hy/2.
c
              write(66,100)y
  100         format(//////////,' XZ VERTICAL PLANE Y=',f5.1,'km',/)
c
              go to (1,2,3,4,5,6,7),l
c
  1           write(66,101)
  101         format(' RETRIEVED P1 (hPa)',//)
              go to 20
  2           write(66,102)
  102         format(' CALCULATED AX (10-8 m-1)',//)
              go to 20
  3           write(66,103)
  103         format(' RETRIEVED d(Pi1)/dX (10-8 m-1)',//)
              go to 20
  4           write(66,104)
  104         format(' CALCULATED AY (10-8 m-1)',//)
              go to 20
  5           write(66,105)
  105         format(' RETRIEVED d(Pi1)/dY (10-8 m-1)',//)
              go to 20
  6           write(66,106)
  106         format(' CALCULATED AZ (10-8 m-1)',//)
              go to 20
  7           write(66,107)
  107         format(' RETRIEVED d(Pi1)/dZ (10-8 m-1)',//)
c
  20              nk=min0(nz,25)
              write(66,200)(kk(k),k=1,nk)
  200         format(/,1x,'i/k ',25i3,/)
              do i=1,nx
                 write(66,300)i,(ip(i,j,kk(k),l),k=1,nk)
              enddo
  300         format(1x,i2,2x,25i3)
              write(66,200)(kk(k),k=1,nk)
c
            endif
c
           enddo
c
      enddo
c
      return
      end
 
      subroutine readced(input,fname,spv,dat,n1,n2,n3,id1,it6,
     +           nx,ny,nz,dx,dy,dz)
c
c  c. liu at UCLA
c
      character*8 cnamf(25),input*80,fname*8,input0*80
      dimension id1(3),it6(6),id2(3),csp(3,3)
      parameter (lbf=50000000)
      integer*2 idm,id(510),ibf(50000000)
      dimension dat(n1,n2,n3)
      common/cedric/reflat,reflon
      data input0/' '/
 
      icall=1
      if(input.ne.input0)icall=0
      input0=input
c      if(icall.ne.0)goto 5
      close(99)
      open(99,file=input,access='direct',status='old',recl=2560)
      read(99,rec=1)(idm,i=1,770),id
 
c     Swap words
      do i=1,510,2
         idm = id(i)
         id(i) = id(i+1)
         id(i+1)=idm
      enddo

      nx=id(162)
      ny=id(167)
      nz=id(172)
      nzz=min(nz,n3)
      id1(1)=id(116)
      id1(2)=id(117)
      id1(3)=id(118)
      it6(1)=id(119)
      it6(2)=id(120)
      it6(3)=id(121)
      id2(1)=id(122)
      id2(2)=id(123)
      id2(3)=id(124)
      it6(4)=id(125)
      it6(5)=id(126)
      it6(6)=id(127)
      sx=1./id(68)
      dd=1.
      do i=1,3
      if(i.eq.3)dd=0.1
      i0=160+(i-1)*5
      csp(1,i)=id(i0)*sx*dd
      csp(2,i)=id(i0+1)*sx*dd
      csp(3,i)=id(i0+3)*0.001
      enddo
      xmin=csp(1,1)
      ymin=csp(1,2)
      zmin=csp(1,3)
      dx=csp(3,1)*1000.
      dy=csp(3,2)*1000.
      dz=csp(3,3)*1000.
 
      nfl=id(175)
      jj=175
      do i=1,25
      cnamf(i)=' '
      if(i.le.nfl)write(cnamf(i),'(4a2)')(id(jj+j),j=1,4)
      jj=jj+5
      enddo
 
      reflat=id(33)+id(34)/60.+id(35)*sx/3600.
      reflon=id(36)+id(37)/60.+id(38)*sx/3600.
      print*,'nx,ny,nz',nx,ny,nz
      print*,'start:  ',(it6(i),i=1,3)
      print*,'end:    ',(it6(i),i=4,6)
      print*,'lat,lon:',reflat,reflon
      print 3,'x:',csp(1,1),csp(2,1),csp(3,1),csp(2,1)-csp(1,1)
      print 3,'y:',csp(1,2),csp(2,2),csp(3,2),csp(2,2)-csp(1,2)
      print 3,'z:',csp(1,3),csp(2,3),csp(3,3),csp(2,3)-csp(1,3)
   3  format(2x,a3,4f8.3)
      print*
 
      print 7,(ifl,cnamf(ifl),ifl=1,nfl)
   7  format((5(2x,i2,': ',a8)))
 
   5  continue
      nx=id(162)
      ny=id(167)
      nz=id(172)
      xmin=csp(1,1)
      ymin=csp(1,2)
      zmin=csp(1,3)
      dx=csp(3,1)*1000.
      dy=csp(3,2)*1000.
      dz=csp(3,3)*1000.
c
c  for ced file
c
      if(icall.eq.0)then
         ibad=id(67)
         nprec=id(96)
         nz=id(106)
         nfld=id(175)
         npts=id(301)
         if(npts.le.0)npts=35366-npts
         nxny=nx*ny
         npts=nx*ny
         nrec=nz*nprec
         ihd=770+510
         nbytes=(ihd+(10+nx*ny*nfld)*nzz)*2
         li2=nbytes/2
         nwd=li2-ihd
c
         if(nwd.gt.lbf)then
            print*,'max buffer is ',lbf
            print*,'max request is ',nwd
            stop
         endif
c
         close(99)
         isave=0
         open(99,file=input,form='unformatted',status='old',
     +          access='direct',recl=nbytes)
         print*,'total ',li2,' words reading, please wait ......'
         if(isave.ne.1)read(99,rec=1)(idm,i=1,ihd),(ibf(j),j=1,nwd)
         print*
         icall=1
      endif
 
      if(fname.eq.' ')return
      read(fname,*,err=8)ifld
      if(ifld.ge.1 .and. ifld.le.nfld)goto 9
   8  continue
      ifld=0
      do i=1,nfl
      if(cnamf(i).eq.fname)then
         ifld=i
         print '(i3,2x,a8)',ifld,cnamf(ifld)
      endif
      enddo
      if(ifld.eq.0)then
         print*,fname,' does net exist',char(7)
         stop
      endif
   9  continue
 
      ipt=0
      do k=1,nzz
      loc=1+10+(k-1)*(10+npts*nfld)+(ifld-1)*npts
      scale=1./id(175+ifld*5)
      do i=1,nxny
      ii=loc+i-1
      i1=mod(i-1,nx)+1
      j1=(i-1)/nx+1
      dat(i1,j1,k)=spv
      if(ibf(ii).ne.ibad)dat(i1,j1,k)=ibf(ii)*scale
      enddo
      enddo
 
      pmin= 1.e20
      pmax=-1.e20
      psum=0.
      ng=0
      ii=0
      do k=1,nzz
      do j=1,ny
      do i=1,nx
      if(dat(i,j,k).ne.spv)then
         pmin=min(dat(i,j,k),pmin)
         pmax=max(dat(i,j,k),pmax)
         psum=psum+dat(i,j,k)
         ii=ii+1
      endif
      enddo
      enddo
      enddo
      if(ii.ne.0)then
         ng=ii
         pmean=psum/ng
      else
         npt=0
         pmin=0.
         pmax=0.
         pmean=0.
      endif
      print*,'npt,min,max,mean',ng,pmin,pmax,pmean
      return
      end
 
      subroutine getqv(ref,qp,vp,teta,pi,rho,k,spv)
c
c  a program to compute qp and vp, (Roux's 1993)
c  ref in dBz
c  qp: 1000 * (kg/kg)  = (g/kg)
c  vp: positive upward
c  c. liu
c
      dimension teta(1),pi(1),rho(1)
 
      if(ref.eq.spv)then
         qp=spv
         vp=spv
         return
      endif
      tc=teta(k)*pi(k)-273.15
      z=10.**(ref/10.)
      denwt=(rho(1)/rho(k))**0.4
      if(tc.ge.0.)then
c tc>0
         qp=2.79e-6*z**0.532/rho(k)
         vp=-denwt*3.50*z**0.084
      else if(tc.le.-10.)then
c tc<-10
         qp=9.73e-6*z**0.571/rho(k)
         vp=-denwt*0.4*1.61*z**0.066
      else
c 0<tc<10
         atc=abs(tc)
         qp1=2.79e-6*z**0.532/rho(k)
         qp2=9.73e-6*z**0.571/rho(k)
         vp1=-denwt*3.50*z**0.084
         vp2=-denwt*1.61*z**0.066
         qp=((10.-atc)*qp1+atc*qp2)/10.
         vp=((10.-atc)*vp1+atc*vp2)/10.
      endif
c
c  qp: 1000*(kg/kg)
c
      qp=qp*1000.
      qp=3.787e-3*z**0.571/rho(k)
      vp=-denwt*2.56*z**0.114
      return
      end
      subroutine adduv(u,v,na,nb,nc,addu,addv,spv)
      dimension u(na*nb*nc),v(na*nb*nc)
      nn=na*nb*nc
      do i=1,nn
      if(u(i).ne.spv)u(i)=u(i)+addu
      if(v(i).ne.spv)v(i)=v(i)+addv
      enddo
      return
      end
      subroutine xread(opt,xbuf)
      character*80 cmd,opt*8,xbuf
  10  continue
      read(*,'(a)',err=20,end=90)cmd
  20  continue
      if(cmd(1:1).eq.'#'.or.cmd(1:1).eq.'!'.or.
     +   cmd(1:1).eq.'*'.or.cmd(1:1).eq.' ')goto 10
      if(cmd(1:7).eq.'comment')then
  30     continue
         read(*,'(a)',err=30,end=91)cmd
         if(cmd(1:6).ne.'endcom')goto 30
         goto 10
      endif
      icl=index(cmd,':')-1
      if(icl.le.0)icl=index(cmd,' ')-1
      icl1=min(8,max(1,icl))
      icl2=min(icl1+2,80)
      opt=cmd(1:icl1)
      xbuf=cmd(icl2:80)
      return
  90  continue
      call quit('please give me an end card, --> exit')
      opt='exit'
      return
  91  continue
      call quit('no endcom card')
      return
      end
 
      subroutine quit(msg)
      character*(*) msg
      print*,msg
      stop
      end

      subroutine leise (f1,f3,m,n,l,spv,nstep,ntime)
      dimension f1(m,n,l),f3(m,n,l)
      do i=1,m
      do j=1,n
      do k=1,l
      f3(i,j,k)=f1(i,j,k)
      enddo
      enddo
      enddo
      ndim=2
      call extend(f3,m,n,l,spv)
      do itm=1,ntime
      if(ndim.eq.3)then
         call t5fltr(f3,m,n,l,nstep)
      else
         do k=1,l
         call t5fltr(f3(1,1,k),m,n,1,nstep)
         enddo
      endif
      enddo
      do i=1,m
      do j=1,n
      do k=1,l
      if(f1(i,j,k).ne.spv)f1(i,j,k)=f3(i,j,k)
      enddo
      enddo
      enddo
      return
      end
      subroutine t5fltr(y,n1,n2,n3,nstep)
c
c                                               jim leise 8/80
c    *****************************************************************
c    hello,
c    i am a multidimensional low-pass filter which needs no extra
c    array space.  thus, the filtered answer is returned in the same
c    array y(n1,n2,n3) that the data is input.  the central filter
c    is a linear 5-pt filter and the boundary filter is computed
c    using a mirror extension.  thus, the total filter is linear.
c
c          ********** nstep control for 1-dim **********
c        step restriction:  5*2**(nstep-1) .le. max(n1,n2,n3)
c         passband .le. 2**(nstep+2)  points/cycle
c         stopband .ge. 2**(nstep)  points/cycle.
c
c          ********** multidimensional use **********
c    parameter control for the three dimensions can be realized
c    via common/fltrpl/ where ns corresponds to nstep.  if this
c    common is not used, the values of ns are defaulted to nstep
c    -i.e. nstep is used in place of any zeros.
c    ******************************************************************
c
         dimension y(1),kord(5),net(5),nns(3),ns(3)
c    initialization of ns for csd applications (4/15/82)
         data ns/0,0,0/
c
c    initialize the 3-d arithmetic.
         if(nstep.le.0)return
         ndim=1
         if(n2.gt.1)ndim=2
         if(n3.gt.1)ndim=3
         kord(1)=max0(1,n1)
         kord(2)=max0(1,n2)
         kord(3)=max0(1,n3)
         kord(4)=kord(1)
         kord(5)=kord(2)
         net(1)=1
         net(2)=kord(1)
         net(3)=kord(1)*kord(2)
         net(4)=net(1)
         net(5)=net(2)
c
c    default parameter transfer.
         mpyrmd=0
         do 10 n=1,ndim
         nns(n)=ns(n)
         if(ns(n).eq.0)nns(n)=nstep
         if(kord(n).lt.5)nns(n)=0
 10      mpyrmd=max0(mpyrmd,nns(n)+nns(n)-1)
         if(mpyrmd.le.0)return
         mstep=(mpyrmd+1)/2
c
c    ***** start the main loop *****
         k1=1
         do 50 main=1,mpyrmd
         do 40 n=1,ndim
c    sampling checks.
         if(10*k1.gt.kord(n))nns(n)=min0(nns(n),main)
         if((main.ge.nns(n)).and.(mpyrmd-main.ge.nns(n)))go to 40
c
c    the 3-d arithmetic.
         m1=k1*net(n)
         m2=m1+m1
         m3=m2+m1
         istop=kord(n+1)
         jstop=kord(n+2)
         do 30 i=1,istop
         do 30 j=1,jstop
         kstrt=1+(i-1)*net(n+1)+(j-1)*net(n+2)
         kstop=kstrt+(kord(n)-1)*net(n)
         kn=kstrt-net(n)
         do 30 k=1,k1
         kn=kn+net(n)
         ln=kn+((kstop-kn)/m1)*m1
c
c    filter the ends using a mirror extension.
         ykn=.875*y(kn)+.1875*y(kn+m1)-.0625*y(kn+m2)
         yln=.875*y(ln)+.1875*y(ln-m1)-.0625*y(ln-m2)
         ykn1=.1875*y(kn)+.625*y(kn+m1)+.25*y(kn+m2)-.0625*y(kn+m3)
         yln1=.1875*y(ln)+.625*y(ln-m1)+.25*y(ln-m2)-.0625*y(ln-m3)
c
c    do the central 5-pt filter.
         ym2=y(kn)
         ym1=y(kn+m1)
         mstrt=kn+m2
         mstop=ln-m2
c
         do 20 m=mstrt,mstop,m1
         ysave=y(m)
         y(m)=.625*y(m)+.25*(ym1+y(m+m1))-.0625*(ym2+y(m+m2))
         ym2=ym1
 20      ym1=ysave
c
         y(kn+m1)=ykn1
         y(ln-m1)=yln1
         y(kn)=ykn
         y(ln)=yln
c
 30      continue
 40      continue
c    update the sampling increment.
         k1=k1+k1
         if(main.ge.mstep)k1=k1/4
 50      continue
c
         return
         end

      subroutine extend(y,n1,n2,n3,spv)
c                                             jim leise 10/80
c    ************************************************************
c    hello,
c    i am a multidimensional interpolation/extrapolation scheme
c    which needs no extra array space.  bad or missing points y(k)
c    are set by the user:  y(k)=spv.  this routine then replaces
c    such flagged values with an iterative replacment method.
c          y(n1,n2,n3)=multidimensional array (input or output).
c          spv       =bad data value or indicator.
c    ************************************************************
c
c  adapted from cedric
c
      dimension y(n1*n2*n3),kord(5),net(5)
c
c    start the 3-d arithmetic.
         ndim=1
         if(n2.gt.1)ndim=2
         if(n3.gt.1)ndim=3
         kord(1)=max0(1,n1)
         kord(2)=max0(1,n2)
         kord(3)=max0(1,n3)
         kord(4)=kord(1)
         kord(5)=kord(2)
         ntot=kord(1)*kord(2)*kord(3)
         kmax=max0(kord(1),kord(2))
         kmax=max0(kmax,kord(3))
         if(kmax.le.1)return
         net(1)=1
         net(2)=kord(1)
         net(3)=kord(1)*kord(2)
         net(4)=net(1)
         net(5)=net(2)
c
c    compute the mean of y and the maximum of abs(y).
         ymax=0.
         ymean=0.
         kount=0
         do 10 k=1,ntot
         if(y(k).eq.spv)go to 10
         ymean=ymean+y(k)
         kount=kount+1
         ymax=amax1(ymax,abs(y(k)))
 10              continue
         if(kount.eq.0.or.kount.eq.ntot)return
         ymean=ymean/kount
         ymax=1.5*ymax
         if(ymax.eq.0.)ymax=1.
c
c    compute a scale for shifting the data to positive values.
c    negative numbers are reserved for the interpolation/
c    extrapolation procedure.  initially, the flagged values are
c    preset with the mean (shifted negative).
         yscl=3.*ymax
         yms=-ymean-ymax
         do 30 k=1,ntot
         if(y(k).eq.spv)go to 20
         y(k)=y(k)+ymax
         go to 30
 20     y(k)=yms
 30     continue
c
c    ***** start the main loop *****
         x=kmax
c    first, compute the largest power of 2 .le.kmax.
         k1=alog(x)/.693
         k1=2**k1
 40     if(k1.le.0)go to 300
         do 100 n=1,ndim
c
c    the 3-d arithmetic.
         m1=k1*net(n)
         istop=kord(n+1)
         jstop=kord(n+2)
         do 90 i=1,istop
         do 90 j=1,jstop
         kstrt=1+(i-1)*net(n+1)+(j-1)*net(n+2)
         kstop=kstrt+(kord(n)-1)*net(n)
         if(kstrt+m1.gt.kstop)go to 90
         kn=kstrt-net(n)
         do 80 kk=1,k1
         kn=kn+net(n)
         ln=kn+((kstop-kn)/m1)*m1
         if(ln.le.kn)go to 80
c
c    initialize the iteration.
         kmark=(abs(y(kn   ))+ymax)/yscl
         knext=(abs(y(kn+m1))+ymax)/yscl
         ymark=abs(y(kn   ))-kmark*yscl
         ynext=abs(y(kn+m1))-knext*yscl
         if(y(kn).gt.0.)go to 50
         factor=kmark+1.
         y(kn)=-factor*yscl-(kmark*ymark+ynext)/factor
c
c   fill the center mod(m1).
 50     mstrt=kn+m1
         mstop=ln-m1
         if(mstrt.gt.mstop)go to 70
         do 60 k=mstrt,mstop,m1
         ylast=ymark
         ymark=ynext
         kmark=knext
         knext=(abs(y(k+m1))+ymax)/yscl
         ynext=abs(y(k+m1))-knext*yscl
         if(y(k).gt.0.)go to 60
         factor=kmark+2.
         y(k)=-factor*yscl-(kmark*ymark+ylast+ynext)/factor
 60      continue
 70     if(y(ln).gt.0.)go to 80
         factor=knext+1.
         y(ln)=-factor*yscl-(knext*ynext+ymark)/factor
 80     continue
 90     continue
 100    continue
c
c   normalize before changing scales.
         do 200 k=1,ntot
         if(y(k).gt.0.)go to 200
         kmark=(-y(k)+ymax)/yscl
         y(k)=y(k)+kmark*yscl
 200    continue
c    update the sampling increment.
         k1=k1/2
         go to 40
c
c    return the data to its original dynamic range.
 300    do 310 k=1,ntot
         y(k)=abs(y(k))-ymax
 310             continue
c
         return
         end
      subroutine bndfil(a,ni,nj,nk,spv,itmax,nquad,minpts)
c
c        performs least-squares data filling of a bounded region
c
c  adapted from cedric
c
      dimension a(ni,nj,nk),c(251,251),am(3),bm(3),cm(3),dm(3),iquad(4)
      data eps/0.00001/

      if(itmax.le.0.or.nquad.le.0.or.minpts.le.0)return
      ptsmin=minpts
      ibeg=2
      iend=ni-1
      jbeg=2
      jend=nj-1
      do k=1,nk
      do 5 i=1,ni
      do 5 j=1,nj
      c(i,j)=a(i,j,k)
   5  continue
      do 50 jo=jbeg,jend
      do 50 io=ibeg,iend
         if(a(io,jo,k).ne.spv) go to 50
         do 15 l=1,3
            am(l)=0.0
            bm(l)=0.0
            cm(l)=0.0          
            dm(l)=0.0
   15    continue
         do 16 l=1,4
            iquad(l)=0
   16    continue
         do 30 l=1,itmax
            j1=max0( 1,jo-l)
            j2=min0(nj,jo+l)
            i1=max0( 1,io-l)
            i2=min0(ni,io+l)
            do 20 j=j1,j2
               iy=j-jo
            do 20 i=i1,i2
               ix=i-io
               if(iabs(ix).ne.l.and.iabs(iy).ne.l) go to 20
               if(a(i,j,k).eq.spv) go to 20
               if(ix.ge.0.and.iy.gt.0) iquad(1)=1
               if(ix.gt.0.and.iy.le.0) iquad(2)=1
               if(ix.le.0.and.iy.lt.0) iquad(3)=1
               if(ix.lt.0.and.iy.ge.0) iquad(4)=1
               am(1)=am(1)+1.0
               am(2)=am(2)+ix
               am(3)=am(3)+iy
               bm(2)=bm(2)+ix*ix
               bm(3)=bm(3)+ix*iy
               cm(3)=cm(3)+iy*iy
               dm(1)=dm(1)+a(i,j,k)
               dm(2)=dm(2)+ix*a(i,j,k)
               dm(3)=dm(3)+iy*a(i,j,k)
   20       continue
            kq=0
            do 25 kk=1,4
   25       kq=kq+iquad(kk)
            if(kq.lt.nquad) go to 30
            if(am(1).lt.ptsmin) go to 30
            bm(1)=am(2)
            cm(1)=am(3)
            cm(2)=bm(3)
            t1=bm(2)*cm(3)-bm(3)*cm(2)
            t2=bm(1)*cm(3)-bm(3)*cm(1)
            t3=bm(1)*cm(2)-bm(2)*cm(1)
            deno=am(1)*t1-am(2)*t2+am(3)*t3
            if(deno.le.eps) go to 30
            anum=dm(1)*t1-dm(2)*t2+dm(3)*t3
            c(io,jo)=anum/deno
            go to 50
   30    continue
   50 continue
      do 60 i=1,ni
      do 60 j=1,nj
      a(i,j,k)=c(i,j)
  60  continue
      enddo
      return
      end

      subroutine sftxy(a,b,m,n,l,xmv,ymv,spv)
c
c  shift grid by (xmv,ymv) grid distance
c
      dimension a(m,n,l),b(m,n,l)
 
      print*,' shifting grid',xmv,ymv
      if(xmv.ge.0..and.ymv.eq.0.)return
 
      do k=1,l
      do i=1,m
      do j=1,n
      b(i,j,k)=a(i,j,k)
      a(i,j,k)=spv
      enddo
      enddo
      enddo
      do k=1,l
      do i=1,m
      do j=1,n
      x=i-xmv
      y=j-ymv
      ix=x
      iy=y
      ix1=ix+1
      iy1=iy+1
      x=x-ix
      y=y-iy
      if(ix.lt.1.or.iy.lt.1)goto 10
      if(ix1.gt.m.or.iy1.gt.n)goto 10
      a(i,j,k)=bilnr(b(ix,iy,k),b(ix1,iy,k),b(ix,iy1,k),
     +         b(ix1,iy1,k),x,y,spv)
  10  continue
      enddo
      enddo
 
      enddo
      return
      end

      function bilnr(a11,a21,a12,a22,dx,dy,spv)
      if(a11.eq.spv.or.a12.eq.spv.or.a21.eq.spv.or.a22.eq.spv)then
         bilnr=spv
      else
         a1=a11+(a21-a11)*dx
         a2=a12+(a22-a12)*dx
         bilnr=a1+(a2-a1)*dy
      endif
      return
      end
