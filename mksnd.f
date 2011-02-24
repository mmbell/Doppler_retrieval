c********** Program to prepare Sounding files for retrivals********
c******************************************************************
c ***** Here zmin, deltaz using Km as uinits  *****
      program prepenv
      parameter (nlu_max=2000,nec_max=50)
      dimension plu(nlu_max),tlu(nlu_max),tdlu(nlu_max)
     &         ,pilu(nlu_max),tetalu(nlu_max),qvlu(nlu_max),zlu(nlu_max)
     &         ,zec(nec_max),tetaec(nec_max),tetavec(nec_max)
     &         ,piec(nec_max),qvec(nec_max),rhoec(nec_max)
     &         ,dztec(nec_max),dzqec(nec_max)
c
      character rs_file*50,env_file*50
c
      spv=-999.
      r=287.
      cp=1005.
      t0=273.15
      es0=3.8e+3
      ae=17.2694
      be=237.29
      ag=21.8746
      bg=265.49
      g=9.81
      c=0.608e-3
      rg=348.432
      xkap=2.502
      xlat=2.5e+3
c
c**** LECTURE DES DONNEES DE CONTROLE
c
      print*,'original snd file'
      read(*,'(a)')rs_file
      open(10,file=rs_file,status='unknown',form='formatted')
c
      print*,'snd output file'
      read(*,'(a)')env_file
      open(20,file=env_file,status='unknown',form='formatted')
c
      print*,'Altitude input (1:readin, 0:to be re-computed):'
      read*,ialtitude
c
      print*,'P(hPa) ABOVE WHICH (Td=T-3degC) IS IMPOSED:'
      read*,pmin
c
      print*,'zmin, delz, kmax+2'
      read*,z1,hz,nec
      zmax=z1+float(nec-2)*hz
c
      k=0
      read(10,*)
  1   continue
      read(10,*,err=2,end=2)zzlu,pplu,ttlu,ddlu
      if(pplu.gt.0..and.ttlu.gt.-900.
     &   .and.(pplu.lt.pmin.or.ddlu.gt.-900.))then
        if(pplu.lt.pmin)ddlu=ttlu-3.
	k=k+1
	nlu=k
	zlu(k)=zzlu/1000.
        zlu(k)=zlu(k)-zlu(1)
	plu(k)=pplu
	tlu(k)=ttlu
	tdlu(k)=ddlu
        pilu(k)=(plu(k)/1000.)**(r/cp)
        tetalu(k)=(tlu(k)+t0)/pilu(k)
        if(tlu(k).ge.0.)then
          a=ae
          b=be
        else
          a=ag
          b=bg
        endif
        qvlu(k)=(es0/plu(k))*exp(a*tdlu(k)/(tdlu(k)+b))
      endif
      goto 1
   2  continue
      print*,'READ LEVELS:',nlu
c
c**** HYDROSTATIC ALTITUDE (IF IALTITUDE=0)
c
      if(ialtitude.eq.0)then
        zlu(1)=0.
        do k=2,nlu
           zlu(k)=zlu(k-1)-(r/g)*(t0+(tlu(k-1)+tlu(k))/2.)
     &                          *(alog(plu(k))-alog(plu(k-1)))/1000.
        enddo
      endif
c
      zec(1)=z1
      do k=2,nec
         zec(k)=z1+float(k-1)*hz
      enddo
c
      print*,'INTERPOLATION AND FILTERING'
c
      call interfil(zlu,zec,pilu,piec,nlu,nec)
      call interfil(zlu,zec,tetalu,tetaec,nlu,nec)
      call interfil(zlu,zec,qvlu,qvec,nlu,nec)
c
      print*,'CALCULATIONS ON THE INTERPOLATED DATA'
c
      s=0.
      sz=0.
      sz2=0.
      sr=0.
      szr=0.
      do k=1,nec
         tetavec(k)=tetaec(k)*(1.+c*qvec(k))
         rhoec(k)=rg*(piec(k)**xkap)/tetaec(k)
         s=s+1.
         sz=sz+zec(k)
         sz2=sz2+zec(k)*zec(k)
         sr=sr+alog(rhoec(k))
         szr=szr+zec(k)*alog(rhoec(k))
      enddo
      det=s*sz2-sz*sz
      rho0=exp((sr*sz2-sz*szr)/det)
      h0=-det/(s*szr-sz*sr)
c
      call deriv(dztec,tetaec,hz,nec)
      call deriv(dzqec,qvec,hz,nec)
c
      print*,'WRITE THE RESULTS ON :',env_file
c
      do k=1,nec
      write(20,3)zec(k),tetaec(k),tetavec(k),piec(k),rhoec(k)
     +         ,dztec(k),dzqec(k)
  3   format(f6.2,6f9.4)
      enddo
      print 5,rho0,h0
  5   format(/,' RHO(z=0)=',f5.3,'    H0=',f6.3)
      em=0.
      eqm=0.
      do k=1,nec
	 rho=rho0*exp(-zec(k)/h0)
	 drho=rho-rhoec(k)
	 em=em+drho
	 eqm=eqm+drho*drho
	 print 6,zec(k),rhoec(k),rho
  6      format (' Z=',f5.2,' written RHO=',f5.3' adjusted RHO=',f5.3)
      enddo
      print 7,em/float(nec),sqrt(eqm/float(nec))
  7   format(/,' mean diff adjRHO-wriRHO :',f5.3
     &        ,'   stand dev adjRHO-wriRHO',f5.3)
c
      do k=1,nec
         tec=tetaec(k)*piec(k)-t0
         pec=1000.*(piec(k)**(cp/r))
         if(tec.ge.0.)then
           a=ae
           b=be
         else
           a=ag
           b=bg
         endif
         qsec=(es0/pec)*exp(a*tec/(tec+b))
         tetaes=tetaec(k)*exp((xlat*qsec)/(cp*(tec+t0)))
         tetae=tetaec(k)*exp((xlat*qvec(k))/(cp*(tec+t0)))
         print 8,zec(k),tetae,tetaes
  8      format(' Z=',f4.1,'  TETA-E=',f5.1,'  TETA-ES=',f5.1)
      enddo
c
      close(10)
      close(20)
c
      stop
      end
c
      subroutine interfil(zlu,zec,vlu,vec,nlu,nec)
      dimension zlu(nlu),vlu(nlu)
     &         ,zec(nec),vec(nec)
     &         ,xx(2000),yy(2000),ss(2000),dd(2000)
c
      do n=1,2000
         xx(n)=0.
         yy(n)=0.
         ss(n)=0.
         dd(n)=0.
      enddo
c
      do k=1,nlu
         xx(k)=zlu(k)
         yy(k)=vlu(k)
      enddo
      q1=(yy(2)-yy(1))/(xx(2)-xx(1))
      qn=(yy(nlu)-yy(nlu-1))/(xx(nlu)-xx(nlu-1))
      ntot=nlu
c
      call spline(xx,yy,ss,dd,q1,qn,ntot)
c
      do k=1,nec
         x=zec(k)
         vec(k)=fsplin(x,xx,yy,ss,dd,q1,qn,ntot)
      enddo
c
      return
      end
c
      subroutine deriv(dzvec,vec,hz,nec)
      dimension dzvec(nec),vec(nec)
c
      dzvec(1)=(-vec(1)+vec(2))/hz
      dzvec(2)=(-vec(1)+vec(3))/(2.*hz)
c
      do k=3,nec-2
         dzvec(k)=(-vec(k-2)/8.-vec(k-1)/4.
     &             +vec(k+1)/4.+vec(k+2)/8.)/hz
      enddo
c
      dzvec(nec-1)=(-vec(nec-2)+vec(nec))/(2.*hz)
      dzvec(nec)=(-vec(nec-1)+vec(nec))/hz
c
      return
      end
c
      subroutine spline(x,u,s,del,q1,qn,n)
      dimension x(2000),u(2000),s(2000),del(2000)
      dimension a(2000),v(2000)
c
      del(2)=x(2)-x(1)
      v(1)=6.*(((u(2)-u(1))/del(2))-q1)
      n1=n-1
      do 10 i=2,n1
      del(i+1)=x(i+1)-x(i)
      v(i)=((u(i-1)/del(i))-u(i)*((1./del(i))+(1./del(i+1)))
     &      +(u(i+1)/del(i+1)))*6.
 10   continue
      v(n)=(qn+(u(n1)-u(n))/del(n))*6.
c
      a(1)=2.*del(2)
      a(2)=1.5*del(2)+2.*del(3)
      v(2)=v(2)-.5*v(1)
      do 20 i=3,n1
      c=del(i)/a(i-1)
      a(i)=2.*(del(i)+del(i+1))-c*del(i)
      v(i)=v(i)-c*v(i-1)
 20   continue
      c=del(n)/a(n1)
      a(n)=2.*del(n)-c*del(n)
      v(n)=v(n)-c*v(n1)
c
      s(n)=v(n)/a(n)
      do 30 j=1,n1
      i=n-j
      s(i)=(v(i)-del(i+1)*s(i+1))/a(i)
 30   continue
      return
      end
c
      function fsplin(v,x,u,s,del,q1,qn,n)
      dimension x(2000),u(2000),s(2000),del(2000)
c
      if(v-x(1))50,10,20
 10   fsplin=u(1)
      return
 20   do 40 k=2,n
      if(v-x(k))30,30,40
 30   k1=k-1
      ff1=s(k1)*(x(k)-v)**3.
      ff2=s(k)*(v-x(k1))**3.
      ff3=1./(6.*del(k))
      f1=(ff1+ff2)*ff3
      f2=(v-x(k1))*(u(k)/del(k)-s(k)*del(k)/6.)
      f3=(x(k)-v)*(u(k1)/del(k)-s(k1)*del(k)/6.)
      fsplin=f1+f2+f3
      return
 40   continue
 50   fsplin=-999.
      return
      end
