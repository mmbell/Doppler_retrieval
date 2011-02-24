      PROGRAM CEDWRITE
C
C     THIS ROUTINE IS A DRIVER FOR WRITING CEDRIC FORMAT FILES.
C     YOU WILL HAVE TO REPLACE THIS PROGRAM AND THE SUBROUTINE FETCHPLANE
C     IN THIS FILE WITH YOUR OWN. THESE ARE JUST EXAMPLES.
C
      PARAMETER (MAXFLD=25, NID=510)
      CHARACTER*4 PROJECT,date*12,suffixAB
      CHARACTER*6 SCINAME,NAMLND(7),RADSTN,TAPE
      CHARACTER*8 VOLNAM,FLDNAM(MAXFLD),SOURCE
      DIMENSION ISCLFLD(MAXFLD),XLND(7),YLND(7),ZLND(7)
      DIMENSION IDAT(256,256)
      INTEGER*2 IDATA(6,2000000)
c      INTEGER*2 IDATA(6,256,256,60)
      REAL st(2000000),fq(2000000),t(2000000),p(2000000)
      REAL xpg(2000000),ypg(2000000)
      CHARACTER KEYWORD*4,FLTNAME*8,STMNAME*12,RADAR*4,EXPERIMENT*32
      CHARACTER CREATIME*32,EXTRA1*28,FILNAM*56,ptfile*58
      INTEGER IMAX,JMAX,KMAX,KOUNT,NMOSM,
     + IUNFLD,IATTEN,FLAG,EXTRA2,EXTRA3
      REAL STIME,ETIME,OLAT,OLON,SX,SY,SZ,XZ,YZ,ZZ,ROT,RA,CO1,
     + CO2,AZMCOR,ELCOR,THRESH,POWERT,BIEL,AZBIEL,ETIME1,STIME2,
     + EXTRA6,EXTRA7
      CHARACTER NAME*56
      integer ih(6)

      WRITE(6,*)'ENTER PT FILE NAME EXCLUDING THE .PT'
      READ(5,'(A56)')NAME
      WRITE(6,*)'ENTER ORIGIN LAT'
      READ(5,*),OLAT
      WRITE(6,*)'ENTER ORIGIN LON'
      READ(5,*),OLON
      WRITE(6,*) OLAT,OLON
      LFN = index(NAME,' ')-1
      ptfile = NAME(1:LFN)//'.pt'
      open(99,file=ptfile,status='unknown',form='unformatted')
      read(99)date,suffixAB,ih,ix0,iy0,iz0,imax,jmax,kmax,sx,sy,sz
     +     ,et1,et2,et3,ep1,ep2,ep3,v0,pm0,pm1,pm2
      print *,date,suffixAB,ih,ix0,iy0,iz0,imax,jmax,kmax,sx,sy,sz
     +     ,et1,et2,et3,ep1,ep2,ep3,v0,pm0,pm1,pm2

      call readpt(st,fq,t,p,xpg,ypg,IMAX,JMAX,KMAX)
      CLOSE(99)
c      CALL WRITEVS(U,V,W,DBZ,DIV,IMAX,JMAX,KMAX)
      CALL CHANGEFORM(st,fq,t,p,xpg,ypg,IMAX,JMAX,KMAX,IDATA,NAME,
     + OLAT,OLON,SX,SY,SZ,XMIN,YMIN)
c      CALL WRITEIDATA(IDATA,IMAX,JMAX,KMAX)
      SCINAME = 'MBELL'
      BASANG  = 90.0+ROT
c      WRITE(6,*)'ENTER VOLUME NAME UP TO 8 CHARACTERS'
c      READ(5,'(A8)')VOLNAM
c      WRITE(6,*)'ENTER FLIGHT ID'
c      READ(5,'(A8)')SOURCE
c      WRITE(6,*)'ENTER YEAR (2-DIGIT)'
c      READ(5,*)IBEGYR
c      WRITE(6,*)'ENTER MONTH NUMBER'
c      READ(5,*)IBEGMNT
c      WRITE(6,*)'ENTER DAY OF MONTH'
c      READ(5,*)IBEGDAY
c      WRITE(6,*)'ENTER 4BYTE PROJECT NAME'
c      READ(5,'(A4)')PROJECT
      TAPE    = '000000'
      RADCON  = -99.9
      VNYQ    = -99.9
c      STIME = 210800
c      IBEGHR=STIME/3600
c      IREM=STIME-3600*IBEGHR
c      IBEGMIN=IREM/60
c      IBEGSEC=IREM-IBEGMIN*60
c      IENDHR=STIME/3600
c      IREM=STIME-3600*IENDHR
c      IENDMIN=IREM/60
c      IENDSEC=IREM-IENDMIN*60
      IBEGHR = ih(1)
      IBEGMIN = ih(2)
      IBEGSEC = ih(3)
      IENDHR = ih(4)
      IENDMIN = ih(5)
      IENDSEC = ih(6)
      IENDYR  = IBEGYR
      IENDMNT = IBEGMNT
      IENDDAY = IBEGDAY

      LATDEG = INT(OLAT)
      REMLAT = OLAT-LATDEG
      LATMIN = INT(REMLAT*60.)
      REMLAT = REMLAT-LATMIN/60.
      LATSEC = NINT(REMLAT*3600.)
      LONDEG = INT(OLON)
      REMLON = OLON-LONDEG
      LONMIN = INT(REMLON*60.)
      REMLON = REMLON-LONMIN/60.
      LONSEC = NINT(REMLON*3600.)

      XMIN  = ix0
      XMAX  = XMIN+(IMAX-1)*SX
      NUMX  = IMAX
      ISPCX = NINT(SX*1000.)

      YMIN  = iy0
      YMAX  = YMIN+(JMAX-1)*SY
      NUMY  = JMAX
      ISPCY = NINT(SY*1000.)

      ZMIN  = 0.
      ZMAX  = 1000*(KMAX-1)*SZ
      NUMZ  = KMAX
      ISPCZ = NINT(SZ*1000.)

      NFLD=6
      FLDNAM(1)  = 'ST'
      FLDNAM(2)  = 'FQ'
      FLDNAM(3)  = 'T'
      FLDNAM(4)  = 'P'
      FLDNAM(5)  = 'XPG'
      FLDNAM(6)  = 'YPG'
      ISCLFLD(1) = 100
      ISCLFLD(2) = 100
      ISCLFLD(3) = 100
      ISCLFLD(4) = 100
      ISCLFLD(5) = 100
      ISCLFLD(6) = 100
      RADSTN     = 'ELDORA'

      NUMLND = 1
      NUMRAD = 0
      NAMLND(1) = 'ANCHOR'
c      NAMLND(1) = 'ORIGIN'
      XLND(1) = 0.0
      YLND(1) = 0.0
      ZLND(1) = 0.0


      write(6,*)'nfld = ',nfld
c      CALL WRITCED(SCINAME,BASANG,VOLNAM,IBEGYR,
c     X     IBEGMNT,IBEGDAY,IBEGHR,IBEGMIN,IBEGSEC,IENDYR,
c     X     IENDMNT,IENDDAY,IENDHR,IENDMIN,IENDSEC,XMIN,
c     X     XMAX,NUMX,ISPCX,YMIN,YMAX,NUMY,ISPCY,ZMIN,
c     X     ZMAX,NUMZ,ISPCZ,NFLD,FLDNAM,ISCLFLD,NUMLND,
c     X     NUMRAD,NAMLND,XLND,YLND,ZLND,IDAT,RADSTN,SOURCE,
c     X     PROJECT,TAPE,RADCON,VNYQ,LATDEG,LATMIN,LATSEC,
c     X     LONDEG,LONMIN,LONSEC,IDATA)


      
      END


      SUBROUTINE CHANGEFORM(st,fq,t,p,xpg,ypg,IMAX,JMAX,KMAX,IDATA,NAME,
     + OLAT,OLON,SX,SY,SZ,XMIN,YMIN)
      REAL st(IMAX,JMAX,KMAX),fq(IMAX,JMAX,KMAX),t(IMAX,JMAX,KMAX)
      REAL p(IMAX,JMAX,KMAX),xpg(IMAX,JMAX,KMAX),ypg(IMAX,JMAX,KMAX)
      INTEGER*2 IDATA(6,IMAX,JMAX,KMAX)
      REAL FDATA(6,301,301,50)
      CHARACTER NAME*56, DATFILE*56, CTLFILE*56
      REAL latrad, fac_lat, fac_lon
      INTEGER LFN

      FLAG=-999.
      DO K=1,KMAX
       DO J=1,JMAX
        DO I=1,IMAX
         IF(st(I,J,K).GT.FLAG)THEN
          FDATA(1,I,J,K)=st(I,J,K)
         ELSE 
          FDATA(1,I,J,K)=-32768
         ENDIF
         IF(fq(I,J,K).GT.FLAG)THEN
          FDATA(2,I,J,K)=fq(I,J,K)
         ELSE
          FDATA(2,I,J,K)=-32768
         ENDIF
         IF(t(I,J,K).GT.FLAG)THEN
          FDATA(3,I,J,K)=t(I,J,K)
         ELSE
          FDATA(3,I,J,K)=-32768
         ENDIF
         IF(p(I,J,K).GT.FLAG)THEN
          FDATA(4,I,J,K)=p(I,J,K)*100
         ELSE
          FDATA(4,I,J,K)=-32768
         ENDIF
         IF(xpg(I,J,K).GT.FLAG)THEN
          FDATA(5,I,J,K)=xpg(I,J,K)
         ELSE
          FDATA(5,I,J,K)=-32768
         ENDIF
         IF(ypg(I,J,K).GT.FLAG)THEN
          FDATA(6,I,J,K)=ypg(I,J,K)
         ELSE
          FDATA(6,I,J,K)=-32768
         ENDIF
        ENDDO
       ENDDO
      ENDDO

c WRITE BINARY FORMAT FOR GRADS : modified by ms cccccccccccccccc
      LFN = index(NAME,' ')-1
      datfile = name(1:LFN)//'.dat'
      OPEN(40, file=datfile, FORM = 'unformatted',
     + ACCESS = 'direct', STATUS = 'unknown', recl=IMAX*JMAX*4)
      irec = 0 

      DO IVAR = 1, 6
      DO K=1,KMAX
      irec = irec + 1 
        WRITE(40,rec=irec)((FDATA(IVAR,I,J,K),I=1,IMAX)
     +     ,J=1,JMAX)
      ENDDO
      ENDDO 

C WRITE CTL FILE FOR GRADS 
      ctlfile = name(1:LFN)//'.ctl'      
      OPEN(50, file=ctlfile, STATUS = 'unknown')   
c      write(50,'(a6,a56)') 'dset ^', DATFILE 
c      write(50,'(a19)') 'title eldora 3d var' 
c      write(50,'(a13)') 'undef -32768'   
c      latrad = OLAT * 1.745329251994e-02
c      fac_lat = 111.13209 - 0.56605 * cos(2.0 * latrad) 
c     +     + 0.00012 * cos(4.0 * latrad) - 0.000002 * cos(6.0 * latrad)
c      fac_lon = 111.41513 * cos(latrad)
c     +     - 0.09455 * cos(3.0 * latrad) + 0.00012 * cos(5.0 * latrad)
c      write(50,'(a5,I3,a19)'), 'xdef ', IMAX,' levels'
c      DO I=1,IMAX
c          write(50,'(f10.4)'),(XMIN + (I-1)*SX)/fac_lon + OLON
c          write(6,*) xmin,i,sx,fac_lon,olon
c      enddo
c      write(50,'(a5,I3,a19)'), 'ydef ', JMAX,' levels'
c       DO J=1,JMAX
c          write(50,'(f10.4)'),(YMIN + (J-1)*SY)/fac_lat + OLAT
c      enddo
c      write(50,'(a5,I3,a14)'), 'zdef ', KMAX,' linear 0. 0.5' 
c      write(50,'(a33)'),'tdef 1 linear 23:50Z26SEP2008 1MN' 
      WRITE(50,'(a6)'),'vars 6' 
      WRITE(50,'(a4,I3,a12)'),'st  ', KMAX,' 99 variable'  
      WRITE(50,'(a4,I3,a12)'),'fq  ', KMAX,' 99 variable'  
      WRITE(50,'(a4,I3,a12)'),'t   ', KMAX,' 99 variable'   
      WRITE(50,'(a4,I3,a12)'),'p   ', KMAX,' 99 variable'
      WRITE(50,'(a4,I3,a12)'),'xpg ', KMAX,' 99 variable'
      WRITE(50,'(a4,I3,a12)'),'ypg ', KMAX,' 99 variable'
      write(50, '(a7)') 'endvars' 

      RETURN
      END

      SUBROUTINE LDPLN(U,V,W,DBZ,DIV,IMAX,JMAX,KMAX,IUV)
      REAL U(IMAX,JMAX,KMAX),V(IMAX,JMAX,KMAX),W(IMAX,JMAX,KMAX)
      REAL DBZ(IMAX,JMAX,KMAX),DIV(IMAX,JMAX,KMAX)
      REAL RWD(256),RWS(256),RWW(256),RDV(256)
      INTEGER*2 WD(256),WS(256),DB(256),WW(256),DV(256)         
      FLAG=-1.0E+10
      DO 1 K=1,KMAX                                                             
       WRITE(6,'("loading plane #",i3)') K                                    
       DO 2 J=1,JMAX                                                          
        IF(IUV.EQ.1)THEN
         READ(99)(RWD(I),RWS(I),RWW(I),DB(I),RDV(I),I=1,IMAX)
        ELSE
         READ(99) (WD(I),WS(I),WW(I),DB(I),DV(I),I=1,IMAX)                   
        ENDIF
        DO 3 I=1,IMAX                                                       
c         WRITE(6,*)'I,J,K,RWD,RWS,RWW,DB,RDV = ',I,J,K,
c     1              RWD(I),RWS(I),RWW(I),
c     1              DB(I),RDV(I)
         IF(IUV.EQ.0)THEN
          WDR=WD(I)*.1                                                     
          WSP=WS(I)*.1                                                     
          IF (WDR.LT.0.) THEN                                              
           U(I,J,K)=FLAG
           V(I,J,K)=FLAG
          ELSE                                                             
c           CALL COMP(XU,XV,WDR,WSP)                                       
           XU=-SIN(WDR*3.14159/180.)*WSP
           XV=-COS(WDR*3.14159/180.)*WSP
           U(I,J,K)=XU
           V(I,J,K)=XV
          ENDIF                                                            
          IF(WW(I).GT.-9000)THEN
           W(I,J,K)=WW(I)*.01
          ELSE
           W(I,J,K)=FLAG
          ENDIF
          IF(DV(I).LT.32767)THEN
           DIV(I,J,K)=DV(I)/100000.
          ELSE
           DIV(I,J,K)=FLAG
          ENDIF
         ELSE
          IF(RWW(I).LE.-100000.)RWD(I)=FLAG
          IF(RWS(I).LE.-100000.)RWS(I)=FLAG
          IF(RWW(I).LE.-100000.)RWW(I)=FLAG
          IF(RDV(I).LE.-100000.)RDV(I)=FLAG
          U(I,J,K)=RWD(I)
          V(I,J,K)=RWS(I)
          W(I,J,K)=RWW(I)
          DIV(I,J,K)=RDV(I)
         ENDIF
         IF(DB(I).GT.-9000)THEN
          DBZ(I,J,K)=DB(I)*.1
         ELSE
          DBZ(I,J,K)=FLAG
         ENDIF
c         WRITE(6,*)'I,J,K,U,V,W,DBZ,DIV = ',I,J,K,U(I,J,K),V(I,J,K),
c     1              W(I,J,K),DBZ(I,J,K),DIV(I,J,K)
3       CONTINUE                                                         
2      CONTINUE                                                               
1     CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       


      SUBROUTINE FETCHPLANE(IDAT,IMAX,JMAX,KMAX,IFIELD,K,IDATA)
C
C     SAMPLE ROUTINE FOR FETCHING Z PLANES OF CEDRIC DATA.
C     FILLS IN ARRAY WITH BOGUS DATA
C
      DIMENSION IDAT(IMAX,JMAX)
      INTEGER*2 IDATA(6,IMAX,JMAX,KMAX)
      DO 100 J=1,JMAX
         DO 50 I=1,IMAX
            IDAT(I,J)=IDATA(IFIELD,I,J,K)
c         write(6,*)'j,k,ifield,idat,idata = ',j,k,
c     1    ifield,(idat(i,j),idata(ifield,i,j,k),i=1,imax)
 50      CONTINUE
 100  CONTINUE

      RETURN

      END

         
      SUBROUTINE READHEADER(LU,FILNAM,KEYWORD,FLTNAME,
     + STMNAME,RADAR,EXPERIMENT,
     + CREATIME,EXTRA1,IMAX,JMAX,KMAX,KOUNT,NMOSM,
     + IUNFLD,IATTEN,FLAG,EXTRA2,EXTRA3,STIME,ETIME,OLAT,OLON,
     + SX,SY,SZ,XZ,YZ,ZZ,ROT,RA,CO1,CO2,AZMCOR,ELCOR,THRESH,
     + POWERT,BIEL,AZBIEL,ETIME1,STIME2,EXTRA6,EXTRA7)
      CHARACTER KEYWORD*4,FLTNAME*8,STMNAME*12,RADAR*4,EXPERIMENT*32
      CHARACTER CREATIME*32,EXTRA1*28,FILNAM*56
      INTEGER IMAX,JMAX,KMAX,KOUNT,NMOSM,
     + IUNFLD,IATTEN,FLAG,EXTRA2,EXTRA3
      REAL STIME,ETIME,OLAT,OLON,SX,SY,SZ,XZ,YZ,ZZ,ROT,RA,CO1,
     + CO2,AZMCOR,ELCOR,THRESH,POWERT,BIEL,AZBIEL,ETIME1,STIME2,
     + EXTRA6,EXTRA7
c      OPEN(LU,FILE=FILNAM,STATUS='OLD',FORM='UNFORMATTED')
      OPEN(LU,STATUS='OLD',FORM='UNFORMATTED')
      READ(LU)KEYWORD,FLTNAME,STMNAME,RADAR,EXPERIMENT,CREATIME,
     + EXTRA1,IMAX,JMAX,KMAX,KOUNT,NMOSM,IUNFLD,IATTEN,FLAG,EXTRA2,
     + EXTRA3,STIME,ETIME,OLAT,OLON,SX,SY,SZ,XZ,YZ,ZZ,ROT,RA,CO1,
     + CO2,AZMCOR,ELCOR,THRESH,POWERT,BIEL,AZBIEL,ETIME1,STIME2,
     + EXTRA6,EXTRA7
      RETURN
      END
      SUBROUTINE WRITEVS(U,V,W,DBZ,DIV,IMAX,JMAX,KMAX)
      REAL U(IMAX,JMAX,KMAX),V(IMAX,JMAX,KMAX),W(IMAX,JMAX,KMAX)
      REAL DBZ(IMAX,JMAX,KMAX),DIV(IMAX,JMAX,KMAX)
      DO K=1,KMAX
       DO J=1,JMAX
        WRITE(6,*)'J,K,U = ',(U(I,J,K),I=1,IMAX)
        WRITE(6,*)'J,K,V = ',(V(I,J,K),I=1,IMAX)
        WRITE(6,*)'J,K,W = ',(W(I,J,K),I=1,IMAX)
        WRITE(6,*)'J,K,DBZ = ',(DBZ(I,J,K),I=1,IMAX)
        WRITE(6,*)'J,K,DIV = ',(DIV(I,J,K),I=1,IMAX)
       ENDDO
      ENDDO
      RETURN
      END
      SUBROUTINE WRITEIDATA(IDATA,IMAX,JMAX,KMAX)
      INTEGER*2 IDATA(6,IMAX,JMAX,KMAX)
      DO K=1,KMAX
       DO J=1,JMAX
        WRITE(6,*)'J,K,IDATU = ',(IDATA(1,I,J,K),I=1,IMAX)
        WRITE(6,*)'J,K,IDATV = ',(IDATA(2,I,J,K),I=1,IMAX)
        WRITE(6,*)'J,K,IDATW = ',(IDATA(3,I,J,K),I=1,IMAX)
        WRITE(6,*)'J,K,IDATDBZ = ',(IDATA(4,I,J,K),I=1,IMAX)
        WRITE(6,*)'J,K,IDATDIV = ',(IDATA(5,I,J,K),I=1,IMAX)
       ENDDO
      ENDDO
      RETURN
      END

      
      subroutine readpt(st,fq,t,p,xpg,ypg,IMAX,JMAX,KMAX)
      REAL st(IMAX,JMAX,KMAX),fq(IMAX,JMAX,KMAX),t(IMAX,JMAX,KMAX)
      REAL p(IMAX,JMAX,KMAX),xpg(IMAX,JMAX,KMAX),ypg(IMAX,JMAX,KMAX)

      do k=1,kmax
         print *,'  LEVEL K=',k
         do j=1,jmax
            do i=1,imax
               read(99)st(i,j,k),fq(i,j,k),t(i,j,k),p(i,j,k),
     +              xpg(i,j,k),ypg(i,j,k)
c               print *,i,j,k,t(i,j,k),p(i,j,k)
c     +              xpg(i,j,k),ypg(i,j,k)
            enddo
         enddo
      enddo

      end
