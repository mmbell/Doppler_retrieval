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
      INTEGER*2 IDATA(6,400000)
c      INTEGER*2 IDATA(6,256,256,60)
      REAL st(1400000),fq(1400000),t(1400000),p(1400000)
      REAL xpg(1400000),ypg(1400000)
      CHARACTER KEYWORD*4,FLTNAME*8,STMNAME*12,RADAR*4,EXPERIMENT*32
      CHARACTER CREATIME*32,EXTRA1*28,FILNAM*56
      INTEGER IMAX,JMAX,KMAX,KOUNT,NMOSM,
     + IUNFLD,IATTEN,FLAG,EXTRA2,EXTRA3
      REAL STIME,ETIME,OLAT,OLON,SX,SY,SZ,XZ,YZ,ZZ,ROT,RA,CO1,
     + CO2,AZMCOR,ELCOR,THRESH,POWERT,BIEL,AZBIEL,ETIME1,STIME2,
     + EXTRA6,EXTRA7
      CHARACTER NAME*56
      integer ih(6)

c      WRITE(6,*)'ENTER PT FILE NAME'
c      READ(5,'(A56)')NAME
      open(99,status='unknown',form='unformatted')
      read(99)date,suffixAB,ih,ix0,iy0,iz0,imax,jmax,kmax,sx,sy,sz
     +     ,et1,et2,et3,ep1,ep2,ep3,v0,pm0,pm1,pm2
      print *,date,suffixAB,ih,ix0,iy0,iz0,imax,jmax,kmax,sx,sy,sz
     +     ,et1,et2,et3,ep1,ep2,ep3,v0,pm0,pm1,pm2

      call readpt(st,fq,t,p,xpg,ypg,IMAX,JMAX,KMAX)
      CLOSE(99)
c      CALL WRITEVS(U,V,W,DBZ,DIV,IMAX,JMAX,KMAX)
      CALL CHANGEFORM(st,fq,t,p,xpg,ypg,IMAX,JMAX,KMAX,IDATA)
c      CALL WRITEIDATA(IDATA,IMAX,JMAX,KMAX)
      SCINAME = 'MBELL'
      BASANG  = 90.0+ROT
      WRITE(6,*)'ENTER VOLUME NAME UP TO 8 CHARACTERS'
      READ(5,'(A8)')VOLNAM
      WRITE(6,*)'ENTER FLIGHT ID'
      READ(5,'(A8)')SOURCE
      WRITE(6,*)'ENTER YEAR (2-DIGIT)'
      READ(5,*)IBEGYR
      WRITE(6,*)'ENTER MONTH NUMBER'
      READ(5,*)IBEGMNT
      WRITE(6,*)'ENTER DAY OF MONTH'
      READ(5,*)IBEGDAY
      WRITE(6,*)'ENTER 4BYTE PROJECT NAME'
      READ(5,'(A4)')PROJECT
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

      sx = 0.5
      sy = 0.5
      sz = 0.5
      XMIN  = 0.
      XMAX  = XMIN+(IMAX-1)*SX
      NUMX  = IMAX
      ISPCX = NINT(SX*1000.)

      YMIN  = 0.
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
      CALL WRITCED(SCINAME,BASANG,VOLNAM,IBEGYR,
     X     IBEGMNT,IBEGDAY,IBEGHR,IBEGMIN,IBEGSEC,IENDYR,
     X     IENDMNT,IENDDAY,IENDHR,IENDMIN,IENDSEC,XMIN,
     X     XMAX,NUMX,ISPCX,YMIN,YMAX,NUMY,ISPCY,ZMIN,
     X     ZMAX,NUMZ,ISPCZ,NFLD,FLDNAM,ISCLFLD,NUMLND,
     X     NUMRAD,NAMLND,XLND,YLND,ZLND,IDAT,RADSTN,SOURCE,
     X     PROJECT,TAPE,RADCON,VNYQ,LATDEG,LATMIN,LATSEC,
     X     LONDEG,LONMIN,LONSEC,IDATA)


      
      END


      SUBROUTINE CHANGEFORM(st,fq,t,p,xpg,ypg,IMAX,JMAX,KMAX,IDATA)
      REAL st(IMAX,JMAX,KMAX),fq(IMAX,JMAX,KMAX),t(IMAX,JMAX,KMAX)
      REAL p(IMAX,JMAX,KMAX),xpg(IMAX,JMAX,KMAX),ypg(IMAX,JMAX,KMAX)
      INTEGER*2 IDATA(6,IMAX,JMAX,KMAX)
      FLAG=-999.
      DO K=1,KMAX
       DO J=1,JMAX
        DO I=1,IMAX
         IF(st(I,J,K).GT.FLAG)THEN
          IDATA(1,I,J,K)=NINT(st(I,J,K)*100.)
         ELSE 
          IDATA(1,I,J,K)=-32768
         ENDIF
         IF(fq(I,J,K).GT.FLAG)THEN
          IDATA(2,I,J,K)=NINT(fq(I,J,K)*100.)
         ELSE
          IDATA(2,I,J,K)=-32768
         ENDIF
         IF(t(I,J,K).GT.FLAG)THEN
          IDATA(3,I,J,K)=NINT(t(I,J,K)*100.)
         ELSE
          IDATA(3,I,J,K)=-32768
         ENDIF
         IF(p(I,J,K).GT.FLAG)THEN
          IDATA(4,I,J,K)=NINT(p(I,J,K))
         ELSE
          IDATA(4,I,J,K)=-32768
         ENDIF
         IF(xpg(I,J,K).GT.FLAG)THEN
          IDATA(5,I,J,K)=NINT(xpg(I,J,K)*100.)
         ELSE
          IDATA(5,I,J,K)=-32768
         ENDIF
         IF(ypg(I,J,K).GT.FLAG)THEN
          IDATA(6,I,J,K)=NINT(ypg(I,J,K)*100.)
         ELSE
          IDATA(6,I,J,K)=-32768
         ENDIF
        ENDDO
       ENDDO
      ENDDO
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
