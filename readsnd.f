      program snd
c
c  read sounding
c
      character*80 input,output,buf*160,site*3
      spv=-999.0

      print*,'snd OUTPUT file'
      read(*,'(a)')output
      open(20,file=output,form='formatted',status='unknown')

      print*,'mon, day'
      read*,im0,id0

   5  continue
      print*,'Enter input file'
      read(*,'(a)')input
      if(input.eq.' ')stop
      open(10,file=input,form='formatted',status='old')

  10  continue
      read(10,'(a)',err=12,end=99)buf
  12  continue
      if(index(buf,'Location').ne.0)read(buf,13)i2,r2,i1,r1
  13  format(35x,i3,f6.2,4x,i2,f6.2)
      rlat=i1+r1/60.
      rlon=-(i2+r2/60.)
      if(index(buf,'GMT').ne.0)read(buf,14)imon,iday,ih,im,is
  14  format(40x,i3,1x,i3,1x,3(1x,i2))
      if(index(buf,'Site').ne.0)read(buf,'(35x,a3)')site
      if(index(buf,'-----').ne.0)goto 20
      goto 10
  20  continue
      print 25,imon,iday,ih,im,is,site,rlat,rlon
  25  format(5i3.2,1x,a3,2f9.3)
      iwt=0
      if(imon.eq.im0.and.iday.eq.id0)iwt=1
      if(iwt.eq.1)write(20,26)site,imon,iday,ih,im,is,rlat,rlon
  26  format(a3,1x,5i3.2,2f9.3)
  30  continue
      read(10,*,err=10,end=99)time,p,t,td,rh,u,v,spd,dir,dz,r2,r1,
     +           r,a,alt,qp,qt,qh,qu,qv,qdz
      if(u.eq.9999.0)u=spv
      if(v.eq.9999.0)v=spv
      if(p.eq.9999.0)p=spv
      if(t.eq.999.0)t=spv
      if(td.eq.999.0)td=spv
      if(spd.eq.999.0)spd=spv
      if(dir.eq.999.0)dir=spv
      if(p.eq.spv)goto 30
c     if(iwt.eq.1)write(20,35)alt,p,t,td,spd,dir
      if(iwt.eq.1)write(20,35)p,t,td,spd,dir
  35  format(3x,6f7.1)
      goto 30
  99  continue
      close(10)
      goto 5
      end
