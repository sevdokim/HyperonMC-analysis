      subroutine hist_book
c====================================================================
c     Histogram booking
c=======================================================================
c
      parameter(npawc=210000000)
      COMMON /PAWC  /  HMEM(npawc)

      character*2 title(6)
      character*12 title2
      character*20 title3
      character*2 title1(10)
      character*6 part(2),peak(4)
      character*5 key(2)
      character*8 tmm(2)
      data part,peak,key,tmm/'[p]^+!','K^+!',
     &                       '[p]^0!','[c]' ,'K^0!','f?2!',
     &                       'under' ,'round',
     &                       't'     ,'missmass'/
      equivalence (title(1),title2)
      equivalence (title1(1),title3)
      data title/'ce','ll','  ','xx',' ,','xx'/
      data title1 /'ce','ll','  ','xx',' ,','xx','  ','sm','al','l '/
      character *96 a32
      common /energy_flow/ aver_map1(640),aver_map2(640),
     +                      hit_map1(640),hit_map2(640)
      call hb_match_mwpc_lgd2
      call hb_scaler
      call hb_cherenkovs
C
Cmike komentarii - moi
C-------- MWPC histogramms
C-------- 1G  chambers :
C......................fired wire position
c      call HBOOK1(-100,'1U wire;Position;Events;',64,0.,64.,0.)
c      call HBOOK1(-200,'1V wire;Position;Events;',64,0.,64.,0.)
c      call HBOOK1(-300,'1X wire;Position;Events;',64,0.,64.,0.)
cC......................cluster center position of fired wires
c      call HBOOK1(-101,'1U cluster center;Position;Events;',
c     *            64,0.,64.,0.)
c      call HBOOK1(-201,'1V cluster center;Position;Events;',
c     *            64,0.,64.,0.)
c      call HBOOK1(-301,'1X cluster center;Position;Events;',
c     *            64,0.,64.,0.)
cC......................cluster width of fired wires
c      call HBOOK1(-102,'1U cluster width;Size;Events;',16,-0.5,15.5,0.)
c      call HBOOK1(-202,'1V cluster width;Size;Events;',16,-0.5,15.5,0.)
c      call HBOOK1(-302,'1X cluster width;Size;Events;',16,-0.5,15.5,0.)
cC......................number of clusters
c      call HBOOK1(-103,'1U Number of clusters;Clusters;Events;',
c     *            32,-0.5,31.5,0.)
c      call HBOOK1(-203,'1V Number of clusters;Clusters;Events;',
c     *            32,-0.5,31.5,0.)
c      call HBOOK1(-303,'1X Number of clusters;Clusters;Events;',
c     *            32,-0.5,31.5,0.)
cC......................tracks
c      call HBOOK1(-1501,'1G chamber: X-X(u,v);mm;mm;', 
c     *            200, -100., 100., 0.)
c      call HBOOK2(-1601,'1G chamber: Y vs X;mm;mm;', 
c     *            50, -100., 100., 50, -100., 100., 0.)
c      call HBOOK1(-1801,'1G chamber: X Track-Hit distance;mm;Events;', 
c     *            100, -50., 50., 0.)
c      call HBOOK1(-1802,'1G chamber: Y Track-Hit distance;mm;Events;', 
c     *            100, -50., 50., 0.)
cC-------- 2G  chambers :
cC......................fired wire position
c      call HBOOK1(-400,'1U wire;Position;Events;',64,0.,64.,0.)
c      call HBOOK1(-500,'1V wire;Position;Events;',64,0.,64.,0.)
c      call HBOOK1(-600,'1Y wire;Position;Events;',64,0.,64.,0.)
cC......................cluster center position of fired wires
c      call HBOOK1(-401,'1U cluster center;Position;Events;',
c     *            64,0.,64.,0.)
c      call HBOOK1(-501,'1V cluster center;Position;Events;',
c     *            64,0.,64.,0.)
c      call HBOOK1(-601,'1Y cluster center;Position;Events;',
c     *            64,0.,64.,0.)
cC......................cluster width of fired wires
c      call HBOOK1(-402,'1U cluster width;Size;Events;',16,-0.5,15.5,0.)
c      call HBOOK1(-502,'1V cluster width;Size;Events;',16,-0.5,15.5,0.)
c      call HBOOK1(-602,'1Y cluster width;Size;Events;',16,-0.5,15.5,0.)
cC......................number of clusters
c      call HBOOK1(-403,'2U Number of clusters;Clusters;Events;',
c     *            32,-0.5,31.5,0.)
c      call HBOOK1(-503,'2V Number of clusters;Clusters;Events;',
c     *            32,-0.5,31.5,0.)
c      call HBOOK1(-603,'2Y Number of clusters;Clusters;Events;',
c     *            32,-0.5,31.5,0.)
cC......................tracks
c      call HBOOK1(-1502,'2G chamber: Y-Y(u,v);mm;mm;', 
c     *            200, -100., 100., 0.)
c      call HBOOK2(-1602,'2G chamber: Y vs X;mm;mm;', 
c     *            50, -100., 100., 50, -100., 100., 0.)
c      call HBOOK1(-1803,'2G chamber: X Track-Hit distance;mm;Events;', 
c     *            100, -50., 50., 0.)
c      call HBOOK1(-1804,'2G chamber: Y Track-Hit distance;mm;Events;', 
c     *            100, -50., 50., 0.)
cC-------- 3XY chambers:
cC......................fired wire position
c      call HBOOK1(-700,'3X wire;Position;Events;',96,0.,96.,0.)
c      call HBOOK1(-800,'3Y wire;Position;Events;',96,0.,96.,0.)
cC......................cluster center position of fired wires
c      call HBOOK1(-701,'3X cluster center;Position;Events;',
c     *            96,0.,96.,0.)
c      call HBOOK1(-801,'3Y cluster center;Position;Events;',
c     *            96,0.,96.,0.)
cC......................cluster width of fired wires
c      call HBOOK1(-702,'3X cluster width;Size;Events;',16,-0.5,15.5,0.)
c      call HBOOK1(-802,'3Y cluster width;Size;Events;',16,-0.5,15.5,0.)
cC......................number of clusters
c      call HBOOK1(-703,'3X Number of clusters;Clusters;Events;',
c     *            32,-0.5,31.5,0.)
c      call HBOOK1(-803,'3Y Number of clusters;Clusters;Events;',
c     *            32,-0.5,31.5,0.)
cC......................tracks
c      call HBOOK2(-1603,'3XY chamber: Y vs X;mm;mm;', 
c     *            50, -100., 100., 50, -100., 100., 0.)
c      call HBOOK1(-1805,'3XY chamber: X Track-Hit distance;mm;Events;', 
c     *            100, -50., 50., 0.)
c      call HBOOK1(-1806,'3XY chamber: Y Track-Hit distance;mm;Events;', 
c     *            100, -50., 50., 0.)
cC......................Planes efficiency
c      call HBOOK1(-1000,'Planes efficiency;Chamber ID;Events;',
c     *           10,0.,10.,0.)
c      call HBOOK2(-1701,'Track coordinates Y vs X at z=0;mm;mm;', 
c     *            50, -100., 100., 50, -100., 100., 0.)
c      call HBOOK2(-1702,'Track slopes Y vs X;X slope;Y slope;', 
c     *            50, -100., 100., 50, -100., 100., 0.)
c      call HBOOK1(-1703,'Chi2(x) of tracks;Chi2-x;Events;', 
c     *            50, 0., 25., 0.)
c      call HBOOK1(-1704,'Chi2(y) of tracks;Chi2-y;Events;', 
c     *            50, 0., 25., 0.)
cC-------- End MWPC histogramms
Cmike konec moih kommentariev
C
          call hbook1(700,'Hits number',800,0.,800.,0.)
          do i=1,640
             aver_map1(i)=0
             aver_map2(i)=0
             hit_map1(i)=0
             hit_map2(i)=0
          enddo
C	  
          call hbook1(5014,'claster length',15,0.,15.,0.)
          call hbook1(5015,'statistika 2',1000,0.,1000.,0.)
          call hbook2(5016,'map 1',24,0.5,24.5,
     +             24,0.5,24.5,0.)
          call hbook2(5017,'map only bad',24,0.5,24.5,
     +             24,0.5,24.5,0.)
          call hbook2(5018,'map with one bad',24,0.5,24.5,
     +             24,0.5,24.5,0.)
          call hbook2(5019,'map 1 small',8,0.5,8.5,
     +             8,0.5,8.5,0.)
C-        call hbook2(5020,'map small only bad',8,0.5,8.5,
C-   +  	   8,0.5,8.5,0.)
C-   	  call hbook2(5021,'map small with one bad',8,0.5,8.5,
C-   +  	   8,0.5,8.5,0.)
C
C----------triggera-----------------
          call hbook1(5101,'c1',4000,0.,4000.,0.)
          call hbook1(5102,'c2',4000,0.,4000.,0.)
          call hbook1(5103,'c3',4000,0.,4000.,0.)
C         call hbook1(5104,'c4', 200,0., 200.,0.)
C-        do i=0,95
C-        call hbook1(5200+i,'hard 1280+i adc',4000,0.,4000.,0.)
C-        enddo
C
          call hbook1(5105,'pile-up discriminator (M4 in the past)'
     +         ,4000,0.,4000.,0.)
          call hbook1(5106,'pile-up discriminator (M4 in future)'
     +         ,4000,0.,4000.,0.)
          do i = 1,24
             title(6) = '  '
             call csetdi(i,title(6),1,2)
             do j=1,24
                id = (i-1)*24+j
                title(4) = '  '
                call csetdi(j,title(4),1,2)
C-              call hbook1(id,title2,200,0.,400.,0.)     !  ; they were empty ; kondr
             enddo
          enddo
          do i = 1,8
             title1(6) = '  '
             call csetdi(i,title1(6),1,2)
             do j=1,8
                id = (i-1)*8+j+1000
                title1(4) = '  '
                call csetdi(j,title1(4),1,2)
C-              call hbook1(id,title3,2000,0.,4000.,0.)   !  ; they were empty ; kondr
             enddo
          enddo
C	  
          call hbook1(20090,'E single gamma',500,500.,15000.,0.)
C	  
          call hbook1(20015,'Cluster structure, big cells',
     *    100,0.,100.,0.)
          call hbook1(20016,'Cluster structure, small cells',
     *    100,0.,100.,0.)
C         
          call hbook1(20020,'Total amplitude by event',
     &    2500.,0.,5000.,0.)
          call hbook1(20021,'Total energy by event',
     &    4000.,0.,10000.,0.)
          call hbook1(20022,'Total energy OK event',
     &    4000.,0.,10000.,0.)   
     
          call hbook1(20030,'Gammas energy',100,0.,5000.,0.)
          call hbook1(20040,'Gammas multiplicity',30,0.,30.,0.)
C-        call hbook1(20041,'all Gammas multiplicity' ,100,0.,100.,0.)
C-        call hbook1(20042,'all Gammas multiplicity2',100,0.,100.,0.)    
     
C 2g-            
	  call hbook1(20100,'E1+E2',500,0.,15000.,0.)
          call hbook1(20101,'(E1-E2)/(E1+E2)',500,-1.,1.,0.)
          call hbook1(20102,'Effmass',500,0.,2000.,0.)
          call hbook1(20103,'E1+E2 (p0)',500,0.,15000.,0.)
          call hbook1(20104,'(E1-E2)/(E1+E2) (p0)',500,-1.,1.,0.)
          call hbook1(20105,'E1 & E2',500,0.,15000.,0.)
          call hbook1(20106,'E1 & E2 (p0)',500,0.,15000.,0.)
          call hbook1(20107,'X1 & X2',500,-0.025,0.075,0.)
          call hbook1(20108,'Y1 & Y2',500,-0.025,0.075,0.)
          call hbook1(20109,'abs((E1-E2)/(E1+E2))',100,0.,1.,0.)
          call hbook1(20110,'abs((E1-E2)/(E1+E2)) (p0)',100,0.,1.,0.)
          call hbook1(20111,'Effmass**2',500,0.,5000.,0.)
          call hbook1(20112,'E1+E2  ',500,0.,15000.,0.)    
C-	  call hbook1(20122,'Etot OK',500,0.,15000.,0.)
C
          call hbook1(20130,'Effective masses (2g events)',
     *     500,0.,2000.,0.)
          call hbook1(20132,'(2g events) centr',
     *     500,0.,2000.,0.)
          call hbook1(20133,'(2g events) kray',
     *     500,0.,2000.,0.)
          call hbook1(20134,'(2g events) centr+kray',
     *     500,0.,2000.,0.)
          call hbook1(20140,'Angle between gammas (2g events)',
     *     5000,0.,0.5,0.)
          call hbook1(20141,'sin(theta12/2) (2g events)',
     *     5000,0.,0.25,0.)    
          call hbook1(20142,'Cluster number  ',50,0.,50.,0.) 
	  call hbook1(20143,'Cluster Energy  ',500,0.,15000.,0.) 
C 3g-
          call hbook1(20231,'Effective masses (3g events)',
     *     500,0.,2000.,0.)   
C 4g-
C-        call hbook1(20350,'Effective masses (4g events)',
C-   *     500,0.,2000.,0.)
C-        call hbook1(20360,'Angle between gammas (4g events)',
C-   *     100,0.,1.,0.)
C-        call hbook1(20370,'Effective masses, selected (4g events)',
C-   *     100,0.,500.,0.)
C-        call hbook1(20380,'Effective masses, selected (all events)',
C-   *     125,0.,250.,0.)
          call hbook1(20381,'Effective masses, selected ( >1500MeV)',
     *     125,0.,250.,0.)
          call hbook1(20382,'E gamma, selected (.gt.1500MeV)',
     *     400,0.,10000.,0.)
          call hbook1(20382,'E1+E2  , selected (.gt.1500MeV)',
     *     400,0.,10000.,0.)
C-        call hbook1(20383,'1/sigmaq, selected (.gt.1500MeV)',
C-   *     100,0.,  2.,0.)
          call hbook1(20384,'1/(sigmaq+C), selected (.gt.1500MeV)',
     *     100,0.,200.,0.) 
C
c      call hbook1(30100,'3g kombinatorika',500,0.,2000.,0.)
c      call hbook1(30109,'abs((E1-E2)/(E1+E2)), 3g',100,0.,1.,0.)
c      call hbook1(30110,'4g kombinatorika',500,0.,2000.,0.)
c      call hbook1(30119,'abs((E1-E2)/(E1+E2)), 4g',100,0.,1.,0.)
c      call hbook1(30120,'5g kombinatorika',500,0.,2000.,0.)
c      call hbook1(30129,'abs((E1-E2)/(E1+E2)), 5g',100,0.,1.,0.)
c      call hbook1(30130,'6g kombinatorika',500,0.,2000.,0.)
c      call hbook1(30139,'abs((E1-E2)/(E1+E2)), 6g',100,0.,1.,0.)
c      call hbook1(30200,'3g - pi0',500,0.,2000.,0.)
c      call hbook1(30210,'4g - pi0',500,0.,2000.,0.)
c      call hbook1(30220,'5g - pi0',500,0.,2000.,0.)
c      call hbook1(30230,'6g - pi0',500,0.,2000.,0.)
c      call hbook1(30300,'4g - pi0+pi0',500,0.,2000.,0.)
c      call hbook1(30400,'4g - dopolnenie k pi0',500,0.,2000.,0.)
c
c---------------gistogrammy s zarezaniem po energii-------
c      do i=1,8
c        write(a32,*)'E single gamma',1500+500*i,'gammas'
c        call hbook1(20290+i,a32,500,500.,20000.,0.)
c---------------effektivnye massy 20401-20408--------------
c        write(a32,*)'Effective masses',i+1,'gammas'
c        call hbook1(20400+i,a32,500,0.,2000.,0.)
c---------------dlia 2gamma: gistogrammy 20501-20508-------
c        write(a32,*)'Effective masses 2g (',1500+500*i,'+)'
c        call hbook1(20500+i,a32,500,0.,2000.,0.)
c---------------dlia 2gamma: gistogrammy 20601-20608-------
c        write(a32,*)'abs((E1-E2)/(E1+E2)),2g (',1500+500*i,'+)'
c          call hbook1(20600+i,a32,100,0.,1.,0.)
c---------------dlia 3gamma: gistogrammy 20511-20518-------
c        write(a32,*)'Effective masses 3g (',1500+500*i,'+)'
c        call hbook1(20510+i,a32,500,0.,2000.,0.)
c---------------------------30101 - 30108------------------
c        write(a32,*)'3g kombinatorika (',1500+500*i,'+)'
c        call hbook1(30100+i,a32,500,0.,2000.,0.)
c---------------dlia 3gamma: gistogrammy 20611-20618-------
c        write(a32,*)'abs((E1-E2)/(E1+E2)),3g (',1500+500*i,'+)'
c          call hbook1(20610+i,a32,100,0.,1.,0.)
c---------------------------30201 - 30208------------------
c        write(a32,*)'3g - pi0 (',1500+500*i,'+)'
c        call hbook1(30200+i,a32,500,0.,2000.,0.)
c---------------dlia 4gamma: gistogrammy 20521-20528-------
c        write(a32,*)'Effective masses 4g (',1500+500*i,'+)'
c        call hbook1(20520+i,a32,500,0.,2000.,0.)
c---------------------------30111 - 30118------------------
c        write(a32,*)'4g kombinatorika (',1500+500*i,'+)'
c        call hbook1(30110+i,a32,500,0.,2000.,0.)
c---------------dlia 4gamma: gistogrammy 20621-20628-------
c        write(a32,*)'abs((E1-E2)/(E1+E2)),4g (',1500+500*i,'+)'
c          call hbook1(20620+i,a32,100,0.,1.,0.)
c---------------------------30211 - 30218------------------
c        write(a32,*)'4g - pi0 (',1500+500*i,'+)'
c        call hbook1(30210+i,a32,500,0.,2000.,0.)
c---------------------------30411 - 30418------------------
c        write(a32,*)'4g - dopolnenie k pi0 (',1500+500*i,'+)'
c        call hbook1(30410+i,a32,500,0.,2000.,0.)
c---------------------------30301 - 30308------------------
c        write(a32,*)'4g - pi0+pi0 (',1500+500*i,'+)'
c        call hbook1(30300+i,a32,200,0.,2000.,0.)
c---------------dlia 5gamma: gistogrammy 20531-20538-------
c        write(a32,*)'Effective masses 5g (',1500+500*i,'+)'
c        call hbook1(20530+i,a32,500,0.,2000.,0.)
c---------------dlia 5gamma: gistogrammy 20631-20638-------
c        write(a32,*)'abs((E1-E2)/(E1+E2)),5g (',1500+500*i,'+)'
c          call hbook1(20630+i,a32,100,0.,1.,0.)
c---------------------------30421 - 30428------------------
c        write(a32,*)'5g - dopolnenie k pi0 (',1500+500*i,'+)'
c        call hbook1(30420+i,a32,500,0.,2000.,0.)
c---------------------------30221 - 30228------------------
c        write(a32,*)'5g - pi0 (',1500+500*i,'+)'
c        call hbook1(30220+i,a32,500,0.,2000.,0.)
c---------------------------30311 - 30318------------------
c        write(a32,*)'5g - pi0+pi0 (',1500+500*i,'+)'
c        call hbook1(30310+i,a32,200,0.,2000.,0.)
c---------------dlia 6gamma: gistogrammy 20541-20548-------
c        write(a32,*)'Effective masses 6g (',1500+500*i,'+)'
c        call hbook1(20540+i,a32,500,0.,2000.,0.)
c---------------------------30231 - 30238------------------
c        write(a32,*)'6g - pi0 (',1500+500*i,'+)'
c        call hbook1(30230+i,a32,500,0.,2000.,0.)
c      enddo
c
CEvd   all gamma events. photon coordinates
         call hbook2(-30001,'XY of all gammas',480,-1026.
     +         ,1026.,480,-1026.,1026.,0.)
         
         call hbook2(-30002,'XY of all gammas, cluSize X,Y > 1',480,
     +        -1026.,1026.,480,-1026.,1026.,0.)
         call hbook1(-30003,'X of all gammas, cluSize X > 1',
     +        960,-1026.,1026.,0.)
         call hbook1(-30004,'Y of all gammas, cluSize Y > 1',
     +        960,-1026.,1026.,0.)
CEvd   cluster size along X, Y
         call hbook1(-30005,'cluster size along X',20,0.,20.,0.)
         call hbook1(-30006,'cluster size along Y',20,0.,20.,0.)
         
CSdv   1g events
c
         call hbook1(30000,'1g events, energy',500,0.,15000.,0.)
         call hbook1(-30000,'E, MeV  1g |x,y|"L#25 cm',500,0.,15000.,0.)
         do j=1,5
            call hbook2(30000+j,'1g events, XY E .lt. jx100',48,-1026.
     +           ,1026.,48,-1026.,1026.,0.)
         enddo
C     -       call hbook2(30006,'1g events, XY',48,-1026.
C-   +           ,1026.,48,-1026.,1026.,0.)
c
      do i=2,6         ! mnozhestvennost' gamma     
        do j=0,9       ! srezy po energii, nachinaya s 2g
c-------summarnaya energiya 30000+------------------------------
          write(a32,*)'summarnaya energiya',i,'g (',1500+500*j,'+)'
          call hbook1(30000+(i-1)*100+j,a32,500,0.,15000.,0.)
c-------energiya otdel'nyh fotonov 30010+-----------------------
          write(a32,*)'otdelnye fotony',i,'g (',
     +                                     1500+500*j,'+)'
          call hbook1(30010+(i-1)*100+j,a32,500,0.,15000.,0.)
c-------effektivnaia massa 30020+-------------------------------
          write(a32,*)'Effective mass',i,'g (', 1500+500*j,'+)'
          call hbook1(30020+(i-1)*100+j,a32,2000,0.,2000.,0.)
C70-
          write(a32,*)'Eff. MisMs < 1.5',i,'g (', 1500+500*j,'+)'
          call hbook1(30070+(i-1)*100+j,a32,2000,0.,2000.,0.)
C	  
          write(a32,*)'Effective mass',i,'g (', 1500+500*j,'-',
     +                                          2000+500*j,'+)'
          call hbook1(32020+(i-1)*100+j,a32,500,0.,2000.,0.)
c-------kombinatorika 30030+------------------------------------
          write(a32,*)'kombinatorika',i,'g (', 1500+500*j,'+)'
          call hbook1(30030+(i-1)*100+j,a32,1000,0.,2000.,0.)
c-------assimetriya 30040+--------------------------------------
          write(a32,*)'(E1-E2)/(E1+E2)',i,'g (', 1500+500*j,'+)'
          call hbook1(30040+(i-1)*100+j,a32,100,0.,1.1,0.)
          write(a32,*)'Dlia pi0: (E1-E2)/(E1+E2)',i,'g(',1500+500*j,'+)'
          call hbook1(32040+(i-1)*100+j,a32,100,0.,1.1,0.)
c-------massa s pi0 30050+--------------------------------------
          write(a32,*)'efmass with pi0',i,'g (', 1500+500*j,'+)'
          call hbook1(30050+(i-1)*100+j,a32,1000,0.,2000.,0.)
C	
        if(i.ne.2) then
c-------kombinatorika dopolneniya k pi0 30060+------------------
           if(i.eq.3) then
c-------massa s eta 30060+--------------------------------------
	   write(a32,*)'efmass with eta',i,'g (',1500+500*j,'+)'
           call hbook1(30060+(i-1)*100+j,a32,1000,0.,2000.,0.)
	        else	  
	        write(a32,*)'dopolnenie k pi0',i,
     +                                'g (', 1500+500*j,'+)'
                call hbook1(30060+(i-1)*100+j,a32,1000,0.,2000.,0.)
	   endif
c-------massa pi0+pi0 30070+------------------------------------
C70-      write(a32,*)'efmass pi0+pi0',i,'g (', 1500+500*j,'+)'
C70-      call hbook1(30070+(i-1)*100+j,a32,1000,0.,2000.,0.)
c-------summa po energii pri pi0 v kombinatorike 30080+---------
          write(a32,*)'E for pi0',i,'g (',1500+500*j ,'+)'
          call hbook1(30080+(i-1)*100+j,a32,500,0.,15000.,0.)
c-------assimetriya dlia pi0-pi0 sobytiy 30090+-----------------
          write(a32,*)'(E1-E2)/(E1+E2) for pi0*2',i,'g (',
     +                                    1500+500*j,'+)'
          call hbook1(30090+(i-1)*100+j,a32,100,0.,1.1,0.)
        else  
	  write(a32,*)'tg(gamma) from pi0',i,'g (', 1500+500*j,'+)'
	  call hbook1(30060+(i-1)*100+j,a32,250,0.,0.5,0.)
	  write(a32,*)'tg(gamma)  out pi0',i,'g (', 1500+500*j,'+)'
C70-	  call hbook1(30070+(i-1)*100+j,a32,250,0.,0.5,0.)
C70-	  write(a32,*)'tg(gamma) from eta',i,'g (', 1500+500*j,'+)'
	  call hbook1(30080+(i-1)*100+j,a32,250,0.,0.5,0.)
	  write(a32,*)'tg(gamma)  out eta',i,'g (', 1500+500*j,'+)'
	  call hbook1(30090+(i-1)*100+j,a32,250,0.,0.5,0.)
        endif  
      enddo
      enddo
C
      do j=0,9                  ! srezy po energii
CEvd----kombinatorika 3g under omega peak -30230-j
        write(a32,*)'kombinatorika 3g under omega peak(',1000+500*j,'+)'
        call hbook1(-30230-j,a32,1000,0.,2000.,0.)
c-------ugol vyleta pi0 31090+----------------------------------
        write(a32,*)'ugol vyleta pi0 2g (',1000+500*j,'+)'
        call hbook1(31090+j,a32,200,0.,.4,0.)
c-------ugol vyleta eta 31190+----------------------------------
        write(a32,*)'ugol vyleta eta 2g (',1000+500*j,'+)'
        call hbook1(31190+j,a32,200,0.,.4,0.)
c-------ugol vyleta pi0ey v SCM f2 31000+-----------------------
        write(a32,*)'ugol vyleta pi0ey v SCM f2'
        call hbook1(31000+j,a32,200,0.,1.1,0.)
c-------tol'ko pi+, 2gamma ---------------------------------------------
        write(a32,*)'2gamma, tolko pi+ (',1000+500*j,'+)'
        call hbook1(31500+j,a32,2000,0.,2000.,0.)
c-------tol'ko K+, 2gamma  ---------------------------------------------
        write(a32,*)'2gamma, tolko K+ (',1000+500*j,'+)'
        call hbook1(31510+j,a32,2000,0.,2000.,0.)
c-------tol'ko p+, 2gamma  ---------------------------------------------
        write(a32,*)'2gamma, tolko p+ (',1000+500*j,'+)'
        call hbook1(31520+j,a32,2000,0.,2000.,0.)
c-------mixed 2gamma, tol'ko pi+ ---------------------------------------------
        write(a32,*)'mixed 2gamma, tolko pi+ (',1000+500*j,'+)'
        call hbook1(-31500-j,a32,2000,0.,2000.,0.)
c-------mixed 2gamma, tol'ko K+ ---------------------------------------------
        write(a32,*)'mixed 2gamma, tolko K+ (',1000+500*j,'+)'
        call hbook1(-31510-j,a32,2000,0.,2000.,0.)
c-------mixed 2gamma, tol'ko p+ ---------------------------------------------
        write(a32,*)'mixed 2gamma, tolko p+ (',1000+500*j,'+)'
        call hbook1(-31520-j,a32,2000,0.,2000.,0.)
c-------tol'ko pi+, 3gamma ---------------------------------------------
        write(a32,*)'3gamma, tolko pi+ (',1000+500*j,'+)'
        call hbook1(31530+j,a32,500,0.,2000.,0.)
c-------tol'ko K+, 3gamma  ---------------------------------------------
        write(a32,*)'3gamma, tolko K+ (',1000+500*j,'+)'
        call hbook1(31540+j,a32,500,0.,2000.,0.)
c-------tol'ko p+, 3gamma  ---------------------------------------------
        write(a32,*)'3gamma, tolko p+ (',1000+500*j,'+)'
        call hbook1(31550+j,a32,500,0.,2000.,0.)
c-------tol'ko pi+, pi0-gamma ------------------------------------------
        write(a32,*)'pi0+gamma, tolko pi+ (',1000+500*j,'+)'
        call hbook1(31560+j,a32,500,0.,2000.,0.)
c-------tol'ko K+, pi0-gamma  ------------------------------------------
        write(a32,*)'pi0+gamma, tolko K+ (',1000+500*j,'+)'
        call hbook1(31570+j,a32,500,0.,2000.,0.)
c-------tol'ko p+, pi0-gamma  ------------------------------------------
        write(a32,*)'pi0+gamma, tolko p+ (',1000+500*j,'+)'
        call hbook1(31580+j,a32,500,0.,2000.,0.)
C
C     Evd+ we have nothing to combine for 2gamma case so let's use 
C          30130..30139 for more energy slices (6000+, 6500+ and so on).
        ig2=2
        write(a32,*)'Effective mass',ig2,'g (', 6000+500*j,'+)'
        call hbook1(30130+j,a32,2000,0.,2000.,0.)
      enddo

          call hbook1(20601,'(E1+E2)/(E1*E2) (1000-3000)',
     *     500,0.0004,0.004,0.)
          call hbook1(20602,'(E1+E2)/(E1*E2) (3000-5000)',
     *     500,0.0004,0.004,0.)
          call hbook1(20603,'(E1+E2)/(E1*E2) (5000-7000)',
     *     500,0.0004,0.004,0.)
          call hbook1(20604,'(E1+E2)/(E1*E2) (7000-9000)',
     *     500,0.0004,0.004,0.)
c- 	  call hbook2(20700,'M vs (E1+E2)/(E1*E2)',
c-   *     500,0.,2000.,500,0.0004,0.004,0.)
c- 	  call hbook2(20701,'M vs M*(E1+E2)/(E1*E2)',
c-   *     500,0.,2000.,500,0.0004,1.,0.)
          call hbook1(20702,'M*(E1+E2)/(E1*E2)',
     *     500,0.0004,1.004,0.)
c
        call hbook2(5001,'fotony ot pi0',48,-1026.,1026.,
     +              48,-1023.,1023.,0.)
Cmike
C-       call hbook2(-5002,'fotony ot eta (Mike)',48,-1026.,1026.,
C-    +           48,-1023.,1023.,0.)
C-       call hbook2(-5004,'m(pi0) vs r(x,y)',70,0.,1400.,
C-    +            14,100.,170.,0.)
C-       call hbook2(-5005,'m(eta) vs r(x,y)',70,0.,1400.,
C-    +            40,450.,650.,0.)     
C-       call hbook2(-5006,'E vs r for pi0 photons',72,0.,14400.,
C     +            140.,0.,7000.,0.)
Cmike
C
        call hbook2(5002,'zaselennost big"G#10',24,0.5,24.5,
     +             24,0.5,24.5,0.)
        call hbook2(5003,'zaselennost small"G#10',8,0.5,8.5,
     +             8,0.5,8.5,0.)
        call hbook1(5004,'dlina sobytiya      ',125,   0., 250.,0.)
        call hbook1(5005,'zhelezniy adres     ',864,1920.,2784.,0.)
        call hbook1(5006,'amplituda v iacheyke',4096,0.,4096.,0.)
        call hbook1(5007,'summa amplitud"G#10 ',1024,0.,4096.,0.)
        call hbook1(5008,'summa amplitud"G#20 ',1024,0.,4096.,0.)
        call hbook1(5009,'zhelezniy adres, 10"L#A"L#250'
     &                                      ,864,1920.,2784.,0.)
C
        call hbook2(5011,'koordinaty pi0',60,-1200.,1200.,
     +                60,-1200.,1200.,0.)
        call hbook1(5012,'x dlia pi0',100,-1200.,1200.,0.)
        call hbook1(5013,'y dlia pi0',100,-1200.,1200.,0.)
        call hbook2(5022,'zaselennost big"G#10,"L#500',24,0.5,24.5,
     +                24,0.5,24.5,0.)
        call hbook2(5023,'zaselennost small"G#10, "L#500',8,0.5,8.5,
     +                 8,0.5,8.5,0.)
        call hbook2(5052,'zaselennost big"L#1500',24,0.5,24.5,
     +                24,0.5,24.5,0.)
        call hbook2(5053,'zaselennost small"L#1500',8,0.5,8.5,
     +                 8,0.5,8.5,0.)
        call hbook2(5062,'zaselennost big"G#1500',24,0.5,24.5,
     +                24,0.5,24.5,0.)
        call hbook2(5063,'zaselennost small"G#1500',8,0.5,8.5,
     +                8,0.5,8.5,0.)
C
        call hbook1(5059,'zhelezniy adres,    A"L#250'
     &      ,864,1920.,2784.,0.)
C     
        call hbook1(5030,'1g, Wall2 channels ',600,0.,600.,0.)     
        call hbook1(5031,'energiya v iacheyke',2048,0.,8192.,0.)
	call hbook1(5032,'summa energy total  ',   500,0.,15000.,0.)
        call hbook1(5033,'energiya fotonov    ' ,  500,0.,15000.,0.)
        call hbook1(5035,'summa energy Ampl OK ',  500,0.,15000.,0.)
	call hbook1(5036,'summa energy Wall2 OK',  500,0.,15000.,0.)
Csdv-
        call hbook1(5037,'summa energy from Calib',500,0.,15000.,0.)
        call hbook1(5038,'Eout-Esum              ',500,0., 1000.,0.)
	call hbook1(5039,'Mass from Calib       ',2000,0., 2000.,0.)
C
        call hbook1(5040,'chislo fotonov       ', 10, 0.5, 10.5,0.)
	call hbook1(5041,'summa energiy fotonov',500, 0.,15000.,0.)
C-      call hbook1(5040,'summa energy Wall2 NotOK=2',500,0.,20000.,0.)
C-      call hbook1(5041,'summa energy Wall2 NotOK=3',500,0.,20000.,0.)
C-      call hbook1(5042,'summa energy Wall2 NotOK=4',500,0.,20000.,0.)
        call hbook1(5043,'energy pi0                ',500,0.,20000.,0.)
        call hbook1(5044,'energia sprava ot pi0',     500,0.,20000.,0.)
        call hbook1(5045,'energia sleva ot pi0 ',     500,0.,20000.,0.)
        call hbook1(5046,'energy eta0',               500,0.,20000.,0.)
        call hbook1(5047,'energia sprava ot eta0',    500,0.,20000.,0.)
        call hbook1(5048,'energia sleva ot eta0 ',    500,0.,20000.,0.)
        call hbook1(5049,'energy omega0',500,0.,             20000.,0.)
        call hbook1(5050,'energia sprava ot omega'   ,500,0.,20000.,0.)
        call hbook1(5051,'energia sleva ot omega '   ,500,0.,20000.,0.)
        call hbook1(5054,'Proton energy          ',  1000,0., 3000.,0.)
        call hbook1(5055,'Etotal+proton energy      ',500,0.,1.5E4, 0.)
C
	call hbook1(5056,'Etotal+proton energy ngam=2',500,0.,1.5E4,0.)
        call hbook1(5057,'Etotal+proton energy ngam=3',500,0.,1.5E4,0.)
	call hbook1(5058,'Etotal+proton energy ngam=4',500,0.,1.5E4,0.)
C
C   --- T & MisMas distributions ---
C Pi0-
        call hbook1(5200,'Mass 2g, Egm.gt.1000 ',2000, 0.,2000.,0.)
	call hbook1(5201,'E2gTcr in pi0, Egm.gt.1000',2000, 0., 1.e4,0.)
        call hbook1(5202,'E2gTcr out pi0,Egm.gt.1000',2000, 0., 1.e4,0.)
        call hbook1(5203,'Mass in pi0,   Egm.gt.1000',2000, 0.,2000.,0.)
        call hbook1(5204,'Mass out pi0,  Egm.gt.1000',2000, 0.,2000.,0.)
	call hbook1(5205,'T in pi0 , Egm.gt.1000    ', 300,-1.,   2.,0.)
        call hbook1(5206,'T out pi0, Egm.gt.1000    ', 300,-1.,   2.,0.)
	call hbook1(5207,'MisMs in pi0 , Egm.gt.1000', 180, 0.,3600.,0.)
        call hbook1(5208,'MisMs out pi0, Egm.gt.1000', 180, 0.,3600.,0.)
	call hbook2(5209,'T & MisMs in pi0,E.gt.1000', 180, 0.,3600.,
     &                                                 100, 0.,   1.,0.)
        call hbook2(5210,'T & MisMs out pi0,E.gt.1000',180, 0.,3600.,
     &                                                 100, 0.,   1.,0.)
C Eta-    
 	call hbook1(5211,'E2gTcr in eta, Egm.gt.1000',2000, 0., 1.e4,0.)
        call hbook1(5212,'E2gTcr out eta,Egm.gt.1000',2000, 0., 1.e4,0.)
        call hbook1(5213,'Mass in eta ,  Egm.gt.1000',2000, 0.,2000.,0.)
        call hbook1(5214,'Mass out eta,  Egm.gt.1000',2000, 0.,2000.,0.)
	call hbook1(5215,'T in eta , Egm.gt.1000    ', 300,-1.,   2.,0.)
        call hbook1(5216,'T out eta, Egm.gt.1000    ', 300,-1.,   2.,0.)
	call hbook1(5217,'MisMs in eta , Egm.gt.1000', 180, 0.,3600.,0.)
        call hbook1(5218,'MisMs out eta, Egm.gt.1000', 180, 0.,3600.,0.)
	call hbook2(5219,'T & MisMs in eta,E.gt.1000', 180, 0.,3600.,
     &               	  			       100, 0.,   1.,0.)
        call hbook2(5220,'T & MisMs out eta,E.gt.1000',180, 0.,3600.,
     &                                                 100, 0.,   1.,0.)
C Omg-    
 	call hbook1(5221,'E2gTcr in omg, Egm.gt.1000', 2000,0., 1.e4,0.)
        call hbook1(5222,'E2gTcr out om, Egm.gt.1000', 2000,0., 1.e4,0.)
        call hbook1(5223,'Mass in omg , Egm.gt.1000 ', 2000,0.,2000.,0.)
        call hbook1(5224,'Mass out omg, Egm.gt.1000 ', 2000,0.,2000.,0.)
	call hbook1(5225,'T in omg , Egm.gt.1000    ',  100,0.,   1.,0.)
        call hbook1(5226,'T out omg, Egm.gt.1000    ',  100,0.,   1.,0.)
	call hbook1(5227,'MisMs in omg , Egm.gt.1000',  175,0.,3500.,0.)
        call hbook1(5228,'MisMs out omg, Egm.gt.1000',  175,0.,3500.,0.)
	call hbook2(5229,'T & MisMs in omg ,E.gt.1000', 175,0.,3500.,
     &                  			        100,0.,   1.,0.)
        call hbook2(5230,'T & MisMs out omg,E.gt.1000', 175,0.,3500.,
     &                                                  100,0.,   1.,0.)  
C f2(1270)-
C
        call hbook1(5250,'Mandelstam s+t+u', 200, 0., 20.,0.) 
Csdv-
C      Kinfit histograms
C
cmike
c     call hbook2(-12000,'x,y ',50,-1200.,1100.,50,-1200.,1100.,0.)
      call hbook2(-12001,'mass'       ,50,0.,25.,50,0.,25.,0.)
      call hbook2(-12002,'good masses',50,0.,25.,50,0.,25.,0.)
      call hbook2(-12003,'nsb masses' ,50,0.,25.,50,0.,25.,0.)
      call hbook2(-12004,'bad masses' ,50,0.,25.,50,0.,25.,0.)
        do i=1,640
          call hbook1(-10000-i,'eff. mass in cell I',500 ,0.,500. ,0.)
          call hbook1(-20000-i,   'energy in cell I',2048,0.,4096.,0.)
        enddo
      call hbook1(-11000,'eff. mass in every cell',500,0.,500.,0.)
      call hbook1(-11001,'mass' ,299,1.,300.,0.)
      call hbook1(-11002,'sigma', 50,0.,100.,0.)
      call hbook1(-11003,'N'	,1000,0.,5000.,0.)
      call hbook1(-11004,'XSHW' ,200,-1200.,1100.,0.)
      call hbook1(-11005,'YSHW' ,200,-1200.,1100.,0.)
c
cmike
c      call hbook2(-13001,'Mass <-> Angle correlations',100,0.,.4,
c     &                                                 350,0.,700.,0.)
c      call hbook2(-13002,'Mass <-> Depth (ln(E)) correlations',
c     &                  45,5.,9.5,350,0.,700.,0.)
c     call hbook2(-13003,'Mass <-> E correlations',550,0.,5500,
c     &                                            350,0.,700.,0.)
c      call hbook2(-13003,'Mass <-> Depth (ln(E)) correlations, komb',
c     &                  45,5.,9.5,350,0.,700.,0.)
c      call hbook2(-13004,'Mass <-> Depth (ln(E)) correlations, dopl',
c     &                  45,5.,9.5,350,0.,700.,0.)
c      call hbook2(-13005,'kakaya-to fignya',60,-1200.,1200.,
c     &                                       60,-1200.,1200.,0.)
c      
c      call hbook1(-5003,'Odinochnye energii v 2g',1100,0.,5500.,0.)
cmike
      call hbook1(-1,'sigmaq',100 ,0.,0.1,0.)
      call hbook1(-2,'chi',   1000,0.,1. ,0.)
      call hbook1(-3,'AB test',1000,-5.,5.,0.)
cmike 
c        call clb_fit
cmike
C-----transverce momentum
      call hbook1(-6000,'transverce momentum, all particles'
     &            ,4000,0.,4000.,0.)
      call hbook1(406000,'transverce momentum, 4 gamma'
     &            ,1000,0.,2.,0.)
      call hbook1(-6001,'transverce momentum, pi0'
     &            ,4000,0.,4000.,0.)
      call hbook1(-6002,'transverce momentum, eta'
     &            ,4000,0.,4000.,0.)
      call hbook1(-6003,'Xf, pi0'
     &            ,500 ,-1.5, 1.5,0.)
      call hbook1(-6004,'Xf, eta'
     &            ,500 ,-1.5, 1.5,0.)
      call hbook2(-6010,'transverce momentum vs mass, 2g'
     &            ,1000,0.,1000.,700,0.,700.,0.)
      call hbook2(-6011,'transverce momentum vs mass, pi0'
     &            ,4000,0.,4000.,70,100.,170.,0.)
      call hbook2(-6012,'transverce momentum vs mass, eta'
     &            ,4000,0.,4000.,200,450.,650.,0.)
c-----t and miss mass
      call hbook1(-61001,'t, all events, [p]^+! as beam particle'
     &            ,2500,-2.,.5,0.)
      call hbook1(-61002,'missmass, all events, [p]^+! as beam particle'
     &            ,1000,0.,10000.,0.)
      call hbook1(-62001,'t, all events, K^+! as beam particle'
     &            ,2500,-2.,.5,0.)
      call hbook1(-62002,'missmass, all events, K^+! as beam particle'
     &            ,1000,0.,10000.,0.)
      call hbook1(-61501,'t, 2[p]^0!, [p]^+! as beam particle'
     &            ,2500,-2.,.5,0.)
      call hbook1(-61502,'missmass, 2[p]^0!, [p]^+! as beam particle'
     &            ,1000,0.,10000.,0.)
      call hbook1(-62501,'t, 2[p]^0!, K^+! as beam particle'
     &            ,2500,-2.,.5,0.)
      call hbook1(-62502,'missmass, 2[p]^0!, K^+! as beam particle'
     &            ,1000,0.,10000.,0.)

      call hbook2(7200,'mass vs transverce momentum, 2g;ef.mass;Pt',
     &            2000,0.,1000.,400,0.,800.,0.)
      call hbook2(7201,'mass vs photon energy, 2g;ef.mass;gamma energy',
     &            2000,0.,1000.,1000,0.,10000.,0.)
C
C-    call hbook2(7300,'mass vs transverce momentum, 3g;ef.mass;Pt',
C-   &  	  3000,0.,1500.,400,0.,800.,0.)
C-    call hbook2(7301,'mass vs photon energy, 3g;ef.mass;gamma energy',
C-   &  	  3000,0.,1500.,1000,0.,10000.,0.)
C-    call hbook2(7400,'mass vs transverce momentum, 4g;ef.mass;Pt',
C-   &  	  3000,0.,2000.,400,0.,800.,0.)
C-    call hbook2(7401,'mass vs photon energy, 4g;ef.mass;gamma energy',
C-   &  	  3000,0.,2000.,1000,0.,10000.,0.)
C
!       call hbook2(7310,'mass vs transverce momentum, 3g,hyp 1;
!      &ef.mass;Pt',            3000,0.,1.5,400, 0.,.8,0.)
!       call hbook2(7311,'mass vs photon energy, 3g, hyp 1; 
!      &ef.mass;gamma energy',  3000,0.,1.5,1000,0.,10.,0.)
! !     call hbook2(7312,'mass vs photon energy, 3g, hyp 1; 
! !    &ef.mass;gamma energy',  3000,0.,1.5,1000,0.,10000.,0.)
! 
!       call hbook2(7410,'mass vs transverce momentum, 4g, hyp 1;
!      &ef.mass;Pt',            3000,0.,2.,400, 0.,.8,0.)
!       call hbook2(7411,'mass vs photon energy, 4g, hyp 1; 
!      &ef.mass;gamma energy',  3000,0.,2.,500,0.,10.,0.)
! !      call hbook2(7412,'mass vs photon energy, 4g, hyp 1; 
! !    &ef.mass;gamma energy',  3000,0.,2.,500,0.,10000.,0.)



      do ipart=1,2
        do ipeak=1,4
          do ikey=1,2
      write(a32,*) tmm(1),', ',key(ikey),' ',peak(ipeak),', ',
     &             part(ipart),' as beam particle '
      if(ikey.ge.3) then
         write(a32,*)'+E.gt.6334MeV'
      endif
      call hbook1(-60001-ipart*1000-ipeak*100-ikey*10,a32
     &            ,2500,-2.,.5,0.)
      write(a32,*) tmm(2),', ',key(ikey),' ',peak(ipeak),', ',
     &             part(ipart),' as beam particle'
      call hbook1(-60002-ipart*1000-ipeak*100-ikey*10,a32
     &            ,1000,0.,10000.,0.)
          enddo
        enddo
      enddo

cmike---------------addition to pi0 from f2
cmike---------------dopolnenie k pi0 iz f2
cmike---------------histogramms -3036i
      do i = 0,10
      write(a32,*) 'addition to pi0 from f2 (efm(4g)"G#1050.MeV),'!,
c     &             'esum"G#',(i*500+1500)
        call hbook1(-30360-i,a32,1500,0.,2000.,0.)
cmike---------------histogramms -3012i
cmike---------------mixmass 
      write(a32,*) 'mixmass, esum"G#',(i*500+1500)
        call hbook1(-30120-i,a32,2000,0.,2000.,0.)
      enddo
cmike
CEvd+
c$-     call hbook2(-75200,'zaselennost small E=weight',8,0.5,8.5,
c$-    + 	 8,0.5,8.5,0.)
c$-     call hbook2(-75100,'zaselennost big E=weight',24,0.5,24.5,
c$-    + 	24,0.5,24.5,0.)
c$-	call hbook1(-75300,'energy in big cell',1000,0.,20000.,0.)
c$-	call hbook1(-75400,'energy in small cell',1000,0.,20000.,0.)
c$C	kinematic variable distributions with different weights
c$-	write(a32,*)'Nchannel total vs Energy total'
c$-	call hbook2(-70060,a32,300,0.,900.,
c$-     *     1000,0.,20000.,0.)
c$-	write(a32,*)'(dN/dEffMass), E .gt. 7000MeV'  
c$-	call hbook1(-70050,a32, 500,0.,2000., 0.)
c$-	write(a32,*)'(dN/dEffMass), E .gt. 7500MeV'  
c$-	call hbook1(-70051,a32, 500,0.,2000., 0.)
c$-	write(a32,*)'(dN/dEffMass), E .gt. 8000MeV'  
c$-	call hbook1(-70052,a32, 500,0.,2000., 0.)
c$-	write(a32,*)'(dN/dEffMass), E .gt. 8500MeV'  
c$-	call hbook1(-70053,a32, 500,0.,2000., 0.)
c$-
c$-	write(a32,*)'(dN/dE)  under   Pi0'  
c$-	call hbook1(-71020,a32, 6000,0.,12000., 0.)
c$-	write(a32,*)'(dN/dE)  under   Eta'  
c$-	call hbook1(-71120,a32, 6000,0.,12000., 0.)
c$-	write(a32,*)'(dN/dE)  around  Pi0'  
c$-	call hbook1(-71320,a32, 6000,0.,12000., 0.)
c$-	write(a32,*)'(dN/dE)  around  Eta'  
c$-	call hbook1(-71420,a32, 6000,0.,12000., 0.)
c$-	write(a32,*)'photon coord. under  Pi0  esum"G# 7000'
c$-	call hbook2(-70030,a32,60,-1200.,1200.,
c$-     +	60,-1200.,1200.,0.)
c$-	write(a32,*)'particle coord. under Pi0 esum"G# 7000'
c$-	call hbook2(-70040,a32,60,-1200.,1200.,
c$-     +	60,-1200.,1200.,0.)
c$-	write(a32,*)'photon coord. around  Pi0  esum"G# 7000'
c$-	call hbook2(-70330,a32,60,-1200.,1200.,
c$-     +	60,-1200.,1200.,0.)
c$-	write(a32,*)'particle coord. around Pi0 esum"G# 7000'
c$-	call hbook2(-70340,a32,60,-1200.,1200.,
c$-     +	60,-1200.,1200.,0.)
c$-	write(a32,*)'photon coord. under  Eta  esum"G# 7000'
c$-	call hbook2(-70130,a32,60,-1200.,1200.,
c$-     +	60,-1200.,1200.,0.)
c$-	write(a32,*)'particle coord. under Eta esum"G# 7000'
c$-	call hbook2(-70140,a32,60,-1200.,1200.,
c$-     +	60,-1200.,1200.,0.)
c$-	write(a32,*)'photon coord. around Eta  esum"G# 7000'
c$-	call hbook2(-70430,a32,60,-1200.,1200.,
c$-     +	60,-1200.,1200.,0.)
c$-	write(a32,*)'particle coord. around Eta esum"G# 7000'
c$-	call hbook2(-70440,a32,60,-1200.,1200.,
c$-     +	60,-1200.,1200.,0.)
c$C
c$-	do j=0,9
c$C	Pi0 in EffMass from 111 to 159 MeV
c$-	write(a32,*)'photon coord. under  Pi0  esum"G#',(j*500+1000)
c$-	call hbook2(-70010-j,a32,60,-1200.,1200.,
c$-     +	60,-1200.,1200.,0.)
c$-	write(a32,*)'particle coord. under Pi0 esum"G#',(j*500+1000)
c$-	call hbook2(-70020-j,a32,60,-1200.,1200.,
c$-     +	60,-1200.,1200.,0.)
c$-	write(a32,*)'(dN/dpt)	under	  Pi0  esum"G#',(j*500+1000)
c$-	call hbook1(-71060-j,a32,5000, 0.,5000., 0.)
c$-	write(a32,*)'(pt^2!	under	  Pi0  esum"G#',(j*500+1000)
c$-	call hbook1(-71030-j,a32,5000, 0.,10.E+06, 0.)
c$-	write(a32,*)'(dN/dXf)	under	  Pi0  esum"G#',(j*500+1000)
c$-	call hbook1(-71070-j,a32, 500,-2.,   2., 0.)
c$-	write(a32,*)'(dN/dXplus)  under   Pi0  esum"G#',(j*500+1000)
c$-	call hbook1(-71080-j,a32, 500,-2.,   2., 0.)
c$-	write(a32,*)'(dN/dXfwr)     under Pi0  esum"G#',(j*500+1000)
c$-	call hbook1(-71090-j,a32, 500,-2.,   2., 0.)
c$-	write(a32,*)'(Pt^2! vs Xf   under Pi0  esum"G#',(j*500+1000)
c$-	call hbook2(-71050-j,a32,500 , 0.,10.E+06, 50,-2.,2.,0.)
c$-	write(a32,*)'(T vs MM^2!    under Pi0  esum"G#',(j*500+1000)
c$-	call hbook2(-71040-j,a32,600 ,-1.,5., 300,0.,15.,0.)
c$-	
c$C
c$-	write(a32,*)'(dN/dpt)*E   under   Pi0  esum"G#',(j*500+1000)
c$-	call hbook1(-72060-j,a32,5000, 0.,5000., 0.)
c$-	write(a32,*)'(dN/dXf)*E   under   Pi0  esum"G#',(j*500+1000)
c$-	call hbook1(-72070-j,a32, 500,-2.,   2., 0.)
c$-	write(a32,*)'(dN/dXplus)*E  under Pi0  esum"G#',(j*500+1000)
c$-	call hbook1(-72080-j,a32, 500,-2.,   2., 0.)
c$-	write(a32,*)'(dN/dXfwr)*E   under Pi0  esum"G#',(j*500+1000)
c$-	call hbook1(-72090-j,a32, 500,-2.,   2., 0.)
c$-	write(a32,*)'(Pt^2! vs Xf *E under Pi0 esum"G#',(j*500+1000)
c$-	call hbook2(-72050-j,a32,500 , 0.,10.E+06, 50,-2.,2.,0.)
c$-	write(a32,*)'(pt^2! *E   under     Pi0  esum"G#',(j*500+1000)
c$-	call hbook1(-72030-j,a32,5000, 0.,10.E+06, 0.)
c$-
c$-
c$C
c$-	write(a32,*)'(dN/dpt)*W   under   Pi0  esum"G#',(j*500+1000)
c$-	call hbook1(-73060-j,a32,5000, 0.,5000., 0.)
c$-	write(a32,*)'(dN/dXf)*W   under   Pi0  esum"G#',(j*500+1000)
c$-	call hbook1(-73070-j,a32, 500,-2.,   2., 0.)
c$-	write(a32,*)'(dN/dXplus)*W  under Pi0  esum"G#',(j*500+1000)
c$-	call hbook1(-73080-j,a32, 500,-2.,   2., 0.)
c$C
c$-	write(a32,*)'(dN/dXpl)*Xpl  under Pi0  esum"G#',(j*500+1000)
c$-	call hbook1(-74080-j,a32, 500,-2.,   2., 0.)
c$-	write(a32,*)'(dN/dT)*E  under	  Pi0  esum"G#',(j*500+1000)
c$-	call hbook1(-72010-j,a32,6000,-1.,5., 0.)
c$-	write(a32,*)'(dN/dT)	under	  Pi0  esum"G#',(j*500+1000)
c$-	call hbook1(-71010-j,a32,6000,-1.,5., 0.)
c$C
c$C	Pi0 in EffMass from 57 to 96 MeV and from 174 to 213 MeV
c$-	write(a32,*)'photon coord. around  Pi0  esum"G#',(j*500+1000)
c$-	call hbook2(-70310-j,a32,60,-1200.,1200.,
c$-     +	60,-1200.,1200.,0.)
c$-	write(a32,*)'particle coord. around Pi0 esum"G#',(j*500+1000)
c$-	call hbook2(-70320-j,a32,60,-1200.,1200.,
c$-     +	60,-1200.,1200.,0.)
c$-	write(a32,*)'(dN/dpt)	around     Pi0  esum"G#',(j*500+1000)
c$-	call hbook1(-71360-j,a32,5000, 0.,5000., 0.)
c$-	write(a32,*)'(pt^2!	around     Pi0  esum"G#',(j*500+1000)
c$-	call hbook1(-71330-j,a32,5000, 0.,10.E+06, 0.)
c$-	write(a32,*)'(dN/dXf)	around     Pi0  esum"G#',(j*500+1000)
c$-	call hbook1(-71370-j,a32, 500,-2.,   2., 0.)
c$-	write(a32,*)'(dN/dXplus)  around   Pi0  esum"G#',(j*500+1000)
c$-	call hbook1(-71380-j,a32, 500,-2.,   2., 0.)
c$-	write(a32,*)'(dN/dXfwr)   around   Pi0  esum"G#',(j*500+1000)
c$-	call hbook1(-71390-j,a32, 500,-2.,   2., 0.)
c$-	write(a32,*)'(Pt^2! vs Xf   around Pi0  esum"G#',(j*500+1000)
c$-	call hbook2(-71350-j,a32,500 , 0.,10.E+06, 50,-2.,2.,0.)
c$-	write(a32,*)'(T vs MM^2!    around Pi0  esum"G#',(j*500+1000)
c$-	call hbook2(-71340-j,a32,600 ,-1.,5., 300,0.,15.,0.)
c$C
c$-	write(a32,*)'(dN/dpt)*E   around   Pi0  esum"G#',(j*500+1000)
c$-	call hbook1(-72360-j,a32,5000, 0.,5000., 0.)
c$-	write(a32,*)'(dN/dXf)*E   around   Pi0  esum"G#',(j*500+1000)
c$-	call hbook1(-72370-j,a32, 500,-2.,   2., 0.)
c$-	write(a32,*)'(dN/dXplus)*E  around Pi0  esum"G#',(j*500+1000)
c$-	call hbook1(-72380-j,a32, 500,-2.,   2., 0.)
c$-	write(a32,*)'(dN/dXfwr)*E   around Pi0  esum"G#',(j*500+1000)
c$-	call hbook1(-72390-j,a32, 500,-2.,   2., 0.)
c$-	write(a32,*)'(Pt^2! vs Xf *E around Pi0 esum"G#',(j*500+1000)
c$-	call hbook2(-72350-j,a32,500 , 0.,10.E+06, 50,-2.,2.,0.)
c$-	write(a32,*)'(pt^2! *E   around    Pi0  esum"G#',(j*500+1000)
c$-	call hbook1(-72330-j,a32,5000, 0.,10.E+06, 0.)
c$C
c$-	write(a32,*)'(dN/dpt)*W   around   Pi0  esum"G#',(j*500+1000)
c$-	call hbook1(-73360-j,a32,5000, 0.,5000., 0.)
c$-	write(a32,*)'(dN/dXf)*W   around   Pi0  esum"G#',(j*500+1000)
c$-	call hbook1(-73370-j,a32, 500,-2.,   2., 0.)
c$-	write(a32,*)'(dN/dXplus)*W  around Pi0  esum"G#',(j*500+1000)
c$-	call hbook1(-73380-j,a32, 500,-2.,   2., 0.)
c$C
c$-	write(a32,*)'(dN/dXpl)*Xpl  around Pi0  esum"G#',(j*500+1000)
c$-	call hbook1(-74380-j,a32, 500,-2.,   2., 0.)
c$-	write(a32,*)'(dN/dT)*E  around     Pi0  esum"G#',(j*500+1000)
c$-	call hbook1(-72310-j,a32,6000,-1.,5., 0.)
c$-	write(a32,*)'(dN/dT)	around     Pi0  esum"G#',(j*500+1000)
c$-	call hbook1(-71310-j,a32,6000,-1.,5., 0.)
c$C
c$C	Eta in EffMass from 490 to 630 MeV
c$-	write(a32,*)'photon coord. under  Eta	esum"G#',(j*500+1000)
c$-	call hbook2(-70110-j,a32,60,-1200.,1200.,
c$-     +	60,-1200.,1200.,0.)
c$-	write(a32,*)'particle coord. under Eta  esum"G#',(j*500+1000)
c$-	call hbook2(-70120-j,a32,60,-1200.,1200.,
c$-     +	60,-1200.,1200.,0.)
c$-	write(a32,*)'(dN/dpt)	under	 Eta	esum"G#',(j*500+1000)
c$-	call hbook1(-71160-j,a32,5000, 0.,5000., 0.)
c$-	write(a32,*)'(pt^2!	under	  Eta  esum"G#',(j*500+1000)
c$-	call hbook1(-71130-j,a32,5000, 0.,10.E+06, 0.)
c$-	write(a32,*)'(dN/dXf)	under	 Eta	esum"G#',(j*500+1000)
c$-	call hbook1(-71170-j,a32, 500,-2.,   2., 0.)
c$-	write(a32,*)'(dN/dXplus)  under  Eta	esum"G#',(j*500+1000)
c$-	call hbook1(-71180-j,a32, 500,-2.,   2., 0.)
c$-	write(a32,*)'(dN/dXfwr)     under Eta	esum"G#',(j*500+1000)
c$-	call hbook1(-71190-j,a32, 500,-2.,   2., 0.)
c$-	write(a32,*)'(Pt^2! vs Xf   under Eta  esum"G#',(j*500+1000)
c$-	call hbook2(-71150-j,a32,500 , 0.,10.E+06, 50,-2.,2.,0.)
c$-	write(a32,*)'(T vs MM^2!    under Eta  esum"G#',(j*500+1000)
c$-	call hbook2(-71140-j,a32,600 ,-1.,5., 300,0.,15.,0.)
c$C
c$-	write(a32,*)'(dN/dpt)*E   under  Eta	esum"G#',(j*500+1000)
c$-	call hbook1(-72160-j,a32,5000, 0.,5000., 0.)
c$-	write(a32,*)'(dN/dXf)*E   under  Eta	esum"G#',(j*500+1000)
c$-	call hbook1(-72170-j,a32, 500,-2.,   2., 0.)
c$-	write(a32,*)'(dN/dXplus)*E under Eta	esum"G#',(j*500+1000)
c$-	call hbook1(-72180-j,a32, 500,-2.,   2., 0.)
c$-	write(a32,*)'(dN/dXfwr)*E   under Eta	esum"G#',(j*500+1000)
c$-	call hbook1(-72190-j,a32, 500,-2.,   2., 0.)
c$-	write(a32,*)'(Pt^2! vs Xf *E under Eta esum"G#',(j*500+1000)
c$-	call hbook2(-72150-j,a32,500 , 0.,10.E+06, 50,-2.,2.,0.)
c$-	write(a32,*)'(pt^2! *E   under    Eta  esum"G#',(j*500+1000)
c$-	call hbook1(-72130-j,a32,5000, 0.,10.E+06, 0.)
c$C
c$-	write(a32,*)'(dN/dpt)*W   under  Eta	esum"G#',(j*500+1000)
c$-	call hbook1(-73160-j,a32,5000, 0.,5000., 0.)
c$-	write(a32,*)'(dN/dXf)*W   under  Eta  esum"G#',(j*500+1000)
c$-	call hbook1(-73170-j,a32, 500,-2.,   2., 0.)
c$-	write(a32,*)'(dN/dXplus)*W  underEta  esum"G#',(j*500+1000)
c$-	call hbook1(-73180-j,a32, 500,-2.,   2., 0.)
c$C
c$-	write(a32,*)'(dN/dXpl)*Xpl  underEta  esum"G#',(j*500+1000)
c$-	call hbook1(-74180-j,a32, 500,-2.,   2., 0.)
c$-	write(a32,*)'(dN/dT)*E  under	  Eta  esum"G#',(j*500+1000)
c$-	call hbook1(-72110-j,a32,6000,-1.,5., 0.)
c$-	write(a32,*)'(dN/dT)	under	  Eta  esum"G#',(j*500+1000)
c$-	call hbook1(-71110-j,a32,6000,-1.,5., 0.)
c$-
c$C	Eta in EffMass from 420 to 490 MeV and from 630 to 700 MeV
c$-	write(a32,*)'photon coord. around Eta	esum"G#',(j*500+1000)
c$-	call hbook2(-70410-j,a32,60,-1200.,1200.,
c$-     +	60,-1200.,1200.,0.)
c$-	write(a32,*)'particle coord. around Eta esum"G#',(j*500+1000)
c$-	call hbook2(-70420-j,a32,60,-1200.,1200.,
c$-     +	60,-1200.,1200.,0.)
c$-	write(a32,*)'(dN/dpt)	around    Eta  esum"G#',(j*500+1000)
c$-	call hbook1(-71460-j,a32,5000, 0.,5000., 0.)
c$-	write(a32,*)'(pt^2!	around    Eta  esum"G#',(j*500+1000)
c$-	call hbook1(-71430-j,a32,5000, 0.,10.E+06, 0.)
c$-	write(a32,*)'(dN/dXf)	around    Eta  esum"G#',(j*500+1000)
c$-	call hbook1(-71470-j,a32, 500,-2.,   2., 0.)
c$-	write(a32,*)'(dN/dXplus)  around  Eta  esum"G#',(j*500+1000)
c$-	call hbook1(-71480-j,a32, 500,-2.,   2., 0.)
c$-	write(a32,*)'(dN/dXfwr)   around  Eta  esum"G#',(j*500+1000)
c$-	call hbook1(-71490-j,a32, 500,-2.,   2., 0.)
c$-	write(a32,*)'(Pt^2! vs Xf   Around Eta  esum"G#',(j*500+1000)
c$-	call hbook2(-71450-j,a32,500 , 0.,10.E+06, 50,-2.,2.,0.)
c$-	write(a32,*)'(T vs MM^2!    around Eta  esum"G#',(j*500+1000)
c$-	call hbook2(-71440-j,a32,600 ,-1.,5., 300,0.,15.,0.)
c$C
c$-	write(a32,*)'(dN/dpt)*E   around  Eta  esum"G#',(j*500+1000)
c$-	call hbook1(-72460-j,a32,5000, 0.,5000., 0.)
c$-	write(a32,*)'(dN/dXf)*E   around  Eta  esum"G#',(j*500+1000)
c$-	call hbook1(-72470-j,a32, 500,-2.,   2., 0.)
c$-	write(a32,*)'(dN/dXplus)*E  aroundEta  esum"G#',(j*500+1000)
c$-	call hbook1(-72480-j,a32, 500,-2.,   2., 0.)
c$-	write(a32,*)'(dN/dXfwr)*E   around Eta  esum"G#',(j*500+1000)
c$-	call hbook1(-72490-j,a32, 500,-2.,   2., 0.)
c$-	write(a32,*)'(Pt^2! vs Xf *E around Eta esum"G#',(j*500+1000)
c$-	call hbook2(-72450-j,a32,500 , 0.,10.E+06, 50,-2.,2.,0.)
c$-	write(a32,*)'(pt^2! *E   around    Pi0  esum"G#',(j*500+1000)
c$-	call hbook1(-72430-j,a32,5000, 0.,10.E+06, 0.)
c$C
c$-	write(a32,*)'(dN/dpt)*W   around  Eta  esum"G#',(j*500+1000)
c$-	call hbook1(-73460-j,a32,5000, 0.,5000., 0.)
c$-	write(a32,*)'(dN/dXf)*W   around  Eta  esum"G#',(j*500+1000)
c$-	call hbook1(-73470-j,a32, 500,-2.,   2., 0.)
c$-	write(a32,*)'(dN/dXplus)*W  aroundEta  esum"G#',(j*500+1000)
c$-	call hbook1(-73480-j,a32, 500,-2.,   2., 0.)
c$-C
c$-	write(a32,*)'(dN/dXpl)*Xpl  aroundEta  esum"G#',(j*500+1000)
c$-	call hbook1(-74480-j,a32, 500,-2.,   2., 0.)
c$-	write(a32,*)'(dN/dT)*E  around     Eta  esum"G#',(j*500+1000)
c$-	call hbook1(-72410-j,a32,6000,-1.,5., 0.)
c$-	write(a32,*)'(dN/dT)	around     Eta  esum"G#',(j*500+1000)
c$-	call hbook1(-71410-j,a32,6000,-1.,5., 0.)
c$-
c$-	enddo
CEvd-
        call book_fit
        return
        end
 
