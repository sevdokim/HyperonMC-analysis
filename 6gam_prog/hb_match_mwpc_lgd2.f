C     *****************************
      SUBROUTINE hb_match_mwpc_lgd2
C     *****************************
C    *******************************************************************
C    * Books the histogramms for MWPC 1G, 2G, 3XY                      *
C    * called by event_decode_2005                                     *
C    * 3XY - 2 planes XY                                               *
C    * 1G  - 3 planes XYU                                              *
C    * 2G  - 3 planes XYV                                              *
C    *                                                                 *
C    * Author : D.I. Patalakha                                         *
C    * Created on :                9 March 2011                        *
C    * Last modification made on : 9 March 2011                        *
C    *******************************************************************
C-------- 1G  chambers :
C......................fired wire position
      call HBOOK1(-100,'1U wire;Position;Events;',64,0.,64.,0.)
      call HBOOK1(-200,'1V wire;Position;Events;',64,0.,64.,0.)
      call HBOOK1(-300,'1X wire;Position;Events;',64,0.,64.,0.)
C......................cluster center position of fired wires
      call HBOOK1(-101,'1U cluster center;Position;Events;',
     *            64,0.,64.,0.)
      call HBOOK1(-201,'1V cluster center;Position;Events;',
     *            64,0.,64.,0.)
      call HBOOK1(-301,'1X cluster center;Position;Events;',
     *            64,0.,64.,0.)
C......................cluster width of fired wires
      call HBOOK1(-102,'1U cluster width;Size;Events;',16,-0.5,15.5,0.)
      call HBOOK1(-202,'1V cluster width;Size;Events;',16,-0.5,15.5,0.)
      call HBOOK1(-302,'1X cluster width;Size;Events;',16,-0.5,15.5,0.)
C......................number of clusters
      call HBOOK1(-103,'1U Number of clusters;Clusters;Events;',
     *            32,-0.5,31.5,0.)
      call HBOOK1(-203,'1V Number of clusters;Clusters;Events;',
     *            32,-0.5,31.5,0.)
      call HBOOK1(-303,'1X Number of clusters;Clusters;Events;',
     *            32,-0.5,31.5,0.)
C......................tracks
      call HBOOK1(-1501,'1G chamber: X-X(u,v);[D]X(mm)mm;Events;', 
     *            200, -100., 100., 0.)
      call HBOOK2(-1601,'1G chamber: Y vs X;X(mm);Y(mm);', 
     *            50, -100., 100., 50, -100., 100., 0.)
      call HBOOK1(-1801,
     *            '1G chamber: [D]X(Track-Hit);[D]X(mm);Events;', 
     *            100, -50., 50., 0.)
      call HBOOK1(-1802,
     *            '1G chamber: [D]Y(Track-Hit);[D]Y(mm);Events;', 
     *            100, -50., 50., 0.)
C-------- 2G  chambers :
C......................fired wire position
      call HBOOK1(-400,'1U wire;Position;Events;',64,0.,64.,0.)
      call HBOOK1(-500,'1V wire;Position;Events;',64,0.,64.,0.)
      call HBOOK1(-600,'1Y wire;Position;Events;',64,0.,64.,0.)
C......................cluster center position of fired wires
      call HBOOK1(-401,'1U cluster center;Position;Events;',
     *            64,0.,64.,0.)
      call HBOOK1(-501,'1V cluster center;Position;Events;',
     *            64,0.,64.,0.)
      call HBOOK1(-601,'1Y cluster center;Position;Events;',
     *            64,0.,64.,0.)
C......................cluster width of fired wires
      call HBOOK1(-402,'1U cluster width;Size;Events;',16,-0.5,15.5,0.)
      call HBOOK1(-502,'1V cluster width;Size;Events;',16,-0.5,15.5,0.)
      call HBOOK1(-602,'1Y cluster width;Size;Events;',16,-0.5,15.5,0.)
C......................number of clusters
      call HBOOK1(-403,'2U Number of clusters;Clusters;Events;',
     *            32,-0.5,31.5,0.)
      call HBOOK1(-503,'2V Number of clusters;Clusters;Events;',
     *            32,-0.5,31.5,0.)
      call HBOOK1(-603,'2Y Number of clusters;Clusters;Events;',
     *            32,-0.5,31.5,0.)
C......................tracks
      call HBOOK1(-1502,'2G chamber: Y-Y(u,v);[D]Y(mm);Events;', 
     *            200, -100., 100., 0.)
      call HBOOK2(-1602,'2G chamber: Y vs X;X(mm);Y(mm);', 
     *            50, -100., 100., 50, -100., 100., 0.)
      call HBOOK1(-1803,
     *            '2G chamber: [D]X(Track-Hit);[D]X(mm);Events;', 
     *            100, -50., 50., 0.)
      call HBOOK1(-1804,
     *            '2G chamber: [D]Y(Track-Hit);[D]Y(mm);Events;', 
     *            100, -50., 50., 0.)
C-------- 3XY chambers:
C......................fired wire position
      call HBOOK1(-700,'3X wire;Position;Events;',96,0.,96.,0.)
      call HBOOK1(-800,'3Y wire;Position;Events;',96,0.,96.,0.)
C......................cluster center position of fired wires
      call HBOOK1(-701,'3X cluster center;Position;Events;',
     *            96,0.,96.,0.)
      call HBOOK1(-801,'3Y cluster center;Position;Events;',
     *            96,0.,96.,0.)
C......................cluster width of fired wires
      call HBOOK1(-702,'3X cluster width;Size;Events;',16,-0.5,15.5,0.)
      call HBOOK1(-802,'3Y cluster width;Size;Events;',16,-0.5,15.5,0.)
C......................number of clusters
      call HBOOK1(-703,'3X Number of clusters;Clusters;Events;',
     *            32,-0.5,31.5,0.)
      call HBOOK1(-803,'3Y Number of clusters;Clusters;Events;',
     *            32,-0.5,31.5,0.)
C......................tracks
      call HBOOK2(-1603,'3XY chamber: Y vs X;X(mm);Y(mm);', 
     *            50, -100., 100., 50, -100., 100., 0.)
      call HBOOK1(-1805,
     *            '3XY chamber: [D]X(Track-Hit);[D]X(mm);Events;', 
     *            100, -50., 50., 0.)
      call HBOOK1(-1806,
     *            '3XY chamber: [D]Y(Track-Hit);[D]Y(mm);Events;', 
     *            100, -50., 50., 0.)
C......................Planes efficiency
      call HBOOK1(-1000,'Planes efficiency;Chamber ID;Events;',
     *           10,0.,10.,0.)
      call HBOOK2(-1701,'Track coordinates Y vs X at z=0;X(mm);Y(mm);', 
     *            50, -100., 100., 50, -100., 100., 0.)
      call HBOOK2(-1702,'Track slopes Y vs X;X slope;Y slope;', 
     *            50, -100., 100., 50, -100., 100., 0.)
      call HBOOK1(-1703,'Chi2(x) of tracks;Chi2-x;Events;', 
     *            50, 0., 25., 0.)
      call HBOOK1(-1704,'Chi2(y) of tracks;Chi2-y;Events;', 
     *            50, 0., 25., 0.)
C-------- End   MWPC histogramms
C-------- Start LGD2 histogramms
      call HBOOK2(-1850,'Track hit on LGD2;X(mm);Y(mm);', 
     *            100, -200., 200., 100, -200., 200., 0.)
      call HBOOK2(-1851,'Track hit on LGD2: chi2<7;X(mm);Y(mm);', 
     *            100, -200., 200., 100, -200., 200., 0.)
      call HBOOK2(-1852,'Match Track hit on LGD2;X(mm);Y(mm);', 
     *            100, -200., 200., 100, -200., 200., 0.)
C-------- End   LGD2 histogramms
C-------- Start Event Builder histograms
      call HBOOK1(-2000,'10 MHz clock MISS;Clock;Events;', 
     *            300, 0., 60000000., 0.) ! max 2.5 seconds
      call HBOOK1(-2001,'10 MHz clock MISS;Clock;Events;', 
     *            300, 0., 60000000., 0.) ! max 2.5 seconds
      call HBOOK1(-2002,'10 MHz clock CAMAC-MISS;Clock;Events;', 
     *            2000, -10000., 10000., 0.) ! max 2.5 seconds
      call HBOOK1(-2003,'10 MHz trigger CAMAC-MISS;[D]trigger;Events;', 
     *            5, -2.5, 2.5, 0.)
      call HBOOK1(-2004,
     *            '10 MHz clock_previous CAMAC-MISS;Clock;Events;', 
     *            2000, -10000., 10000., 0.) ! max 2.5 seconds
      call HBOOK1(-2005,
     *          '10 MHz trigger_previous CAMAC-MISS;[D]trigger;Events;', 
     *            5, -2.5, 2.5, 0.)
      call HBOOK1(-2006,
     *           '10 MHz clock_min_no_match CAMAC-MISS;Clock;Events;', 
     *            2000, -10000., 10000., 0.) ! max 2.5 seconds
      call HBOOK1(-2007,
     *      '10 MHz trigger_min_no_match CAMAC-MISS;[D]trigger;Events;', 
     *           1501, -750.5, 750.5, 0.)
      call HBOOK2(-2011,
     *'10 MHz [D] clock CAMAC-MISS;[D]clock min;[D]clock min previous;', 
     *            200, -30000., 3000., 
     *            200,  -1000., 9000., 0.) ! max 2.5 seconds
      call HBOOK1(-3000,'10 MHz clock CAMAC;Clock;Events;', 
     *            300, 0., 60000000., 0.) ! max 2.5 seconds
      call HBOOK1(-3001,'10 MHz clock CAMAC;Clock;Events;', 
     *            300, 0., 60000000., 0.) ! max 2.5 seconds
C-------- End   Event Builder histograms
      RETURN
      END
