      subroutine booking
c======================================================================
c     Histogram or Ntuple booking
c     Author Nadia Russakovich 13.12.03
c     Updated  13.04.04  NLR
c======================================================================

      parameter(npawc=210000000)
      COMMON /PAWC  /  HMEM(npawc)
      common /quest/ iquest(100)
      integer lunin,lunout,lunst
      COMMON /LUN   /  LUNIN,LUNOUT,LUNST
c---NT=1 - NTUPLE, NT=0 - HISTOGRAMMS, ICON=0 - NEW, ICON=1 - CONTINUE
        integer nt,icon
      COMMON /NTSTAT/ NT,icon

      call hlimit(npawc)


      IF(NT.gt.0) THEN
         IF(ICON.EQ.0) THEN
            IQUEST(10) = 65000
cc YK            CALL HROPEN(lunst,'hyperon','calibr.ntup','NQ',4096,IOSTAT)
cc YK            CALL ntup_book
         ELSE
            IQUEST(10) = 65000
cc YK            CALL HROPEN(lunst,'hyperon','calibr.ntup','U',4096,IOSTAT)
cc YK            CALL HRIN(333,999999,IOFSE)
cc YK            CALL HNOENT(333,NEVENT)
         END IF
      ELSE
         IF(ICON.EQ.0) THEN 
            CALL HROPEN(lunst,'hyperon','calibr.hbook','N',1024,IOSTAT)
            call hist_book
         ELSE
            CALL HROPEN(lunst,'hyperon','calibr.hbook','U',1024,IOSTAT)
            CALL HRIN(0,9999,0)
         END IF
      END IF
      
      return
      end
