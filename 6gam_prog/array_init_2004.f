C
      SUBROUTINE   array_init
C=======================================================================
C=                                                                     =
C=    INITIALISATIONG OF VARIABLES                                     =
C=    CREATED  11 FEB 86    MALJUKOV  DEC86                            =
C=    LAST CORRECTION  02.02.93   NLR                                  =
C=======================================================================
COMMON BLOCKS
c---NT=1 - NTUPLE, NT=0 - HISTOGRAMMS, ICON=0 - NEW, ICON=1 - CONTINUE
        integer nt,icon
      COMMON /NTSTAT/ NT,icon

c-- records of different kinds
      COMMON /NREC  /  NREC(10)
c-- number of iterations
      common /CALIB / NSTEP,NSTMAX,ratio(576)
c-- lost events
      COMMON /LOSSD /  LOSSD(70)
c-- cells filling
      integer ifullb,ifulls
      COMMON /IFULLB/  IFULLB(576),IFULLS(64)
c-- cells filling above given threshold
      integer nfilwb,nfilws
      COMMON /NFILW2/  NFILWB(576),NFILWS(64)
      COMMON /CELFIL/  ICFIL(24,24),ISFIL(8,8)
c-- effective masses
      COMMON /EFMEAN/  EFMN2(24,24),EFMNS(8,8)
      COMMON /NMFIL /  WM2FIL(24,24),WMSFIL(8,8)
      COMMON /W2FIL /  WFIL(24,24,50)
      COMMON /WSFIL /  WSFIL(8,8,50),NITERS(8,8)

      integer addr,ibig(900)
      integer cellnum(900)
      common /address/ addr,cellnum,ibig
      integer nchannel
      common /nchann/ nchannel

      integer hard_addr(24,24),hard_addr_small(8,8)
      integer pedest(24,24),pedest_small(8,8)
      integer sigma(24,24),sigma_small(8,8)
      common /caddress/ hard_addr,hard_addr_small,pedest,pedest_small,
     *                  sigma,sigma_small
C
      nstep = 0
      call vzero(cellnum,900)
      call vzero(ibig,900)
      CALL  VZERO (LOSSD,70)
      CALL  VZERO (NREC,10)
      CALL  VZERO (IFULLB,576)
      CALL  VZERO (IFULLS,64)
      CALL  VZERO (nfilwb,576)
      CALL  VZERO (nfilws,64)
      CALL  VZERO (ICFIL,576)
      CALL  VZERO (ISFIL,64)
      CALL  VZERO (EFMN2,576)
      CALL  VZERO (EFMNS,64)
      CALL  VZERO (WM2FIL,576)
      CALL  VZERO (WMSFIL,64)
      CALL  VZERO (WFIL,28800)
      CALL  VZERO (WSFIL,3200)

      call vzero(pedest,576)
      call vzero(pedest_small,64)
      call vzero(sigma,576)
      call vzero(sigma_small,64)
      call vzero(hard_addr,576)
      call vzero(hard_addr_small,64)
C
      RETURN
      END
