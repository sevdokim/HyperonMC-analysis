      parameter (N_device_max=2)   ! maximal number of crates
      parameter (Nstation_max=24)  ! maximum stations in one crate)
      parameter (N16_max=96)       ! maximal number of channels in one station
      parameter (iA_hard_max=3000) ! maximal hard chan. address in one crate 
      parameter (ireject_channel_max=30)
      common/reject_common/ireject_channel_dat(0:4*ireject_channel_max)
  
      common/Nrun_common/Nrun,iped_flag,itrigger_selector
c      dimension stancii(2,20)
      integer stancii(2,20)
      common/stancii/stancii
      integer XxY(24,24),XxY2(8,8)
c      dimension XxY(24,24)
      dimension ped(24,24)

      common/matrix/XxY,XxY2

      common/recons_flagi/iflag_calibr
      common/Ccalibr_common/Ccalibr(iA_hard_max+1,N_device_max) ! calibr. coef
      common/igood_calib_common/igood_calibr(iA_hard_max+1,N_device_max)
                                   ! =-1 no calibration, 1 -by fit, 2 -by force

      common/device_common/N_device! number of crates
     *, ID_device(N_device_max)    ! ID=ID_device(i) as funct. i
	                           ! al following are function of ID (not i !)
     *, Ns_device(N_device_max)    ! number of stations in every crate
     *, Nch_device(N_device_max)   ! number of channels in every crate 
     *, N16_device(N_device_max)   ! number of channels in one station
     *, Istation_hard(iA_hard_max+1,N_device_max) ! list of stations
     *, Ichannel_hard(iA_hard_max+1,N_device_max) ! and channels for current run
     *, List_hard_channel(0:iA_hard_max*N_device_max+N_device_max)
c--              
     *,iA_hard_good(iA_hard_max+1,N_device_max)   ! flags of good channels
		   ! If crate identificator  is ID=0,1, ...
		   ! and hard channel addres is  i=0,1, ...
		   !  iA_hard_good(i+1,ID+1) = -1 ! hardware error
		   !  iA_hard_good(i+1,ID+1) =  0 ! channel BAD  
                   !  iA_hard_good(i+1,ID+1) =  1 ! channel GOOD
		   !  iA_hard_good(i+1,ID+1) =  2 ! channel calibrated by force 
				    
     *,ihistogram_created(iA_hard_max+1,N_device_max)				    

      common/Acut_common/A1_amplitude,A2_amplitude ! cuts on every raw amplitude					  
     *          ,A1_amplitude_sum,A2_amplitude_sum
     *          ,A1_amplitude_gev(N_device_max)! min cuts on every signal (GeV) 
     *          ,A2_amplitude_gev              ! max cut  on every signal (GeV) 					  
     *          ,A1_amplitude_sum_gev,A2_amplitude_sum_gev ! cuts on Etot (GeV)

      common/Icut_common/N1_hits_sum,N2_hits_sum  ! cuts on number of hits					       

      common/calulated_Idata_common/Idat_best_channel(2),
     *          iAsum_all_channel,Nhitsum_all_channel

      integer i,j

