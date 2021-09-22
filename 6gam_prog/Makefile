#------------------------------------------------------------------------------
TOP_DIR := ../

############### PATH to include directories ##################################
INCLUDE_DIRS := ./ 

############## SOURCE ########################################################
source_dirs  := ./
source_files := ampl_energy.f angle.f array_init_2004.f booking.f calibr2.f calibr_fin.f calibr_init_2004.f calibr_main.cxx cal_koef_correc.f cameras_process.f Cherenkovs.f correc2.f cosdir2.f counters_process.f decode_chamber.cxx efmass.f event_decode_2005.f filling2.f fngam.f geometry.f hb_cherenkovs.f hb_match_mwpc_lgd2.f hb_scaler.f hist_book_2004.f hyperon_scalers.cxx idofdetfun.f libapr84.f matr_ent2.f mclus.f new_clb.f ntup_book.f pi0fitbu.f queue_class_function.cxx shower_recon.f string_text.cxx track_scalers_prnt.f tracks.cxx user_cards.f wall2_proc.f wlprnt2.f wrcoef2.f

############# binary ##########################################################
object_files := $(notdir $(source_files) )
object_files := $(object_files:.cxx=.o)
object_files := $(object_files:.f=.o)

#F77LIBS      := -L/usr/local/lib -lcrypt -lnsl
F77LIBS      := -lz 
#CERN_LIB     := `cernlib packlib,mathlib,kernlib`
CERN_LIB     := -Wl,-static -lmathlib -lpacklib -lkernlib -Wl,-dy -L/usr/lib/x86_64-linux-gnu/  -lm -lnsl -lcrypt -ldl -lgfortran
################ Compiler for C++ #############################################
CXX          := g++

OPT_DEB      := -g
DEFINES      := -Dlong32="int" -Dlong64="long long" -DLINUX
INCLUDES     := $(addprefix -I, $(INCLUDE_DIRS))

CXXFLAGS     := $(OPT_DEB) -Wall  -fPIC 
CXXFLAGS     += $(DEFINES) $(INCLUDES)

################# compiler for Fortran ########################################
F77		= gfortran

F77FLAGS     := $(OPT_DEB) -Waliasing -Wampersand -Wsurprising -Wintrinsics-std -Wno-tabs -Wintrinsic-shadow -Wline-truncation -Wreal-q-constant  -cpp -fmax-errors=150 # -Wconversion -Wunused 
F77FLAGS     += -finit-local-zero -fbounds-check -fno-automatic 
F77FLAGS     += $(INCLUDES)

#------------------------------------------------------------------------------

# compilation
VPATH   := $(source_dirs)
%.o: %.cxx
	$(CXX) -c $(CXXFLAGS) $<
%.o: %.f
	$(F77) -c $(F77FLAGS) $<

#------------------------------------------------------------------------------

# main goal, i.e. name of executable

all:	calibr.x8664

# linking
calibr.x8664: $(object_files)
	$(CXX) $(CXXFLAGS)  \
	$^ -o $@ $(F77LIBS) \
	$(CERN_LIB)

clean:
	rm -rf *.o calibr.x8664

#Makes rule clean and then rule all
rebuild: clean all
