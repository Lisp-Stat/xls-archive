# ***************************************************************************************
#  Copyright (C) 1992  Human Interface Technology Lab, Seattle				*
# 											*
#  This program is free software; you can redistribute it and/or modify			*
#  it under the terms of the VEOS License which cab be found in file                    *
#  VEOS_LICENSE in the root of the veos directory tree.     	    	    	    	*
# 											*
#  This program is distributed in the hope that it will be useful,			*
#  but WITHOUT ANY WARRANTY; without even the implied warranty of			*
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the			*
#  VEOS License for more details.   							*
# 											*
#   Questions about this software should be addressed to: 				*
# 											*
#  	Software Support								*
#  	Human Interface Technology Laboratory						*
#   	FJ-15, University of Washington							*
#   	Seattle, Washington 	98195	USA						*
# 											*
#   or by email:									*
# 											*
#   	veos-support@hitl.washington.edu						*
# 											*
# ***************************************************************************************


# use these for Sun4

UPDATE_LIB = granlib
CC = /usr/local/bin/GNU/gcc -O4 -w ${VEOS_INCLUDE_DIRS} -D_SUN_ 
AR = /usr/local/bin/GNU/gar rcv
#CC = cc -g -Bstatic -w ${VEOS_INCLUDE_DIRS} -D_SUN_
#AR = ar rcv
ASSOC_LIBS = -lm

