/****************************************************************************************
 *											*
 * file: shell.c							       		*
 *											*
 * September 7, 1990:  Wow, that's old.							*
 *											*
 * by Geoffrey P. Coco at the HITLab, Seattle.		  				*
 *											*
 ****************************************************************************************/


/****************************************************************************************
 * Copyright (C) 1992  Geoffrey P. Coco, Human Interface Technology Lab, Seattle	*
 ****************************************************************************************/



/****************************************************************************************
 * 			         include the papa include file				*/

#include <signal.h>

#define MAIN_MODULE
#include "kernel.h"

/****************************************************************************************/


/****************************************************************************************
 * Kernel_Init										*/

TVeosErr Kernel_Init(iPort, pMessFun)
    int			iPort;
    TVeosErr		(*pMessFun) ();
{
    TVeosErr		iErr = VEOS_FAILURE;

    if (KERNEL_INIT) {
	fprintf(stderr, "veos kernel already initialized...\n");
	fprintf(stderr, "no action was taken.\n");
	}
    else {
	/** initialize often-used globals with defualt values **/
	
	iErr = Shell_InitCaches();
	if (iErr == VEOS_SUCCESS) {
	    
	    
	    /** perform one-time nancy initialization **/
	    
	    iErr = Nancy_Init();
	    if (iErr == VEOS_SUCCESS) {
		
		
		/** initialize inter-entity communication handler **/
		/** this puts an ear to the network **/
		
		iErr = Talk_HelloTalk(iPort, pMessFun);
		if (iErr == VEOS_SUCCESS) {
		    
		    
		    /** provide for graceful exit **/
		    
		    Shell_SetupErrorTraps();
		    
		    
		    /** print gratuitous credits **/
		    
		    Shell_StartUpMessage();
		    
		    KERNEL_INIT = TRUE;
		    }
		}
	    }
	
	if (iErr != VEOS_SUCCESS)
	    fprintf(stderr, "shell %s: kernel initialization failed, error: %d\n",
		    WHOAMI, iErr);
	}

    return(iErr);

    } /* Kernel_Init */
/****************************************************************************************/



/****************************************************************************************
 * Kernel_Shutdown									*/

TVeosErr Kernel_Shutdown()
{
    TVeosErr		iSuccess = VEOS_FAILURE;

    if (KERNEL_INIT) {
	
	/** cleanup network loose ends **/
	
	iSuccess = Talk_ByeTalk();
	
	
	/** dispose all memory **/
	
	fprintf(stderr, "\n");


	KERNEL_INIT = FALSE;
	}


    return(iSuccess);

    } /* Kernel_Shutdown */
/****************************************************************************************/
	



/****************************************************************************************
 * Kernel_SystemTask									*/

TVeosErr Kernel_SystemTask()
{
    /** inspect every open connection for queued outgoing messages **/
    
    if (SPEAK_DIRTY)
	Talk_DispatchQedSpeakMessages();


    /** accept new entity connections **/
/*
    if (++TASKS % 10 == 0)
*/
    Talk_EstNewListenConnections();
	

    /** check each open listen connection for incoming messages **/
    
    Talk_GatherListenMessages();
    

    return(VEOS_SUCCESS);

    } /* Kernel_SystemTask */
/****************************************************************************************/



/****************************************************************************************/
TVeosErr Shell_NewBlock(iSize, hBlock, sDebug)
    int			iSize;
    char *		*hBlock;
    char		*sDebug;
{
    char *		*pBlock;
    TVeosErr		iSuccess;
    
    if (iSize >= SHELL_CHAIN_HASH_MAX) {
	
	NEWPTR(pBlock, char **, iSize);
#ifndef OPTIMAL	    
	if (SHELL_BUGS)
	    fprintf(stderr, "shell %s: %s >> size: %d, regular\n",
		    WHOAMI, sDebug ? sDebug : "", iSize);
#endif
	}
    
    else {
	if (pBlock = (char **) MEM_CHAINS[iSize]) {
	    
	    BLOCKS_OUT[iSize] ++;
	    
	    /** remove nearest block in chain **/
	    
	    MEM_CHAINS[iSize] = *pBlock;
	    *pBlock = nil;
#ifndef OPTIMAL	    
	    if (SHELL_BUGS)
		fprintf(stderr, "shell %s: %s >> size: %d, recycle, blocks out: %d\n",
			WHOAMI, sDebug ? sDebug : "", iSize, BLOCKS_OUT[iSize]);
#endif
	    }
	
	else {
	    BLOCKS_OUT[iSize] ++;
	    NEWPTR(pBlock, char **, iSize);
#ifndef OPTIMAL	    
	    if (SHELL_BUGS)
		fprintf(stderr, "shell %s: %s >> size: %d, new, blocks out: %d\n",
			WHOAMI, sDebug ? sDebug : "", iSize, BLOCKS_OUT[iSize]);
#endif
	    }
	}	
    
    if (*hBlock = (char *) pBlock)
	iSuccess = VEOS_SUCCESS;
    else
	iSuccess = VEOS_FAILURE;


    return(iSuccess);
    
    } /* Shell_NewBlock */
/****************************************************************************************/




/****************************************************************************************/
TVeosErr Shell_ReturnBlock(pBlock, iSize, sDebug)
    char *		*pBlock;
    int			iSize;
    char		*sDebug;
{
    int			iSuccess;

    iSuccess = VEOS_SUCCESS;

    if (iSize >= SHELL_CHAIN_HASH_MAX) {

	DUMP(pBlock);
#ifndef OPTIMAL
	if (SHELL_BUGS)
	    fprintf(stderr, "shell %s: %s << size: %d, regular\n",
		    WHOAMI, sDebug ? sDebug : "", iSize);
#endif
	}

    else {
	BLOCKS_OUT[iSize] --;
	
	*pBlock = MEM_CHAINS[iSize];
	
	MEM_CHAINS[iSize] = (char *) pBlock;
#ifndef OPTIMAL	
	if (SHELL_BUGS) {
	    fprintf(stderr, "shell %s: %s << size: %d, blocks out: %d\n",
		    WHOAMI, sDebug ? sDebug : "", iSize, BLOCKS_OUT[iSize]);
	    }
#endif
	}    

    return(iSuccess);
    
    } /* Shell_ReturnBlock */
/****************************************************************************************/



/****************************************************************************************
 *					local functions					*
 ****************************************************************************************/



/****************************************************************************************/
TVeosErr Shell_InitCaches()
{
    TVeosErr		iSuccess;
    int			iChain;
    str63		sHostName;

    iSuccess = VEOS_FAILURE;


    /** chain list is as long as largest possible block size **/
    
    for (iChain = 0; iChain < SHELL_CHAIN_HASH_MAX; iChain ++) {
	MEM_CHAINS[iChain] = nil;
	BLOCKS_OUT[iChain] = 0;
	}


    /** publicly accessible globals  **/

    if (gethostname(sHostName, sizeof(sHostName)) != -1)
	iSuccess = Sock_StrHost2IP(sHostName, &IDENT_ADDR.lHost);

    IDENT_ADDR.iPort = TALK_BOGUS_FD;
    
    Shell_UpdateUid();

    
    TERMINATE = FALSE;

    
    /** kernel debugging flags **/
    
    TALK_BUGS = FALSE;
    SHELL_BUGS = FALSE;
    NANCY_BUGS = FALSE;
    
    
    /** other kernel globals **/
    
    TRAP_FLAGS = 0;
    TASKS = 0;

    iSuccess = VEOS_SUCCESS;


    return(iSuccess);

    } /* Shell_InitCaches */
/****************************************************************************************/



/****************************************************************************************/
void Shell_TrapErr(iSignal, iCode, context, pAddr)
    int			iSignal, iCode;
    struct sigcontext	*context;
    char		*pAddr;
{
    str255		sErr;
    str63		sSignal;
    static boolean	bGone = FALSE;

    /** interupt was already trapped once with no service **/

    if (TRAP_FLAGS & 0x00000001 << iSignal) {
	Shell_Signal2String(iSignal, sSignal);
	sprintf(sErr, "fatal signal: %s, entity: %s\n", sSignal, WHOAMI);

	if (bGone)
	    exit(0);

	else {
	    bGone = TRUE;
	    Shell_BailOut(sErr);
	    }
	}
    else {
#ifndef OPTIMAL
	if (SHELL_BUGS) {
	    Shell_Signal2String(iSignal, sSignal);
	    fprintf(stderr, "shell %s: interrupt occurred: %s.\n", WHOAMI, sSignal);
	    }
#endif    
	TRAP_FLAGS |= 0x00000001 << iSignal;
	}

    } /* Shell_TrapErr */
/****************************************************************************************/



/****************************************************************************************/
TVeosErr Shell_Signal2String(iSignal, sSignal)
    int		iSignal;
    char	*sSignal;
{
    switch (iSignal) {

    case SIGBUS:
	strcpy(sSignal, "bus error");
	break;

    case SIGKILL:
	strcpy(sSignal, "forceful kill");
	break;

    case SIGQUIT:
	strcpy(sSignal, "user quit request");
	break;

    case SIGSEGV:
	strcpy(sSignal, "segmentation fault");
	break;

    case SIGINT:
	strcpy(sSignal, "keyboard interrupt");
	break;

    case SIGPIPE:
	strcpy(sSignal, "broken pipe");
	break;

    default:
	strcpy(sSignal, "unknown signal");
	break;
	}

    return(VEOS_SUCCESS);

    } /* Shell_Signal2String */
/****************************************************************************************/



/****************************************************************************************/
TVeosErr Shell_SetupErrorTraps()
{
    if (SIG_ENABLE) {
	signal(SIGINT, Shell_TrapErr);
	signal(SIGQUIT, Shell_TrapErr);
	signal(SIGBUS, Shell_TrapErr); 
	signal(SIGSEGV, Shell_TrapErr); 
	signal(SIGPIPE, Shell_TrapErr);
	}

    } /* Shell_SetupErrorTraps */
/****************************************************************************************/



/****************************************************************************************/
TVeosErr Shell_StartUpMessage()
{
    fprintf(stderr, "\n\n\n");
    fprintf(stderr, "----------------------------------------------------\n");
    fprintf(stderr, "             VEOS 2.0   by Geoffrey Coco            \n");
    fprintf(stderr, " Copyright (C) 1992, Human Interface Technology Lab \n");
    fprintf(stderr, "----------------------------------------------------\n\n\n\n");

    return(VEOS_SUCCESS);

    } /* Shell_StartUpMessage */
/****************************************************************************************/




/****************************************************************************************/
TVeosErr Shell_UpdateUid()
{
    TVeosErr 		iSuccess;
    str63		sHostName;

    /** update WHOAMI C variable **/

    iSuccess = Sock_IP2StrHost(IDENT_ADDR.lHost, sHostName);
    if (iSuccess == VEOS_SUCCESS)

	sprintf(WHOAMI, "%s_%d", sHostName, IDENT_ADDR.iPort);


    return(iSuccess);

    } /* Shell_UpdateUid */
/****************************************************************************************/



#ifdef _DEC_
/****************************************************************************************/
char *strdup(sSrc)
    char	*sSrc;
{
    char	*sReturn;

    sReturn = nil;

    if (sSrc) {
	if (NEWPTR(sReturn, char *, strlen(sSrc) + 1))
	    strcpy(sReturn, sSrc);
	}

    return(sReturn);

    } /* strdup */
/****************************************************************************************/
#endif



