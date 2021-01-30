/****************************************************************************************
 * file: kernel.h									*
 *											*
 * May 18, 1991:  the kernel's private defines, and structures.				*
 * 	       										*
 * by Geoffrey P. Coco at the HITLab, Seattle.		  				*
 *											*
 ****************************************************************************************/


/****************************************************************************************
 * Copyright (C) 1992  Geoffrey P. Coco, Human Interface Technology Lab, Seattle	*
 ****************************************************************************************/


#include "world.h"

#ifdef _SG_
#include <ulocks.h>
#endif

#ifdef MAIN_MODULE
#define EXTERN
#else
#define EXTERN extern
#endif

/****************************************************************************************
				       Talk Module
 ****************************************************************************************/


#define TALK_CREATE		-11		/* socket() error */
#define TALK_HOST		-12		/* gethostbyname() error */
#define TALK_PROTOCOL		-13		/* getprotobyname() error */
#define TALK_SERVICE		-14		/* getservbyname() error */

#define TALK_CONNECT		-15		/* connect() error */
#define TALK_BIND		-16		/* bind(): bind to an address */
#define TALK_NAME		-17		/* getsockname(): can't name socket */
#define TALK_LISTEN		-18		/* listen() error */
#define TALK_FLAGS		-23		/* ioctl error **/
#define TALK_SELECT		-19		/* select() error */
#define TALK_SELECT_TIMEOUT	-20		/* select() time out; nonfatal error */
#define TALK_NOCONN		-21		/* FD_ISSET() failed, try again */
#define TALK_ACCEPT		-22		/* accept(): can't accept selection */
#define TALK_CLOSE		-90		/* close() error */
#define TALK_CONN_CLOSED  	-30

#define TALK_SPEAK_BLOCKED	-252
#define TALK_LISTEN_BLOCKED	-253

#define TALK_DEFAULT_PROTOCOL	"tcp"		/* standard for messages */
#define TALK_DEFAULT_HOST	"localhost"	/* standard loop-back */

#define TALK_SELECT_TIME	0		/* quick check for incoming conns */
#define TALK_QUEUE_SIZE		5		/* size of listening queue */
#define TALK_MAX_BUFFER		8192
#define TALK_MAX_SPEAK_FAIL	20

#define TALK_AGRESSIVE  	-25		/* forcefully take listen port */
#define TALK_PASSIVE    	-24		/* leave used ports alone */

#define TALK_MIN_PORT		5500
#define TALK_MAX_PORT		20000

#define TALK_SOCK_CONN		1
#define TALK_SHMEM_CONN		2

#define SHMEM_SHARED_BUF_SIZE	0xFFFF
#define SHMEM_RW_BUF_SIZE	0x0FFF

#define SHMEM_FULL		-272
#define SHMEM_INIT_ERR		-270
#define SHMEM_ARENA_FILE	"/tmp/veos_shmem_arena"

#define UID_COMPARE(uid1, uid2)  ((uid1)->iPort == (uid2)->iPort && \
				  (uid1)->lHost == (uid2)->lHost)

/****************************************************************************************
				    Entity Addressing
 ****************************************************************************************/

typedef struct {
    u_long		lHost;
    int			iPort;
    }   TUid,
        *TPUid,
        **THUid;

typedef struct uidnode {
    TUid		addr;
    struct uidnode 	*pNext;
    } TUidNode,
      *TPUidNode,
      **THUidNode;

/****************************************************************************************
				      Shared Memory
 ****************************************************************************************/

#ifdef _SG_
typedef usptr_t 	*TPShMem;
typedef usema_t 	*TPSemaphor;
#else
typedef char 		*TPShMem;
typedef char	 	*TPSemaphor;
#endif

typedef struct memrec {
    int			iPort;
    TPSemaphor		pSem;
    char		pBuffer[SHMEM_RW_BUF_SIZE];
    char		*pAvail, *pEnd;
    struct memrec	*pNext;

    } TSharedRec,
      *TPSharedRec,
      **THSharedRec;


typedef struct {
    TPSemaphor		pChainSem;
    TPSharedRec		pChannelChain;

    } TShDomainRec,
      *TPShDomainRec,
      **THShDomainRec;

/****************************************************************************************
				  Talk Message Queueing
 ****************************************************************************************/

typedef struct listennode {

    int	        	iSocketFD;
    struct listennode	*pLink;

    }	TListenNode,
    	*TPListenNode,
    	**THListenNode;

typedef struct {
    char		*sMessage;
    int			iLen;

    }	TMsgRec,
        *TPMsgRec,
        **THMsgRec;

typedef struct messageq {

    char		*sMessage;
    int			iMsgLen;
    struct messageq	*pLink;

    }	TMessageNode,
    	*TPMessageNode,
    	**THMessageNode;

typedef	struct speaknode {

    TUid		destRec;
    int			iConnType;
    int			iSocketFD;
    TPMessageNode	pMessageQ;
    struct speaknode	*pLink;

    } 	TSpeakNode,
    	*TPSpeakNode,
    	**THSpeakNode;

/****************************************************************************************
			       Hostname Hash Table Entries
 ****************************************************************************************/

typedef struct hostnode {
    char		*sHostName;
    u_long		lHost;
    struct hostnode	*pNext;

    } THostNode,
      *TPHostNode,
      **THHostNode;

/****************************************************************************************
				      Talk Globals
 ****************************************************************************************/

EXTERN int			talk_iListenFD;
EXTERN char			*talk_pTransBuffer;
EXTERN TUid			talk_selfRec;

EXTERN fd_set			talk_InetSocket_OpenWriteSockets;
EXTERN fd_set			talk_InetSocket_OpenReadSockets;

EXTERN TPHostNode		talk_pHostHash[26];
EXTERN TPSpeakNode		talk_pOutConns;
EXTERN TPListenNode		talk_pInConns;

EXTERN boolean			talk_bTalkDebugging;
EXTERN boolean			talk_bOutDirty;
EXTERN long			talk_lTasks;

EXTERN TPShDomainRec		talk_pPublicDomain;
EXTERN TPShMem			talk_pTheArena;
EXTERN TPSharedRec		talk_pListenChannel;
EXTERN TVeosErr			(*talk_pIncomingMsgFun) ();

/** quick access to talk globals **/

#define SPEAK_SET		talk_pOutConns
#define LISTEN_SOCKETFD		talk_iListenFD
#define LISTEN_SET		talk_pInConns
#define TASKS			talk_lTasks
#define TALK_BUFFER  		talk_pTransBuffer
#define TALK_BUGS  		talk_bTalkDebugging
#define SPEAK_DIRTY  		talk_bOutDirty
#define OPEN_READ_SOCKETS 	talk_InetSocket_OpenReadSockets
#define OPEN_WRITE_SOCKETS 	talk_InetSocket_OpenWriteSockets
#define IDENT_ADDR		talk_selfRec

#define SHMEM_DOMAIN		talk_pPublicDomain
#define SHMEM_ARENA		talk_pTheArena
#define SHMEM_CHANNEL		talk_pListenChannel

#define TALK_MSG_FUNC		talk_pIncomingMsgFun
#define SOCK_HOSTS		talk_pHostHash

/****************************************************************************************
				      Nancy Module
 ****************************************************************************************/

typedef struct {
    int			iLeft, iRight;

    }   TInterval,
        *TPInterval;

typedef struct repnode {
    TPGrouple		pEnviron;
    TInterval		pWipeList[10];
    int			iZones;
    int			iInsertElt;
    struct repnode	*pNext;

    }	TReplaceRec,
        *TPReplaceRec,
        **THReplaceRec;

typedef struct {
    TPGrouple		pPatGr, pSrcGr;
    int			iDestroyFlag, iFreqFlag;
    TPReplaceRec	pReplaceList;
    TPReplaceRec	pTouchList;

    }   TMatchRec,
        *TPMatchRec,
        **THMatchRec;

/****************************************************************************************/

#define VEOS_GROUPLE_BUF_SIZE  		1024
#define NANCY_EltListInc		8
#define NANCY_AllocHashMax		30

/****************************************************************************************/

#define ELTS_TO_ALLOCATE(iElts) \
    ((((iElts - 1) / NANCY_EltListInc) + 1) * NANCY_EltListInc)

#define ELTS_ALLOCATED(elts)		((elts < NANCY_AllocHashMax) ? \
					 ALLOC_ELTS[elts] : ELTS_TO_ALLOCATE(elts))

#define BLOCKS_ALLOCATED(iBlocks, iSegSize) \
    ((((iBlocks - 1) / iSegSize) + 1) * iSegSize)

#define GET_TIME(time)			(time) = ++NANCY_TIME
#define PRINT_TIME(time, stream)    	fprintf(stream, "(ts: %d) ", time)

/****************************************************************************************/


/** nancy global declarations **/

EXTERN FILE			*nancy_pStreamGlob;
EXTERN int	        	nancy_iLineCount;
EXTERN TPGrouple		nancy_pInSpace, nancy_pWorkSpace;
EXTERN TElt			nancy_eltNil;
EXTERN char			*nancy_pAllPurposeBuf;
EXTERN int			nancy_pAllocElts[NANCY_AllocHashMax];
EXTERN int			nancy_pTypeSizes[30];
EXTERN boolean			nancy_bNancyDebugging;
EXTERN TTimeStamp		nancy_lTime;
EXTERN TTimeStamp		nancy_lMinTime;


/** quick access to nancy globals **/

#define WORK_SPACE		nancy_pWorkSpace
#define GR_INSPACE		nancy_pInSpace

#define LINE_COUNT		nancy_iLineCount
#define GR_STREAM		nancy_pStreamGlob
#define NANCY_BUF		nancy_pAllPurposeBuf
#define TYPE_SIZES		nancy_pTypeSizes
#define ALLOC_ELTS		nancy_pAllocElts
#define NANCY_BUGS		nancy_bNancyDebugging
#define NIL_ELT			nancy_eltNil
#define NANCY_TIME		nancy_lTime
#define NANCY_MINTIME		nancy_lMinTime
    
/****************************************************************************************
				      Shell Module
 ****************************************************************************************/

#define SHELL_CHAIN_HASH_MAX	65

/****************************************************************************************/

EXTERN long			shell_lTrapFlags;
EXTERN boolean			shell_bShellDebugging;
EXTERN char			*shell_pChains[SHELL_CHAIN_HASH_MAX];
EXTERN int			shell_pBlocks[SHELL_CHAIN_HASH_MAX];

#ifdef MAIN_MODULE
boolean 			shell_bSignals = TRUE;
boolean				shell_bKernelSetup = FALSE;
#else
extern boolean 			shell_bSignals;
extern boolean 			shell_bKernelSetup;
#endif


/** quick access to shell globals **/

#define TRAP_FLAGS		shell_lTrapFlags
#define MEM_CHAINS		shell_pChains
#define BLOCKS_OUT		shell_pBlocks
#define SIG_ENABLE		shell_bSignals
#define SHELL_BUGS		shell_bShellDebugging
#define KERNEL_INIT		shell_bKernelSetup

/****************************************************************************************/



