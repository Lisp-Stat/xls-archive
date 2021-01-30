/****************************************************************************************
 *											*
 * file: ShMem.c						                        *
 *											*
 * April 6, 1992: The shared memory handler for the Talk module of VEOS			*
 *											*
 * by Geoffrey P. Coco at the HITLab, Seattle.		  				*
 *											*
 ****************************************************************************************/

/****************************************************************************************
 * Copyright (C) 1992  Geoffrey P. Coco, Human Interface Technology Lab, Seattle	*
 ****************************************************************************************/


/****************************************************************************************
 * 			         include the papa include file				*/

#include "kernel.h"
#include <signal.h>

/****************************************************************************************/



/****************************************************************************************/
TVeosErr ShMem_Init()
{
    TVeosErr    	iErr;
    boolean		bTrap;
    str255		sSave;

    iErr = VEOS_SUCCESS;

#ifdef _SG_
    usconfig(CONF_INITSIZE, SHMEM_SHARED_BUF_SIZE);
    
    iErr = SHMEM_INIT_ERR;

    SHMEM_ARENA = usinit(SHMEM_ARENA_FILE);

    CATCH_TRAP(SIGBUS, bTrap);
    if (bTrap || (SHMEM_ARENA == nil)) {
	strcpy(sSave, "/bin/rm/ -f ");
	strcat(sSave, SHMEM_ARENA_FILE);
	system(sSave);
	SHMEM_ARENA = usinit(SHMEM_ARENA_FILE);
	}

    if (TALK_BUGS)
	fprintf(stderr, "talk %s: attaching to shared memory arena %s\n",
		WHOAMI, SHMEM_ARENA ? "was successful" : "failed");
    
    if (SHMEM_ARENA) {
	
	SHMEM_DOMAIN = usgetinfo(SHMEM_ARENA);
	
	if (TALK_BUGS)
	    fprintf(stderr, "talk %s: veos communication domain %s\n", 
		    WHOAMI, SHMEM_DOMAIN ? "found" : "not found, creating one...");

	if (SHMEM_DOMAIN == nil) {
	    /** first entity on this machine,
	     ** initialize the shmem domain
	     **/
	    
	    chmod(SHMEM_ARENA_FILE, 0777);
	    
	    iErr = VEOS_MEM_ERR;
	    SHMEM_DOMAIN = usmalloc(sizeof(TShDomainRec), SHMEM_ARENA);
	    
	    if (SHMEM_DOMAIN) {
		
		SHMEM_DOMAIN->pChainSem = usnewsema(SHMEM_ARENA, 1);
		SHMEM_DOMAIN->pChannelChain = nil;
		
		usputinfo(SHMEM_ARENA, SHMEM_DOMAIN);
		}
	    }

	
	if (SHMEM_DOMAIN) {
	    
	    if (TALK_BUGS)
		fprintf(stderr, "talk %s: creating memory listen channel...\n", WHOAMI);
	    
	    iErr = VEOS_MEM_ERR;
	    SHMEM_CHANNEL = usmalloc(sizeof(TSharedRec), SHMEM_ARENA);

	    if (SHMEM_CHANNEL) {
		
		SHMEM_CHANNEL->iPort = IDENT_ADDR.iPort;
		SHMEM_CHANNEL->pSem = usnewsema(SHMEM_ARENA, 1);
		SHMEM_CHANNEL->pAvail = &SHMEM_CHANNEL->pBuffer[0];
		SHMEM_CHANNEL->pEnd = &SHMEM_CHANNEL->pBuffer[0] + SHMEM_RW_BUF_SIZE;
		
		
		/** link new entity channel into shared domain record **/
		
		uspsema(SHMEM_DOMAIN->pChainSem);
		
		SHMEM_CHANNEL->pNext = SHMEM_DOMAIN->pChannelChain;
		SHMEM_DOMAIN->pChannelChain = SHMEM_CHANNEL;
		
		usvsema(SHMEM_DOMAIN->pChainSem);
		
		iErr = VEOS_SUCCESS;
		}
	    }
	}
#endif

    return(iErr);

    } /* ShMem_Init */
/****************************************************************************************/



/****************************************************************************************/
TVeosErr ShMem_Close()
{
    TVeosErr   		iErr;
    boolean		bLast = FALSE;
    THSharedRec		hFinger;
    TPSharedRec		pSaveLink;
    TPSemaphor		pSaveSem;

    iErr = VEOS_SUCCESS;

#ifdef _SG_
    /** stop others from looking at the channel chain **/
    uspsema(SHMEM_DOMAIN->pChainSem);

    /** this channel is about to vanish
     ** wait for others to finish looking at this channel
     **/
    pSaveSem = SHMEM_CHANNEL->pSem;
    uspsema(pSaveSem);

    /** find our channel in the domain channel chain, 
     ** remove it, recoupling the links, and free the memory
     **/
    hFinger = &SHMEM_DOMAIN->pChannelChain;
    while (*hFinger) {

	if (*hFinger == SHMEM_CHANNEL) {
	    pSaveLink = (*hFinger)->pNext;
	    usfree(*hFinger, SHMEM_ARENA);
	    *hFinger = pSaveLink;
	    break;
	    }
	hFinger = &(*hFinger)->pNext;
	}

    /** release and remove the channel semaphore **/
    usvsema(pSaveSem);
    usfreesema(pSaveSem, SHMEM_ARENA);

    if (SHMEM_DOMAIN->pChannelChain == nil)
	bLast = TRUE;

    /** allow others to cleanly find no channel **/
    usvsema(SHMEM_DOMAIN->pChainSem);
    
    if (bLast) {
	usfreesema(SHMEM_DOMAIN->pChainSem, SHMEM_ARENA);
	usfree(SHMEM_DOMAIN, SHMEM_ARENA);
	unlink(SHMEM_ARENA_FILE);
	}
#endif

    return(iErr);

    } /* ShMem_Close */
/****************************************************************************************/



/****************************************************************************************/
TVeosErr ShMem_WriteMessages(pSpeakNode)
    TPSpeakNode		pSpeakNode;
{
    TVeosErr		iErr = VEOS_FAILURE;
    int			iLen;
    TPMessageNode	pSaveLink;
    char		*sMessage;
    TPSharedRec		pWriteChannel;


#ifdef _SG_
    iErr = ShMem_FindChannel(pSpeakNode->destRec.iPort, &pWriteChannel);

    if (iErr != VEOS_SUCCESS)
	iErr = TALK_CONN_CLOSED;

    else {

	/** dispatch message sending...	         
	 ** oldest jobs first to enforce sequencing
	 **/	    
	
	do {
	    /** attempt to transmit oldest message **/
	    
	    sMessage = pSpeakNode->pMessageQ->sMessage;
	    iLen = pSpeakNode->pMessageQ->iMsgLen;
	    


	    /** wait for exclusive rights to memory channel **/

	    uspsema(pWriteChannel->pSem);



	    /** check for available space in buffer **/
#ifndef OPTIMAL	    
	    if (TALK_BUGS) {
		fprintf(stderr, "speak %s: buffer has %d bytes avail.\n",
			WHOAMI, pWriteChannel->pEnd - pWriteChannel->pAvail);
		}
#endif
	    if (pWriteChannel->pAvail + iLen > pWriteChannel->pEnd)
		iErr = SHMEM_FULL;

	    else {
		/** write the message **/
		    
		bcopy(sMessage, pWriteChannel->pAvail, iLen);
		pWriteChannel->pAvail += iLen;
#ifndef OPTIMAL
		if (TALK_BUGS)
		    fprintf(stderr, "speak %s: wrote message, length: %d\n",
			    WHOAMI, iLen);
#endif
		}

	    /** give up rights to memory channel **/

	    usvsema(pWriteChannel->pSem);


	    if (iErr == VEOS_SUCCESS) {

		/** dequeue this message from connection record **/
		
		DUMP(sMessage);
		
		
		pSaveLink = pSpeakNode->pMessageQ->pLink;
		Shell_ReturnBlock(pSpeakNode->pMessageQ,
				  sizeof(TMessageNode), "message node");
		pSpeakNode->pMessageQ = pSaveLink;
		}

	    } while (pSpeakNode->pMessageQ && iErr == VEOS_SUCCESS);
	}

#endif

    return(iErr);

    } /* ShMem_WriteMessages */
/****************************************************************************************/




/****************************************************************************************/
TVeosErr ShMem_GatherMessages()
{
    TVeosErr		iErr = VEOS_SUCCESS;
    char		*pFinger;
    TMsgRec		pbMsg;

#ifdef _SG_
    uspsema(SHMEM_CHANNEL->pSem);
    
    /** check for any data in buffer **/
    if (SHMEM_CHANNEL->pAvail > SHMEM_CHANNEL->pBuffer) {
	
	pFinger = SHMEM_CHANNEL->pBuffer;
	while (pFinger < SHMEM_CHANNEL->pAvail) {

	    pbMsg.iLen = ((int *) pFinger)[0];
	    pFinger += 4;
	    pbMsg.sMessage = pFinger;
	    
	    (*TALK_MSG_FUNC) (&pbMsg);

	    pFinger += pbMsg.iLen;
	    }
	
	/** mark buffer empty again **/
	SHMEM_CHANNEL->pAvail = SHMEM_CHANNEL->pBuffer;
	}

    usvsema(SHMEM_CHANNEL->pSem);
#endif

    return(iErr);

    } /* ShMem_GatherMessages */
/****************************************************************************************/




/****************************************************************************************/
boolean ShMem_CanShareMem(pUid)
    TPUid	pUid;
{
    boolean	bSharedMem = FALSE;

#ifdef _SG_
    if (pUid->lHost == IDENT_ADDR.lHost &&
	pUid->iPort != IDENT_ADDR.iPort)
	bSharedMem = TRUE;
#endif

    return(bSharedMem);

    } /* ShMem_CanShareMem */
/****************************************************************************************/



/****************************************************************************************/
TVeosErr ShMem_FindChannel(iPort, hChannel)
    int			iPort;
    THSharedRec		hChannel;
{
    TVeosErr		iErr = VEOS_FAILURE;
    TPSharedRec		pFinger;

    *hChannel = nil;

#ifdef _SG_
    /** find channel for this destination **/
    
    uspsema(SHMEM_DOMAIN->pChainSem);

    pFinger = SHMEM_DOMAIN->pChannelChain;

    while (pFinger) {
	if (pFinger->iPort != iPort)
	    pFinger = pFinger->pNext;
	else {
	    *hChannel = pFinger;
	    iErr = VEOS_SUCCESS;
	    break;
	    }
	}

    usvsema(SHMEM_DOMAIN->pChainSem);
#endif

    return(iErr);
    }
/****************************************************************************************/
