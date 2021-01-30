/****************************************************************************************
 *											*
 * file: talk.c										*
 *											*
 * October 20, 1990: an entity's network interface to other entities.		       	*
 *											*
 *		     this library represents the presentation & session layers of the	*
 *		     ISO network systems model.						*
 *											*
 *                   the network and transport layers provided by socket.c		*
 *											*
 * by Geoffrey P. Coco at the HITLab, Seattle.		  				*
 *											*
 ****************************************************************************************/


/****************************************************************************************
 * Copyright (C) 1992  Geoffrey P. Coco, Human Interface Technology Lab, Seattle	*
 ****************************************************************************************/


/****************************************************************************************
				      Preliminaries
 ****************************************************************************************/

#include "kernel.h"
#include "errno.h"
#include "sys/signal.h"

/****************************************************************************************/

TVeosErr Talk_DummyMsgHandler();

/****************************************************************************************/



/****************************************************************************************
 * Talk_HelloTalk									*
 *											*
 * talk initialization.  call Talk_HelloTalk() once, and don't proceed upon failure.	*/

TVeosErr Talk_HelloTalk(iPort, pMessFun)
    int			iPort;
    TVeosErr		(*pMessFun) ();
{
    TVeosErr		iErr;
    

    /** allocate fast message buffers **/
    
    iErr = VEOS_MEM_ERR;
    if (NEWPTR(TALK_BUFFER, char *, TALK_MAX_BUFFER)) {
	

	/** initialize cache settings **/
	
	SPEAK_SET = nil;
	
	LISTEN_SOCKETFD = TALK_BOGUS_FD;
	LISTEN_SET = nil;
	
	FD_ZERO(&OPEN_READ_SOCKETS);
	FD_ZERO(&OPEN_WRITE_SOCKETS);
	
	TALK_MSG_FUNC = pMessFun ? pMessFun : Talk_DummyMsgHandler;
	
	bzero(SOCK_HOSTS, 26 * sizeof(TPHostNode));
	
	
	/** initialize public cache settings **/
	
	SPEAK_DIRTY = FALSE;
	
	
	
	/** initialize listen connection **/
	
	iErr = Talk_OpenPort(iPort);
	}
    
    return(iErr);
    
    } /* Talk_HelloTalk */
/****************************************************************************************/




/****************************************************************************************
 * Talk_ByeTalk										*
 * 											*
 * cleanup network and memory allocation only after finished using talk library.	*/

TVeosErr Talk_ByeTalk()
{
    TPSpeakNode	        pSpeakFinger, pTempSpeak;
    TPListenNode        pListenFinger, pTempListen;

    /** take down listen port **/
    
    Talk_ClosePort();
    
    
    /** close all speak connections **/
    
    pSpeakFinger = SPEAK_SET;
    while (pSpeakFinger) {
	
	pTempSpeak = pSpeakFinger->pLink;
	
	Talk_KillSpeakConnection(pSpeakFinger);
	
	pSpeakFinger = pTempSpeak;
	}
    
    
    /** close listen connections (normal mechanism is while attempting to read) **/
    
    pListenFinger = LISTEN_SET;
    while (pListenFinger) {
	
	pTempListen = pListenFinger->pLink;
	
	Talk_KillListenConnection(pListenFinger);
	
	pListenFinger = pTempListen;
	}

    
    /** deallocate local library cache **/
    
    if (TALK_BUFFER)
	DUMP(TALK_BUFFER);
    
    
    return(VEOS_SUCCESS);
    
    } /* Talk_ByeTalk */
/****************************************************************************************/



/****************************************************************************************/
TVeosErr Talk_SpeakToMany(pDests, pMsg)
    TPUidNode		pDests;
    TPMsgRec		pMsg;
{
    TVeosErr		iErr;
    TPUidNode		pFinger;

    iErr = VEOS_FAILURE;

    if (pDests && pMsg) {

	iErr = VEOS_SUCCESS;

	pFinger = pDests;
	while (pFinger && iErr == VEOS_SUCCESS) {

	    iErr = Talk_PostSpeakMessage(&pFinger->addr, pMsg);

	    pFinger = pFinger->pNext;
	    }
	}

    return(iErr);

    } /* Talk_SpeakToMany */
/****************************************************************************************/



/****************************************************************************************
 *				     private functions					*
 ****************************************************************************************/


/****************************************************************************************
 * Talk_OpenPort									*
 *											*
 * establish an incoming message gateway for the entity client.				*
 * Talk_OpenPort() should be paired with a call to Talk_ClosePort().			*/

TVeosErr Talk_OpenPort(iPort)
    int			iPort;
{
    TVeosErr		iErr;
    boolean		bFound;

    
    /** install entity connection handler **/
/*
    signal(SIGIO, Talk_ConnectTrap);
*/
    
    
    /** initiate network comminication. alert world of our existence. **/

    if (TALK_BUGS)
	fprintf(stderr, "listen %s: trying to listen...\n", WHOAMI);


    if (iPort != TALK_BOGUS_PORT) {
	
	/** use chosen port number **/
	
	iErr = Sock_Listen(&LISTEN_SOCKETFD,
				     iPort,
				     "tcp",
				     TALK_AGRESSIVE);
	}
    else {
	/** try all sockets until we find an unused one **/
	
	iPort = TALK_MIN_PORT;
	iErr = VEOS_FAILURE;
	
	while (iErr != VEOS_SUCCESS &&
	       iPort < TALK_MAX_PORT) {
	    
	    iErr = Sock_Listen(&LISTEN_SOCKETFD,
					 iPort,
					 "tcp",
					 TALK_PASSIVE);
	    if (iErr != VEOS_SUCCESS)
		iPort ++;
	    }
	}
    
    
    if (iErr != VEOS_SUCCESS) {
	if (TALK_BUGS)
	    fprintf(stderr, "listen %s: cannot listen, talk: %d, sys: %d\n",
		    WHOAMI,  iErr, errno);
	}

    else {
	/** maintain references to entity uid **/
	
	IDENT_ADDR.iPort = iPort;
	Shell_UpdateUid();

	if (TALK_BUGS)
	    fprintf(stderr, "listen %s: successful listen\n", WHOAMI);


	/** setup shared memory channel **/

	iErr = ShMem_Init();
	}

    return(iErr);
    
    } /* Talk_OpenPort */
/****************************************************************************************/



/****************************************************************************************
 * Talk_ClosePort									*
 *	       										*
 * close the incoming message gateway.  the cleanup counterpart to Talk_OpenTalk().	*/

TVeosErr Talk_ClosePort()
{
    TVeosErr		iErr;
    

    /** unregister ourselves from net **/
    
    if (TALK_BUGS)
	fprintf(stderr, "listen %s: about to close main listen socket: %d, sys: %d\n",
		WHOAMI, LISTEN_SOCKETFD, errno);
    

    iErr = Sock_Close(&LISTEN_SOCKETFD);
    
    if (TALK_BUGS)
	fprintf(stderr, "listen %s: closed main listen port, talk: %d, sys: %d\n",
		WHOAMI, iErr, errno);


    /** takedown shared memory channel **/
    
    ShMem_Close();


    return(iErr);
    
    } /* Talk_ClosePort */
/****************************************************************************************/




/****************************************************************************************/
TVeosErr Talk_GatherListenMessages()
{
    TVeosErr   		iErr;
    int			iSocketFD;
    THListenNode	hListenFinger;
    TPListenNode        pSaveLink;
    
    
    hListenFinger = &LISTEN_SET;
    
    while (*hListenFinger) {

	pSaveLink = *hListenFinger;
	iSocketFD = (*hListenFinger)->iSocketFD;
	

	/** attempt to read messages from the connection **/
	
	iErr = Talk_RetrieveMessages(iSocketFD);
	if (iErr != TALK_CONN_CLOSED)
	    
	    hListenFinger = &(*hListenFinger)->pLink;	    
	
	
	else {
	    /** connection was lost from other end **/

	    if (TALK_BUGS)
		fprintf(stderr, "listen %s: lost connection on socket: %d\n",
			WHOAMI, iSocketFD);

	    pSaveLink = (*hListenFinger)->pLink;
	    
	    Talk_KillListenConnection(*hListenFinger);
	    
	    *hListenFinger = pSaveLink;
	    }
	}


    /** all shared memory messages come in through one channel **/
    
    ShMem_GatherMessages();


    return(iErr);
    
    } /* Talk_GatherListenMessages */
/****************************************************************************************/




/****************************************************************************************
 * Talk_EstNewListenConnections								*/

TVeosErr Talk_EstNewListenConnections()
{
    TVeosErr		iErr = VEOS_SUCCESS;
    TPListenNode	pNewNode;
    
    iErr = Talk_NewListenConnection(LISTEN_SOCKETFD, &pNewNode);
    if (iErr == VEOS_SUCCESS) {
	
	/** store this record locally **/
	
	pNewNode->pLink = LISTEN_SET;
	LISTEN_SET = pNewNode;
	}

    return(VEOS_SUCCESS);
    
    } /* Talk_EstNewListenConnections */
/****************************************************************************************/



/****************************************************************************************/
TVeosErr Talk_DispatchQedSpeakMessages()
{
    TVeosErr		iErr;
    THSpeakNode		hSpeakFinger;
    TPSpeakNode		pSaveLink;
    boolean		bLocalDirty;
    
    iErr = VEOS_SUCCESS;
    

    /** inspect every open connection for queued outgoing messages **/
    
    hSpeakFinger = &SPEAK_SET;
    SPEAK_DIRTY = FALSE;
    
    while (*hSpeakFinger) {
	
	
	/** attempt to send queued messages for this connection in order **/
	
	iErr = Talk_ThrowMessages(*hSpeakFinger, &bLocalDirty);
	if (iErr == TALK_CONN_CLOSED) {
	    
	    
	    /** connection was lost from other end **/

	    if (TALK_BUGS)
		fprintf(stderr, "speak %s: lost connection to (%d %d) on socket: %d\n",
			WHOAMI,
			(*hSpeakFinger)->destRec.lHost,
			(*hSpeakFinger)->destRec.iPort,
			(*hSpeakFinger)->iSocketFD);
	    
	    pSaveLink = (*hSpeakFinger)->pLink;
	    
	    Talk_KillSpeakConnection(*hSpeakFinger);
	    
	    *hSpeakFinger = pSaveLink;
	    }
	
	else {
	    if (bLocalDirty)
		SPEAK_DIRTY = TRUE;
	    
	    
	    /** check next connection **/
	    
	    hSpeakFinger = &(*hSpeakFinger)->pLink;	    
	    }
	}

    return(iErr);
    
    } /* Talk_DispatchQedSpeakMessages */
/****************************************************************************************/




/****************************************************************************************
 * Talk_NewListenConnection								*/

TVeosErr Talk_NewListenConnection(iListenSocketFD, hListenNode)
    int			iListenSocketFD;
    THListenNode	hListenNode;
{
    TPListenNode	pNewNode;
    TVeosErr		iErr;
    int			iSocketFD;
    
    
    iErr = Sock_ReadSelect(iListenSocketFD);
    if (iErr == VEOS_SUCCESS) {	

	if (TALK_BUGS)
	    fprintf(stderr, "listen %s: another entity is trying to connect...\n",
		    WHOAMI);
	
	/** establish a full-duplex connection **/
	
	iErr = Sock_Accept(iListenSocketFD, &iSocketFD);
	
	if (iErr != VEOS_SUCCESS)
	    perror("talk: _Accept");
	
	else {

	    if (TALK_BUGS)
		fprintf(stderr, "listen %s: another entity connected on socket: %d\n",
			WHOAMI, iSocketFD);

	    /** allocate listen connection record **/
	    
	    iErr = Shell_NewBlock(sizeof(TListenNode), &pNewNode, "listen node");
	    if (iErr != VEOS_SUCCESS) {

		Sock_Close(&iSocketFD);
		}
	    
	    else {
		/** setup connection record **/
		
		pNewNode->iSocketFD = iSocketFD;
		pNewNode->pLink = nil;
		
		*hListenNode = pNewNode;
		}
	    }
	}
    
    return(iErr);
    
    } /* Talk_NewListenConnection */
/****************************************************************************************/




/****************************************************************************************/
TVeosErr Talk_KillListenConnection(pDeadNode)
    TPListenNode	pDeadNode;
{
    TVeosErr		iErr;
    
    iErr = VEOS_FAILURE;
    
    if (pDeadNode) {			/* sane? */
	
	if (TALK_BUGS) 
	    fprintf(stderr, "listen %s: closing connection on socket: %d\n",
		    WHOAMI, pDeadNode->iSocketFD);
	
	/** strike this connection in all respects **/
	
        iErr = Sock_Close(&pDeadNode->iSocketFD);


	if (TALK_BUGS)
	    fprintf(stderr, "listen %s: closed listen connection, talk: %d, sys: %d\n",
		    WHOAMI, iErr, errno);


	Shell_ReturnBlock(pDeadNode, sizeof(TListenNode), "listen node");
	
	iErr = VEOS_SUCCESS;
	}
    
    return(iErr);
    
    } /* Talk_KillListenConnection */
/****************************************************************************************/



/****************************************************************************************/
TVeosErr Talk_PostSpeakMessage(pDest, pMsgBlock)
    TPUid		pDest;
    TPMsgRec		pMsgBlock;
{
    TVeosErr		iErr;
    TPSpeakNode		pDestNode, pSaveLink;
    THSpeakNode		hFinger;
    boolean		bLocalDirty;

    if (pDest) {


	/** do we already have a connection to this destination? **/
	
	hFinger = &SPEAK_SET;
	iErr = VEOS_FAILURE;
	
	while (*hFinger && iErr != VEOS_SUCCESS) {
	    
	    if (UID_COMPARE(&(*hFinger)->destRec, pDest)) {
		
		pDestNode = *hFinger;
		iErr = VEOS_SUCCESS;
		}
	    else
		hFinger = &(*hFinger)->pLink;		
	    }
	
	
	/** attempt to connect to this destaination for the first time **/
	
	if (iErr != VEOS_SUCCESS) {
	    iErr = Talk_NewSpeakConnection(pDest,
					       &pDestNode);
	    if (iErr == VEOS_SUCCESS) {
		
		/** add connection record to local list **/
		
		pDestNode->pLink = SPEAK_SET;
		SPEAK_SET = pDestNode;
		hFinger = &SPEAK_SET;
		}
	    }
	
	
	/** add new message to this connection's queue **/
	
	if (iErr == VEOS_SUCCESS) {
	    iErr = Talk_AddSpeakJob(pDestNode, pMsgBlock); 
	    if (iErr == VEOS_SUCCESS) {
		
		
		/** attempt to send this message right now **/
		
		iErr = Talk_ThrowMessages(pDestNode, &bLocalDirty);
		if (iErr == TALK_CONN_CLOSED) {
		    
		    
		    /** connection was lost from other end **/

		    if (TALK_BUGS)
			fprintf(stderr, "speak %s: lost connection to (%d %d) on socket: %d\n",
				WHOAMI,
				pDestNode->destRec.lHost,
				pDestNode->destRec.iPort,
				pDestNode->iSocketFD);

		    
		    pSaveLink = pDestNode->pLink;
		    
		    Talk_KillSpeakConnection(pDestNode);
		    
		    *hFinger = pSaveLink;
		    }

		else if (bLocalDirty)
		    SPEAK_DIRTY = TRUE;
		}
	    }
	}

    return(iErr);
    
    } /* Talk_PostSpeakMessage */
/****************************************************************************************/



/****************************************************************************************
 * Talk_NewSpeakConnection								*/

TVeosErr Talk_NewSpeakConnection(pDest, hSpeakNode)
    TPUid		pDest;
    THSpeakNode		hSpeakNode;
{
    TPSpeakNode		pNewNode;
    int			iSocketFD;
    TPSharedRec		pChannel;
    TVeosErr		iErr;
    boolean		bSharedMem;

    *hSpeakNode = nil;
    
    
    /** try to connect to destination shell **/

    if (TALK_BUGS)	
	fprintf(stderr, "speak %s: trying to connect to entity: (%d %d)\n",
		WHOAMI, pDest->lHost, pDest->iPort);


    /** special case communications - shared memory **/

    bSharedMem = ShMem_CanShareMem(pDest);

    if (bSharedMem) {
	iErr = ShMem_FindChannel(pDest->iPort, &pChannel);
	}
    else {
	iErr = Sock_Connect(&iSocketFD,
			    pDest,
			    TALK_DEFAULT_PROTOCOL);
	}
    

    if (iErr != VEOS_SUCCESS)  {

	if (TALK_BUGS)
	    fprintf(stderr, "speak %s: cannot connect, talk: %d, sys: %d\n",
		    WHOAMI, iErr, errno);

	iErr = VEOS_FAILURE;
	}
    
    else {

	if (TALK_BUGS) 
	    fprintf(stderr, "speak %s: connected to entity: (%d %d) on socket: %d\n",
		    WHOAMI, pDest->lHost, pDest->iPort, iSocketFD);
	
	
	/** allocate a new connection record **/
	
	iErr = Shell_NewBlock(sizeof(TSpeakNode), &pNewNode, "speak node");
	if (iErr != VEOS_SUCCESS) {
	    
	    if (!bSharedMem)
		Sock_Close(&iSocketFD);
	    }

	else {
	    /** setup connection record **/
	    
	    bcopy(pDest, &pNewNode->destRec, sizeof(TUid));
	    
	    if (bSharedMem) {
		pNewNode->iConnType = TALK_SHMEM_CONN;
		}
	    else {
		pNewNode->iSocketFD = iSocketFD;
		pNewNode->iConnType = TALK_SOCK_CONN;
		}

	    pNewNode->pMessageQ = nil;
	    
	    
	    
	    /** return the address of record **/
	    
	    *hSpeakNode = pNewNode;
	    }
	}
    
    return(iErr);
    
    } /* Talk_NewSpeakConnection */
/****************************************************************************************/




/****************************************************************************************/
TVeosErr Talk_KillSpeakConnection(pDeadNode)
    TPSpeakNode		pDeadNode;
{
    TVeosErr		iErr;
    TPMessageNode	pMessageFinger, pTempLink;

    iErr = VEOS_FAILURE;
    
    if (pDeadNode) {			/* sane? */
	
	if (pDeadNode->iConnType == TALK_SOCK_CONN) {

	    if (TALK_BUGS)
		fprintf(stderr, "speak %s: closing connection on socket: %d\n",
			WHOAMI, pDeadNode->iSocketFD);

	    
	    /** close the speak connection **/
	    
	    iErr = Sock_Close(&pDeadNode->iSocketFD);
	    }


	if (TALK_BUGS)
	    fprintf(stderr, "speak %s: closed speak connection, talk: %d, sys: %d\n",
		    WHOAMI, iErr, errno);
	
	
	/** deallocate queued messages nodes **/
	
	pMessageFinger = pDeadNode->pMessageQ;
	while (pMessageFinger) {
	    
	    DUMP(pMessageFinger->sMessage);
	    
	    pTempLink = pMessageFinger->pLink;
	    Shell_ReturnBlock(pMessageFinger, sizeof(TMessageNode), "message node");
	    
	    pMessageFinger = pTempLink;
	    }
	
	
	/** free allocated node **/
	
	Shell_ReturnBlock(pDeadNode, sizeof(TSpeakNode), "speak node");
	
	
	iErr = VEOS_SUCCESS;
	}
    
    return(iErr);
    
    } /* Talk_killSpeakConnection */
/****************************************************************************************/




/****************************************************************************************
 * Talk_RetrieveMessages								*/

TVeosErr Talk_RetrieveMessages(iSocketFD)
    int			iSocketFD;
{
    TVeosErr		iErr;
    int			iBufferSize, iMsgLen, iBytesRead;
    TMsgRec		pbMsg;
    char		*pFinger;

    /** grab each waiting message in buffer **/
    
    do {

	/** read size of next message from FD **/
	
	iBufferSize = sizeof(int);
	
	iErr = Sock_Receive(iSocketFD, &iMsgLen, &iBufferSize);
	if (iErr == VEOS_SUCCESS) {
	    
	    iMsgLen = ntohl(iMsgLen);
#ifndef OPTIMAL
	    if (TALK_BUGS)
		fprintf(stderr, "listen %s: incoming message size: %d\n",
			WHOAMI, iMsgLen);
#endif
	    /** read actual grouple message **/
	    
	    iBytesRead = 0, pFinger = TALK_BUFFER;
	    while (iErr == VEOS_SUCCESS) {

		iBufferSize = iMsgLen - iBytesRead;
		iErr = Sock_Receive(iSocketFD, pFinger, &iBufferSize);
#ifndef OPTIMAL
		if (TALK_BUGS) {
		    fprintf(stderr, "listen %s: receive returned %d bytes, talk: %d, sys: %d\n",
			    WHOAMI, iBufferSize, iErr, errno);
		    }
#endif
		if (iErr == VEOS_SUCCESS) {

		    iBytesRead += iBufferSize;
		    pFinger += iBufferSize;

		    if (iBytesRead == iMsgLen) {
			pbMsg.iLen = iBytesRead;
			pbMsg.sMessage = TALK_BUFFER;
			
			(*TALK_MSG_FUNC) (&pbMsg);
			break;
			}
		    }

		/** spin and wait for remaining data.
		 ** there is a minimal possibility of deadlock
		 ** introduced here.
		 ** situations that may cause spinning to deadlock:
		 ** 1. sending an oversized message to yourself.
		 ** 2. sending an oversized message to a process
		 **    that is simultaneously sending an oversized
		 **    message to you.
		 ** Assumption: oversized messages are infrequent.
		 **/
		else if (iBytesRead > 0 && iErr == TALK_LISTEN_BLOCKED) {
#ifndef OPTIMAL
		    if (TALK_BUGS)
			fprintf(stderr, "listen %s: spinning on partial message..\n", WHOAMI);
#endif
		    iErr = VEOS_SUCCESS;
		    }
		}
	    }

	} while (iErr == VEOS_SUCCESS);
    
    return(iErr);
    
    } /* Talk_RetrieveMessages */
/****************************************************************************************/



/****************************************************************************************
 * Talk_ThrowMessages									*/

TVeosErr Talk_ThrowMessages(pSpeakNode, pDirty)
    TPSpeakNode		pSpeakNode;
    boolean		*pDirty;
{
    TVeosErr		iErr = VEOS_FAILURE;
    
    *pDirty = FALSE;

    if (pSpeakNode->pMessageQ) {		/* any messages? */
	
	if (pSpeakNode->iConnType == TALK_SOCK_CONN)
	    iErr = Talk_SendMsgsOverNet(pSpeakNode);

	else if (pSpeakNode->iConnType == TALK_SHMEM_CONN)
	    iErr = ShMem_WriteMessages(pSpeakNode);


	/** mark this connection for pending messages **/

	if (pSpeakNode->pMessageQ)
	    *pDirty = TRUE;
	}
    
    return(iErr);
    
    } /* Talk_ThrowMessages */
/****************************************************************************************/



/****************************************************************************************/
TVeosErr Talk_SendMsgsOverNet(pSpeakNode)
    TPSpeakNode		pSpeakNode;
{
    int			iLen, iMsgLen, iBytesWritten;
    TPMessageNode	pSaveLink;
    char		*pFinger;
    int			iSocketFD;
    TVeosErr		iErr = VEOS_SUCCESS;
    
    iSocketFD = pSpeakNode->iSocketFD;
    
    /** dispatch message sending...
     ** oldest jobs first to enforce sequencing
     **/	    
    
    do {
	/** attempt to transmit oldest message **/
	
	pFinger = pSpeakNode->pMessageQ->sMessage;
	iMsgLen = pSpeakNode->pMessageQ->iMsgLen;
	iBytesWritten = 0;

	while (iErr == VEOS_SUCCESS) {

	     
	    /** transmit the message **/

	    iLen = iMsgLen - iBytesWritten;
	    iErr = Sock_Transmit(iSocketFD, pFinger, &iLen);
#ifndef OPTIMAL	    
		if (TALK_BUGS) {
		    fprintf(stderr, "speak %s: transmit sent %d bytes, talk: %d, sys: %d\n",
			    WHOAMI, iLen, iErr, errno);
		    }
#endif	    

	    if (iErr == VEOS_SUCCESS) {

		iBytesWritten += iLen;
		pFinger += iLen;

		if (iBytesWritten == iMsgLen) {

		    /** dequeue this message from connection record **/
		    
		    DUMP(pSpeakNode->pMessageQ->sMessage);
		    
		    pSaveLink = pSpeakNode->pMessageQ->pLink;
		    Shell_ReturnBlock(pSpeakNode->pMessageQ,
				      sizeof(TMessageNode), "message node");
		    pSpeakNode->pMessageQ = pSaveLink;

		    break;
		    }
		}

	    /** spin and wait for line to free.
	     ** there is a minimal possibility of deadlock
	     ** introduced here.
	     ** situations that may cause spinning to deadlock:
	     ** 1. sending an oversized message to yourself.
	     ** 2. sending an oversized message to a process
	     **    that is simultaneously sending an oversized
	     **    message to you.
	     ** Assumption: oversized messages are infrequent.
	     **/
	    else if (iBytesWritten > 0 && iErr == TALK_SPEAK_BLOCKED) {
#ifndef OPTIMAL
		if (TALK_BUGS)
		    fprintf(stderr, "speak %s: spinning on partial message..\n", WHOAMI);
#endif
		iErr = VEOS_SUCCESS;
		}

	    } /* while more to write */
	

	} while (pSpeakNode->pMessageQ);

    return(iErr);

    } /* Talk_SendMsgsOverNet */
/****************************************************************************************/



/****************************************************************************************
 * Talk_AddSpeakJob									*/

TVeosErr Talk_AddSpeakJob(pDestNode, pMsgBlock)
    TPSpeakNode		pDestNode;
    TPMsgRec		pMsgBlock;
{
    TVeosErr		iErr;
    TPMessageNode	pNewJob;
    THMessageNode	hFinger;
    
    /** allocate new outgoing message job record **/
    
    iErr = Shell_NewBlock(sizeof(TMessageNode), &pNewJob, "message node");
    if (iErr == VEOS_SUCCESS) {
	

	/** setup job record **/

	if (NEWPTR(pNewJob->sMessage, char *, pMsgBlock->iLen + 4)) {
	    
	    /** insert length code at front of message **/
	    *(long *) pNewJob->sMessage = htonl(pMsgBlock->iLen);
	    
	    bcopy(pMsgBlock->sMessage, pNewJob->sMessage + 4, pMsgBlock->iLen);
	    pNewJob->iMsgLen = pMsgBlock->iLen + 4;
	    pNewJob->pLink = nil;
	
	    /** append new job to destination's message queue **/
	    
	    hFinger = &pDestNode->pMessageQ;
	    while (*hFinger)
		hFinger = &(*hFinger)->pLink;
	    
	    *hFinger = pNewJob;
	    
	    iErr = VEOS_SUCCESS;
	    }
	}

    
    if (iErr != VEOS_SUCCESS) {
	
	/** cleanup any mess **/
	
	if (pNewJob) {
	    if (pNewJob->sMessage)
		DUMP(pNewJob->sMessage);
	    Shell_ReturnBlock(pNewJob, sizeof(TMessageNode), "message node");
	    }
	}
    
    return(iErr);
    
    } /* Talk_AddSpeakJob */
/****************************************************************************************/



/****************************************************************************************/
TVeosErr Talk_DummyMsgHandler(pMsgRec)
    TPMsgRec		pMsgRec;
{
    char		*pBuf;
    
    if (TALK_BUGS) {
	fprintf(stderr, "listen %s: message not converted.\n", WHOAMI);
	}	

    return(VEOS_SUCCESS);

    } /* Talk_DummyMsgHandler */
/****************************************************************************************/


#ifdef banana
int Talk_ConnectTrap();

/****************************************************************************************/
int Talk_ConnectTrap(iSignal, iCode, context, pAddr)
    int			iSignal, iCode;
    struct sigcontext	*context;
    char		*pAddr;
{
    if (iSignal == SIGIO)
	NEW_CONN = TRUE;

    return(0);
    }
/****************************************************************************************/
#endif








