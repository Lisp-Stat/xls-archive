/****************************************************************************************
 *											*
 * file: socket.c						                        *
 *											*
 * November 14, 1990: The network and transport layer for inter-entity message passing	*
 * 		      library, 'talk' for the VEOS project.                             *
 *											*
 * by Geoffrey P. Coco at the HITLab, Seattle.		  				*
 * these functions are based on BSD socket code by Dan Pezely.		       		*
 *											*
 ****************************************************************************************/

/****************************************************************************************
 * Copyright (C) 1992  Geoffrey P. Coco, Human Interface Technology Lab, Seattle	*
 ****************************************************************************************/



/****************************************************************************************
 * 			         include the papa include file				*/

#include "kernel.h"

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <netdb.h>			/* for get_*_byname() */
#include <stropts.h>			/* ioctl() streamio */
#include <fcntl.h>
#include "signal.h"

/****************************************************************************************/



/****************************************************************************************
 * 			        forward function declarations				*/

TVeosErr Sock_Connect();
TVeosErr Sock_Listen();
TVeosErr Sock_ReadSelect();
TVeosErr Sock_WriteSelect();
TVeosErr Sock_Accept();
TVeosErr Sock_Transmit();
TVeosErr Sock_Receive();
TVeosErr Sock_Close();

/****************************************************************************************/



/****************************************************************************************
 * 			        local function declarations				*/

TVeosErr Sock_MixItUp();
TVeosErr Sock_ResolveHost();
u_long Sock_ConvertAddr();

/****************************************************************************************/




/****************************************************************************************/
TVeosErr Sock_Connect(iSocketFD, pUid, sProtocolName)
    int 		*iSocketFD;
    TPUid		pUid;
    char 		*sProtocolName;
{
    struct sockaddr_in  socketName;
    TVeosErr		iErr;
    int			iProto, iOption, iBufSize;
    

    /** translate given network params into useable form **/

    iErr = Sock_MixItUp(&pUid->iPort, sProtocolName, &iProto);
    if (iErr == VEOS_SUCCESS) {


	/** copy the address of the receiving host **/

	socketName.sin_addr.s_addr = pUid->lHost;

	    
	/** create socket with specified protocol **/
	
	socketName.sin_family = AF_INET;
	socketName.sin_port = htons(pUid->iPort);
	
	*iSocketFD = socket(socketName.sin_family, SOCK_STREAM, iProto);
	
	if (*iSocketFD == TALK_BOGUS_FD)
	    iErr = TALK_CREATE;
	
	else {
	    
	    
	    /** attempt to connect to given address **/
	    
	    if (connect(*iSocketFD, &socketName, sizeof(socketName)) < 0)
		
		iErr = TALK_CONNECT;
	    
	    
	    else {
/*
		iBufSize = 16384;
		if (setsockopt(*iSocketFD, SOL_SOCKET, SO_SNDBUF,
			       (char *) &iBufSize, sizeof(int)) < 0)
		    iErr = TALK_FLAGS;
*/		    
		iOption = TRUE;
		if (setsockopt(*iSocketFD, IPPROTO_TCP, TCP_NODELAY,
			       &iOption, sizeof(int)) == -1)
		    iErr = TALK_FLAGS;

		/** set non-blocking write bit **/
		
		fcntl(*iSocketFD, F_SETFL, FNDELAY);
		
		FD_SET(*iSocketFD, &OPEN_WRITE_SOCKETS);
		}
	    
	    if (iErr != VEOS_SUCCESS)
		Sock_Close(iSocketFD);
	    }
	}

    return(iErr);

    } /* Sock_Connect */
/****************************************************************************************/




/****************************************************************************************/
TVeosErr Sock_Listen(iSocketFD, iPortNumber, sProtocolName, iAttitude)
    int 		*iSocketFD;
    int 		iPortNumber;
    char 		*sProtocolName;
    int			iAttitude;
{
    struct sockaddr_in  socketName;
    TVeosErr		iErr;
    int			iProto, iOption;
    int			iZoot;

    iErr = Sock_MixItUp(&iPortNumber, sProtocolName, &iProto);
    if (iErr == VEOS_SUCCESS) {



	/** create socket with specified protocol **/

	socketName.sin_family = AF_INET;   /* specify socket to be of INTERNET family */

	*iSocketFD = socket(socketName.sin_family, SOCK_STREAM, iProto);

	if (*iSocketFD == TALK_BOGUS_FD)
	    iErr = TALK_CREATE;

	else {
	    socketName.sin_addr.s_addr = htonl(INADDR_ANY);
	    socketName.sin_port = htons(iPortNumber);
	    
	    if (iAttitude == TALK_AGRESSIVE) {
		iOption = TRUE;
		if (setsockopt(*iSocketFD, SOL_SOCKET, SO_REUSEADDR,
			       &iOption, sizeof(int)) == -1)
		    iErr = TALK_FLAGS;
		}
	    
	    if (iErr == VEOS_SUCCESS) {
		
		/** register this socket with system for us **/
		
		if (bind(*iSocketFD, &socketName, sizeof(socketName)) < 0) {
		    
		    iErr = TALK_BIND;
		    }
		
		else {
		    /** listen on the socket **/
		    
		    if (listen(*iSocketFD, TALK_QUEUE_SIZE ) < 0)
			iErr = TALK_LISTEN;
		    
		    else {
			/** have this socket generate an interrupt
			 ** when another entity connects.
			 **/
/*
			fcntl(*iSocketFD, F_SETOWN, getpid());
			fcntl(*iSocketFD, F_SETFL, FASYNC);
*/			
			FD_SET(*iSocketFD, &OPEN_READ_SOCKETS);
			}
		    }
		}		
	    }
	if (iErr != VEOS_SUCCESS) {
	    
	    Sock_Close(iSocketFD);
	    *iSocketFD = TALK_BOGUS_FD;
	    }
	}

    return(iErr);
    
    } /* Sock_Listen */
/****************************************************************************************/




/****************************************************************************************/
TVeosErr Sock_ReadSelect(iSocketFD)
    int		iSocketFD;
{
    struct timeval  	timeVal;
    fd_set  		tempFDSet;
    int 		iSize;
    TVeosErr		iErr;
    
    
    iErr = VEOS_SUCCESS;
    
    
    /** create a local copy of the fd_set since it gets modified by select() **/
    
    bcopy((char*) &OPEN_READ_SOCKETS, (char*) &tempFDSet, sizeof(fd_set));
    
    
    
    /** some implementations of select() might modify timeVal, so we	**
     ** must keep resetting it rather then making it global or static.	**/
    
    timeVal.tv_sec = 0;
    timeVal.tv_usec = 0;
    
    iSize = select(FD_SETSIZE, &tempFDSet, nil, nil, &timeVal);
    
    if (iSize <  0)
	iErr = TALK_SELECT;
    
    else if (iSize == 0)
	iErr = TALK_SELECT_TIMEOUT;
    
    else if (!FD_ISSET(iSocketFD, &tempFDSet))
	iErr = TALK_NOCONN;
    
    
    return(iErr);
    
    } /* Sock_ReadSelect */
/****************************************************************************************/




/****************************************************************************************
 * Sock_ReadSelect									*/

TVeosErr Sock_WriteSelect(iSocketFD)
     int		iSocketFD;
{
    struct timeval  	timeVal;
    fd_set  		tempFDSet;
    int 		iSize;
    TVeosErr		iErr;
    
    
    iErr = VEOS_SUCCESS;
    
    
    /** create a local copy of the fd_set since it gets modified by select() **/
    
    bcopy((char*) &OPEN_WRITE_SOCKETS, (char*) &tempFDSet, sizeof(fd_set));
    
    
    
    /** some implementations of select() might modify timeVal, so we	**
     ** must keep resetting it rather then making it global or static.	**/
    
    timeVal.tv_sec = 0;
    timeVal.tv_usec = 0;
    
    iSize = select(FD_SETSIZE, nil, &tempFDSet, nil, &timeVal);
    
    if (TRAP_FLAGS & 0x00000001 << SIGPIPE) {
	TRAP_FLAGS = TRAP_FLAGS & ~(0x00000001 << SIGPIPE);
	TERMINATE = FALSE;
	iErr = TALK_CONN_CLOSED;
	}

    else if (iSize <  0)
	iErr = TALK_SELECT;
    
    else if (iSize == 0)
	iErr = TALK_SELECT_TIMEOUT;
    
    else if (!FD_ISSET(iSocketFD, &tempFDSet))
	iErr = TALK_NOCONN;
    
    
    return(iErr);
    
    } /* Sock_WriteSelect */
/****************************************************************************************/




/****************************************************************************************
 * Sock_Accept										*/

TVeosErr Sock_Accept(iSocketFD, iSocketIOFD)
    int 		iSocketFD;
    int 		*iSocketIOFD;
{
    TVeosErr		iErr;
    int			iBufSize;

    iErr = TALK_ACCEPT;
    
    *iSocketIOFD = accept(iSocketFD, nil, nil);
    if (*iSocketIOFD >= 0) {

        /** setup socket for large buffers and non-blocking reading **/
/*
	iBufSize = 16384;
	if (setsockopt(*iSocketIOFD, SOL_SOCKET, SO_RCVBUF,
		       (char *) &iBufSize, sizeof(int)) < 0 ||
*/
	/** convert msgsock to streams message-nondiscard-mode **/

	if (fcntl(*iSocketIOFD, F_SETFL, FNDELAY) == -1)
	    Sock_Close(iSocketIOFD);

	else {
	    FD_SET(*iSocketIOFD, &OPEN_READ_SOCKETS);
	    iErr = VEOS_SUCCESS;
	    }
	}

    return(iErr);
    
} /* Sock_Accept */
/****************************************************************************************/




/****************************************************************************************
 * Sock_Transmit								        */

TVeosErr Sock_Transmit(iSocketFD, sMessage, pLen)
    int			iSocketFD;
    char		*sMessage;
    int			*pLen;
{    
    int			iNetAction;
    TVeosErr		iErr;
    boolean		bTrap;

    iErr = VEOS_FAILURE;	
    

    /** send the string to the given socket destination **/
    
    iNetAction = write(iSocketFD, sMessage, *pLen);

    CATCH_TRAP(SIGPIPE, bTrap);
    if (bTrap)
	iErr = TALK_CONN_CLOSED;


    else if (iNetAction < 0) {

	/** expected result when can't write **/

	if (errno == EAGAIN || errno == EWOULDBLOCK)	    
	    iErr = TALK_SPEAK_BLOCKED;

	else
	    perror("shell: write");
        }

    else if (iNetAction > 0) {

	*pLen = iNetAction;
	iErr = VEOS_SUCCESS;
	}

    return(iErr);

    } /* Sock_Transmit */
/****************************************************************************************/




/****************************************************************************************
 * Sock_Receive									        */

TVeosErr Sock_Receive(iSocketFD, sBuffer, iBufferSize)
    int			iSocketFD;
    char		*sBuffer;
    int			*iBufferSize;
{
    TVeosErr	        iErr;
    int			iNetAction;


    iErr = VEOS_FAILURE;				/* pessimism */


    /** look for unread data in socket **/

    iNetAction = read(iSocketFD, sBuffer, *iBufferSize);



    /** connection still open, but no data **/

    if (iNetAction < 0) {

	/** expected result when no data **/

	if (errno == EAGAIN || errno == EWOULDBLOCK)		
	    iErr = TALK_LISTEN_BLOCKED;

	else
	    perror("shell: read");
        }


    /** there was some data in the socket **/

    else if (iNetAction > 0) {

	iErr = VEOS_SUCCESS;
	*iBufferSize = iNetAction;
	}


    /** conneciton closed from other end **/

    else
        iErr = TALK_CONN_CLOSED;


    return(iErr);

    } /* Sock_Receive */
/****************************************************************************************/




/****************************************************************************************
 ** Inet Socket Close
 **
 ** usage:  status = Sock_Close( &socketFD );
 ** params: pointer to file descriptor of socket
 ** returns: VEOS_SUCCESS or TALK_CLOSE
 **/

TVeosErr Sock_Close(iSocketFD)
    int       	*iSocketFD;
{
    TVeosErr	iErr;
    
    iErr = VEOS_SUCCESS;    


    if (*iSocketFD != TALK_BOGUS_FD) {
	
	FD_CLR(*iSocketFD, &OPEN_WRITE_SOCKETS);
	FD_CLR(*iSocketFD, &OPEN_READ_SOCKETS);

	shutdown(*iSocketFD, 2);

	if (close(*iSocketFD) == -1)
	    iErr = TALK_CLOSE;

	else
	    *iSocketFD = TALK_BOGUS_FD;
	}

    return(iErr);

} /* Sock_Close */
/****************************************************************************************/




/****************************************************************************************
 * 				      	local routines					*
 ****************************************************************************************/



/****************************************************************************************
 * Sock_MixItUp										*/

TVeosErr Sock_MixItUp(iPortNumber, sProtocolName, iProto)
    char		*sProtocolName;
    int			*iPortNumber, *iProto;
{
    struct protoent 	*protocolInfo, *getprotobyname();
    TVeosErr		iErr;

    iErr = VEOS_FAILURE;

    if (*iPortNumber > 0) {

	protocolInfo = getprotobyname(sProtocolName);
	if (protocolInfo == nil)
	    iErr = TALK_PROTOCOL;

	else {
	    *iProto = protocolInfo->p_proto;
	    iErr = VEOS_SUCCESS;
	    }
	}

    return(iErr);

    } /* Sock_MixItUp */
/****************************************************************************************/




/****************************************************************************************/
TVeosErr Sock_ResolveHost(sHostName, pIpAddr)
    char		*sHostName;
    u_long		*pIpAddr;
{
    TVeosErr		iErr;


    /** host address may already be in internet form **/

    if (isdigit(sHostName[0]))
	iErr = Sock_StrAddr2IP(sHostName, pIpAddr);

    else
	iErr = Sock_StrHost2IP(sHostName, pIpAddr);


    return(iErr);

} /* Sock_ResolveHost */
/****************************************************************************************/



/****************************************************************************************/
TVeosErr Sock_StrHost2IP(sHostName, pIpAddr)
    char 	*sHostName;
    u_long	*pIpAddr;
{
    TVeosErr		iErr;
    struct hostent  	*hostInfo, *gethostbyname();
    TPHostNode		pFinger;

    iErr = VEOS_FAILURE;

    if (sHostName) {

	/** try to find this host in hash table first **/

	for (pFinger = SOCK_HOSTS[sHostName[0] - 'a'];
	     pFinger;
	     pFinger = pFinger->pNext) {

	    if (strcmp(pFinger->sHostName, sHostName) == 0) {
		iErr = VEOS_SUCCESS;
		break;
		}
	    }


	if (!pFinger) {

	    /** find host by calling unix kernel **/

	    iErr = TALK_HOST;			
	    if (hostInfo = gethostbyname(sHostName)) {

		iErr = Shell_NewBlock(sizeof(THostNode), &pFinger, "host-node");
		if (iErr == VEOS_SUCCESS) {
		    
		    pFinger->sHostName = strdup(sHostName);
		    pFinger->lHost = *(u_long *) hostInfo->h_addr_list[0];
		    
		    
		    /** insert new host into hash table **/
		    
		    pFinger->pNext = SOCK_HOSTS[sHostName[0] - 'a'];
		    SOCK_HOSTS[sHostName[0] - 'a'] = pFinger;
		    }
		}
	    }

	if (pFinger)
	    *pIpAddr = pFinger->lHost;
	}

    return(iErr);

    } /* Sock_StrHost2IP */
/****************************************************************************************/




/****************************************************************************************/
TVeosErr Sock_IP2StrHost(lIPAddr, sHostName)
    u_long	lIPAddr;
    char 	*sHostName;
{
    TVeosErr		iErr;
    struct hostent  	*hostInfo, *gethostbyaddr();
    char		*pFinger;

    iErr = VEOS_FAILURE;

    if (sHostName) {

	if (hostInfo = gethostbyaddr((char *) &lIPAddr, sizeof(u_long), AF_INET)) {
	    strcpy(sHostName, hostInfo->h_name);

	    if (pFinger = strchr(sHostName, '.'))
		pFinger[0] = '\0';

	    iErr = VEOS_SUCCESS;
	    }
	else
	    iErr = TALK_HOST;			
	}

    return(iErr);

    } /* Sock_IP2StrHost */
/****************************************************************************************/




/****************************************************************************************/
TVeosErr Sock_StrAddr2IP(sHostName, pIpAddr)
    char 	*sHostName;
    u_long	*pIpAddr;
{
    u_long	lResult, lTemp;
    char 	*pCharFinger;
    TVeosErr	iErr;

    iErr = VEOS_FAILURE;
    if (sHostName) {
	
	lResult = 0;
	pCharFinger = sHostName;  
	
	
	/* first byte */
	lTemp = (u_long) atoi(pCharFinger);
	lResult |= lTemp << 24;
	
	
	/* second byte */
	pCharFinger = strchr(pCharFinger, '.');
	pCharFinger ++;
	
	lTemp = (u_long) atoi(pCharFinger);
	lResult |= lTemp << 16;
	
	
	/* third byte */
	pCharFinger = strchr(pCharFinger, '.');
	pCharFinger ++;
	
	lTemp = (u_long) atoi(pCharFinger);
	lResult |= lTemp << 8;
	
	
	/* fourth byte */
	pCharFinger = strchr(pCharFinger, '.');
	pCharFinger ++;
	
	lTemp = (u_long) atoi(pCharFinger);
	lResult |= lTemp;
	
	
	*pIpAddr = lResult;

	iErr = VEOS_SUCCESS;
	}

    return(iErr);

    } /* Sock_StrAddr2IP */
/****************************************************************************************/



/****************************************************************************************/
TVeosErr Sock_IP2StrAddr(lIpAddr, sHostName)
    u_long	lIpAddr;
    char 	*sHostName;
{
    TVeosErr		iErr;

    iErr = VEOS_FAILURE;
    if (sHostName) {
	
	sprintf(sHostName, "%d.%d.%d.%d",
		(lIpAddr >> 24) & 0x000000FF,
		(lIpAddr >> 16) & 0x000000FF,
		(lIpAddr >> 8) & 0x000000FF,
		lIpAddr & 0x000000FF);

	iErr = VEOS_SUCCESS;
	}

    return(iErr);

    } /* Sock_IP2StrAddr */
/****************************************************************************************/






