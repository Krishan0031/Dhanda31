      ******************************************************************
      **                                                              **
      **                  IBM MQSeries for Windows NT                 **
      **                                                              **
      **  COPYBOOK NAME:  CMQCDV                                      **
KSCHG1**  # ADDED 1 change here                                       **
      **  DESCRIPTION:    Channel Definition Structure                **
      **                                                              **
      ******************************************************************
      **  @START_COPYRIGHT@                                           **
      **  Licensed Materials - Property of IBM                        **
      **                                                              **
      **  04L1830, 5639-B43                                           **
      **                                                              **
      **  (C) Copyright IBM Corporation 1993, 1999.                   **
      **                                                              **
      **  Status: Version 5 Release 1                                 **
      **  @END_COPYRIGHT@                                             **
      ******************************************************************
      **                                                              **
      **  FUNCTION:       This file declares the structure MQCD,      **
      **                  which is used by exits and the MQCONNX      **
      **                  call.                                       **
      **                                                              **
      **  PROCESSOR:      COBOL                                       **
      **                                                              **
      ******************************************************************
 
      **   MQCD structure
        10 MQCD.
      **    Channel definition name
         15 MQCD-CHANNELNAME            PIC X(20) VALUE SPACES.
      **    Structure version number
         15 MQCD-VERSION                PIC S9(9) BINARY VALUE 6.
      **    Channel type
         15 MQCD-CHANNELTYPE            PIC S9(9) BINARY VALUE 1.
      **    Transport type
         15 MQCD-TRANSPORTTYPE          PIC S9(9) BINARY VALUE 1.
      **    Channel description
         15 MQCD-DESC                   PIC X(64) VALUE SPACES.
      **    Queue-manager name
         15 MQCD-QMGRNAME               PIC X(48) VALUE SPACES.
      **    Transmission queue name
         15 MQCD-XMITQNAME              PIC X(48) VALUE SPACES.
      **    First 20 bytes of connection name
         15 MQCD-SHORTCONNECTIONNAME    PIC X(20) VALUE SPACES.
      **    Reserved
         15 MQCD-MCANAME                PIC X(20) VALUE SPACES.
      **    LU 6.2 Mode name
         15 MQCD-MODENAME               PIC X(8) VALUE SPACES.
      **    LU 6.2 transaction program name
         15 MQCD-TPNAME                 PIC X(64) VALUE SPACES.
      **    Batch size
         15 MQCD-BATCHSIZE              PIC S9(9) BINARY VALUE 50.
      **    Disconnect interval
         15 MQCD-DISCINTERVAL           PIC S9(9) BINARY VALUE 6000.
      **    Short retry count
         15 MQCD-SHORTRETRYCOUNT        PIC S9(9) BINARY VALUE 10.
      **    Short retry wait interval
         15 MQCD-SHORTRETRYINTERVAL     PIC S9(9) BINARY VALUE 60.
      **    Long retry count
         15 MQCD-LONGRETRYCOUNT         PIC S9(9) BINARY VALUE
              999999999.
      **    Long retry wait interval
         15 MQCD-LONGRETRYINTERVAL      PIC S9(9) BINARY VALUE 1200.
      **    Channel security exit name
         15 MQCD-SECURITYEXIT           PIC X(128) VALUE SPACES.
      **    Channel message exit name
         15 MQCD-MSGEXIT                PIC X(128) VALUE SPACES.
      **    Channel send exit name
         15 MQCD-SENDEXIT               PIC X(128) VALUE SPACES.
      **    Channel receive exit name
         15 MQCD-RECEIVEEXIT            PIC X(128) VALUE SPACES.
      **    Highest allowable message sequence number
         15 MQCD-SEQNUMBERWRAP          PIC S9(9) BINARY VALUE
              999999999.
      **    Maximum message length
         15 MQCD-MAXMSGLENGTH           PIC S9(9) BINARY VALUE 4194304.
      **    Put authority
         15 MQCD-PUTAUTHORITY           PIC S9(9) BINARY VALUE 1.
      **    Data conversion
         15 MQCD-DATACONVERSION         PIC S9(9) BINARY VALUE 0.
      **    Channel security exit user data
         15 MQCD-SECURITYUSERDATA       PIC X(32) VALUE SPACES.
      **    Channel message exit user data
         15 MQCD-MSGUSERDATA            PIC X(32) VALUE SPACES.
      **    Channel send exit user data
         15 MQCD-SENDUSERDATA           PIC X(32) VALUE SPACES.
      **    Channel receive exit user data
         15 MQCD-RECEIVEUSERDATA        PIC X(32) VALUE SPACES.
      **    User identifier
         15 MQCD-USERIDENTIFIER         PIC X(12) VALUE SPACES.
      **    Password
         15 MQCD-PASSWORD               PIC X(12) VALUE SPACES.
      **    First 12 bytes of MCA user identifier
         15 MQCD-MCAUSERIDENTIFIER      PIC X(12) VALUE SPACES.
      **    Message channel agent type
         15 MQCD-MCATYPE                PIC S9(9) BINARY VALUE 1.
      **    Connection name
         15 MQCD-CONNECTIONNAME         PIC X(264) VALUE SPACES.
      **    First 12 bytes of user identifier from partner
         15 MQCD-REMOTEUSERIDENTIFIER   PIC X(12) VALUE SPACES.
      **    Password from partner
         15 MQCD-REMOTEPASSWORD         PIC X(12) VALUE SPACES.
      **    Channel message retry exit name
         15 MQCD-MSGRETRYEXIT           PIC X(128) VALUE SPACES.
      **    Channel message retry exit user data
         15 MQCD-MSGRETRYUSERDATA       PIC X(32) VALUE SPACES.
      **    Number of times MCA will try to put the message, after the
      **    first attempt has failed
         15 MQCD-MSGRETRYCOUNT          PIC S9(9) BINARY VALUE 10.
      **    Minimum interval in milliseconds after which the open or put
      **    operation will be retried
         15 MQCD-MSGRETRYINTERVAL       PIC S9(9) BINARY VALUE 1000.
      **    Time in seconds between heartbeat flows
         15 MQCD-HEARTBEATINTERVAL      PIC S9(9) BINARY VALUE 300.
      **    Batch duration
         15 MQCD-BATCHINTERVAL          PIC S9(9) BINARY VALUE 0.
      **    Speed at which nonpersistent messages are sent
         15 MQCD-NONPERSISTENTMSGSPEED  PIC S9(9) BINARY VALUE 2.
      **    Length of MQCD structure
         15 MQCD-STRUCLENGTH            PIC S9(9) BINARY VALUE 1648.
      **    Length of exit name
         15 MQCD-EXITNAMELENGTH         PIC S9(9) BINARY VALUE 128.
      **    Length of exit user data
         15 MQCD-EXITDATALENGTH         PIC S9(9) BINARY VALUE 32.
      **    Number of message exits defined
         15 MQCD-MSGEXITSDEFINED        PIC S9(9) BINARY VALUE 0.
      **    Number of send exits defined
         15 MQCD-SENDEXITSDEFINED       PIC S9(9) BINARY VALUE 0.
      **    Number of receive exits defined
         15 MQCD-RECEIVEEXITSDEFINED    PIC S9(9) BINARY VALUE 0.
      **    Address of first MsgExit field
         15 MQCD-MSGEXITPTR             POINTER VALUE NULL.
      **    Address of first MsgUserData field
         15 MQCD-MSGUSERDATAPTR         POINTER VALUE NULL.
      **    Address of first SendExit field
         15 MQCD-SENDEXITPTR            POINTER VALUE NULL.
      **    Address of first SendUserData field
         15 MQCD-SENDUSERDATAPTR        POINTER VALUE NULL.
      **    Address of first ReceiveExit field
         15 MQCD-RECEIVEEXITPTR         POINTER VALUE NULL.
      **    Address of first ReceiveUserData field
         15 MQCD-RECEIVEUSERDATAPTR     POINTER VALUE NULL.
      **    Address of first cluster record
         15 MQCD-CLUSTERPTR             POINTER VALUE NULL.
      **    Number of cluster records
         15 MQCD-CLUSTERSDEFINED        PIC S9(9) BINARY VALUE 0.
      **    Network priority
         15 MQCD-NETWORKPRIORITY        PIC S9(9) BINARY VALUE 0.
      **    Length of long MCA user identifier
         15 MQCD-LONGMCAUSERIDLENGTH    PIC S9(9) BINARY VALUE 0.
      **    Length of long remote user identifier
         15 MQCD-LONGREMOTEUSERIDLENGTH PIC S9(9) BINARY VALUE 0.
      **    Address of long MCA user identifier
         15 MQCD-LONGMCAUSERIDPTR       POINTER VALUE NULL.
      **    Address of long remote user identifier
         15 MQCD-LONGREMOTEUSERIDPTR    POINTER VALUE NULL.
      **    MCA security identifier
         15 MQCD-MCASECURITYID          PIC X(40) VALUE LOW-VALUES.
      **    Remote security identifier
         15 MQCD-REMOTESECURITYID       PIC X(40) VALUE LOW-VALUES.
 
      ******************************************************************
      **  End of CMQCDV                                               **
      ******************************************************************
