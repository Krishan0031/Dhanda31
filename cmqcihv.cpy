      ******************************************************************
      **                                                              **
      **                  IBM MQSeries for Windows NT                 **
      **                                                              **
      **  COPYBOOK NAME:  CMQCIHV                                     **
      **                                                              **
      **  DESCRIPTION:    CICS Information Header Structure           **
      **                                                              **
      ******************************************************************
      **  @START_COPYRIGHT@                                           **
      **  Licensed Materials - Property of IBM                        **
      **                                                              **
      **  04L1830, 5639-B43                                           **
      **                                                              **
      **  (C) Copyright IBM Corporation 1998, 1999.                   **
      **                                                              **
      **  Status: Version 5 Release 1                                 **
      **  @END_COPYRIGHT@                                             **
      ******************************************************************
      **                                                              **
      **  FUNCTION:       This file declares the structure MQCIH,     **
      **                  which is used by the main MQI.              **
      **                                                              **
      **  PROCESSOR:      COBOL                                       **
      **                                                              **
      ******************************************************************
 
      **   MQCIH structure
        10 MQCIH.
      **    Structure identifier
         15 MQCIH-STRUCID            PIC X(4) VALUE 'CIH '.
      **    Structure version number
         15 MQCIH-VERSION            PIC S9(9) BINARY VALUE 2.
      **    Length of MQCIH structure
         15 MQCIH-STRUCLENGTH        PIC S9(9) BINARY VALUE 180.
      **    Reserved
         15 MQCIH-ENCODING           PIC S9(9) BINARY VALUE 0.
      **    Reserved
         15 MQCIH-CODEDCHARSETID     PIC S9(9) BINARY VALUE 0.
      **    MQ format name
         15 MQCIH-FORMAT             PIC X(8) VALUE SPACES.
      **    Reserved
         15 MQCIH-FLAGS              PIC S9(9) BINARY VALUE 0.
      **    Return code from bridge
         15 MQCIH-RETURNCODE         PIC S9(9) BINARY VALUE 0.
      **    MQ completion code or CICS EIBRESP
         15 MQCIH-COMPCODE           PIC S9(9) BINARY VALUE 0.
      **    MQ reason or feedback code, or CICS EIBRESP2
         15 MQCIH-REASON             PIC S9(9) BINARY VALUE 0.
      **    Unit-of-work control
         15 MQCIH-UOWCONTROL         PIC S9(9) BINARY VALUE 273.
      **    Wait interval for MQGET call issued by bridge task
         15 MQCIH-GETWAITINTERVAL    PIC S9(9) BINARY VALUE -2.
      **    Link type
         15 MQCIH-LINKTYPE           PIC S9(9) BINARY VALUE 1.
      **    Output COMMAREA data length
         15 MQCIH-OUTPUTDATALENGTH   PIC S9(9) BINARY VALUE -1.
      **    Bridge facility release time
         15 MQCIH-FACILITYKEEPTIME   PIC S9(9) BINARY VALUE 0.
      **    Send/receive ADS descriptor
         15 MQCIH-ADSDESCRIPTOR      PIC S9(9) BINARY VALUE 0.
      **    Whether task can be conversational
         15 MQCIH-CONVERSATIONALTASK PIC S9(9) BINARY VALUE 0.
      **    Status at end of task
         15 MQCIH-TASKENDSTATUS      PIC S9(9) BINARY VALUE 0.
      **    BVT token value
         15 MQCIH-FACILITY           PIC X(8) VALUE LOW-VALUES.
      **    MQ call name or CICS EIBFN function
         15 MQCIH-FUNCTION           PIC X(4) VALUE SPACES.
      **    Abend code
         15 MQCIH-ABENDCODE          PIC X(4) VALUE SPACES.
      **    Password or passticket
         15 MQCIH-AUTHENTICATOR      PIC X(8) VALUE SPACES.
      **    Reserved
         15 MQCIH-RESERVED1          PIC X(8) VALUE SPACES.
      **    MQ format name of reply message
         15 MQCIH-REPLYTOFORMAT      PIC X(8) VALUE SPACES.
      **    Remote sysid to use
         15 MQCIH-REMOTESYSID        PIC X(4) VALUE SPACES.
      **    Remote transid to attach
         15 MQCIH-REMOTETRANSID      PIC X(4) VALUE SPACES.
      **    Transaction to attach
         15 MQCIH-TRANSACTIONID      PIC X(4) VALUE SPACES.
      **    Terminal emulated attributes
         15 MQCIH-FACILITYLIKE       PIC X(4) VALUE SPACES.
      **    AID key
         15 MQCIH-ATTENTIONID        PIC X(4) VALUE SPACES.
      **    Transaction start code
         15 MQCIH-STARTCODE          PIC X(4) VALUE SPACES.
      **    Abend transaction code
         15 MQCIH-CANCELCODE         PIC X(4) VALUE SPACES.
      **    Next transaction to attach
         15 MQCIH-NEXTTRANSACTIONID  PIC X(4) VALUE SPACES.
      **    Reserved
         15 MQCIH-RESERVED2          PIC X(8) VALUE SPACES.
      **    Reserved
         15 MQCIH-RESERVED3          PIC X(8) VALUE SPACES.
      **    Cursor position
         15 MQCIH-CURSORPOSITION     PIC S9(9) BINARY VALUE 0.
      **    Offset of error in message
         15 MQCIH-ERROROFFSET        PIC S9(9) BINARY VALUE 0.
      **    Item number of last message read
         15 MQCIH-INPUTITEM          PIC S9(9) BINARY VALUE 0.
      **    Reserved
         15 MQCIH-RESERVED4          PIC S9(9) BINARY VALUE 0.
 
      ******************************************************************
      **  End of CMQCIHV                                              **
      ******************************************************************
