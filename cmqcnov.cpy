      ******************************************************************
      **                                                              **
      **                  IBM MQSeries for Windows NT                 **
      **                                                              **
      **  COPYBOOK NAME:  CMQCNOV                                     **
      **                                                              **
      **  DESCRIPTION:    Connect Options Structure                   **
      **                                                              **
      ******************************************************************
      **  @START_COPYRIGHT@                                           **
      **  Licensed Materials - Property of IBM                        **
      **                                                              **
      **  04L1830, 5639-B43                                           **
      **                                                              **
      **  (C) Copyright IBM Corporation 1997, 1999.                   **
      **                                                              **
      **  Status: Version 5 Release 1                                 **
      **  @END_COPYRIGHT@                                             **
      ******************************************************************
      **                                                              **
      **  FUNCTION:       This file declares the structure MQCNO,     **
      **                  which is used by the main MQI.              **
      **                                                              **
      **  PROCESSOR:      COBOL                                       **
      **                                                              **
      ******************************************************************
 
      **   MQCNO structure
        10 MQCNO.
      **    Structure identifier
         15 MQCNO-STRUCID          PIC X(4) VALUE 'CNO '.
      **    Structure version number
         15 MQCNO-VERSION          PIC S9(9) BINARY VALUE 1.
      **    Options that control the action of MQCONNX
         15 MQCNO-OPTIONS          PIC S9(9) BINARY VALUE 0.
      **    Offset of MQCD structure for client connection
         15 MQCNO-CLIENTCONNOFFSET PIC S9(9) BINARY VALUE 0.
      **    Address of MQCD structure for client connection
         15 MQCNO-CLIENTCONNPTR    POINTER VALUE NULL.
 
      ******************************************************************
      **  End of CMQCNOV                                              **
      ******************************************************************
