      ******************************************************************
      **                                                              **
      **                  IBM MQSeries for Windows NT                 **
      **                                                              **
      **  COPYBOOK NAME:  CMQDHL                                      **
      **                                                              **
      **  DESCRIPTION:    Distribution Header Structure               **
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
      **  FUNCTION:       This file declares the structure MQDH,      **
      **                  which is used by the main MQI.              **
      **                                                              **
      **  PROCESSOR:      COBOL                                       **
      **                                                              **
      ******************************************************************
 
      **   MQDH structure
        10 MQDH.
      **    Structure identifier
         15 MQDH-STRUCID         PIC X(4).
      **    Structure version number
         15 MQDH-VERSION         PIC S9(9) BINARY.
      **    Length of MQDH structure plus following records
         15 MQDH-STRUCLENGTH     PIC S9(9) BINARY.
      **    Encoding of message data
         15 MQDH-ENCODING        PIC S9(9) BINARY.
      **    Coded character-set identifier of message data
         15 MQDH-CODEDCHARSETID  PIC S9(9) BINARY.
      **    Format name of message data
         15 MQDH-FORMAT          PIC X(8).
      **    General flags
         15 MQDH-FLAGS           PIC S9(9) BINARY.
      **    Flags indicating which MQPMR fields are present
         15 MQDH-PUTMSGRECFIELDS PIC S9(9) BINARY.
      **    Number of object records present
         15 MQDH-RECSPRESENT     PIC S9(9) BINARY.
      **    Offset of first object record from start of MQDH
         15 MQDH-OBJECTRECOFFSET PIC S9(9) BINARY.
      **    Offset of first put message record from start of MQDH
         15 MQDH-PUTMSGRECOFFSET PIC S9(9) BINARY.
 
      ******************************************************************
      **  End of CMQDHL                                               **
      ******************************************************************
