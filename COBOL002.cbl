       IDENTIFICATION DIVISION.
      ****************************************************************
      *                                                              *
      * Program name: AMQMSET2                                       *
      *                                                              *
      * Description: Sample MicroFocus COBOL program using MQSET     *
      *                                                              *
      *  Statement:     Licensed Materials - Property of IBM         *                      
      *                                                              *      
      *                 04L1773, 5765-B73                            *            
      *                 04L1802, 5639-B42                            *            
      *                 04L1788, 5765-B74                            *            
      *                 04L1816, 5765-B75                            *            
      *                 04L1830, 5639-B43                            *            
      *                 (C) Copyright IBM Corp. 1994, 1998           *
      *                                                              *
KSCHG1* First change --> at 12:50 AM 7/14/23                         *
      *                                                              *
      ****************************************************************
      *                                                              *
      * Function:                                                    *
      *                                                              *
      *                                                              *
      *   AMQMSET2 is a sample COBOL program that inhibits PUTs      *
      *   to a message queue, an example of MQSET.   It is           *
      *   intended to run as a triggered program, and so receives    *
      *   input in a trigger parameter.                              *
      *                                                              *
      *   NOTE: One way to run AMQMSET2 is to be running a           *
      *         trigger monitor (for example runmqtrm) and to        *
      *         use AMQ0REQ0 to send request messages (containing    *
      *         queue names) to a queue (eg SYSTEM.SAMPLE.SET)       *
      *         which must have been defined to trigger AMQMSET2     *
      *                                                              *
      *      -- gets request messages from a queue whose name is     *
      *         in the trigger parameter; the content of each        *
      *         message is the name of a message queue               *
      *                                                              *
      *      -- inhibits PUTs to the queue named in the request,     *
      *         and sends a message to the named reply queue to      *
      *         confirm it was done                                  *
      *                                                              *
      *         alternatively, sends a report message if the         *
      *         queue can't  be inhibited - eg, if the queue         *
      *         to be inhibited does not exist                       *
      *                                                              *
      *      -- stops when the input queue becomes empty             *
      *                                                              *
      *      -- writes a message for each MQI reason other than      *
      *         MQRC-NONE; stops if there is a MQI completion code   *
      *         of MQCC-FAILED                                       *
      *                                                              *
      *    Program logic:                                            *
      *         MQCONNect to default queue manager                   *
      *         MQOPEN message queue (A) for shared input            *
      *         while no MQI failures,                               *
      *         .  MQGET next message from queue A                   *
      *         .  if message is a request,                          *
      *         .  .  MQOPEN queue (B) named in request for SET      *
      *         .  .  Use MQSET, inhibit PUTs to queue B             *
      *         .  .  Prepare reply message if PUT was inhibited     *
      *         .  .  MQCLOSE queue B                                *
      *         .  .  Prepare report message if reply not available  *
      *         .  .  MQPUT1, send reply or report to reply queue    *
      *         MQCLOSE queue A                                      *
      *         MQDISConnect from queue manager                      *
      *                                                              *
      *                                                              *
      ****************************************************************
      *                                                              *
      *                                                              *
      *                                                              *
      *   Exceptions signaled:  none                                 *
      *   Exceptions monitored: none                                 *
      *                                                              *
      *   AMQMSET2 has 1 parameter - a string (MQTMC2) based on the  *
      *       the initiation trigger message; only the QName field   *
      *       is used in this example                                *
      *                                                              *
      ****************************************************************
       PROGRAM-ID. AMQMSET2.

      ****************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
      **  Declare MQI structures needed
      * MQI named constants
       01 MY-MQ-CONSTANTS.
          COPY CMQV.
      * Object Descriptor
       01 OBJECT-DESCRIPTOR.
          COPY CMQODV.
      * Message Descriptor
       01 MESSAGE-DESCRIPTOR.
          COPY CMQMDV.
      * Get message options
       01 GMOPTIONS.
          COPY CMQGMOV.
      * Put message options
       01 PMOPTIONS.
          COPY CMQPMOV.
      ** note, sample uses defaults where it can
       01 QM-NAME                    PIC X(48) VALUE SPACES.
       01 HCONN                      PIC S9(9) BINARY.
       01 Q-HANDLE                   PIC S9(9) BINARY.
       01 SET-HANDLE                 PIC S9(9) BINARY.
       01 OPTIONS                    PIC S9(9) BINARY.
       01 COMPLETION-CODE            PIC S9(9) BINARY.
       01 OPEN-CODE                  PIC S9(9) BINARY.
       01 CON-REASON                 PIC S9(9) BINARY.
       01 REASON                     PIC S9(9) BINARY.
       01 BUFFER-LENGTH              PIC S9(9) BINARY.
       01 DATA-LENGTH                PIC S9(9) BINARY.
       01 SELECT-COUNT               PIC S9(9) BINARY VALUE 1.
       01 SELECTOR-TABLE.
         02 SELECTOR                 PIC S9(9) BINARY OCCURS 1 TIMES.
       01 INT-ATTR-COUNT             PIC S9(9) BINARY VALUE 1.
       01 INT-ATTR-TABLE.
         02 INT-ATTR                 PIC S9(9) BINARY OCCURS 1 TIMES.
       01 CHAR-ATTR-LENGTH           PIC S9(9) BINARY VALUE 0.
       01 CHAR-ATTRS                 PIC X(100).

      ** message is read into buffer; reply contains an extension
       01 REPLY.
         02 BUFFER                   PIC X(48).
         02 BUF1                     PIC X(14) VALUE " PUT inhibited".

      ** The trigger monitor passes the trigger message delimited
      ** by double quotes -  " MQTMC Structure ". The filler
      ** character put in below removes the " so that the contents
      ** of the structure can be accessed correctly.
       01 TRIGGER-DATA.
          02 FILLER                         PIC X(1).
          02 TRIG-INFO.
           COPY CMQTMC2L.

      ****************************************************************
       PROCEDURE DIVISION.
       P0.
      ** indicate that sample program has started
           DISPLAY "AMQMSET2 start".

      ****************************************************************
      *                                                              *
      *   Get the command line parameter.                            *
      *                                                              *
      *   NOTE - There is a compiler restriction in that only 127    *
      *          bytes of trigger data are actully received          *
      *          using ACCEPT FROM COMMAND-LINE.                     *
      *          This does not cause problems with this sample as    *
      *          it only uses the Queue Manager name. If further     *
      *          trigger information is required replace the         *
      *          following line with code to get the OS/2 command    *
      *          line parameter directly using the OS/2 API call     *
      *          DosGetInfoBlock.                                    *
      *                                                              *
      ****************************************************************
           ACCEPT TRIGGER-DATA FROM COMMAND-LINE.

      ****************************************************************
      *                                                              *
      *   This sample includes an explicit connect (MQCONN) to the   *
      *   default queue manager.                                     *
      *   This call is not required on all MQSeries platforms but    *
      *   has been included here to allow the sample to be portable  *
      *   between platforms.                                         *
      *                                                              *
      ****************************************************************
           CALL "MQCONN"
            USING QM-NAME, HCONN,
            COMPLETION-CODE, CON-REASON.

      *      report reason and stop if it failed
           IF COMPLETION-CODE IS EQUAL TO MQCC-FAILED
             DISPLAY "MQCONN ended with reason code " CON-REASON
             MOVE CON-REASON TO RETURN-CODE
             GOBACK
             END-IF.
      *
      ****************************************************************
      *                                                              *
      *   Open the message queue for shared input                    *
      *                                                              *
      ****************************************************************
       OPENS.
           MOVE MQTMC-QNAME TO MQOD-OBJECTNAME.
           ADD MQOO-INPUT-SHARED MQOO-FAIL-IF-QUIESCING
                     GIVING OPTIONS.
           CALL "MQOPEN"
            USING HCONN, OBJECT-DESCRIPTOR,
            OPTIONS, Q-HANDLE,
            OPEN-CODE, REASON.

      *      report reason, if any; stop if failed
           IF REASON IS NOT EQUAL TO MQRC-NONE
             DISPLAY "MQOPEN (input) ended with reason code " REASON
             END-IF.

           IF OPEN-CODE IS EQUAL TO MQCC-FAILED
             DISPLAY "unable to open queue for input"
             MOVE REASON TO RETURN-CODE
             GOBACK
             END-IF.

      ****************************************************************
      *                                                              *
      *   Get messages from the message queue                        *
      *                                                              *
      ****************************************************************
       GETS.
           MOVE OPEN-CODE TO COMPLETION-CODE.
           PERFORM GETR THRU RESPR WITH TEST BEFORE
             UNTIL COMPLETION-CODE IS EQUAL TO MQCC-FAILED.

      ****************************************************************
      *                                                              *
      *   Close the source queue                                     *
      *                                                              *
      ****************************************************************
       CLOSES.
           MOVE MQCO-NONE TO OPTIONS.
           CALL "MQCLOSE"
            USING HCONN, Q-HANDLE, OPTIONS,
            COMPLETION-CODE, REASON.

      *      report reason, if any
           IF REASON IS NOT EQUAL TO MQRC-NONE
             DISPLAY "MQCLOSE ended with reason code " REASON
             END-IF.

      ****************************************************************
      *                                                              *
      *  Disconnect from queue manager (if not previously connected) *
      *                                                              *
      ****************************************************************
       DISCS.
           IF CON-REASON IS NOT EQUAL TO MQRC-ALREADY-CONNECTED
             CALL "MQDISC"
              USING HCONN, COMPLETION-CODE, REASON

      *      report reason, if any
             IF REASON IS NOT EQUAL TO MQRC-NONE
               DISPLAY "MQDISC ended with reason code " REASON
             END-IF
           END-IF.

       OVER.
      ** indicate that sample program has finished
           DISPLAY "AMQMSET2 end".
           MOVE ZERO TO RETURN-CODE.
           GOBACK.

      ****************************************************************
      *                                                              *
      *   Get one message (request)                                  *
      *                                                              *
      *   In order to read the messages in sequence, MSGID and       *
      *   CORRELID must have the default value.  MQGET sets them     *
      *   to the values for the message it returns, so re-initialise *
      *   them each time                                             *
      *                                                              *
      ****************************************************************
       GETR.
           MOVE MQMI-NONE TO MQMD-MSGID.
           MOVE MQCI-NONE TO MQMD-CORRELID.
           MOVE SPACES TO BUFFER.
           ADD MQGMO-ACCEPT-TRUNCATED-MSG MQGMO-WAIT
                GIVING MQGMO-OPTIONS.
           MOVE 5000 TO MQGMO-WAITINTERVAL.
           MOVE 48 to BUFFER-LENGTH.

           CALL "MQGET"
            USING HCONN, Q-HANDLE,
            MESSAGE-DESCRIPTOR, GMOPTIONS,
            BUFFER-LENGTH, BUFFER, DATA-LENGTH,
            COMPLETION-CODE, REASON.

           IF REASON IS NOT EQUAL TO MQRC-NONE
             IF REASON IS EQUAL TO MQRC-NO-MSG-AVAILABLE
               DISPLAY "no more messages"
             ELSE
               DISPLAY "MQGET ended with reason code " REASON
             END-IF
           END-IF.

      ****************************************************************
      *                                                              *
      *   Display message received                                   *
      *                                                              *
      ****************************************************************
       DISPM.
           IF COMPLETION-CODE IS NOT EQUAL TO MQCC-FAILED
             DISPLAY "message is <" BUFFER ">"
           END-IF.

      ****************************************************************
      *                                                              *
      *  Respond to requests only                                    *
      *                                                              *
      ****************************************************************
       RESPR.
           IF COMPLETION-CODE IS EQUAL TO MQCC-OK
             IF MQMD-MSGTYPE IS NOT EQUAL TO MQMT-REQUEST
               DISPLAY "  -- not a request and discarded"
             ELSE
               PERFORM RESP1 THRU RESP3
             END-IF
           END-IF.

      ****************************************************************
      *                                                              *
      *  Process a request message                                   *
      *                                                              *
      ****************************************************************
       RESP1.
           MOVE BUFFER TO MQOD-OBJECTNAME.
           PERFORM SOPEN.
           IF COMPLETION-CODE IS EQUAL TO MQCC-OK
             PERFORM SETS THRU SCLOSE
           END-IF.

      ****************************************************************
      *                                                              *
      *  Create report if not a reply                                *
      *  In this sample, the Queue name is sent in the report        *
      *                                                              *
      ****************************************************************
       RESP2.
           IF MQMD-MSGTYPE IS EQUAL TO MQMT-REPORT
             MOVE 48 TO BUFFER-LENGTH.

      ****************************************************************
      *                                                              *
      *  Send the reply or report message                            *
      *                                                              *
      ****************************************************************
       RESP3.
      ****************************************************************
      *  Copy reply queue names                                      *
      ****************************************************************
           MOVE MQMD-REPLYTOQ TO MQOD-OBJECTNAME.
           MOVE MQMD-REPLYTOQMGR TO MQOD-OBJECTQMGRNAME.

      ****************************************************************
      *  Carry out default action for updating reply CORRELID        *
      *                                                              *
      *  NOTE - If the application does not know that the calling    *
      *         program requires the default action on setting the   *
      *         reply MSGID and CORRELID then it should check what   *
      *         is required by looking at the values set by the      *
      *         calling program in MQMD-REPORT.                      *
      *         An example of this is in the Inquire sample.         *
      ****************************************************************
           MOVE MQMD-MSGID TO MQMD-CORRELID.

      ****************************************************************
      *  Stop further reports                                        *
      ****************************************************************
           MOVE MQRO-NONE TO MQMD-REPORT.

      ****************************************************************
      *  Put single message, reply or report                         *
      *                                                              *
      *  NOTE - This sample stops if MQPUT1 fails; in some           *
      *         applications it may be appropriate to                *
      *         continue after selected REASON codes                 *
      ****************************************************************
           CALL "MQPUT1"
            USING HCONN, OBJECT-DESCRIPTOR,
            MESSAGE-DESCRIPTOR, PMOPTIONS,
            BUFFER-LENGTH, REPLY,
            COMPLETION-CODE, REASON.

           IF REASON IS NOT EQUAL TO MQRC-NONE
             DISPLAY "MQPUT1 ended with reason code " REASON
             END-IF.

      ****************************************************************
      *                                                              *
      *  Open subject queue for SET                                  *
      *                                                              *
      ****************************************************************
       SOPEN.
           ADD MQOO-SET MQOO-FAIL-IF-QUIESCING
                     GIVING OPTIONS.
           CALL "MQOPEN"
            USING HCONN, OBJECT-DESCRIPTOR,
            OPTIONS, SET-HANDLE,
            COMPLETION-CODE, REASON.

           IF COMPLETION-CODE IS EQUAL TO MQCC-FAILED
             MOVE MQMT-REPORT to MQMD-MSGTYPE
             MOVE REASON TO MQMD-FEEDBACK
             END-IF.

      ****************************************************************
      *                                                              *
      *  Inhibits PUTs to the queue (MQSET)                          *
      *                                                              *
      ****************************************************************
       SETS.
           MOVE MQIA-INHIBIT-PUT TO SELECTOR(1).
           MOVE MQQA-PUT-INHIBITED TO INT-ATTR(1).
           CALL "MQSET"
            USING HCONN, SET-HANDLE,
            SELECT-COUNT, SELECTOR-TABLE,
            INT-ATTR-COUNT, INT-ATTR-TABLE,
            CHAR-ATTR-LENGTH, CHAR-ATTRS,
            COMPLETION-CODE, REASON.

      ****************************************************************
      *                                                              *
      *  Prepare reply message if Inhibit (MQSET) worked             *
      *                                                              *
      ****************************************************************
       SREPLY.
           IF COMPLETION-CODE IS EQUAL TO MQCC-OK
             MOVE 62 TO BUFFER-LENGTH
             MOVE MQMT-REPLY TO MQMD-MSGTYPE
           ELSE
             MOVE MQMT-REPORT TO MQMD-MSGTYPE
             MOVE REASON TO MQMD-FEEDBACK
           END-IF.

      ****************************************************************
      *                                                              *
      *  CLOSE the MQSET queue                                       *
      *                                                              *
      ****************************************************************
       SCLOSE.
           MOVE MQCO-NONE TO OPTIONS.
           CALL "MQCLOSE"
            USING HCONN, SET-HANDLE, OPTIONS,
            COMPLETION-CODE, REASON.

      *      report reason, if any
           IF REASON IS NOT EQUAL TO MQRC-NONE
             DISPLAY "MQCLOSE ended with reason code " REASON
             END-IF.

      ****************************************************************
      *                                                              *
      * END OF AMQMSET2                                              *
      *                                                              *
      ****************************************************************
