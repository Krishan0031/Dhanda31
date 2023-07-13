       IDENTIFICATION DIVISION.
      ****************************************************************
      *                                                              *
      * Program name: AMQ0GBR0                                       *
      *                                                              *
      * Description: Sample COBOL program that displays messages     *
      *              that are on a message queue (example using      *
      *              Browse option of MQGET)                         *
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
      *                                                              *
      ****************************************************************
      *                                                              *
      * Function:                                                    *
      *                                                              *
      *                                                              *
      *   AMQ0GBR0 is a sample COBOL program to display messages     *
      *   that are on a message queue, and is an example using       *
      *   Browse option of MQGET                                     *
      *                                                              *
      *      -- sample gets messages from the queue which is         *
      *         obtained from the console                            *
      *                                                              *
      *      -- displays the contents of the message queue,          *
      *         assuming each message data to represent a line       *
      *         of text to be written                                *
      *                                                              *
      *         only the first 50 characters of each message is      *
      *         shown; incomplete messages are noted                 *
      *                                                              *
      *         leaves the messages on the queue                     *
      *                                                              *
      *         the displayed name of the queue is the resolved      *
      *         local queue name if the object name is an alias      *
      *                                                              *
      *      -- writes a message for each MQI reason other than      *
      *         MQRC-NONE; stops if there is a MQI completion code   *
      *         of MQCC-FAILED                                       *
      *                                                              *
      *    Program logic:                                            *
      *         display prompt for queue name                        *
      *         ACCEPT the input queue name from the console         *
      *         MQCONNect to default queue manager                   *
      *         MQOPEN queue for BROWSE                              *
      *         while no MQI failures,                               *
      *         .  MQGET next message (browse)                       *
      *         .  display Resolved Queue Name first time            *
      *         .  display up to 20 bytes of message data            *
      *         MQCLOSE the source queue                             *
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
      *   AMQ0GBR0 has no parameters                                 *
      *                                                              *
      ****************************************************************
       PROGRAM-ID. 'AMQ0GBR0'.

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
      ** note, sample uses defaults where it can
       01 QM-NAME                    PIC X(48) VALUE SPACES.
       01 HCONN                      PIC S9(9) BINARY.
       01 Q-HANDLE                   PIC S9(9) BINARY.
       01 OPTIONS                    PIC S9(9) BINARY.
       01 COMPLETION-CODE            PIC S9(9) BINARY.
       01 OPEN-CODE                  PIC S9(9) BINARY.
       01 CON-REASON                 PIC S9(9) BINARY.
       01 REASON                     PIC S9(9) BINARY.
       01 BUFFER                     PIC X(60).
       01 BUFFER-LENGTH              PIC S9(9) BINARY.
       01 DATA-LENGTH                PIC S9(9) BINARY.
       01 MSG-COUNT                  PIC 9999.
       01 TARGET-QUEUE               PIC X(48).

      ****************************************************************
       PROCEDURE DIVISION.
       P0.
      ** indicate that sample program has started
           DISPLAY 'AMQ0GBR0 start'.

      ****************************************************************
      *                                                              *
      *    Display prompt for the name of the target queue           *
      *                                                              *
      ****************************************************************
           DISPLAY 'Please enter the name of the target queue '

      ** get the target queue from StdIn.
           ACCEPT TARGET-QUEUE FROM CONSOLE.

      ****************************************************************
      *                                                              *
      *   Connect to default queue manager                           *
      *                                                              *
      ****************************************************************
           CALL 'MQCONN'
            USING QM-NAME, HCONN,
            COMPLETION-CODE, CON-REASON.

      *      report reason and stop if it failed
           IF COMPLETION-CODE IS EQUAL TO MQCC-FAILED
             DISPLAY 'MQCONN ended with reason code ' CON-REASON
             MOVE CON-REASON TO RETURN-CODE
             GOBACK
             END-IF.
      *
      ****************************************************************
      *                                                              *
      *   Open the message queue for Browse (and fail if MQM         *
      *   is quiescing)                                              *
      *                                                              *
      ****************************************************************
       OPENS.
           MOVE TARGET-QUEUE TO MQOD-OBJECTNAME.
           ADD MQOO-BROWSE MQOO-FAIL-IF-QUIESCING
                     GIVING OPTIONS.
           CALL 'MQOPEN'
            USING HCONN, OBJECT-DESCRIPTOR,
            OPTIONS, Q-HANDLE,
            OPEN-CODE, REASON.

      *      report reason, if any; stop if failed
           IF REASON IS NOT EQUAL TO MQRC-NONE
             DISPLAY 'MQOPEN ended with reason code ' REASON
             END-IF.

           IF OPEN-CODE IS EQUAL TO MQCC-FAILED
             DISPLAY 'unable to open server queue for output'
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
           MOVE 0 TO MSG-COUNT.
           PERFORM GETR THRU DISPR WITH TEST BEFORE
             UNTIL COMPLETION-CODE IS EQUAL TO MQCC-FAILED.

      ****************************************************************
      *                                                              *
      *   Close the source queue                                     *
      *                                                              *
      ****************************************************************
       CLOSES.
           MOVE MQCO-NONE TO OPTIONS.
           CALL 'MQCLOSE'
            USING HCONN, Q-HANDLE, OPTIONS,
            COMPLETION-CODE, REASON.

      *      report reason, if any
           IF REASON IS NOT EQUAL TO MQRC-NONE
             DISPLAY 'MQCLOSE ended with reason code ' REASON
             END-IF.

      ****************************************************************
      *                                                              *
      *  Disconnect from queue manager (if not previously connected) *
      *                                                              *
      ****************************************************************
       DISCS.
           IF CON-REASON IS NOT EQUAL TO MQRC-ALREADY-CONNECTED
             CALL 'MQDISC'
              USING HCONN, COMPLETION-CODE, REASON

      *      report reason, if any
             IF REASON IS NOT EQUAL TO MQRC-NONE
               DISPLAY 'MQDISC ended with reason code ' REASON
             END-IF
           END-IF.

       OVER.
      ** indicate that sample program has finished
           DISPLAY 'AMQ0GBR0 end'.
           MOVE ZERO TO RETURN-CODE.
           GOBACK.

      ****************************************************************
      *                                                              *
      *   Get one message                                            *
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
           ADD MQGMO-NO-WAIT MQGMO-BROWSE-NEXT
               MQGMO-ACCEPT-TRUNCATED-MSG
                     GIVING MQGMO-OPTIONS.
           MOVE 15000 TO MQGMO-WAITINTERVAL.
           MOVE 60 to BUFFER-LENGTH.

           CALL 'MQGET'
            USING HCONN, Q-HANDLE,
            MESSAGE-DESCRIPTOR, GMOPTIONS,
            BUFFER-LENGTH, BUFFER, DATA-LENGTH,
            COMPLETION-CODE, REASON.

      ****************************************************************
      *                                                              *
      *   Display message received                                   *
      *                                                              *
      ****************************************************************
       DISPM.
           IF COMPLETION-CODE IS NOT EQUAL TO MQCC-FAILED
             IF MSG-COUNT IS EQUAL TO 0
               DISPLAY 'Messages in ' MQGMO-RESOLVEDQNAME
             END-IF
             ADD 1 TO MSG-COUNT
             DISPLAY MSG-COUNT ' <' BUFFER '>'
           END-IF.

      ****************************************************************
      *                                                              *
      *  Report reason, if any                                       *
      *                                                              *
      ****************************************************************
       DISPR.
           IF REASON IS NOT EQUAL TO MQRC-NONE
             IF REASON IS EQUAL TO MQRC-NO-MSG-AVAILABLE
               DISPLAY 'no more messages'
             ELSE
               IF DATA-LENGTH IS GREATER THAN BUFFER-LENGTH
                 DISPLAY '   --- truncated'
               ELSE
                 DISPLAY 'MQGET ended with reason code ' REASON
               END-IF
             END-IF
           END-IF.

      ****************************************************************
      *                                                              *
      * END OF AMQ0GBR0                                              *
      *                                                              *
      ****************************************************************
