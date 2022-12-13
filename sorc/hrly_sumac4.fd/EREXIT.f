
      SUBROUTINE EREXIT(ICODE)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK 
C                .      .    .                                       .  
C SUBPROGRAM:    EREXIT      WRITE MESSAGE AND STOP FOR ABEND           
C   PRGMMR: VLCEK            ORG: W/NP12    DATE: 1999-06-07             
C                                                                       
C ABSTRACT: GIVES 'DISASTER' MESSAGE INDICATING COMPLETE FAILURE OF     
C   SUMAC4 PROGRAM.  MORE SPECIFIC MESSAGES ON FT06F001 REGARDING       
C   THE CAUSE OF THE TERMINATING CONDITION ARE USUALLY WRITTEN AT       
C   THE POINT WHERE THE PROBLEM OCCURRED BEFORE CALLING THIS SUB-       
C   ROUTINE.  HOWEVER, INPUT ARGUMENT /ICODE/ WILL TRIGGER APPROP-      
C   PRIATE ADDITIONAL ERROR MESSAGE.
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   76-??-??  STACKPOLE                                                 
C   89-01-25  VLCEK       SPECIFY INPUT ARGUMENT /ICODE/ TO HOLD        
C                         ERROR CODE NUMBER FOR W3LOG MESSAGE AND       
C                         COMPLETION (STOP) CODE.                       
C   96-06-25  Y. ZHNAG    MOVE IT TO CRAY AND REFINE IT.
C   98-07-28  VLCEK       REMOVE W3LOG AND W3AS02 AND COMPILE IN F90.           
C   99-06-07  VLCEK       COMPILE ON IBM RS6000.           
C                                                                       
C USAGE:    CALL EREXIT(ICODE)                                          
C   INPUT ARGUMENT LIST:  ICODE -- NUMBER INDICATING ERRMSG TO PRINT
C                                                                       
C   OUTPUT ARGUMENT LIST:  NONE                                         
C                                                                       
C   OUTPUT FILES:                                                       
C     FT06F001 - 'TOTAL DISASTER' MESSAGE.                              
C                                                                       
C REMARKS: MOST FREQUENT CAUSE OF THIS MESSAGE IS PROBLEM WITH          
C   ADPUPA FILE OR IN QUALITY CONTROL PROCESS (EMPTY ANL FILE OR        
C   MISMATCHED DATES).                                                  
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: VS FORTRAN 90   IBM FORTRAN                                             
C   MACHINE:  IBM RS6000
C                                                                       
C$$$                                                                    

      INTEGER       ICODE

C     ARRAY FOR ERROR MESSAGES.                                

      CHARACTER(50) ERRMSG(10)
      DATA          ERRMSG
     &         /'SUB PARTSM: RUN TYPE CAN NOT BE PROCESSED NOW.    ',
     &          'SUB ACCUMU: FORECAST FOR FHR CAN NOT BE PROCESSED.',
     &          'MAIN      : PARAMETER GORFIL HAS TO BE AMPLIFIED. ',
     &          '                                                  ',
     &          '                                                  ',
     &          'SUB ROTWND: FORECAST MAP TYPE NOT RECOGNIZED.     ',
     &          'SUB STIJ: FORECAST MAP TYPE NOT RECOGNIZED.       ',
     &          '                                                  ',
     &          '                                                  ',
     &          '                                                  '/

C     ALL DISASTERS COME OUT HERE                                    

      PRINT 10, ERRMSG(ICODE)
   10 FORMAT('  ', A50, //,
     &       ' TOTAL DISASTER IN THE VERIFICATION AGAINST ADP CODE')
      CALL W3TAGE('SUMAC4')                                          
      STOP 
      END
