                                                                       
      SUBROUTINE EREXIT(ICODE)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
C                .      .    .                                       .  
C SUBPROGRAM:    EREXIT      WRITE MESSAGE AND STOP FOR ABEND           
C   PRGMMR: STACKPOLE        ORG: W/NP12    DATE: 89-01-25             
C                                                                       
C ABSTRACT: GIVES 'DISASTER' MESSAGE INDICATING COMPLETE FAILURE        
C   ACUMVER4 PROGRAM.  MORE SPECIFIC MESSAGES ON FT06F001 REGARDING     
C   THE CAUSE OF THE TERMINATING CONDITION ARE USUALLY WRITTEN AT       
C   THE POINT WHERE THE PROBLEM OCCURRED BEFORE CALLING THIS SUB-       
C   ROUTINE.  HOWEVER, INPUT ARGUMENT /ICODE/ WILL TRIGGER APPROP-      
C   PRIATE MESSAGE AND COMPLETION (STOP) CODE NUMBER.             
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   76-??-??  STACKPOLE                                                 
C   89-01-25  VLCEK       SPECIFY INPUT ARGUMENT /ICODE/ TO HOLD        
C                         ERROR CODE NUMBER FOR MESSAGE AND       
C                         COMPLETION (STOP) CODE.                       
C   98-07-29  VLCEK       REMOVE W3LOG, COMPILE IN F90.
C                                                                       
C USAGE:    CALL EREXIT(ICODE)                                          
C   INPUT ARGUMENT LIST:  NONE                                          
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
C   LANGUAGE: VS FORTRAN 90     CRAY FORTRAN                                        
C   MACHINE:  CRAY
C                                                                       
C$$$                                                                    

C     ARRAY FOR ERROR MESSAGES.                                

      CHARACTER(50)  ERRMSG(10)
      DATA           ERRMSG
     &    /'SUB ADPTOS: CAN NOT OPEN ANALYSIS FILE FT11       ',
     &     'SUB ADPTOS: CAN NOT OPEN THE INDEX FILE FT12      ',
     &     'SUB ADPTOS: DATE MISMATCH FILES /ANL/ AND FT10F001',
     &     'SUB ADPTOS: FATAL GETGB ERROR READING ANL FILE.   ',
     &     'SUB ADPTOS: TOO FEW ADP REPORTS IN FT10 TO USE.   ',
     &     'MAP TYPE NOT MATCHED IN SUB ROTWND.               ',
     &     'MAP TYPE NOT MATCHED IN SUB STIJ.                 ',
     &     '                                                  ',
     &     '                                                  ',
     &     '                                                  '/

C     ALL DISASTERS COME OUT HERE                                    

      PRINT 10, ERRMSG(ICODE)
   10 FORMAT(' ', A50, //,                                         
     &       ' TOTAL DISASTER IN THE VERIFICATION AGAINST ADP CODE')
C     CALL W3TAGE('QCADP   ')                                          
      STOP
      END
