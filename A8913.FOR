C *********************************************************************
C ESDU International Plc
C
C     Program name changed                                      Amendment A
      PROGRAM A8913
C
C     ESDUpac Number:   A8913
C     ESDUpac Title:    Transverse (through-the-thickness) shear
C                       stiffnesses of fibre reinforced composite
C                       laminated plates
C     Data Item Number: 89013
C     Data Item Title:  Transverse (through-the-thickness) shear
C                       stiffnesses of fibre reinforced composite
C                       laminated plates
C
C     ESDUpac Status
C     --------------
C     Version:  1.0     December 1989
C     Version:  1.1     July 1991     -Data Item  Amendment A
C                       All variables explicitly declared. Program
C                       and output headers standardised.
C     Next 8 lines added                                        Amendment A
C     NOTE:             This program requires more than 64 kbytes of
C                       memory. This may exceed the capacity provided
C                       by the normal system. Special compilation
C                       options may be used or array sizes reduced to
C                       run the program.
C
C                       This program uses DOUBLE PRECISION and has been
C                       tested on the assumption that this is in the
C                       range E-308 TO E+308. Run time errors may occur
C                       if a lower range is used.
C                                                        End of Amendment A
C
C
C-----------------------------------------------------------------------
C
C     PURPOSE
C          TO ESTIMATE THE THROUGH-THE-THICKNESS SHEAR STIFFNESSES OF A
C          LAMINATED COMPOSITE FROM THE PROPERTIES OF THE LAMINATES
C          AND THEIR ORIENTATIONS. SEE DATA ITEM NO. 89013 FOR
C          EXPLANATION OF THEORY, APPLICATIONS, LIMITATIONS.
C
C     DESCRIPTION OF INPUT AND OUTPUT CHANNELS
C          (STANDARD INPUT/OUTPUT)
C          INPUT CHANNEL =5
C          OUTPUT CHANNEL=6
C
C     DESCRIPTION OF MAJOR PARAMETERS IN CONTROLLING PROGRAM
C          THROUGHOUT ALL FLAGS ARE OF THE FORM NOTEXX WHERE XX IS AN
C          IDENTIFYING NUMBER WHICH IS INITILISED AT ZERO AND RAISED
C          TO ONE TO TRIP THE NOTE
C            KOUNT  DUMMY VALUE SET AT UNITY, USED IN CALL OF C8713A
C            LAYS   ARRAY THE FIRST ROW OF WHICH IS SET EQUAL TO ARRAY
C                   LAYSS SO THAT C8713A CAN BE USED
C          EQUATION NUMBERS GIVEN RELATE ESDU DOCUMENTATION OF THE
C          SOLUTION
C
C     SUBROUTINES AND FUNCTIONS USED
C          B8913A   X8913A   C8713A    D8913A 
C          G8913A   Q8913A   Z8913A
C
C......................................................................
C
C     Next 6 lines amended                                      Amendment A
      DOUBLE PRECISION A(3,3),BC(3,3),D(3,3)
      INTEGER LAYSS(500),LAYS(50,500),LS(500),LAYERS(500),IN,IOUT,LSORTS
     1,LSS,KOUNT,I,NOTE10,NOTE11,NOTE13,NOTE42,NOTE31,NOTE32,NOTE33,
     2NOTE34,NOTE35,NOTE36,NOTE37,NOTE38,NOTE39,NOTE40,NOTE1,NOTE6,NOTE4
     3,NOTE7,NOTE41,LAYNO
      REAL ZZ(500),AIN(6,6),ALPH(8),E(2,2),G(2,2),PNL(50,12),TT
C                                                        End of Amendment A
C
      LOGICAL ILLCON
      ILLCON=.FALSE.
C
C     SET OUTPUT AND INPUT CHANNELS
      IN=5
      IOUT=6
C
C     THE FOLLOWING TWO LINES ARE OPTIONAL
C     OPEN(UNIT=IN,STATUS='OLD',FILE='A8913IN')
C     OPEN(UNIT=IOUT,STATUS='OLD',FILE='A8913RES')
C
C     CALL SUBROUTINE TO READ IN DATA AND SWITCH TO OUTPUT ROUTINE IF
C     THE ALLOWED ARRAY SIZES ARE EXCEEDED
      CALL B8913A(IN,LSORTS,PNL,LSS,LAYSS,NOTE10,NOTE11,NOTE13)
      IF(NOTE10.EQ.1.OR.NOTE11.EQ.1.OR.NOTE13.EQ.1) GOTO 20
C
C     CALL SUBROUTINE TO DETERMINE LAYER TYPES USED IN LAY-UP
      CALL U8913A(LSS,LAYSS,LAYERS)
C
C     CALL SUBROUTINE TO CHECK THAT PROPERTIES OF EACH LAYER RELATE TO A
C     VAILD ELASTIC MATERIAL. FIRST SET TRIP TO SWITCH TO OUTPUT ROUTINE
C     (NOTE42) IF ANY CHECKS FAIL
      NOTE42=0
      CALL X8913A(LSS,LAYERS,PNL,NOTE31,NOTE32,NOTE33,NOTE34,NOTE35,NOTE
     136,NOTE37,NOTE38,NOTE39,NOTE40,NOTE41,NOTE42,LAYNO)
      IF(NOTE42.EQ.1) GOTO 20
C
C     PREPARE DATA FOR CALL OF C8713A (STIFFNESS CALCULATIONS)
      KOUNT=1
      LS(1)=LSS
      DO 10 I=1,LSS
        LAYS(1,I)=LAYSS(I)
  10  CONTINUE
C
C     CALL SUBROUTINE TO CALCULATE STIFFNESSES
      CALL C8713A(LS,PNL,KOUNT,A,BC,D,LAYS,ZZ,TT)
C
C     CALL SUBROUTINE TO PERFORM THE CALCULATIONS DEPENDENT ON
C     ALL THE PROPERTIES OF THE COMPLETE PLATE
      CALL D8913A(A,BC,D,AIN,NOTE1,ALPH,NOTE6)
      IF(NOTE1.EQ.1.OR.NOTE6.EQ.1) GOTO 20
C
C     CALL SUBROUTINE TO LOOP THROUGH LAYERS CALCULATING AND THEN
C     CUMULATING THEIR VALUES READY FOR SOLUTION OF EQUATION (4)
C
C     Unused variable ZZ deleted from the next line.            Amendment A
      CALL G8913A(PNL,TT,LSS,LAYSS,AIN,ALPH,E,NOTE4)
C
C     Next line amended since variable NOTE5 unused.            Amendment A
      IF(NOTE4.EQ.1) GOTO 20
C
C     INVERT MATRIX OF EQUATION (2) TO OBTAIN G OF EQUATION (1) AND SET
C     NOTE7 IF ILL-CONDITIONED
      NOTE7=0
      CALL Q8913A(E,G,2,ILLCON)
      IF(ILLCON) NOTE7=1
C
C     CALL SUBROUTINE TO OUTPUT ALL INPUT,ERROR MESSAGES AND RESULTS
C
C     Unused variables ZZ, TT and NOTE7 deleted from next line. Amendment A
   20 CALL Z8913A(IOUT,A,BC,D,LAYSS,PNL,LSS,G,NOTE1,NOTE4,NOTE6,
     1LAYERS,NOTE10,NOTE11,NOTE13,NOTE31,NOTE32,NOTE33,NOTE34,NOTE35
     2,NOTE36,NOTE37,NOTE38,NOTE39,NOTE40,NOTE41,NOTE42,LAYNO)
C                                                        End of Amendment A
C
      END
C
C********************************************************************
C     SUBROUTINE B8913A
C 
C     PURPOSE 
C        TO READ IN ALL THE DATA
C     USAGE 
C        CALLED IN PROGRAM A8913A
C     DESCRIPTION OF PARAMETERS 
C        SEE INPUT DATA SECTION OF DATA ITEM NO. 89013
C        LSORTS- NUMBER OF LAYER TYPES  
C        PNL   - ARRAY CONTAINING LAYER PROPERTIES: LIMITED TO 50
C                STORES THE FOLLOWING, IN ORDER, FOR EACH LAYER K:
C                PNL(K,1)=C,PNL(K,2) =EA ,PNL(K,3) =EB ,PNL(K,4) =EZ
C                           PNL(K,5) =VAB,PNL(K,6) =VAZ,PNL(K,7) =VBZ
C                           PNL(K,8) =GBZ,PNL(K,9) =GAZ,PNL(K,10)=GAB
C                           PNL(K,11)=TL, PNL(K,12)=PHI
C                NOTE THAT ORDER OF INPUT IS
C                         C           EA            EB            EZ
C                                     VAB           VBZ           VAZ
C                                     GAB           GBZ           GAZ
C                INPUT ORDER IS SET INTO ARRAY PNL TO ACCOUNT FOR THE
C                DIFFERENCE IN ORDER OF PROPERTIES BETWEEN ABOVE SETS
C        LSS   - NUMBER OF LAYERS IN STACKING SEQUENCE
C        LAYSS - ARRAY STORING LAY-UP CODE FOR LAMINATES
C                SIGN IS SIGN OF PHI
C        NOTE10 =1 IF MORE THAN 50 MATERIALS IN FILE
C        NOTE11 =1 IF MORE THAN 500 LAYERS SPECIFIED
C        NOTE13 =1 IF STACK CALLS UNDECLARED LAYER
C     SUBROUTINES AND FUNCTIONS CALLED
C        NONE
C     METHOD
C        SEE DATA INPUT SECTION OF DATA ITEM NO.89013
C 
C.......................................................................
C 
      SUBROUTINE B8913A(IN,LSORTS,PNL,LSS,LAYSS,NOTE10,NOTE11,NOTE13)
C
C     Next 2 lines added                                        Amendment A
      INTEGER LAYSS(500),JA,KA,J,IJ,LL,NC,M,N
      REAL PNL(50,12)
C
C     DIMENSION statement deleted                               Amendment A
C 
C     SET NOTES TO ZERO
      NOTE10=0
      NOTE11=0
      NOTE13=0
C
C     READ IN NUMBER OF SORTS OF LAYER AND CHECK SIZE LIMIT
      READ(IN,*) LSORTS
      IF(LSORTS.GT.50.OR.LSORTS.EQ.0) THEN
        NOTE10=1
        GOTO 70
      ENDIF
C
C     SET TO ZERO USED PART OF ARRAY PNL
      DO 20 JA=1,LSORTS
        DO 20 KA=1,12
          PNL(JA,KA)=0.0
   20 CONTINUE
C 
C     READ INTO ARRAY PNL LAYER TYPE NUMBER AND PROPERTIES 
      DO 30 J=1,LSORTS
         READ(IN,*)(PNL(J,IJ),IJ=1,4) 
         READ(IN,*) PNL(J,5), PNL(J,7),PNL(J,6)
         READ(IN,*) PNL(J,10),PNL(J,8),PNL(J,9)
         READ(IN,*)(PNL(J,IJ),IJ=11,12)
   30 CONTINUE
C
C     READ IN STACK LENGTH, THAT IS NUMBER OF LAYERS, AND CHECK SIZE
      READ(IN,*) LSS
      IF(LSS.GT.500.OR.LSS.EQ.0) THEN
        NOTE11=1
        GOTO 70
      ENDIF
C
C     SET USED PART OF ARRAY STORING STACK TO ZERO
      DO 40 LL=1,LSS
        LAYSS(LL)=0
   40 CONTINUE
C
C     READ IN STACK
      READ(IN,*)(LAYSS(NC), NC=1,LSS)
C
C     CHECK THAT LAYER TYPES LISTED IN THE STACK HAVE BEEN DECLARED
C     IN ARRAY PNL
      DO 60 M=1,LSS
        DO 50 N=1,LSORTS
          IF((ABS(LAYSS(M))).EQ.(PNL(N,1))) GOTO 60
   50   CONTINUE
        NOTE13=1
        GOTO 70
   60 CONTINUE
C
   70 CONTINUE
C
      RETURN
C 
      END 
C
C***********************************************************************
C     SUBROUTINE X8913A
C
C     PURPOSE
C        TO CHECK THAT THE LAYER PROPERTIES READ IN CAN APPLY TO A
C        VALID ELASTIC MATERIAL SEE SECTION 2.4 OF DATA ITEM 89013
C     USAGE
C        CALLED IN PROGRAM A8913A
C     DESCRIPTION OF PARAMETERS
C        FOR NOTE31 TO NOTE41 SEE MESSAGES PROMPTED IN OUTPUT ROUTINE
C        K    = LAYER NUMBER
C        I    = LAYER TYPE IDENTIFICATION NUMBER
C        LAYNO= LAYER TYPE IDENTIFICATION NUMBER RETAINED TO IDENTIFY
C               A LAYER FAILING A TEST
C     SUBROUTINES AND FUNCTIONS CALLED
C        NONE
C
C.......................................................................
C
      SUBROUTINE X8913A(LSS,LAYERS,PNL,NOTE31,NOTE32,NOTE33,NOTE34,NOTE3
     15,NOTE36,NOTE37,NOTE38,NOTE39,NOTE40,NOTE41,NOTE42,LAYNO)
C
C     Next 2 lines added                                        Amendment A
      REAL PNL(50,12),A,B,C,D,E
      INTEGER LAYERS(500),I,K
C
C     DIMENSION statement deleted                               Amendment A
C
C     SET ALL NOTES TO ZERO
      NOTE31=0
      NOTE32=0
      NOTE33=0
      NOTE34=0
      NOTE35=0
      NOTE36=0
      NOTE37=0
      NOTE38=0
      NOTE39=0
      NOTE40=0
      NOTE41=0
C
C     LOOP THROUGH ALL USED LAYERS CHECKING PROPERTIES
      DO 10 K=1,LSS
C       OMIT DUPLICATED LAYERS AS DICTATED BY ARRAY LAYERS BEING ZERO
        IF(LAYERS(K).NE.0)THEN
          I=IABS(LAYERS(K))
C         CHECK THAT ALL STIFFNESSES ARE POSITIVE
C         Next 6 lines changed, 0 replaced by 0.0001            Amendment A
          IF(PNL(I,2).LE.0.0001) NOTE31=1
          IF(PNL(I,3).LE.0.0001) NOTE32=1
          IF(PNL(I,4).LE.0.0001) NOTE33=1
          IF(PNL(I,8).LE.0.0001) NOTE34=1
          IF(PNL(I,9).LE.0.0001) NOTE35=1
          IF(PNL(I,10).LE.0.0001)NOTE36=1
C
C         CHECK THAT LAYER HAS THICKNESS
C         Next line changed, 0 replaced by 1e-6.                Amendment A
          IF(PNL(I,11).LE.1E-6) NOTE37=1
C         Next line inserted                                    Amendment A
          IF(NOTE31.NE.1.AND.NOTE32.NE.1) THEN
C          CHECK POISSONS RATIO DUAL COMBINATIONS
C          Next 3 lines 0 replaced by 0.0001                    Amendment A
           IF((1.0-(PNL(I,7))**2.0*PNL(I,4)/PNL(I,3)).LE.0.0001)NOTE38=1
           IF((1.0-(PNL(I,6))**2.0*PNL(I,4)/PNL(I,2)).LE.0.0001)NOTE39=1
           IF((1.0-(PNL(I,5))**2.0*PNL(I,3)/PNL(I,2)).LE.0.0001)NOTE40=1
C
C          CHECK ON RATIOS OF PROPERTIES
           A=PNL(I,5)**2.0*PNL(I,3)/PNL(I,2)
           B=PNL(I,7)**2.0*PNL(I,4)/PNL(I,3)
           C=PNL(I,6)**2.0*PNL(I,4)/PNL(I,2)
           D=2.0*PNL(I,5)*PNL(I,6)*PNL(I,7)
           D=D*PNL(I,3)*PNL(I,4)/(PNL(I,2)*PNL(I,3))
           E=1.0-A-B-C-D
C
C          Next line changed, 0 replaced by 0.0001              Amendment A
           IF(E.LE.0.0001) NOTE41=1
C          Next line inserted                                   Amendment A
          ENDIF
C
C       RECORD LAYER NUMBER IN CASE IT IS A FAILURE CASE AND SET NOTE42
C       IF ANY OF ABOVE TESTS IS FAILED AND RETURN
          LAYNO=I
          IF((NOTE31+NOTE32+NOTE33+NOTE34+NOTE35+NOTE36+NOTE37+NOTE38+NO
     1TE39+NOTE40+NOTE41).GT.0) THEN
          NOTE42=1
C
          RETURN
C
        ENDIF
C
      ENDIF
C
   10 CONTINUE
C
      RETURN
C
      END
C
C***********************************************************************
C     SUBROUTINE C8713A
C 
C     PURPOSE 
C        TO CALCULATE STIFFNESSES INCLUDING THE IN-PLANE,
C        FLEXURAL AND COUPLING STIFFNESSES FROM LAYER PROPERTIES.
C        NOTE: THIS SUBROUTINE IS IDENTICAL TO SUBROUTINE S05STF IN
C        DATA ITEM NO. 86020, ESDUpac A8620 (Previously ESDUpac 1051).
C        THE SUBROUTINE ALSO APPEARS IN ESDUpac A8815.
C     USAGE 
C        CALL C8713A(LS,PNL,KOUNT,A,BC,D,LAYS,ZZ,TT)
C        CALLED IN PROGRAM A8913, DATA ITEM NO.89013
C     DESCRIPTION OF PARAMETERS 
C        KOUNT -NUMBER OF PLATE(S) IN DATA SEQUENCE-LIMITED TO 100
C        LS    -ARRAY OF NUMBER OF LAYERS IN LAMINATE FOR EACH PLATE  
C               IN SEQUENCE -LIMITED TO 50 PLATES
C        LAYS  -ARRAY OF STACKING SEQUENCES (ORDERED WITH LS) 50 OFF
C               LIMITED TO 500 LAYERS IN EACH
C        PNL   -ARRAY STORING LAYER PROPERTIES -LIMITED TO 50. DATA
C               MUST BE IN THE FOLLOWING ORDER:-   LAYER IDENTIFICATION 
C               NUMBER, EAA,EBB,EZZ,VAB,VAZ,VBZ,GBZ,GAZ,GAB,THICKNESS
C               ANGLE (DEGREES, ALWAYS POSITIVE)
C        A     -IN-PLANE STIFFNESS MATRIX 
C        BC    -COUPLED IN-PLANE AND FLEXURAL STIFFNESS MATRIX
C        D     -FLEXURAL STIFFNESS MATRIX 
C        ST    -CUMULATIVE LAYER THICKNESS FROM ONE SIDE
C               INITIALLY SET TO ZERO 
C        K     -CURRENT LAYER NUMBER IN CALCULATION 
C        KS    -CURRENT LAYER TYPE IDENTIFICATION NUMBER
C        PHI   -ANGLE OF LAYER AXIS RADIANS 
C        ZK    -DISTANCE OF LAYER MID-PLANE FROM MID-PLANE OF PLATE
C        T     -LAYER THICKNESS 
C        TT    -TOTAL THICKNESS OF LAMINATE
C        AT,DT -LARGEST TERMS IN A AND D RESPECTIVELY 
C        AZ    -TERMS OF A WHICH ARE ZERO 
C        BCZ   -TERMS OF B WHICH ARE ZERO 
C        DZ    -TERMS OF D WHICH ARE ZERO 
C        ZZ    -ARRAY OF LAYER Z VALUES WITH RESPECT TO PLATE MID PLANE
C     REMARKS 
C        NONE 
C     SUBROUTINES AND FUNCTIONS REQUIRED  
C        NONE 
C     METHOD
C        METHOD FOLLOWS DATA ITEM NO.75002. THE COMMENTS 
C        INDICATE THE EQUATION NUMBERS IN THAT DATA ITEM. 
C 
C.......................................................................
C 
      SUBROUTINE C8713A(LS,PNL,KOUNT,A,BC,D,LAYS,ZZ,TT)
C
C     Next 4 lines changed                                      Amendment A
      DOUBLE PRECISION CK(3,3),TK(3,3),TKT(3,3),TKTCK(3,3),BK(3,3),
     1A(3,3),BC(3,3),D(3,3)
      REAL PNL(50,12),ZZ(500),ST,RO,PHI,ZK,T,AT,DT,BCT,AZ,BCZ,DZ,BALL
      INTEGER LAYS(50,500),LS(500),I,J,K,KS,M,N
C                                                        End of Amendment A
C 
C     CALCULATE TOTAL PLATE THICKNESS TT 
C
      TT=0.0
C
      DO 10 I=1,LS(KOUNT) 
         J=IABS(LAYS(KOUNT,I)) 
         TT=TT+PNL(J,11)
   10 CONTINUE
C 
C     BUILD MATRICES RELATING STRAIN COMPONENTS IN A LAYER TK (EQ(2)) 
C     AND ELEMENTS OF A,B AND C MATRICES FOR EACH LAYER AND CUMULATE
C     FOR THE PLATE (EQS(2),(3)AND(5))
C     FIRST SET CUMULATIVE THICKNESS FROM ONE SIDE TO ZERO AND
C     SET MATRICES TO ZERO BEFORE CALCULATION OF STIFFNESSES
C
      ST=0.0
C
      DO 20 I=1,3 
         DO 20 J=1,3
            CK(I,J)=0.0 
            A(I,J)=0.0
            BC(I,J)=0.0 
            D(I,J)=0.0
   20 CONTINUE
C 
C     LOOP THROUGH LAYERS PICKING APPROPRIATE LAYER ACCORDING TO CODED  
C     STRING LAYS FROM PNL
C
      DO 90 K=1,LS(KOUNT)
C 
C        BUILD SINGLE LAYER STRESS STRAIN RELATIONSHIP MATRIX CK (EQ(1))
C
         KS=IABS(LAYS(KOUNT,K))
         RO=1-PNL(KS,5)*PNL(KS,5)*PNL(KS,3)/PNL(KS,2)
C
         CK(1,1)=PNL(KS,2)/RO
         CK(2,2)=PNL(KS,3)/RO
         CK(3,3)=PNL(KS,10) 
         CK(2,1)=PNL(KS,5)*PNL(KS,3)/RO
         CK(1,2)=CK(2,1) 
C 
C     BUILD TK MATRIX(EQ(2))
C
         PHI=PNL(KS,12)*0.0174533*KS/LAYS(KOUNT,K)
C
         TK(1,1)= COS(PHI)*COS(PHI) 
         TK(2,2)= TK(1,1)
         TK(1,2)= SIN(PHI)*SIN(PHI)
         TK(2,1)= TK(1,2)
         TK(1,3)= 0.5*SIN(2*PHI) 
         TK(2,3)= -TK(1,3) 
         TK(3,2)= SIN(2*PHI) 
         TK(3,1)= -TK(3,2) 
         TK(3,3)= COS(2*PHI) 
C 
C     BUILD TRANSPOSED TK MATRIX TKT
C
         DO 30 M=1,3 
            DO 30 N=1,3 
               TKT(M,N)=TK(N,M)
   30    CONTINUE
C 
C     CALCULATE LAYER MATRIX BK (EQ(3)) 
         DO 50 I=1,3
            DO 50 J=1,3
               TKTCK(I,J)=0.0
C
               DO 40 M=1,3
                  TKTCK(I,J)=TKTCK(I,J)+TKT(I,M)*CK(M,J)
   40          CONTINUE
C
   50    CONTINUE
C
         DO 70 I=1,3
            DO 70 J=1,3
               BK(I,J)=0.0 
C
               DO 60 M=1,3
                  BK(I,J)=BK(I,J)+TKTCK(I,M)*TK(M,J)
   60          CONTINUE
C
   70    CONTINUE
C 
C        CALCULATE LAYER DISTANCE FROM NEUTRAL AXIS OF PLATE ZK 
C        CALCULATED TO MID-PLANE OF PLATE 
C
         ST=ST+PNL(KS,11)
         ZK=0.5*TT-ST+PNL(KS,11)/2
         ZZ(K)=ZK
C 
C        CALCULATE LAYER A, B, D  VALUES 
C
         T=PNL(KS,11)
C
         DO 80 M=1,3
            DO 80 N=1,3 
               A(M,N)=A(M,N)+T*BK(M,N) 
C        ***NOTE*** SIGN IN FOLLOWING LINE IS POSITIVE. THIS FOLLOWS THE
C                   SIGN CONVENTION OF DERIVATION 1 WHICH IS CONTRARY TO
C                   THAT OF DATA ITEM NO.75002. THIS SIGN CONVENTION
C                   DOES NOT INFLUENCE THE SIGN OF THE SHEAR STIFFNESSES
C                   SEE NOTES IN DATA ITEM NO.89013. THE SIGN OF THE B
C                   STIFFNESS MATRIX OUTPUT IS CORRECTED TO THE
C                   CONVENTION OF DATA ITEM NO.75002 IN THE OUTPUT
C                   SUBROUTINE
               BC(M,N)=BC(M,N)+T*ZK*BK(M,N)
               D(M,N) =D(M,N)+(T*ZK*ZK+T*T*T/12.0)*BK(M,N)
   80    CONTINUE
C 
   90 CONTINUE
C 
C     FIND LARGEST TERM IN STIFFNESS SUB-MATRICES (AT OF A, DT OF D, AND
C     BCT THE GEOMETRIC MEAN OF AT AND DT)
C
      AT=0.0
      DT=0.0
C
      DO 110 I=1,3 
         DO 110 J=1,3 
            IF(ABS(A(I,J)).GT.AT) AT=ABS(A(I,J))
            IF(ABS(D(I,J)).GT.DT) DT=ABS(D(I,J))
  110 CONTINUE
C
      BCT=SQRT(AT*DT) 
C 
C     ZEROISE NEGLIGIBLE TERMS OF STIFFNESS SUB-MATRICES (MADE ZERO IF  
C     6 ORDERS LESS THAN AT, BCT OR DT FOR A, BC AND D RESPECTIVELY)
C     ALSO SUM TERMS WHICH SHOULD BE ZERO FOR SPECIAL ORTHOTROPY
C
      AZ  =0.0
      BCZ =0.0 
      DZ  =0.0
      BALL=0.0
C
      DO 120 I=1,3
         DO 120 J=1,3 
            IF(((ABS(A(I,J)))/AT).LT.10E-6) A(I,J)=0.0
            IF(((ABS(BC(I,J)))/BCT).LT.10E-6)BC(I,J)=0.0
            IF(((ABS(D(I,J)))/DT).LT.10E-6)D(I,J)=0.0 
  120 CONTINUE
C 
      RETURN
C 
      END 
C
C********************************************************************
C     SUBROUTINE D8913A
C
C     PURPOSE
C        TO CALL SUBROUTINES TO PERFORM THE CALCULATIONS DEPENDENT ON
C        OVERALL PLATE PROPERTIES
C     USAGE
C        CALLED IN PROGRAM A8913A
C     DESCRIPTION OF PARAMETERS
C        NO SPECIAL PARAMETERS USED TO CALL OTHER SUBROUTINES ONLY
C     SUBROUTINES AND FUNCTIONS USED
C        E8913A    F8913A
C
C......................................................................
C
      SUBROUTINE D8913A(A,BC,D,AIN,NOTE1,ALPH,NOTE6)
C
      DIMENSION A(3,3),BC(3,3),D(3,3),AIN(6,6),ALPH(8)
C
      DOUBLE PRECISION A,BC,D
C
C     CALL SUBROUTINE TO BUILD ARRAY AA OF EQUATION (21) AND INVERT IT
C     TO GIVE ARRAY AIN
      CALL E8913A(A,BC,D,AIN,NOTE1)
C     
C     CHECK NOTE1, IF UNITY INDICATES ILL-CONDITIONING OF INVERSION
      IF(NOTE1.EQ.1) RETURN
C
C     CALL SUBROUTINE TO CALCULATE ALPHA TERMS OF EQUATIONS 11A AND 11B
      CALL F8913A(AIN,ALPH,NOTE6)
C
      RETURN
C
      END
C
C**********************************************************************
C     SUBROUTINE E8913A
C
C     PURPOSE
C        TO COMPILE THE MATRIX OF EQUATION (21) FROM A,BC AND D
C        AND THEN TO INVERT TO OBTAIN MATRIX AIN
C     USAGE
C        CALLED IN SUBROUTINE D8913A
C     DESCRIPTION OF PARAMETERS 
C         AA   FULL PLATE STIFFNESS MATRIX (6 BY 6)
C         AIN  INVERTED PLATE STIFFNESS MATRIX
C     SUBROUTINES AND FUNCTIONS REQUIRED
C        Q8913A
C     REMARKS
C        NONE
C
C.......................................................................
C
      SUBROUTINE E8913A(A,BC,D,AIN,NOTE1)
C
C     NOTE AA AND AIN NOT DOUBLE PRECISION TO AVOID PROBLEMS IN
C     MATRIX INVERSION ROUTINE
C
C     Next 2 lines added.                                       Amendment A
      REAL    AA
      INTEGER I,J
C
      DOUBLE PRECISION A,BC,D
      DIMENSION A(3,3),BC(3,3),D(3,3),AA(6,6),AIN(6,6)
      LOGICAL ILLCON
      ILLCON=.FALSE.
C
C     SET NOTE1 INDICATING CONDITION OF MATRIX TO ZERO
      NOTE1=0
C
C     LOOP THROUGH A,BC AND D MATRICES AND BUILD FULL 6 BY 6 PLATE
C     STIFFNESS MATRIX AA
      DO 10 I=1,3
         DO 10 J=1,3
            AA(I,J)    =A(I,J)
            AA(I+3,J+3)=D(I,J)
            AA(I+3,J)  =BC(I,J)
            AA(I,J+3)  =BC(I,J)
   10 CONTINUE
C
C     CALL SUBROUTINE TO INVERT MATRIX AA
      CALL Q8913A(AA,AIN,6,ILLCON)
C
C     RESET NOTE1 IF AA IS ILL-CONDITIONED
      IF(ILLCON) NOTE1=1
C
      RETURN
C
      END
C
C**********************************************************************
C     SUBROUTINE F8913A         
C
C     PURPOSE
C        SEE EQUATIONS 11A AND 11B
C        TO CONSTRUCT THE 8 ELEMENTS OF ARRAY ALPH FROM THE 6 BY 6
C        ARRAY AIN
C
C     USAGE
C        CALLED IN SUBROUTINE D8913A
C
C     REMARKS
C        NOTE6 IS SET TO UNITY IF DIVISION BY A DENOMINATOR OF
C        THIRTY ORDERS LESS THAN THE NUMERATOR IS IMMINENT AND
C        RETURN TO CALL IS IMPLEMENTED
C     SUBROUTINES AND FUNCTIONS CALLED 
C        NONE
C
C......................................................................
      SUBROUTINE F8913A(AIN,ALPH,NOTE6)
C
C     Next Line added                                           Amendment A
      REAL RG,DENOM,DENOMM
C
      DIMENSION AIN(6,6),ALPH(8)
C
C     SET NOTE6 TO ZERO
      NOTE6=0
C
C     SET MACHINE ACCURACY RANGE CHECK LIMIT RG
      RG=1E+30
C
C     SOLUTION OF EQUATION 11A
C     EVALUATE DENOMINATOR
      DENOM=AIN(5,5)*AIN(2,2)-AIN(2,5)**2.0
C
      ALPH(1)=(AIN(4,5)*AIN(2,5)-AIN(5,5)*AIN(1,5))
      IF(ABS(ALPH(1)).GT.(RG*ABS(DENOM))) GOTO 10
      ALPH(1)=ALPH(1)/DENOM
      ALPH(2)=(AIN(5,6)*AIN(2,5)-AIN(5,5)*AIN(2,6))
      IF(ABS(ALPH(2)).GT.(RG*ABS(DENOM))) GOTO 10
      ALPH(2)=ALPH(2)/DENOM
      ALPH(3)=(AIN(1,5)*AIN(2,5)-AIN(2,2)*AIN(4,5))
      IF(ABS(ALPH(3)).GT.(RG*ABS(DENOM))) GOTO 10
      ALPH(3)=ALPH(3)/DENOM
      ALPH(4)=(AIN(2,6)*AIN(2,5)-AIN(2,2)*AIN(5,6))
      IF(ABS(ALPH(4)).GT.(RG*ABS(DENOM))) GOTO 10
      ALPH(4)=ALPH(4)/DENOM
C
C     SOLUTION OF EQUATION 11B
C     EVALUATE DENOMINATOR
      DENOMM=AIN(4,4)*AIN(1,1)-AIN(1,4)**2.0
C
      ALPH(5)=(AIN(4,6)*AIN(1,4)-AIN(4,4)*AIN(1,6))
      IF(ABS(ALPH(5)).GT.(RG*ABS(DENOMM))) GOTO 10
      ALPH(5)=ALPH(5)/DENOMM
      ALPH(6)=(AIN(4,5)*AIN(1,4)-AIN(4,4)*AIN(1,5))
      IF(ABS(ALPH(6)).GT.(RG*ABS(DENOMM))) GOTO 10
      ALPH(6)=ALPH(6)/DENOMM
      ALPH(7)=(AIN(1,6)*AIN(1,4)-AIN(1,1)*AIN(4,6))
      IF(ABS(ALPH(7)).GT.(RG*ABS(DENOMM))) GOTO 10
      ALPH(7)=ALPH(7)/DENOMM
      ALPH(8)=(AIN(1,5)*AIN(1,4)-AIN(1,1)*AIN(4,5))
      IF(ABS(ALPH(8)).GT.(RG*ABS(DENOMM))) GOTO 10
      ALPH(8)=ALPH(8)/DENOMM
C
      RETURN
C
  10  NOTE6=1
C
      RETURN
C
      END
C 
C**********************************************************************
C     SUBROUTINE G8913A
C
C     PURPOSE
C        TO LOOP THROUGH SUBROUTINES WHICH CALCULATE LAYER DEPENDENT
C        TERMS AND SUM THEM AS NECESSARY
C     USAGE
C        CALLED BY PROGRAM A8913A
C
C     DESCRIPTION OF PARAMETERS
C        K     LAYER NUMBER IN PLATE
C        LSS   TOTAL NUMBER OF LAYERS IN PLATE
C        TT    TOTAL THICKNESS OF PLATE
C        ZZ    ARRAY OF Z VALUES FOR LAYERS
C        PNL   ARRAY CONTAINING LAYER PROPERTIES (SEE INPUT ROUTINE)
C        AIN AND ALPH - SEE PREVIOUS ARRAYS
C        E     INVERSE OF SHEAR STIFFNESS MATRIX SOUGHT
C
C     SUBROUTINES AND FUNCTIONS USED
C        H8913A  L8913A  M8913A  N8913A  08913A 
C        P8913A  Q8913A  R8913A  S8913A  T8913A
C        T8913A
C     REMARKS
C        NONE
C
C......................................................................
C
C     Unused variable ZZ deleted from next line.                Amendment A
      SUBROUTINE G8913A(PNL,TT,LSS,LAYSS,AIN,ALPH,E,NOTE4)
C
C     Next 4 lines changed                                      Amendment A
      REAL S(6,6),DIS(6,6),DPRIM(5,5),RK(2,2),Q(3,3),R(2,2,500),
     1PNL(50,12),W(500,4),H(500,4),CMK(500,4),
     2Z(500,2),AIN(6,6),ALPH(8),THI(4,4,500),E(2,2),AL(16),SN,CM
      INTEGER LAYSS(500),K,II,JJ
C                                                        End of Amendment A
C
      LOGICAL ILLCON
      ILLCON=.FALSE.
C
C     SET ALL NOTES TO ZERO
      NOTE4=0
C
C     LOOP THROUGH LAYERS CALCULATING ALL LAYER DEPENDENT PARAMETERS
C     K IS LAYER NUMBER IN PLATE
      DO 100, K=1,LSS
C       CALL SUBROUTINE TO CALCULATE LAYER S MATRIX USING EQUATION (16)
C
C     Unrequired variable LSS deleted from next line.           Amendment A
        CALL H8913A(K,PNL,LAYSS,S,SN,CM)
C
C       CALL SUBROUTINE TO INVERT S TO DIS, EQUATION (14)
        CALL Q8913A(S,DIS,6,ILLCON)
        IF(ILLCON) NOTE4=1
        IF(NOTE4.EQ.1) RETURN
C
C       CALL SUBROUTINE TO CALCULATE DPRIME TERMS OF EQUATION (15) USING
C       EQUATIONS (13)
        CALL L8913A(DIS,DPRIM)
C
C       CALL SUBROUTINE TO CALCULATE Q TERMS OF EQUATION (12)
        CALL M8913A(DPRIM,CM,SN,Q)
C
C       CALL SUBROUTINE TO OBTAIN R FROM EQUATIONS (18),(19) AND (20)
        CALL N8913A(DPRIM,RK,SN,CM)
C
C       STACK LAYER R(K) ARRAY INTO THREE-DIMENSIONAL ARRAY IN WHICH
C       THE THIRD DIMENSION REPRESENTS THE LAYERS
        DO 50 II=1,2
          DO 50 JJ=1,2
            R(II,JJ,K)=RK(II,JJ)
   50   CONTINUE
C
C       CALL SUBROUTINE TO SOLVE L (AL) TERMS OF EQUATIONS (9) AND (10)
        CALL O8913A(Q,AIN,AL)
C
C       CALL SUBROUTINE TO SOLVE H AND W TERMS OF EQUATIONS (7) AND (8)
        CALL P8913A(K,H,ALPH,W,AL)
C
  100 CONTINUE
C
C     CALL SUBROUTINE TO CALCULATE C(M,K) TERMS OF EQUATION (6)
      CALL R8913A(TT,H,W,PNL,LSS,LAYSS,Z,CMK)
C
C     CALL SUBROUTINE TO CALCULATE THI TERMS OF EQUATION (5)
C
C     Unrequired variables TT,PNL and LAYSS deleted from next line. Amend A
      CALL S8913A(LSS,H,W,Z,CMK,THI)
C
C     CALL SUBROUTINE TO CALCULATE E TERMS OF EQUATION (4)
      CALL T8913A(LSS,R,THI,E)
C
      RETURN
C
      END
C
C**********************************************************************
C     SUBROUTINE H8913A
C
C     PURPOSE
C        TO BUILD THE COMPLIANCE MATRIX S OF EQUATION (16) USING
C        EQUATION (17) FOR THE CURRENT LAYER
C     USAGE
C        CALLED BY G8913A
C     DESCRIPTION OF PARAMETERS
C        K    =LAYER NUMBER IN CURRENT STACK
C        S    =COMPLIANCE MATRIX FOR LAYER
C        SIGN =SIGN OF ANGLE OF CURRENT LAYER WITH RESPECT TO AXIS
C        PHI  =ANGLE OF CURRENT LAYER IN RADIANS
C        SN   =SINE OF ANGLE
C        CM   =COSINE OF ANGLE
C
C     SOBROUTINES AN FUNCTIONS CALLED
C        NONE
C     REMARKS
C        WHERE THE LAYER ANGLE IS CLOSE TO 0 OR 90 DEGREES (OR
C        MULTIPLES OF 90) SN AND CM ARE ADJUSTED TO 1 OR 0 AS
C        APPROPRIATE
C
C......................................................................
C
C     Unrequired variabler LSS deleted from next line.          Amendment A
      SUBROUTINE H8913A(K,PNL,LAYSS,S,SN,CM)
C
C     Next 2 lines added                                        Amendment A
      REAL    PHI
      INTEGER I,J,N
C                                                        End of Amendment A
      DIMENSION PNL(50,12),LAYSS(500),S(6,6)
C
C     SET TERMS OF ARRAY S TO ZERO
      DO 10 I=1,6
        DO 10 J=1,6
          S(I,J)=0.0
  10  CONTINUE
C
C     SET N TO CURRENT LAYER TYPE NUMBER IN STACK
      N=IABS(LAYSS(K))
C
C     CALCULATE TERMS OF A LAYER COMPLIANCE MATRIX ACCORDING TO
C     EQUATION (17) AND COMPILE THE MATRIX OF EQUATION (16) FOR
C     FOR LAYER K IN ARRAY LAYS AND CALCULATE SINE AND COSINE OF
C     ANGLE OF LAYER FOR LATER USE
C     N=PLATE REFERENCE NUMBER (ROW NUMBER) IN ARRAY PNL FOR CURRENT
C     PLATE 
        S(1,1)= 1.0/(PNL(N,2))
        S(2,2)= 1.0/(PNL(N,3))
        S(3,3)= 1.0/(PNL(N,4))
        S(1,2)=-PNL(N,5)/(PNL(N,2))
        S(2,1)= S(1,2)
        S(1,3)=-PNL(N,6)/(PNL(N,2))
        S(3,1)= S(1,3)
        S(2,3)=-PNL(N,7)/(PNL(N,3))
        S(3,2)= S(2,3)
        S(4,4)= 1.0/(PNL(N,8))
        S(5,5)= 1.0/(PNL(N,9))
        S(6,6)= 1.0/(PNL(N,10))
C
C     OBTAIN SINE (SN) AND COSINE (CM) OF ANGLE OF LAYER
      SIGN=N/LAYSS(K)
      PHI=PNL(N,12)*0.0174533*SIGN
      SN=SIN(PHI)
      CM=COS(PHI)
C      
C     INSERT TRAP FOR PHI 0 OR 90 DEGREES CASES
      IF(ABS(SN).LT.1E-6) SN=0.0
      IF(ABS(CM).LT.1E-6) CM=0.0
      IF(ABS(SN).GT.0.999999) SN=1.0
      IF(ABS(CM).GT.0.999999) CM=1.0
C 
      RETURN
C
      END
C
C***********************************************************************
C     SUBROUTINE L8913A
C
C     PURPOSE
C        TO BUILD MATRIX OF EQUATION (15) FROM MATRIX OF EQUATION (14)
C        AND EQUATIONS (13)
C     USAGE
C        CALLED IN SUBROUTINE G8913A
C
C     SUBROUTINES AND FUNCTIONS CALLED
C        NONE
C
C......................................................................
C
      SUBROUTINE L8913A(DIS,DPRIM)
C
C     Next line added                                           Amendment A
      INTEGER I,J
C
      DIMENSION DIS(6,6),DPRIM(5,5)
C
C     SET TERMS TO ZERO
      DO 10 I=1,5
        DO 10 J=1,5
          DPRIM(I,J)=0.0
   10 CONTINUE
C
      DPRIM(1,1)=DIS(1,1)-DIS(1,3)**2.0/DIS(3,3)
      DPRIM(1,2)=DIS(1,2)-DIS(1,3)*DIS(2,3)/DIS(3,3)
      DPRIM(2,2)=DIS(2,2)-DIS(2,3)**2.0/DIS(3,3)
C
      DPRIM(2,1)=DPRIM(1,2)
      DPRIM(3,3)=DIS(4,4)
      DPRIM(4,4)=DIS(5,5)
      DPRIM(5,5)=DIS(6,6)
C
      RETURN
C
      END
C
C***********************************************************************
C
C     SUBROUTINE M8913A
C
C     PURPOSE
C        SEE EQUATION 12
C     USAGE
C        CALLED IN PROGRAM A8913A
C
C     SUBROUTINES AND FUNCTIONS CALLED
C        NONE
C
C.......................................................................
C  
      SUBROUTINE M8913A(DPRIM,CM,SN,Q)
C
      DIMENSION Q(3,3),DPRIM(5,5)
C
      Q(1,1)=DPRIM(1,1)*(CM**4.0)+2.0*(DPRIM(1,2)+2.0*DPRIM(5,5))*(CM**2
     1.0)*(SN**2.0)+DPRIM(2,2)*(SN**4.0)
      Q(2,2)=DPRIM(1,1)*(SN**4.0)+2.0*(DPRIM(1,2)+2.0*DPRIM(5,5))*(CM**2
     1.0)*(SN**2.0)+DPRIM(2,2)*(CM**4.0)
      Q(1,2)=(DPRIM(1,1)+DPRIM(2,2)-4.0*DPRIM(5,5))*(CM**2.0)*(SN**2.0)+
     1DPRIM(1,2)*((CM**4.0)+(SN**4.0))
      Q(1,3)=(DPRIM(1,1)-DPRIM(1,2)-2.0*DPRIM(5,5))*(CM**3.0)*SN+(DPRIM(
     11,2)-DPRIM(2,2)+2.0*DPRIM(5,5))*CM*(SN**3.0)
      Q(2,3)=(DPRIM(1,1)-DPRIM(1,2)-2.0*DPRIM(5,5))*CM*(SN**3.0)+(DPRIM(
     11,2)-DPRIM(2,2)+2.0*DPRIM(5,5))*(CM**3.0)*SN
      Q(3,3)=(DPRIM(1,1)+DPRIM(2,2)-2.0*DPRIM(1,2)-2.0*DPRIM(5,5))*(CM**
     12.0)*(SN*2.0)+DPRIM(5,5)*((CM**4.0)+(SN**4.0))
C
      Q(2,1)=Q(1,2)
      Q(3,2)=Q(2,3)
      Q(3,1)=Q(1,3) 
C
      RETURN
C
      END
C
C***********************************************************************
C     SUBROUTINE N8913A
C
C     PURPOSE 
C       TO OBTAIN R(K) TERMS FROM EQUATIONS (18), (19) AND (20)
C       FOR LAYER K
C     USAGE
C       CALLED IN SUBROUTINE G8913A
C       
C     SUBROUTINES AND FUNCTIONS REQUIRED
C       Q8913A
C
C.......................................................................
C
      SUBROUTINE N8913A(DPRIM,RK,SN,CM)
C
C     Next line added                                           Amendment A
      REAL Q1
C
      LOGICAL ILLCON
      DIMENSION DPRIM(5,5),RK(2,2),Q1(2,2)
C
C     TO OBTAIN R(K) TERMS FROM EQUATIONS (18),(19),(20)
      Q1(1,1) =  DPRIM(3,3)*CM**2.0 + DPRIM(4,4)*SN**2.0
      Q1(1,2) = -DPRIM(3,3)*CM*SN   + DPRIM(4,4)*CM*SN
      Q1(2,2) =  DPRIM(3,3)*SN**2.0 + DPRIM(4,4)*CM**2.0
      Q1(2,1) =  Q1(1,2)
C
C     CALL SUBROUTINE TO INVERT Q1 TO GIVE R(K)
      CALL Q8913A(Q1,RK,2,ILLCON)
C
      RETURN
C
      END
C
C***********************************************************************
C     SUBROUTINE O8913A
C
C     PURPOSE
C        TO SOLVE EQUATIONS (9) AND (10)
C     USAGE
C        CALLED IN PROGRAM A8913A
C     SUBROUTINES AND FUNCTIONS CALLED
C        NONE
C
C.......................................................................
C
      SUBROUTINE O8913A(Q,AIN,AL)
C
      DIMENSION Q(3,3),AIN(6,6),AL(16)
C
C     SOLUTION OF EQUATION (9)
      AL(1)=Q(1,1)*AIN(1,2)+Q(1,2)*AIN(2,2)+Q(1,3)*AIN(2,3)
      AL(2)=Q(1,1)*AIN(1,4)+Q(1,2)*AIN(1,5)+Q(1,3)*AIN(1,6)
      AL(3)=Q(1,1)*AIN(1,5)+Q(1,2)*AIN(2,5)+Q(1,3)*AIN(2,6)
      AL(4)=Q(1,1)*AIN(1,6)+Q(1,2)*AIN(2,6)+Q(1,3)*AIN(3,6)
      AL(5)=AL(3)
      AL(6)=Q(1,1)*AIN(4,4)+Q(1,2)*AIN(4,5)+Q(1,3)*AIN(4,6)
      AL(7)=Q(1,1)*AIN(4,5)+Q(1,2)*AIN(5,5)+Q(1,3)*AIN(5,6)
      AL(8)=Q(1,1)*AIN(4,6)+Q(1,2)*AIN(5,6)+Q(1,3)*AIN(6,6)
C
C     SOLUTION OF EQUATION (10)
      AL(9) =Q(1,2)*AIN(1,1)+Q(2,2)*AIN(1,2)+Q(2,3)*AIN(1,3)
      AL(10)=Q(1,2)*AIN(1,4)+Q(2,2)*AIN(1,5)+Q(2,3)*AIN(1,6)
      AL(11)=Q(1,2)*AIN(1,5)+Q(2,2)*AIN(2,5)+Q(2,3)*AIN(2,6)
      AL(12)=Q(1,2)*AIN(1,6)+Q(2,2)*AIN(2,6)+Q(2,3)*AIN(3,6)
      AL(13)=AL(10)
      AL(14)=Q(1,2)*AIN(4,4)+Q(2,2)*AIN(4,5)+Q(2,3)*AIN(4,6)
      AL(15)=Q(1,2)*AIN(4,5)+Q(2,2)*AIN(5,5)+Q(2,3)*AIN(5,6)
      AL(16)=Q(1,2)*AIN(4,6)+Q(2,2)*AIN(5,6)+Q(2,3)*AIN(6,6)
C
      RETURN
C
      END
C
C**********************************************************************
C     SUBROUTINE P8913A
C
C     PURPOSE
C        TO SOLVE EQUATIONS (7) AND (8)
C     USAGE
C        CALLED IN PROGRAM A8913A
C     SUBROUTINES AND FUNCTIONS CALLED
C        NONE
C
C.......................................................................
C
      SUBROUTINE P8913A(K,H,ALPH,W,AL)
C
      DIMENSION H(500,4),ALPH(8),W(500,4),AL(16)
C
C     SOLUTION OF EQUATION (7)
      H(K,1)=ALPH(1)*AL(1)+ALPH(3)*AL(3) +AL(2)
      H(K,2)=ALPH(2)*AL(1)+ALPH(4)*AL(3) +AL(4)
      H(K,3)=ALPH(5)*AL(9)+ALPH(7)*AL(10)+AL(12)
      H(K,4)=ALPH(6)*AL(9)+ALPH(8)*AL(10)+AL(11)
C     
C     SOLUTION OF EQUATION (8)
      W(K,1)=ALPH(1)*AL(3) +ALPH(3)*AL(7) +AL(6)
      W(K,2)=ALPH(2)*AL(3) +ALPH(4)*AL(7) +AL(8)
      W(K,3)=ALPH(5)*AL(10)+ALPH(7)*AL(14)+AL(16)
      W(K,4)=ALPH(6)*AL(10)+ALPH(8)*AL(14)+AL(15)
C
      RETURN
C
      END
C
C***********************************************************************
C     SUBROUTINE R8913A
C
C     PURPOSE
C       TO CALCULATE C(M,K) TERMS OF EQUATION (6)
C       DISTANCES OF LAYER INTERFACES FROM TOP (DATUM) SURFACE OF PLATE
C       ARE FIRST CALCULATED AND STORED IN ARRAY Z. C(M,K) IS THEN
C       CALCULATED
C     USAGE
C       CALLED IN SUBROUTINE G8913A
C
C     SUBROUTINES AND FUNCTIONS CALLED
C       NONE
C
C......................................................................
C
      SUBROUTINE R8913A(TT,H,W,PNL,LSS,LAYSS,Z,CMK)
C
C     Next 3 lines added                                        Amendment A
      REAL    H(500,4),W(500,4),CMK(500,4),ACMK(500,4),BCMK(500,4),ZS
     1       ,AACMK,BBCMK
      INTEGER MX,MY,I,J,LT,M,K
C
C     1 DIMENSION statement deleted                             Amendment A
      DIMENSION PNL(50,12),LAYSS(500),Z(500,2)
C
C     SET ARRAYS CMK AND L TO ZERO
      DO 10 MX=1,LSS
         Z(MX,1)=0.0
         Z(MX,2)=0.0
         DO 10 MY=1,4
           CMK(MX,MY)=0.0
   10 CONTINUE
C
C     SET UP LOOPS THROUGH LAYERS TO CALCULATE Z(PRIME K)=(I,1) AND
C     Z(PRIME K+1)=(I,2) OF EQUATIONS
      DO 30 I=1,LSS
        ZS=0.0
C       CALCULATE Z(K) DISTANCE OF LOWER FACE OF A LAYER FROM THE TOP
C       (DATUM) SURFACE OF THE PLATE
        DO 20 J=1,I
C         FIND LAYER TYPES (ABSOLUTE BECAUSE LAYSS TERMS HAVE SIGN FOR
C         PHI AS PRIFIX) AND CUMULATE THICKNESSES DOWN TO AND 
C         INCLUDING CURRENT LAYER
          LT=IABS(LAYSS(J))
          ZS=ZS+PNL(LT,11)
   20   CONTINUE
C
C       STORE AS Z(PRIME K) TOP SURFACE AND Z(PRIME K+1) LOWER SURFACE
C       IN Z
        Z(I,1)=(TT/2.0)-ZS+PNL((IABS(LAYSS(I))),11)
        Z(I,2)=(TT/2.0)-ZS
   30 CONTINUE
C
C     CALCULATE TWO PARTS OF C(MK) USING EQUATION (6) AND COMBINE THEM
C     SET CUMULATORS TO ZERO FOR EACH VALUE OF M
      DO 60 M=1,4
        AACMK=0.0
        BBCMK=0.0
C
C       LOOP THROUGH LAYERS (K)
        DO 50 K=1,LSS
          ACMK(K,M)=AACMK+Z(K,1)*H(K,M)+Z(K,1)**2.0*W(K,M)/2.0
          AACMK=ACMK(K,M)
          IF(K.GT.1) THEN
            J=K-1
            BCMK(K,M)=BBCMK+Z(J,2)*H(J,M)+Z(J,2)**2.0*W(J,M)/2.0
            BBCMK=BCMK(K,M)
          ENDIF
C
        CMK(K,M)=AACMK-BBCMK
C
   50   CONTINUE
C
   60 CONTINUE 
C     
      RETURN
C
      END
C
C***********************************************************************
C     SUBROUTINE S8913A
C
C     PURPOSE
C       TO CALCULATE THE TERMS OF EQUATION (5)
C       THESE ARE STORED IN A THREE DIMENSIONAL ARRAY THI, TWO
C       DIMENSIONS ARE R AND S AND THE THIRD IS THE LAYER K
C
C     USAGE
C       CALLED IN SUBROUTINE G8913A
C
C     DESCRIPTION OF PARAMETERS
C       IR   =R
C       IS   =S
C       K    =LAYER NUMBER
C
C     FUNCTIONS AND SUBROUTINES CALLED
C       NONE
C
C.......................................................................
C
C     Unrequired variables TT,PNL and LAYSS deleted from next line. Amend A
      SUBROUTINE S8913A(LSS,H,W,Z,CMK,THI)
C
C     Next 2 lines added.                                        Amendment A
      INTEGER II,JJ,KK,K,IR,IS
      REAL    ATHI,BTHI,CTHI,DTHI,ETHI
C
C     Next line changed                                         Amendment A
      DIMENSION CMK(500,4),THI(4,4,500),Z(500,2)
      DIMENSION H(500,4),W(500,4)
C
C     SET ARRAY THI TO ZERO
      DO 10 II=1,LSS
        DO 10 JJ=1,4
          DO 10 KK=1,4
            THI(KK,JJ,II)=0.0
   10 CONTINUE
C     CALCULATION OF THI, K=LAYER,IR=ROW,IS=COLUMN
      DO 30 K=1,LSS
        DO 30 IR=1,4
          DO 30 IS=1,4
C           SKIP LOWER DIAGONAL OF R S ARRAY OF THI AS NOT REQUIRED
            IF(IR.LE.IS) THEN
C
              ATHI=CMK(K,IR)*CMK(K,IS)*(Z(K,1)-Z(K,2))
C
              BTHI=(CMK(K,IR)*H(K,IS)+CMK(K,IS)*H(K,IR))
              BTHI=BTHI*(Z(K,1)**2.0-Z(K,2)**2.0)/2.0
C
              CTHI=(CMK(K,IR)*W(K,IS)/6.0)+(CMK(K,IS)*W(K,IR)/6.0)
              CTHI=CTHI-(H(K,IR)*H(K,IS)/3.0)
              CTHI=CTHI*(Z(K,1)**3.0-Z(K,2)**3.0)
C
              DTHI=(H(K,IR)*W(K,IS)+H(K,IS)*W(K,IR))/8.0
              DTHI=DTHI*(Z(K,1)**4.0-Z(K,2)**4.0)
C
              ETHI=W(K,IR)*W(K,IS)*(Z(K,1)**5.0-Z(K,2)**5.0)/20.0
C         
              THI(IR,IS,K)=ATHI-BTHI-CTHI+DTHI+ETHI
C
            ENDIF
C
   30 CONTINUE
C
      RETURN
C
      END
C
C***********************************************************************
C     SUBROUTINE T8913A
C
C     PURPOSE
C       TO BUILD THE E TERMS OF EQUATION (4)
C     USAGE
C       CALLED IN SUBROUTINE G8913A
C
C     SUBROUTINES AND FUNCTIONS CALLED
C       NONE
C
C.......................................................................
C
      SUBROUTINE T8913A(LSS,R,THI,E)
C
C     Next line added                                           Amendment A
      INTEGER K
      DIMENSION R(2,2,500),THI(4,4,500),E(2,2)
C
C     SET ARRAY E TERMS TO ZERO
      E(1,1)=0.0
      E(1,2)=0.0
      E(2,2)=0.0
C
C     LOOP THROUGH K (THE LAYERS OF THE PLATE) AND BUILD E TERMS
      DO 10 K=1,LSS
        E(1,1)=E(1,1)+(R(1,1,K)*THI(3,3,K)+2.0*R(1,2,K)*THI(1,3,K)+
     1         R(2,2,K)*THI(1,1,K))
C
        E(1,2)=E(1,2)+(R(1,1,K)*THI(3,4,K)+R(1,2,K)*(THI(2,3,K)+
     1         THI(1,4,K))+R(2,2,K)*THI(1,2,K))
C
        E(2,2)=E(2,2)+(R(1,1,K)*THI(4,4,K)+2.0*R(1,2,K)*THI(2,4,K)+
     1         R(2,2,K)*THI(2,2,K))
C
   10 CONTINUE
C
      E(2,1)=E(1,2)
C
      RETURN
C
      END
C
C***********************************************************************
C     SUBROUTINE U8913A
C
C     PURPOSE
C       TO DETERMINE THE UNIQUE LAYER TYPE USED IN THE LAY-UP STACK SO
C       THAT THE PROPERTIES OF ONLY THOSE LAYERS NEED BE OUTPUT FROM
C       THE COMPLETE FILE OF PROPERTIES STORED IN ARRAY PNL. NOTE THAT
C       THE SIGN OF THE ANGLE THI IS LOST IN THIS ROUTINE
C     USAGE
C       CALLED IN PROGRAM A8913A
C     DESCRIPTION OF PARAMETERS
C       LAYSS  ARRAY CONTAINING LAYER STACKING SEQUENCE
C       LAYERS ARRAY SIMILAR TO LAYSS BUT CONTAINING A ZERO WHERE LAYERS
C              ARE REPEATED AND OMITTING LAYER SIGNS
C
C     SUBROUTINES AND FUNCTIONS CALLED
C       NONE
C
C.......................................................................
C
      SUBROUTINE U8913A(LSS,LAYSS,LAYERS)
C
C     Next line added                                           Amendment A
      INTEGER   II,J,NCOUNT,K
      DIMENSION LAYSS(500),LAYERS(500)
C
C     SET ARRAY LAYERS EQUAL TO ARRAY LAYSS
      DO 10 II=1,LSS
         LAYERS(II)=LAYSS(II)
   10 CONTINUE
C
C     LOOP THROUGH STACK TO BE CHECKED
      DO 20 J=1,LSS
C       SET COUNT TO ZERO
        NCOUNT=0
C       LOOP THROUGH STACK CHECKING AGAINST J DESIGNATED LAYER CHECKING 
C       FOR REPEATED LAYER TYPES
          DO 20 K=1,LSS
            IF((IABS(LAYERS(K))).EQ.(IABS(LAYERS(J)))) NCOUNT=NCOUNT+1
            IF(NCOUNT.GT.1) LAYERS(K)=0
            IF(NCOUNT.GT.1) NCOUNT=1
   20 CONTINUE
C
      RETURN
C
      END
C
C***********************************************************************
C     SUBROUTINE Z8913A
C 
C     PURPOSE 
C        TO PRINT ALL THE INPUT AND STIFFNESSES OUTPUT AND ALL MESSAGES
C
C     USAGE 
C        CALLED BY A8913A
C
C     DESCRIPTION OF PARAMETERS 
C        SEE INPUT AND OUTPUT DATA SECTIONS OF DATA ITEM NO.89013
C        NOTES (ALL NOTES ARE SET TO ZERO AND REMAIN SO IN SUCCESSFUL
C               CALCULATIONS)
C          NOTE1 =1 IF MATRIX INVERTION FAILS OWING TO ILL-CONDITIONING
C                   OF TOTAL PLATE STIFFNESS MATRIX
C          NOTE4 =1 IF MATRIX INVERSION FAILS OWING TO ILL-CONDITIONING
C                   OF COMPLIANCE MATRIX OF A LAYER
C          NOTE6 =1 IF THE DENOMINATOR OF EQUATIONS 11A OR 11B IS ZERO
C          NOTE7 =1 IF MATRIX INVERSION OF E TO G (EQUATIONS (2) TO (1))
C                   IS ILL-CONDITIONED
C          NOTE10=1 IF MORE THAN ALLOWED 50 LAYER TYPES IN DATA
C          NOTE11=1 IF STACK LENGTH EXCEEDS 500
C          NOTE13=1 IF STACK CALLS AN UNDECLARED LAYER
C          NOTE31 TO NOTE41=1 SEE RELEVANT FORMAT STATEMENTS THEY CALL
C          NOTE42=1 IF MATERIAL PROPERTY CHECKS FOR ANY LAYER FAIL. USED
C                   TO SWITCH OUTPUT FOR THESE FAILURE CASES
C
C     SUBROUTINES AND FUNCTIONS CALLED 
C        NONE
C            
C......................................................................
C 
C     Unrequired variables ZZ,TT and NOTE7 removed from next line.  Amend A
      SUBROUTINE Z8913A(IOUT,A,BC,D,LAYSS,PNL,LSS,G,NOTE1,NOTE4,NOTE6,
     1LAYERS,NOTE10,NOTE11,NOTE13,NOTE31,NOTE32,NOTE33,NOTE34,NOTE35,
     2NOTE36,NOTE37,NOTE38,NOTE39,NOTE40,NOTE41,NOTE42,LAYNO)
C                                                        End of Amendment A
C
C     Next 2 lines added.                                       Amendment A
      REAL    ANGLE,ANG
      INTEGER I,K,IL,KL,KK,II
C
C     Next line changed.                                        Amendment A
      DIMENSION PNL(50,12),A(3,3),BC(3,3),D(3,3),LAYSS(500)
      DIMENSION G(2,2),LAYERS(500)
C
      DOUBLE PRECISION A,BC,D
C
C     """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
C     PRINT HEADINGS ON A NEW PAGE  
      WRITE(IOUT,1000)
 1000 FORMAT('1')
      WRITE(IOUT,1400)
      WRITE(IOUT,1100)
C
C     Next 4 FORMAT statements changed                          Amendment A
 1100 FORMAT(' ESDU International plc.',//,' PROGRAM A8913A',//,
     1' ESDUpac Number:   A8913',/,' ESDUpac Title:    Transverse ',
     2'(through-the-thickness) shear stiffnesses',/,22X,'of fibre ',
     3'reinforced composite laminated plates')
      WRITE(IOUT,1200)
 1200 FORMAT(' Data Item Number: 89013',/,' Data Item Title:  ',
     1'Transverse (through-the-thickness) shear stiffnesses',/,22X,
     2'of fibre reinforced composite laminated plates')
      WRITE(IOUT,1300) 
 1300 FORMAT(' ESDUpac Version:  1.1 July 1991 -- Data Item Amendment ',
     1'A',/,' (See Data Item for full input/output specification and ',
     2'interpretation)')
      WRITE(IOUT,1400)
 1400 FORMAT(/,1X,76('-'),/)
C                                                        End of Amendment A
C
C     """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
C     PRINT OUT ERROR MESSAGES FOR INCORRECT INPUT SPECIFICATION
      IF(NOTE10.EQ.1) THEN
        WRITE(IOUT,5000)
        WRITE(IOUT,1410)
        WRITE(IOUT,5000)
      ENDIF
C
      IF(NOTE11.EQ.1) THEN
        WRITE(IOUT,5000)
        WRITE(IOUT,1420)
        WRITE(IOUT,5000)
      ENDIF
C
      IF(NOTE13.EQ.1) THEN
        WRITE(IOUT,5000)
        WRITE(IOUT,1430)
        WRITE(IOUT,5000)
      ENDIF
C     SWITCH TO END OF SUBROUTINE IF ANY INPUT INCORRECTLY SPECIFIED
      IF(NOTE10.EQ.1.OR.NOTE11.EQ.1.OR.NOTE13.EQ.1) GOTO 999
C
 1410 FORMAT(65H NUMBER OF LAYER TYPES ZERO OR LIMIT EXCEEDED, PROGRAM T
     1ERMINATED)
 1420 FORMAT(73H NUMBER OF LAYERS IN THE PLATE ZERO OR LIMIT EXCEEDED, P
     1ROGRAM TERMINATED)
C
C     Next format statements changed.                           Amendment A
 1430 FORMAT(' A LAYER IDENTIFICATION NUMBER IN THE LAYER STACKING SEQUE
     1NCE',/,'    HAS NOT BEEN DECLARED          PROGRAM TERMINATED')
C
C     """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
C     PRINT OUT PROPERTIES OF ONLY THE LAYER TYPES USED
      WRITE(IOUT,1500) 
 1500 FORMAT(40H PLATE LAYER PROPERTIES AND CONSTRUCTION,/) 
         DO 10 I=1,LSS
C          OMIT PRINTING OF REPEATED LAYER TYPES AS DICTATED BY ARRAY
C          LAYERS
           IF(LAYERS(I).NE.0)THEN
              K=IABS(LAYSS(I))
              ANGLE=PNL(K,12)*K/LAYSS(I)
C
C     Next 2 lines added.                                       Amendment A
              II=INT(PNL(K,1))
              WRITE(IOUT,1600) II
C
 1600         FORMAT(21H LAYER TYPE NUMBER = ,I4) 
              WRITE(IOUT,1700) PNL(K,2), PNL(K,3),PNL(K,4)
              WRITE(IOUT,1800) PNL(K,5), PNL(K,7),PNL(K,6)
              WRITE(IOUT,1900) PNL(K,10),PNL(K,8),PNL(K,9)
              WRITE(IOUT,2000) PNL(K,11),ANGLE
C
            ENDIF
C
   10     CONTINUE
C
C     Next 4 FORMAT statements changed.                         Amendment A
 1700         FORMAT(15H E(ALPHA)     =,E11.4,15H   E(BETA)    =,E11.4
     1        ,15H  E(Z)        =,E11.4,)
 1800         FORMAT(15H V(ALPHA,BETA)=,E11.4,15H   V(BETA,Z)  =,E11.4
     1        ,15H  V(ALPHA,Z)  =,E11.4,)
 1900         FORMAT(15H G(ALPHA,BETA)=,E11.4,15H   G(BETA,Z)  =,E11.4
     1        ,15H  G(ALPHA,Z)  =,E11.4,)
 2000         FORMAT(15H THICKNESS    =,E11.4,15H   ANGLE(DEG) =,E11.4
     1        ,/)
C
C     PRINT LAYER HEADINGS
      WRITE(IOUT,2100)
      WRITE(IOUT,2200)
 2100 FORMAT(55H LAYER NUMBER   LAYER TYPE   THICKNESS    ANGLE DEGREES)
 2200 FORMAT(10H  IN STACK,/)
C     LOOP THROUGH LAYERS PRINTING VALUES
      DO 20 IL=1,LSS
        KL=IABS(LAYSS(IL))
        ANG=PNL(KL,12)*KL/LAYSS(IL)
        WRITE(IOUT,2300)IL,IABS(LAYSS(IL)),PNL((IABS(LAYSS(IL))),11),ANG
   20 CONTINUE
C
 2300 FORMAT(4X,I4,11X,I4,6X,E11.4,3X,E11.4)
C
C     """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
C     PRINT RESULT OF LAYER MATERIAL CHECKS IF ANY FAILURES ARE FOUND
C     IF FAILURES ARE FOUND THEN AFTER MESSAGE SWITCH TO THE END OF THE
C     PROGRAM
      IF(NOTE42.EQ.1) THEN
        WRITE(IOUT,5000)
        WRITE(IOUT,3000) LAYNO
        IF(NOTE31.EQ.1) WRITE(IOUT,3010)
        IF(NOTE32.EQ.1) WRITE(IOUT,3020)
        IF(NOTE33.EQ.1) WRITE(IOUT,3030)
        IF(NOTE34.EQ.1) WRITE(IOUT,3040)
        IF(NOTE35.EQ.1) WRITE(IOUT,3050)
        IF(NOTE36.EQ.1) WRITE(IOUT,3060)
        IF(NOTE37.EQ.1) WRITE(IOUT,3070)
        IF(NOTE38.EQ.1) WRITE(IOUT,3080)
        IF(NOTE39.EQ.1) WRITE(IOUT,3090)
        IF(NOTE40.EQ.1) WRITE(IOUT,3100)
        IF(NOTE41.EQ.1) WRITE(IOUT,3110)
        WRITE(IOUT,5000)
        GOTO 999
      ENDIF
C      
 3000 FORMAT(27H PROPERTIES OF LAYER NUMBER,I4,9H IN ERROR)
 3010 FORMAT(26H E(ALPHA) ZERO OR NEGATIVE,/)
 3020 FORMAT(25H E(BETA) ZERO OR NEGATIVE,/)
 3030 FORMAT(22H E(Z) ZERO OR NEGATIVE,/)
 3040 FORMAT(32H G(BETA,Z)      ZERO OR NEGATIVE,/)
 3050 FORMAT(32H G(ALPHA,Z)     ZERO OR NEGATIVE,/)
C
C     Next FORMAT statements changed.                           Amendment A
 3060 FORMAT(32H G(ALPHA,BETA)  ZERO OR NEGATIVE,/)
 3070 FORMAT(33H LAYER THICKNESS ZERO OR NEGATIVE,/)
 3080 FORMAT(29H NU(BETA,Z)**2*E(Z)/E(BETA)>1,/) 
 3090 FORMAT(31H NU(ALPHA,Z)**2*E(Z)/E(ALPHA)>1,/)
 3100 FORMAT(36H NU(ALPH,BETA)**2*E(BETA)/E(ALPHA)>1,/)
 3110 FORMAT(62H COMBINED RATIOS OF PROPERTIES INVALID FOR AN ELASTIC MA
     1TERIAL,/)
C
C     """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
C     PRINT STIFFNESS MATRICES A,BC,D 
C     NOTE THAT THE ELEMENTS OF BC (THE B MATRIX) ARE NEGATED IN ORDER
C     OUTPUT THEM IN THE SIGN CONVENTION OF DATA ITEM NO.75002
      WRITE(IOUT,4100)
C
C     Next FORMAT statements changed.                           Amendment A
 4100 FORMAT(/4X,21H A IN-PLANE STIFFNESS,20X,20H B COUPLED STIFFNESS,/)
      DO 30 KK=1,3
      WRITE(IOUT,4300)A(KK,1),A(KK,2),A(KK,3),-BC(KK,1),-BC(KK,2),-BC(KK
     1,3)
   30 CONTINUE
      WRITE(IOUT,4200)
C
C     Next FORMAT statements changed.                           Amendment A
 4200 FORMAT(/4X,20H B COUPLED STIFFNESS,21X,21H D FLEXURAL STIFFNESS,/)
      DO 40 KK=1,3
      WRITE(IOUT,4300)-BC(KK,1),-BC(KK,2),-BC(KK,3),D(KK,1),D(KK,2),D(KK
     1,3)
   40 CONTINUE
C
C     Next FORMAT statements changed.                           Amendment A
 4300 FORMAT(3E12.3,5X,3E12.3)
      WRITE(IOUT,1400)
C
C     """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
C     PRINT FAILURE MESSAGES FOR NOTES 1,4 OR 6
      IF(NOTE1.EQ.1) THEN
        WRITE(IOUT,5000)
        WRITE(IOUT,4400)
        WRITE(IOUT,5000)
      ENDIF
C
      IF(NOTE6.EQ.1) THEN
        WRITE(IOUT,5000)
        WRITE(IOUT,4500)
        WRITE(IOUT,5000)
      ENDIF
C
      IF(NOTE4.EQ.1) THEN
        WRITE(IOUT,5000)
        WRITE(IOUT,4600)
        WRITE(IOUT,5000)
      ENDIF
C
      IF(NOTE1.EQ.1.OR.NOTE4.EQ.1.OR.NOTE6.EQ.1) GOTO 999
C
 4400 FORMAT(/,86H INVERSION OF STIFFNESS MATRIX FOR COMPLETE PLATE HAS 
     1FAILED OWING TO ILL-CONDITIONING,/,49H CHECK THAT INPUT MATERIAL P
     2ROPERTIES ARE CORRECT)
 4500 FORMAT(/,66H DENOMINATOR OF EQUATION (1.3.8) APPROACHES ZERO, CHEC
     1K INPUT DATA)
 4600 FORMAT(/,80H INVERSION OF COMPLIANCE MATRIX FOR A LAYER HAS FAILED
     1 OWING TO ILL-CONDITIONING,/,29H CHECK LAYER INPUT PROPERTIES)
C
C     """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
C     PRINT OUT SHEAR STIFFNESSES
      WRITE(IOUT,5000)
      WRITE(IOUT,4700)
 4700 FORMAT(18H SHEAR STIFFNESSES,/)
      WRITE(IOUT,4800) G(1,1),G(1,2),G(2,1),G(2,2)
      WRITE(IOUT,5000)
C
 4800 FORMAT(/,8H Q(X) = ,E11.4,15H GAMMA(XZ)  +  ,E11.4,10H GAMMA(YZ),/
     1,8H Q(Y) = ,E11.4,15H GAMMA(XZ)  +  ,E11.4,10H GAMMA(YZ))
C
C     Next FORMAT statement changed.                            Amendment A
 5000 FORMAT(/,1X,76('*'),/)
C
C     """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
C
  999 CONTINUE
C
      RETURN
C 
      END
C 
C***********************************************************************
      SUBROUTINE Q8913A(MATRIX,INV,N,ILLCON)
C
C     PURPOSE : TO INVERT AN NxN MATRIX
C
C     METHOD 
C     1.  A FORM OF GAUSSIAN ELIMINATION IS USED.  THE PIVOT ROW IS 
C     CHOSEN BY CONSIDERING THE GREATEST ABSOLUTE ELEMENT IN THE FIRST
C     COLUMN OF THE SCALED MATRIX.  THE SCALING FACTOR FOR EACH ROW IS
C     THE GREATEST ABSOLUTE ELEMENT IN THAT ROW
C     2.  THE PIVOT ROW IS DECOMPOSED USING CROUT'S TECHNIQUE
C     3.  THE FIRST TWO STEPS ARE REPEATED FOR THE RESULTANT SQUARE
C     MATRICES UNTIL THE DECOMPOSITION IS COMPLETE
C     4.  THE INVERSE MATRIX IS OBTAINED BY BACKSUBSTITUTION N TIMES
C     5.  THE CONDITION OF THE MATRIX IS INVESTIGATED AND, IF POSSIBLE,
C     ITERATIVE IMPROVEMENT IS CARRIED OUT ON THE CALCULATED INVERSE
C
C     LIST OF PARAMETERS
C     MATRIX   -   MATRIX TO BE INVERTED
C     INV      -   INVERSE OF MATRIX
C     N        -   DIMENSION OF MATRICES
C     ILLCON   -   ILL-CONDITIONING FLAG VARIABLE
C     A        -   DECOMPOSED FORM OF MATRIX
C     S        -   ARRAY CONTAINING SCALING FACTORS
C     PIVOT    -   ARRAY RECORDING ROW INTERCHANGES
C
C     ..................................................................
C
C     1 DIMENSION statement deleted                             Amendment A
C
C     Next 2 lines changed                                      Amendment A
      REAL A(6,6),S(6),B(6),INV(N,N),MATRIX(N,N),E
      INTEGER PIVOT(6),AV,IOUT,I,J
C
      LOGICAL ILLCON
      ILLCON=.FALSE.
C
      IOUT=6
C
C     Next line subroutine name changed                         Amendment A
      CALL ESMA13(MATRIX,N,AV)
      E=10.0**AV
      DO 10 I=1,N
      DO 10 J=1,N
      MATRIX(I,J)=MATRIX(I,J)/E
   10 CONTINUE
      CALL ESMAT1(AV,A,S,PIVOT,N,MATRIX,ILLCON)
      IF(ILLCON)RETURN
C
      CALL ESMAT4(INV,N,PIVOT,A,B)
C
C     Parameters Z and B deleted from next line                 Amendment A
      CALL ESMAT6(INV,MATRIX,N,A,PIVOT,ILLCON)
      DO 20 I=1,N
      DO 20 J=1,N
      MATRIX(I,J)=MATRIX(I,J)*E
      INV(I,J)=INV(I,J)/E
   20 CONTINUE
C
      RETURN
      END
C
C
C     ..................................................................
C
      SUBROUTINE ESMAT1(AV,A,S,PIVOT,N,MATRIX,ILLCON)
C
C     PURPOSE : TO DECOMPOSE THE MATRIX 
C
C     METHOD
C     CROUT DECOMPOSITION
C
C     LIST OF PARAMETERS
C     AV       -   AVERAGE OF ARRAY MATRIX()
C     A        -   DECOMPOSED FORM OF MATRIX
C     S        -   ARRAY CONTAINING SCALING FACTORS
C     PIVOT    -   ARRAY RECORDING ROW INTERCHANGES
C     N        -   DIMENSION OF MATRICES
C     MATRIX   -   MATRIX TO BE INVERTED
C     ILLCON   -   ILL-CONDITIONING FLAG VARIABLE
C
C     ..................................................................
C
C     1 DIMENSION statement deleted                             Amendment A
C
C     Next line changed                                         Amendment A
      REAL A(N,N),S(N),MATRIX(N,N),DET,E
      DOUBLE PRECISION ADD
C
C     Next line changed                                         Amendment A
      INTEGER SS,PIVOT(6),AV,I,J,K
      LOGICAL ILLCON
C
C     DETERMINANT IS INITIALLY SET TO 1
      DET=1.0
C
      DO 999 I=1,N
      DO 999 J=1,N
      E=10.0**AV
      A(I,J)=MATRIX(I,J)
999   CONTINUE
C
      CALL ESMAT2(MATRIX,S,N)
C 
      DO 22 K=1,N
      CALL ESMAT3(A,S,PIVOT,N,DET,K,ILLCON)
      IF(ILLCON) GOTO 101
      DO 66 J=1,K
      ADD=0.0
      IF(J.EQ.1) GOTO 65
      DO 77 SS=1,J-1
      ADD=ADD+DBLE(A(K,SS)*A(SS,J))
77    CONTINUE
65    A(K,J)=A(K,J)-SNGL(ADD)
66    CONTINUE
C
      IF(ABS(A(K,K)).LE.1.0E-6)ILLCON=.TRUE.
      IF(ILLCON)GOTO 101
      DET=A(K,K)*DET
C
      IF(K.EQ.N) GOTO 22
      DO 88 J=K+1,N
      ADD=0.0
      IF(K.EQ.1) GO TO 87
      DO 99 SS=1,K-1
      ADD=ADD+DBLE(A(K,SS)*A(SS,J))
99    CONTINUE  
87    A(K,J)=(A(K,J)-SNGL(ADD))/A(K,K)
88    CONTINUE
C
22    CONTINUE
C
101   RETURN
      END
C
C     ..................................................................
C
      SUBROUTINE ESMAT2(MATRIX,S,N)
C
C     PURPOSE : TO SCALE THE MATRIX PRIOR TO CHOOSING THE PIVOT
C
C     METHOD
C     THE GREATEST ABSOLUTE ELEMENT IN EACH ROW IS USED AS THE SCALING
C     FACTOR FOR THAT ROW
C
C     LIST OF PARAMETERS
C     MATRIX   -   MATRIX TO BE SCALED
C     S        -   ARRAY CONTAINING SCALING FACTORS
C     N        -   DIMENSION OF MATRICES
C
C     ..................................................................
C
C     Next line added                                           Amendment A
      INTEGER I,J
      DIMENSION MATRIX(N,N),S(N)
      REAL MATRIX
      DO 12 I=1,N
      S(I)=0.0
      DO 11 J=1,N
      IF(S(I).LT.ABS(MATRIX(I,J))) S(I)=ABS(MATRIX(I,J))
11    CONTINUE
12    CONTINUE
      RETURN
      END
C
C     ..................................................................
C
      SUBROUTINE ESMAT3(A,S,PIVOT,N,DET,K,ILLCON)
C 
C     PURPOSE : TO CHOOSE THE PIVOT ROW OF A SQUARE SYSTEM
C
C     METHOD
C     THE ROW WHICH CONTAINS THE GREATEST ABSOLUTE ELEMENT IN THE FIRST
C     COLUMN IS TAKEN TO BE THE PIVOT ROW
C
C     LIST OF PARAMETERS
C     A        -   DECOMPOSED FORM OF MATRIX
C     S        -   ARRAY CONTAINING SCALING FACTORS
C     PIVOT    -   ARRAY RECORDING ROW INTERCHANGES
C     N        -   DIMENSION OF MATRICES
C     DET      -   DETERMINANT OF MATRIX
C     K        -   CURRENT ROW
C     ILLCON   -   ILL-CONDITIONING FLAG VARIABLE
C
C     ..................................................................
C
C     Next 2 lines changed                                      Amendment A
      REAL A(N,N),S(N),C(6),O2
      INTEGER PIVOT(N),I,I0,J
C
      LOGICAL ILLCON
C
      C(K)=0.0
C
      DO 33 I=K,N
      IF(C(K).LT.ABS(A(I,K)/S(I))) I0=I
      IF(C(K).LT.ABS(A(I,K)/S(I))) C(K)=ABS(A(I,K)/S(I))
33    CONTINUE
C
      PIVOT(K)=I0
C
C     PIVOT ELEMENT IS CHECKED TO SEE IF IT IS TOO CLOSE TO ZERO.  THE
C     CUT-OFF POINT CHOSEN IS DEPENDENT ON THE ACCURACY OF THE MACHINE
      IF(C(K).LE.1E-6) GOTO 100
C
      IF(I0.EQ.K) GOTO 44
      DET=-DET
C
      DO 55 J=1,N
      O2=A(K,J)
      A(K,J)=A(I0,J)
      A(I0,J)=O2
55    CONTINUE
      GO TO 44
C
100   ILLCON=.TRUE.
44    RETURN
      END
C
C     ..................................................................
C
      SUBROUTINE ESMAT4(INV,N,PIVOT,A,B)
C
C     PURPOSE : TO INVERT THE MATRIX 
C
C     METHOD
C     SUCCESSIVE BACKSUBSTITUTION ON THE COLUMNS OF THE NxN IDENTITY
C     MATRIX
C
C     LIST OF PARAMETERS
C     INV      -   INVERSE OF MATRIX
C     N        -   DIMENSION OF MATRICES
C     PIVOT    -   ARRAY RECORDING ROW INTERCHANGES
C     A        -   DECOMPOSED FORM OF MATRIX
C     B        -   BACKSUBSTITUTION SOLUTION VECTOR
C
C     ..................................................................
C
C     1 DIMENSION statement deleted                             Amendment A
C
C     Next 2 lines changed                                      Amendment A
      REAL Z(6),INV(N,N),B(N),A(N,N)
      INTEGER PIVOT(N),I,J,K
C
      DO 11 I=1,N
C
      DO 22 K=1,N
      Z(K)=0.0
      IF(K.EQ.I)Z(K)=1.0
22    CONTINUE
C
      CALL ESMAT5(PIVOT,Z,N,A,B)
C
      DO 33 J=1,N
      INV(J,I)=B(J)
33    CONTINUE
11    CONTINUE
C 
      RETURN
      END
C
C     ..................................................................
C
      SUBROUTINE ESMAT5(PIVOT,Z,N,A,B)
C
C     PURPOSE : TO OBTAIN THE SOLUTION VECTOR CORRESPONDING TO A GIVEN 
C     R.H.S. VECTOR
C
C     METHOD
C     BACKSUBSTITUTION
C
C     LIST OF PARAMETERS
C     PIVOT    -   ARRAY RECORDING ROW INTERCHANGES
C     N        -   DIMENSION OF MATRICES
C     A        -   DECOMPOSED FORM OF MATRIX
C     B        -   BACKSUBSTITUTION SOLUTION VECTOR
C
C     ..................................................................
C
C     Next line added                                           Amendment A
      REAL D,AA
      DIMENSION PIVOT(N),B(N),D(6),A(N,N),Z(N)
      DOUBLE PRECISION ADD
C
C     Next line changed                                         Amendment A
      INTEGER SS,PIVOT,K,J,I
C
      DO 11 K=1,N
      ADD=0.0
      J=PIVOT(K)
      IF(J.EQ.K) GO TO 12
      AA=Z(J)
      Z(J)=Z(K)
      Z(K)=AA
C
12    D(K)=Z(K)
      IF(K.EQ.1) GO TO 13
      DO 22 SS=1,K-1
      ADD=ADD+DBLE(A(K,SS)*D(SS))
22    CONTINUE
13    D(K)=(D(K)-SNGL(ADD))/A(K,K)
11    CONTINUE
C
      DO 33 I=N,1,-1
      ADD=0.0
      B(I)=D(I)
      IF(I.EQ.N) GO TO 32
      DO 44 SS=I+1,N
      ADD=ADD+DBLE(A(I,SS)*B(SS))
44    CONTINUE
32    B(I)=B(I)-SNGL(ADD)
33    CONTINUE
C
      RETURN
      END
C
C     ..................................................................
C
C     Parameters BS and B deleted from next line                Amendment A
      SUBROUTINE ESMAT6(INV,MATRIX,N,A,PIVOT,ILLCON)
C
C     PURPOSE : TO DETERMINE WHETHER MATRIX IS SUFFICIENTLY WELL-
C     CONDITIONED FOR ITERATIVE IMPROVEMENT TO OCCUR
C
C     METHOD
C     CHECK TO SEE IF PROCESS WILL CONVERGE
C
C     LIST OF PARAMETERS
C     INV      -   INVERSE OF MATRIX
C     MATRIX   -   MATRIX TO BE INVERTED
C     N        -   DIMENSION OF MATRICES
C     A        -   DECOMPOSED FORM OF MATRIX
C     PIVOT    -   ARRAY RECORDING ROW INTERCHANGES
C     ILLCON   -   ILL-CONDITIONING FLAG VARIABLE
C
C     ..................................................................
C
C     1 DIMENSION statement deleted                             Amendment A
C
C     Next 2 lines changed                                      Amendment A
      REAL INV(N,N),MATRIX(N,N),C(6,6),A(N,N),ROW
      INTEGER PIVOT(N),I,J
      LOGICAL ILLCON
C
      CALL ESMAT7(INV,MATRIX,C,N,N,N)
C
      DO 10 I=1,N
      DO 10 J=1,N
      C(I,J)=-C(I,J)
      IF(I.EQ.J)C(I,J)=1+C(I,J)
10    CONTINUE
C
C     ESMAT8 IS CALLED TO CHECK THAT CONVERGENCE IS GUARANTEED
      CALL ESMAT8(C,N,N,ROW)
      IF(ROW.EQ.0.0)GO TO 30
      IF(ROW.GE.1.0)GO TO 20
C
C     Next line subroutine name changed                         Amendment A
      CALL ESMA12(MATRIX,N,PIVOT,A,INV)
C 
      GO TO 30
C
20    ILLCON=.TRUE.
30    RETURN
      END
C
C     ..................................................................
C
      SUBROUTINE ESMAT7(A,B,C,L,M,N)
C 
C     PURPOSE : TO PERFORM MATRIX MULTIPLICATION 
C     
C     METHOD
C     MAT C = A*B
C
C     LIST OF PARAMETERS
C     A  -  MATRIX [A]
C     B  -  MATRIX [B]
C     C  -  SOLUTION MATRIX [C]
C     L  -  NUMBER OF ROWS IN MATRIX [A]
C     M  -  NUMBER OF COLUMNS IN MATRIX [A]
C     N  -  NUMBER OF COLUMNS IN MATRIX [B]
C 
C     ..................................................................
C
C     Next 2 lines added                                        Amendment A
      INTEGER I,J,K
      REAL S
      DIMENSION A(N,N),B(N,N),C(N,N)
C 
      DO 10 I=1,L 
      DO 10 J=1,N 
      S=0.0 
      DO 20 K=1,M 
      S=S+A(I,K)*B(K,J) 
   20 CONTINUE
C 
      C(I,J)=S
   10 CONTINUE
C 
      RETURN
      END 
C
C     ..................................................................
C
      SUBROUTINE ESMAT8(A,N,M,ROW)
C
C     PURPOSE : TO CALCULATE THE NORM OF A MATRIX
C
C     METHOD
C     THE NORM USED IS MAXIMUM VALUE OF THE SUM OF THE ABSOLUTE VALUE OF
C     THE ELEMENTS IN A ROW, FOR THE ROW 1 TO N
C
C     LIST OF PARAMETERS
C     A        -   GIVEN MATRIX
C     N,M      -   DIMENSION OF MATRIX [A]
C     ROW      -   ROW OF MATRIX [A]
C
C     ..................................................................
C
      INTEGER I,J
      REAL SUM
      DIMENSION A(N,N)
C
      ROW=0.0
C
      DO 200 I=1,N
      SUM=0.0
      DO 100 J=1,M
      SUM=SUM+ABS(A(I,J))
100   CONTINUE
      IF(SUM.GT.ROW)ROW=SUM
200   CONTINUE
C
      RETURN
      END
C
C     ..................................................................
C
      SUBROUTINE ESMAT9(MATRIX,B,N,BS,PIVOT,A)
C
C     PURPOSE : TO IMPROVE THE ACCURACY OF A GIVEN SOLUTION VECTOR
C
C     METHOD
C     USES BACKSUBSTITUTION TO ESTIMATE THE ERROR BETWEEN THE TRUE
C     SOLUTION AND THE CALCULATED SOLUTION
C
C     LIST OF PARAMETERS
C     MATRIX   -   MATRIX TO BE INVERTED
C     B        -   GIVEN SOLUTION VECTOR
C     N        -   DIMENSION OF MATRICES
C     BS       -   ERROR BETWEEN THE TRUE SOLUTION AND THE CALCULATED
C                  SOLUTION
C     PIVOT    -   ARRAY RECORDING ROW INTERCHANGES
C     A        -   DECOMPOSED FORM OF MATRIX
C
C     ..................................................................
C
C     1 DIMENSION statement deleted                             Amendment A
C
C     Next 2 lines changed
      REAL MATRIX(N,N),B(N),BS(N),A(N,N),R(6),E(6),Y(6),ROW
      INTEGER PIVOT(N),I
C
44    CALL ESMA10(MATRIX,B,N,BS,R)
C
      CALL ESMAT5(PIVOT,R,N,A,E)
C
      DO 200 I=1,N
      Y(I)=B(I)
      B(I)=B(I)+E(I)
      IF(Y(I).EQ.0.0)Y(I)=1.0
      Y(I)=E(I)/Y(I)
200   CONTINUE
C
      CALL ESMAT8(Y,N,1,ROW)
      IF(ROW.LT.1E-6)GO TO 56
C
      GO TO 44
C
56    RETURN
      END
C
C     ..................................................................
C
C     Next line subroutine name changed                         Amendment A
      SUBROUTINE ESMA10(MATRIX,B,N,BS,R)
C
C     PURPOSE : TO CALCULATE THE RESIDUAL
C
C     METHOD
C     THE RESIDUAL IS CALCULATED USING DOUBLE PRECISION MULTIPLICATION
C
C     LIST OF PARAMETERS
C     MATRIX   -   MATRIX TO BE INVERTED
C     B        -   GIVEN SOLUTION VECTOR
C     N        -   DIMENSION OF MATRICES
C     BS       -   ERROR BETWEEN THE TRUE SOLUTION AND THE CALCULATED
C                  SOLUTION
C     R        -   ARRAY RECORDING THE RESIDUAL
C
C     N.B  IT IS VITAL THAT THE RESIDUAL IS CALCULATED AT A HIGHER
C     PRECISION THAN THE REST OF THE PROGRAM
C     ..................................................................
C
C     1 DIMENSION statement deleted                             Amendment A
C
C     Next 2 lines changed                                      Amendment A
      REAL MATRIX(N,N),B(N),BS(N),R(N)
      INTEGER I
      DOUBLE PRECISION CV(6)
C
C     Next line subroutine name changed                         Amendment A
      CALL ESMA11(MATRIX,B,CV,N,N)
C
      DO 10 I=1,N
      R(I)=SNGL(DBLE(BS(I))-CV(I))
10    CONTINUE
C
      RETURN
      END
C
C     ..................................................................
C  
C     Next line subroutine name changed                         Amendment A
      SUBROUTINE ESMA11(A,B,C,L,M)
C
C     PURPOSE : TO MULTIPLY A MATRIX AND VECTOR 
C
C     METHOD 
C     THE PRODUCT SUM IS ACCUMULATED IN DOUBLE PRECISION
C
C     LIST OF PARAMETERS
C     A        -   MATRIX
C     B        -   VECTOR
C     C        -   SOLUTION COLUMN MATRIX
C     L,M      -   DIMENSION OF MATRICES
C
C     ..................................................................
C
C     1 DIMENSION statement deleted                             Amendment A
C
C     Next 2 lines added                                        Amendment A
      REAL A(M,M),B(M)
      INTEGER I,K
C
C     Next line changed                                         Amendment A
      DOUBLE PRECISION S,C(M)
C
      DO 10 I=1,L
      S=0.0
      DO 20 K=1,M
      S=S+DBLE(A(I,K))*DBLE(B(K))
   20 CONTINUE
      C(I)=S
   10 CONTINUE
C
      RETURN
      END
C
C     ..................................................................
C
C     Next line subroutine name changed                         Amendment A
      SUBROUTINE ESMA12(MATRIX,N,PIVOT,A,INV)
C
C     PURPOSE : TO IMPROVE THE CALCULATED INVERSE
C
C     METHOD
C     SUCCESSIVE IMPROVEMENT OF THE COLUMNS OF THE INVERSE
C
C     LIST OF PARAMETERS
C     MATRIX   -   MATRIX TO BE INVERTED
C     N        -   DIMENSION OF MATICES
C     PIVOT    -   ARRAY RECORDING ROW INTERCHANGES
C     A        -   DECOMPOSED FORM OF MATRIX
C     INV      -   INVERSE OF MATRIX
C
C     ..................................................................
C
C     1 DIMENSION statement deleted                             Amendment A
C
C     Next 2 line changed                                       Amendment A
      REAL MATRIX(N,N),A(N,N),INV(N,N),X(6),BS(6)
      INTEGER PIVOT(N),I,J
C
      DO 123 J=1,N
      DO 456 I=1,N
      X(I)=INV(I,J)
      BS(I)=0.0
      IF(I.EQ.J)BS(I)=1.0
456   CONTINUE
      CALL ESMAT9(MATRIX,X,N,BS,PIVOT,A)
      DO 789 I=1,N
      INV(I,J)=X(I)
789   CONTINUE
123   CONTINUE
      RETURN
      END
C
C     ..................................................................
C
C     Next line subroutine name changed                         Amendment A
      SUBROUTINE ESMA13(MATRIX,N,AV)
C
C     PURPOSE : TO CALCULATE AN AVERAGE OF ARRAY MATRIX()
C
C     METHOD
C     AV=A(1,1)+ ... + A(N,N)/N**2
C
C     LIST OF PARAMETERS
C     MATRIX   -   MATRIX TO BE INVERTED
C     N        -   DIMENSION OF MATICES
C     AV       -   AVERAGE OF ARRAY MATRIX()
C
C     ..................................................................
C
C     1 DIMENSION statement deleted                             Amendment A
C
C     Next line added                                           Amendment A
      REAL MATRIX(N,N),MMAT,NU,START,E
      LOGICAL CON
C
C     Next line changed                                         Amendment A
      INTEGER TOT,AV,SIGN,I,J,K
C
      TOT=0
      AV =0
      NU=0
      DO 10 I=1,N
      DO 10 J=1,N
      SIGN=1
      START=10.0
      K=1
      MMAT=ABS(MATRIX(I,J))
      IF(MMAT.GT.0.0) NU=NU+1
      IF(MMAT.LE.1.0.AND.MMAT.GT.0.0) THEN
        SIGN =-1
        START=0.1
      END IF
   30 E=START**K
      CON=.FALSE.
      IF(SIGN.EQ.1) THEN
        IF(E.GT.MMAT) CON=.TRUE.
      ELSE
        IF(E.LT.MMAT) CON=.TRUE.
      END IF
      IF(CON) THEN
        TOT=TOT+K*SIGN
      ELSE
        K=K+1
        GOTO 30
      END IF
   10 CONTINUE
      AV=TOT/NU
C
      RETURN
      END
C***************************************************************
