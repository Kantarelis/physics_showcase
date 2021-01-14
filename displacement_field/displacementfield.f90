PROGRAM DEFECT

    !
    CHARACTER(LEN=30) :: FORMAT
    !
    !   .. SCALAR VARIABLES ..
    !
    INTEGER*4  :: N_MATER_DIM, N_MATER, I_MATER, N_INTERF_DIM, N_INTERF, INTERF, ML, MR, SPACER
    INTEGER*4  :: N_LAYER, I_LAYER, N_LAYER_DIM, RESOLUTION, N_DIM, I, I_B, FIELD, MAT, DOWN
    !
    PARAMETER  (N_MATER_DIM=3, N_INTERF_DIM=60, N_LAYER_DIM=N_INTERF_DIM+1, N_DIM=8000)
    !
    REAL*8     :: OMEGA_1, OMEGA_2, QLDL, QRDR, PI, T_TILES, R_TILES, OMEGA, SWIFT
    REAL*8     :: LENGTH, OM_RES, EMACH, LEFT_BOUNDARY, RIGHT_BOUNDARY, Z_FOR_PHASE_1, Z_FOR_PHASE_N
    !
    COMPLEX*16 ::  QIS, QIIS, QIIIS, QIVS, QIS_L, QIIS_L, QIIIS_L, QIVS_L, QIS_R, QIIS_R, QIIIS_R, QIVS_R
    COMPLEX*16 :: CZERO, CONE, CI
    !
    DATA CONE/(1.D0,0.D0)/, CI/(0.D0,1.D0)/, CZERO/(0.D0,0.D0)/, EMACH/0.00000001D0/
    !
    !   .. ARRAY VARIABLES ..
    !
    INTEGER*4  :: MATER_L(N_INTERF_DIM), MATER_R(N_INTERF_DIM), MATER(N_LAYER_DIM)
    !
    REAL*8     :: RHO(N_MATER_DIM), YOUNG(N_MATER_DIM), VL(N_MATER_DIM), Z(N_MATER_DIM), DL(N_INTERF_DIM), DR(N_INTERF_DIM)
    REAL*8     :: DIS(N_LAYER_DIM), Z_ORIGIN(N_LAYER_DIM), X_INN(N_INTERF_DIM,N_DIM), U_RES_REAL(N_INTERF_DIM,N_DIM)
    REAL*8     :: U_RES_IMAG(N_INTERF_DIM,N_DIM), FI_RES(N_INTERF_DIM,N_DIM)
    REAL*8     :: X_A(N_DIM), X_C(N_DIM), U_RES_A(N_DIM), U_RES_C(N_DIM), K_W(N_DIM), QZ(N_INTERF_DIM,N_DIM)
    REAL*8     :: U_RES_ABS(N_INTERF_DIM,N_DIM), Z_FOR_PHASE(N_LAYER_DIM), X_1(N_DIM), X_N(N_DIM), U_RES_1(N_DIM), U_RES_N(N_DIM)

    COMPLEX*16 :: QI(N_INTERF_DIM), QII(N_INTERF_DIM), QIII(N_INTERF_DIM), QIV(N_INTERF_DIM)
    COMPLEX*16 :: WAQ(N_DIM), COEFF_P(N_DIM), COEFF_M(N_DIM), U_RES_Z(N_INTERF_DIM,N_DIM)
    !
    !   .. INTRINSIC FUNCTIONS ..
    !
    INTRINSIC DATAN, DFLOAT, ZEXP, DCONJG, INT, DREAL, DIMAG
    !------------------------------------------------------------------------------------

    !--------------------------------Define Constants------------------------------------
        PI = 4.D0*DATAN(1.D0)
    !------------------------------------------------------------------------------------

    !------------------------------------------Control Panel----------------------------------------------
        FIELD = 1                       ! It's either 0 or 1 
        SPACER = 0                      ! It's either 0, 4, 8 or 12
                                        ! It's either 0 or 1 (Close or Open;
                                        ! Open only when you need too print the field of both bond and anti - bond states
                                        ! Open ------> prints the field of down state.
                                        ! Closed -------> prints the field of upper state.)
        DOWN = 0
        LEFT_BOUNDARY = 4.D0
        RIGHT_BOUNDARY = LEFT_BOUNDARY
    !-----------------------------------------------------------------------------------------------------

    !----------------------------------------------File format--------------------------------------------

        !---------------------------------------------Notes-----------------------------------------------
        ! Choose how many decimals you want for the output data by changing the number after foolstop.
        !-------------------------------------------------------------------------------------------------

        FORMAT = '(100g20.9)'

    !-----------------------------------------------------------------------------------------------------
    IF (SPACER .EQ. 0)THEN
        OPEN(UNIT=20,FILE="RTQ_D_INPUT_DATA")
    ELSEIF (SPACER .EQ. 4)THEN
        OPEN(UNIT=20,FILE="RTQ_D_4_SPACER")
    ELSEIF (SPACER .EQ. 8)THEN
        OPEN(UNIT=20,FILE="RTQ_D_8_SPACER")
    ELSEIF (SPACER .EQ. 12)THEN
        OPEN(UNIT=20,FILE="RTQ_D_12_SPACER")
    ENDIF
    !
    !   READING INPUT DATA
    !
    READ(20,*) N_MATER
    IF(N_MATER .GT. N_MATER_DIM) STOP 'Number of materials is greater than dimensioned'

    READ(20,*) N_INTERF
    N_LAYER = N_INTERF - 1
    IF(N_INTERF .GT. N_INTERF_DIM) STOP 'Number of interfaces is greater than dimensioned'

    READ(20,*) RESOLUTION

    DO I_MATER = 1, N_MATER
        READ(20,*) RHO(I_MATER), YOUNG(I_MATER)
        VL(I_MATER) = DSQRT(YOUNG(I_MATER) / RHO(I_MATER))
        Z(I_MATER) = RHO(I_MATER)*VL(I_MATER)
    END DO


    LENGTH = 0.D0

    DO INTERF = 1, N_INTERF
        READ(20,*) MATER_L(INTERF), DL(INTERF)
        IF (INTERF .NE. 1) THEN
            IF (MATER_L(INTERF) .NE. MATER_R(INTERF-1)) STOP 'Improper material sequence'
            I_LAYER = INTERF - 1
            MATER(I_LAYER) = MATER_L(INTERF)
            DIS(I_LAYER) = DR(INTERF-1) + DL(INTERF)
            OPEN(878, FILE = '/home/varoth/Desktop/Physics/PLOTS/Python/rtq_plots/layers.txt', status='unknown')
            WRITE(878,FORMAT) DIS(I_LAYER)
            OPEN(879, FILE = '/home/varoth/Desktop/Physics/PLOTS/Python/rtq_plots/matter.txt', status='unknown')
            WRITE(879,FORMAT) MATER(I_LAYER)
        END IF

        READ(20,*) MATER_R(INTERF), DR(INTERF)
        LENGTH = LENGTH + DL(INTERF) + DR(INTERF)
    END DO
    CLOSE(878)
    CLOSE(879)
    PRINT *, 'Length of the slab', LENGTH

    ! ------------------------------- Swift the place of the plot in x - axis ----------------------------
    SWIFT = LENGTH / 2.D0
    !-----------------------------------------------------------------------------------------------------

    MATER(1) = MATER_L(1)
    MATER(N_LAYER) = MATER_R(N_INTERF)
    DIS(1) = DL(1) + DR(1)
    DIS(N_LAYER) = DR(N_INTERF) + DL(N_INTERF)
    Z_ORIGIN(1) = 0.0D0

    DO I_LAYER = 2, N_LAYER
        INTERF = I_LAYER
        Z_ORIGIN(I_LAYER) = Z_ORIGIN(I_LAYER-1) + DL(INTERF) + DR(INTERF-1)
    END DO

    IF (RESOLUTION .GT. N_DIM) STOP 'Number of z-points grater than dimensioned'
    READ(20,*) OMEGA_1, OMEGA_2, RESOLUTION
    IF(OMEGA_2 .LT. OMEGA_1 .OR. RESOLUTION .LT. 2) STOP 'Improper frequency mesh'
    READ(20,*) OM_RES

    OPEN(777, FILE = '/home/varoth/Desktop/Physics/PLOTS/Python/rtq_plots/spacer.txt', status='unknown')
    WRITE(777,FORMAT) DIS(1), DIS(N_LAYER), N_LAYER, LEFT_BOUNDARY, RIGHT_BOUNDARY, LENGTH, N_MATER, SWIFT
    CLOSE(777)

    IF (FIELD .EQ. 0) THEN

        DO I=1, RESOLUTION

            OMEGA = (OMEGA_1) + (((DFLOAT(I - 1)) / (DFLOAT(RESOLUTION - 1))) * (OMEGA_2 - OMEGA_1))

            DO INTERF = 1, N_INTERF
                ML = MATER_L(INTERF)
                MR = MATER_R(INTERF)
                QLDL = DL(INTERF) * OMEGA * (2.D0 * PI) / VL(ML)
                QRDR = DR(INTERF) * OMEGA * (2.D0 * PI) / VL(MR)
                QI  (INTERF) = (   2.D0 * Z(ML) / (Z(ML) + Z(MR))) * ZEXP(CI * (QLDL + QRDR))
                QII (INTERF) = ((Z(MR) - Z(ML)) / (Z(ML) + Z(MR))) * ZEXP(2.D0 * CI * QRDR)
                QIII(INTERF) = ((Z(ML) - Z(MR)) / (Z(ML) + Z(MR))) * ZEXP(2.D0 * CI * QLDL)
                QIV (INTERF) = (   2.D0 * Z(MR) / (Z(ML) + Z(MR))) * ZEXP(CI * (QLDL + QRDR))
            END DO

            CALL PAIR(QI,QII,QIII,QIV,N_INTERF_DIM,1,N_INTERF,QIS,QIIS,QIIIS,QIVS)

            T_TILES = ZABS(QIS * QIS)
            R_TILES = ZABS(QIIIS * QIIIS)

            !------------------------------------------Safety Measure---------------------------------------------
            IF (REAL(ZABS(R_TILES + T_TILES - CONE)) .GT. EMACH) STOP "Refletion and Transmission are wrong"
            IF (REAL((R_TILES + T_TILES)) .LT. EMACH) STOP "Refletion and Transmission are wrong"
            !-----------------------------------------------------------------------------------------------------

            !---------------------------------------Extract for Python Plotting---------------------------
            OPEN(188, FILE = '/home/varoth/Desktop/Physics/PLOTS/Python/rtq_plots/rtq_data_band_d.txt', status='unknown')
            WRITE(188,FORMAT) OMEGA, "            ", T_TILES
            IF (SPACER .EQ. 4)THEN
                OPEN(189, FILE = '/home/varoth/Desktop/Physics/PLOTS/Python/rtq_plots/rtq_data_band_d_4_spacer.txt', &
                status='unknown')
                WRITE(189,FORMAT) OMEGA, "            ", T_TILES
            ELSEIF (SPACER .EQ. 8)THEN
                OPEN(190, FILE = '/home/varoth/Desktop/Physics/PLOTS/Python/rtq_plots/rtq_data_band_d_8_spacer.txt', &
                status='unknown')
                WRITE(190,FORMAT) OMEGA, "            ", T_TILES
            ELSEIF (SPACER .EQ. 12)THEN
                OPEN(191, FILE = '/home/varoth/Desktop/Physics/PLOTS/Python/rtq_plots/rtq_data_band_d_12_spacer.txt', &
                status='unknown')
                WRITE(191,FORMAT) OMEGA, "            ", T_TILES
            ENDIF
        ENDDO

        CLOSE(188)
        IF (SPACER .EQ. 4)THEN
            CLOSE(189)
        ELSEIF (SPACER .EQ. 8)THEN
            CLOSE(190)
        ELSEIF (SPACER .EQ. 12)THEN
            CLOSE(191)
        ENDIF

    ELSEIF (FIELD .EQ. 1)THEN

        DO INTERF = 1, N_INTERF
            ML = MATER_L(INTERF)
            MR = MATER_R(INTERF)
            QLDL = DL(INTERF) * OM_RES * (2.D0 * PI) / VL(ML)
            QRDR = DR(INTERF) * OM_RES * (2.D0 * PI) / VL(MR)
            QI  (INTERF) = (   2.D0 * Z(ML) / (Z(ML) + Z(MR))) * ZEXP(CI * (QLDL + QRDR))
            QII (INTERF) = ((Z(MR) - Z(ML)) / (Z(ML) + Z(MR))) * ZEXP(2.D0 * CI * QRDR)
            QIII(INTERF) = ((Z(ML) - Z(MR)) / (Z(ML) + Z(MR))) * ZEXP(2.D0 * CI * QLDL)
            QIV (INTERF) = (   2.D0 * Z(MR) / (Z(ML) + Z(MR))) * ZEXP(CI * (QLDL + QRDR))
        END DO

        CALL PAIR(QI,QII,QIII,QIV,N_INTERF_DIM,1,N_INTERF,QIS,QIIS,QIIIS,QIVS)

        PRINT *, 'Verification', ZABS(QIS * QIS)    ! Verification

        !----------------------------------------Displacement Field----------------------------------------------

        !------------------------------------------Phase Swifts--------------------------------------------------

        !--------------------------------------------------------------------------------------------------------
        DO I=1, RESOLUTION

            X_A(I) = ((0.D0) + (((DFLOAT(I - 1)) / (DFLOAT(RESOLUTION - 1))) * ( - LEFT_BOUNDARY)))

            X_1(I) = (0.D0) + (((DFLOAT(I - 1)) / (DFLOAT(RESOLUTION - 1))) * (DIS(1) + DIS(N_LAYER)))

            Z_FOR_PHASE_1 = 0.D0

            Z_FOR_PHASE_N = LENGTH

            X_N(I) = (LENGTH - DIS(N_LAYER) - DIS(1)) + (((DFLOAT(I - 1)) / (DFLOAT(RESOLUTION - 1))) * (DIS(N_LAYER) + DIS(1)))
            WRITE(15,FORMAT) DIS(1)

            X_C(I) = ((LENGTH) + (((DFLOAT(I - 1)) / (DFLOAT(RESOLUTION - 1))) * (RIGHT_BOUNDARY)))

        !----------------------------------------REGION I-----------------------------------------------

            K_W(1) = OM_RES * (2.D0 * PI) / VL(1)

            U_RES_A(I) = (ZABS((ZEXP(CI * K_W(1) * (X_A(I) - Z_FOR_PHASE_1))) + &
                        (QIIIS * ZEXP(-CI * K_W(1) * (X_A(I) - Z_FOR_PHASE_1)))))
        !-----------------------------------------------------------------------------------------------

        !----------------------------------------REGION 1-----------------------------------------------

            K_W(1) = OM_RES * (2.D0 * PI) / VL(1)

            U_RES_1(I) = (ZABS((ZEXP(CI * K_W(1) * (X_1(I) - Z_FOR_PHASE_1))) + &
                        (QIIIS * ZEXP(-CI * K_W(1) * (X_1(I) - Z_FOR_PHASE_1)))))
        !-----------------------------------------------------------------------------------------------
        ENDDO

        !----------------------------------------REGION II-----------------------------------------------
        DO I_LAYER = 2, N_LAYER

            INTERF = I_LAYER
            I_B = N_INTERF - INTERF

            CALL PAIR(QI, QII, QIII, QIV, INTERF, 1, INTERF, QIS_L, QIIS_L, QIIIS_L, QIVS_L)
            CALL PAIR(QI, QII, QIII, QIV, I_B, INTERF+1, N_INTERF, QIS_R, QIIS_R, QIIIS_R, QIVS_R)

            WAQ(I_LAYER) = (CONE - QIIS_L * QIIIS_R)
            COEFF_P(I_LAYER) = QIS_L / WAQ(I_LAYER)
            COEFF_M(I_LAYER) = (QIS_L * QIIIS_R) / WAQ(I_LAYER)

            DO I=1, RESOLUTION
                X_INN(I_LAYER,I) = (Z_ORIGIN(I_LAYER)) + (((DFLOAT(I - 1)) / (DFLOAT(RESOLUTION - 1))) * &
                (DIS(I_LAYER)))

                MAT = MATER(I_LAYER)

                Z_FOR_PHASE(I_LAYER) = Z_ORIGIN(I_LAYER) + DL(I_LAYER+1)
                QZ(I_LAYER,I) = OM_RES * (2.D0 * PI) * (X_INN(I_LAYER,I) - Z_FOR_PHASE(I_LAYER)) / VL(MAT)

                U_RES_Z(I_LAYER,I) = ((COEFF_P(I_LAYER) * ZEXP(CI * QZ(I_LAYER,I)) + &
                                         COEFF_M(I_LAYER) * ZEXP(-CI * QZ(I_LAYER,I))))

                U_RES_ABS(I_LAYER,I) = (ZABS(U_RES_Z(I_LAYER,I))) ** (2.D0)

                U_RES_REAL(I_LAYER,I) = (DREAL(U_RES_Z(I_LAYER,I)))

                U_RES_IMAG(I_LAYER,I) = (DIMAG(U_RES_Z(I_LAYER,I)))

                FI_RES(I_LAYER,I) = DATAN2(U_RES_IMAG(I_LAYER,I), U_RES_REAL(I_LAYER,I))
            ENDDO
        ENDDO

        IF (DOWN .EQ. 0) THEN
            OPEN(531, FILE = '/home/varoth/Desktop/Physics/PLOTS/Python/rtq_plots/rtq_data_field_d_INNER_X.txt', status='unknown')
            DO I=1, RESOLUTION
                WRITE(531,FORMAT) (X_INN(I_LAYER,I) - SWIFT, I_LAYER=1, N_LAYER)
            ENDDO
            CLOSE(531)

            OPEN(532, FILE = '/home/varoth/Desktop/Physics/PLOTS/Python/rtq_plots/rtq_data_field_d_INNER_Y.txt', status='unknown')
            DO I=1, RESOLUTION
                WRITE(532,FORMAT) (U_RES_ABS(I_LAYER,I), I_LAYER=1,N_LAYER)
            ENDDO
            CLOSE(532)

            OPEN(533, FILE = '/home/varoth/Desktop/Physics/PLOTS/Python/rtq_plots/rtq_data_d_IN_ARGUMENT.txt', status='unknown')
            DO I=1, RESOLUTION
                WRITE(533,FORMAT) (FI_RES(I_LAYER,I), I_LAYER=1,N_LAYER)
            ENDDO
            CLOSE(533)
        ELSEIF (DOWN .EQ. 1) THEN
            OPEN(534, FILE = '/home/varoth/Desktop/Physics/PLOTS/Python/rtq_plots/rtq_data_field_d_INNER_X_D.txt', status='unknown')
            DO I=1, RESOLUTION
                WRITE(534,FORMAT) (X_INN(I_LAYER,I) - SWIFT, I_LAYER=1, N_LAYER)
            ENDDO
            CLOSE(534)

            OPEN(535, FILE = '/home/varoth/Desktop/Physics/PLOTS/Python/rtq_plots/rtq_data_field_d_INNER_Y_D.txt', status='unknown')
            DO I=1, RESOLUTION
                WRITE(535,FORMAT) (U_RES_ABS(I_LAYER,I), I_LAYER=1,N_LAYER)
            ENDDO
            CLOSE(535)

            OPEN(536, FILE = '/home/varoth/Desktop/Physics/PLOTS/Python/rtq_plots/rtq_data_d_IN_ARGUMENT_D.txt', status='unknown')
            DO I=1, RESOLUTION
                WRITE(536,FORMAT) (FI_RES(I_LAYER,I), I_LAYER=1,N_LAYER)
            ENDDO
            CLOSE(536)
        ENDIF
        !-----------------------------------------------------------------------------------------------

        !----------------------------------------REGION N-----------------------------------------------
        DO I=1, RESOLUTION

            K_W(1) = OM_RES * (2.D0 * PI) / VL(1)

            U_RES_N(I) = (ZABS((QIS * ZEXP(CI * K_W(1) * (X_N(I) - Z_FOR_PHASE_N))) + &
                        (CZERO * ZEXP(-CI * K_W(1) * (X_N(I) - Z_FOR_PHASE_N)))))
        !-----------------------------------------------------------------------------------------------

        !----------------------------------------REGION III---------------------------------------------

            U_RES_C(I) = (ZABS(QIS * ZEXP(CI * OM_RES * (2.D0 * PI) * VL(N_INTERF) * (X_C(I) - (LENGTH)))))
        ENDDO
        !-----------------------------------------------------------------------------------------------

        IF (DOWN .EQ. 0) THEN
            !-----------------------------------------------------------------------------------------------
            DO I=1, RESOLUTION
                OPEN(537, FILE = '/home/varoth/Desktop/Physics/PLOTS/Python/rtq_plots/rtq_data_field_d_LRB.txt', status='unknown')
                WRITE(537,FORMAT) X_A(I) - SWIFT, "            ", X_1(I) - SWIFT, "            ", X_N(I) - SWIFT, &
                "            ", X_C(I) - SWIFT, "            ", U_RES_A(I), "            ", U_RES_1(I), "            ", &
                U_RES_N(I), "            ", U_RES_C(I)
            END DO
            !-----------------------------------------------------------------------------------------------
            CLOSE(537)
        ELSEIF (DOWN .EQ. 1) THEN
            !-----------------------------------------------------------------------------------------------
            DO I=1, RESOLUTION
                OPEN(538, FILE = '/home/varoth/Desktop/Physics/PLOTS/Python/rtq_plots/rtq_data_field_d_LRB_D.txt', status='unknown')
                WRITE(538,FORMAT) X_A(I) - SWIFT, "            ", X_1(I) - SWIFT, "            ", X_N(I) - SWIFT, &
                "            ", X_C(I) - SWIFT, "            ", U_RES_A(I), "            ", U_RES_1(I), "            ", &
                U_RES_N(I), "            ", U_RES_C(I)
            END DO
            !-----------------------------------------------------------------------------------------------
            CLOSE(538)
        ENDIF
    ENDIF

ENDPROGRAM DEFECT
!_____________________________________________________________________________________________________

!--------------------------------------------------------------------------
SUBROUTINE PAIR(QI,QII,QIII,QIV,N_INTERF_DIM,I1,I2,QIS,QIIS,QIIIS,QIVS)
!--------------------------------------------------------------------------
!     This routine calculates the transmission and reflection
!     coefficients,QS, of a slab consisting of successive homogeneous
!     layers separated by I1,I1+1,..., I2
!     interfaces from those, Q, of the individual interfaces.
!--------------------------------------------------------------------------
!
!     .. PARAMETER STATEMENTS ..
!
INTEGER*4 N_INTERF_DIM
!   PARAMETER(N_INTERF_DIM=20)
!
!     .. SCALAR ARGUMENTS ..
!
INTEGER*4  I1,I2
COMPLEX*16 QIS,QIIS,QIIIS,QIVS
!
!     .. ARRAY ARGUMENTS ..
!
COMPLEX*16 QI(N_INTERF_DIM),QII(N_INTERF_DIM),QIII(N_INTERF_DIM),QIV(N_INTERF_DIM)

!
!     .. LOCAL SCALARS ..
!
INTEGER*4  I
COMPLEX*16 QIA,QIIA,QIIIA,QIVA,QIB,QIIB,QIIIB,QIVB,W,CONE
DATA CONE/(1.D0,0.D0)/
!--------------------------------------------------------------------------
IF(I2.LT.I1) STOP 'Wrong interface indices'

IF(I2.EQ.I1) THEN
  QIS  =QI  (I1)
  QIIS =QII (I1)
  QIIIS=QIII(I1)
  QIVS =QIV (I1)
ELSE
  QIA  =QI  (I1)
  QIIA =QII (I1)
  QIIIA=QIII(I1)
  QIVA =QIV (I1)
  DO I=I1+1,I2
      QIB  =QI  (I)
      QIIB =QII (I)
      QIIIB=QIII(I)
      QIVB =QIV (I)
      W=CONE-QIIA*QIIIB
      QIS  =QIA*QIB/W
      QIIS =QIIB+QIB*QIIA*QIVB/W
      QIIIS=QIIIA+QIVA*QIIIB*QIA/W
      QIVS =QIVA*QIVB/W
      QIA  =QIS
      QIIA =QIIS
      QIIIA=QIIIS
      QIVA =QIVS
  END DO
END IF
RETURN
ENDSUBROUTINE PAIR
!=====================================================================================================

!-----------------------------------------------------------------------------------------------------
!Prof. Dr. Nikolaos Stefanou
!Department of Solid State Physics
!National and Kapodistrian University of Athens
!Panepistimioupolis Zografou
!GR-157 84 Athens, GREECE