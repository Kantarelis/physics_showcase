!__Band Structure, Transmittance and Reflectance of an Infinite and Semi-infinite Phononic Crystal____

PROGRAM PTC
    IMPLICIT NONE

!-----------------------------------------Integer Parameters------------------------------------------
    INTEGER :: N_DIM, LDA, LWORK, LDVL, LDVR
!-----------------------------------------------------------------------------------------------------

!-------------------------------Change The Parameter N_DIM If Necessary-------------------------------
    PARAMETER (N_DIM = 6500)
    PARAMETER (LDA = N_DIM)
    PARAMETER (LWORK = 2*N_DIM)
    PARAMETER (LDVL = N_DIM)
    PARAMETER (LDVR = N_DIM)
!-----------------------------------------------------------------------------------------------------

!----------------------------------------Define Everything--------------------------------------------
    CHARACTER(LEN=30) :: FORMAT

    REAL*8 :: PI, R_1, R_2, ALPHA, BETA, DISTANCE_1, DISTANCE_2, DISTANCE_INSIDE_1, T_TILES, R_TILES
    REAL*8 :: DISTANCE_INSIDE_2, K_MAX_REAL, E_01, E_02, ALPHA_PRECENTAGE, D_ALPHA_PRECENTAGE
    REAL*8 :: D_BETA_PRECENTAGE, S_PP, S_PM, S_MP, S_MM, E_1, E_2, Z_1, Z_2
    REAL*8 :: R_MAX, T_MAX, EMACH
    REAL*8 :: RWORK(2*N_DIM), K_1(N_DIM), K_2(N_DIM)
    REAL*8 :: OMEGA(N_DIM), MU_REAL(N_DIM), MU_IMAG(N_DIM), R_SEMI_INFINITY(N_DIM)

    COMPLEX*16 :: CI, CZERO, CONE, QIS, QIIS, QIIIS, QIVS, F_ANGLE
    COMPLEX*16 :: M(LDA,LDA), VL(N_DIM,N_DIM), VR(N_DIM,N_DIM), WORK(LWORK), LAMBDA(N_DIM)
    COMPLEX*16 :: QI(N_DIM), QII(N_DIM), QIII(N_DIM), QIV(N_DIM)

    INTEGER :: I, N, INFO, I_1, I_2, COUNTER, K_MAX, IMAGINE, INTERFACES, TILES

    DATA CI/(0.D0,1.D0)/, CZERO/(0.D0,0.D0)/, CONE/(1.D0,0.D0)/, EMACH/0.00000001D0/
!-----------------------------------------------------------------------------------------------------

!----------------------------------------Define Constants---------------------------------------------
    PI = 4.D0*DATAN(1.D0)
!-----------------------------------------------------------------------------------------------------

!----------------------------------------------File format--------------------------------------------

    !---------------------------------------------Notes-----------------------------------------------
    ! Choose how many decimals you want for the output data by changing the number after foolstop.
    !-------------------------------------------------------------------------------------------------

    FORMAT = '(100g20.9)'

!-----------------------------------------------------------------------------------------------------

!----------------------------------------Dimensions of NxN Matrix M-----------------------------------
    N = 2
!----------------------------------------------------------------------------------------------------

!----------------------------------------Changable Parameters----------------------------------------

    ! ------------------------------Define Densities and Young Modulus-------------------------------
    TILES = 16              !!For many_reflections: 8, 16 and 32
    R_1 = 0.7454D0
    R_2 = 1.2546D0
    E_01 = 1.D0
    E_02 = 1.D0
    ! -----------------------------------------------------------------------------------------------

    ! ---------Define Intices and Precentages of Relative Lengths inside the Primitive Shell---------
    INTERFACES = 2
    I_1 = 1
    I_2 = INTERFACES
    IMAGINE = 0
    ALPHA_PRECENTAGE = 0.6D0
    D_ALPHA_PRECENTAGE = 0.5D0
    D_BETA_PRECENTAGE = 0.5D0
    ! -----------------------------------------------------------------------------------------------

!----------------------------------------------------------------------------------------------------

!--------------------------------------------Resolution----------------------------------------------
    K_MAX = 6000
    K_MAX_REAL = DFLOAT(K_MAX)
!----------------------------------------------------------------------------------------------------

!----------------------------Define Length of Unit Slice and it's counterparts-----------------------
    ALPHA = ALPHA_PRECENTAGE
    BETA = (1.D0 - ALPHA_PRECENTAGE)
!----------------------------------------------------------------------------------------------------

!---------------------------------------Relative Distances for Qs' ------------------------------------
    DISTANCE_1 = (D_ALPHA_PRECENTAGE * ALPHA)
    DISTANCE_2 = ((1.D0 - D_ALPHA_PRECENTAGE) * ALPHA)
    DISTANCE_INSIDE_1 = (D_BETA_PRECENTAGE * BETA)
    DISTANCE_INSIDE_2 = ((1.D0 - D_BETA_PRECENTAGE) * BETA)
!----------------------------------------------------------------------------------------------------

!------------------------------------------Safety Measures--------------------------------------------
    IF (N .GT. N_DIM) STOP "N exceeds N_DIM: increase N_DIM"
    IF (DABS(DISTANCE_1 + DISTANCE_2 - ALPHA) .GT. EMACH) STOP "Fix Distances of ALPHA"
    IF (DABS(DISTANCE_INSIDE_1 + DISTANCE_INSIDE_2 - BETA) .GT. EMACH) STOP "Fix Distances of BETA"
!-----------------------------------------------------------------------------------------------------

!-----------------------------------------------Main Loop---------------------------------------------
    DO COUNTER=1, K_MAX

        OMEGA(COUNTER) = (0.D0) + (((DFLOAT(COUNTER - 1)) / (DFLOAT(K_MAX - 1))) * (10.D0))

        ! ---------------------Change "How Young Modulus' change over space"--------------------------
        E_1 = E_01! * (0.05D0 * (DFLOAT(COUNTER)))
        E_2 = E_02! * (45.0D0 * DCOS(DFLOAT(COUNTER)))
        ! --------------------------------------------------------------------------------------------

        K_1(COUNTER) = (OMEGA(COUNTER) * SQRT((R_1) / (E_1)))
        K_2(COUNTER) = (OMEGA(COUNTER) * SQRT((R_2) / (E_2)))

        Z_1 = SQRT(E_1 * R_1)
        Z_2 = SQRT(E_2 * R_2)

        S_PP = (2.D0 * Z_1) / (Z_1 + Z_2)
        S_PM = (Z_2 - Z_1) / (Z_1 + Z_2)
        S_MP = (Z_1 - Z_2) / (Z_1 + Z_2)
        S_MM = (2.D0 * Z_2) / (Z_1 + Z_2)

        QI(1)   = S_PP * ZEXP(CI * (K_1(COUNTER) * DISTANCE_1 + K_2(COUNTER) * DISTANCE_INSIDE_1))
        QII(1)  = S_PM * ZEXP(2.D0 * CI * K_2(COUNTER) * DISTANCE_INSIDE_1)
        QIII(1) = S_MP * ZEXP(2.D0 * CI * K_1(COUNTER) * DISTANCE_1)
        QIV(1)  = S_MM * ZEXP(CI * (K_1(COUNTER) * DISTANCE_1 + K_2(COUNTER) * DISTANCE_INSIDE_1))

        QI(2)   = S_MM * ZEXP(CI * (K_2(COUNTER) * DISTANCE_INSIDE_2 + K_1(COUNTER) * DISTANCE_2))
        QII(2)  = S_MP * ZEXP(2.D0 * CI * K_1(COUNTER) * DISTANCE_2)
        QIII(2) = S_PM * ZEXP(2.D0 * CI * K_2(COUNTER) * DISTANCE_INSIDE_2)
        QIV(2)  = S_PP * ZEXP(CI * (K_2(COUNTER) * DISTANCE_INSIDE_2 + K_1(COUNTER) * DISTANCE_2))

        CALL PAIR(QI, QII, QIII, QIV, INTERFACES, I_1, I_2, QIS, QIIS, QIIIS, QIVS)

        M(1,1) = QIS
        M(1,2) = QIIS
        M(2,1) = - (((QIIIS) * (QIS)) / (QIVS))
        M(2,2) = (1.D0 - (QIIIS * QIIS)) / (QIVS)

        CALL ZGEEV('N', 'V', N, M, LDA, LAMBDA, VL, LDVL, VR, LDVR, WORK, LWORK, RWORK, INFO)

        !------------------------------------------Data for Plotting----------------------------------

        DO I=1, N
            MU_REAL(I) = REAL(-CI * LOG(LAMBDA(I)))
            MU_IMAG(I) = IMAG(-CI * LOG(LAMBDA(I)))
        END DO

                        !----------------Reflectance of Semi - Infinite Crystal-----------------
        R_SEMI_INFINITY(1) = REAL((ABS((QIIIS) / (1.D0 - (QIVS * LAMBDA(1))))) ** (2.D0))
                        !-----------------------------------------------------------------------------------

                            !----------------Reflectance of N - Tile - Crystal----------------------

        F_ANGLE = ACOS(((QIS * QIS) - (QIIIS * QIIIS) + 1.D0) / (2.D0 * QIVS))

        T_TILES = REAL((1.D0) / (1.D0 + ((-((QIIIS * QIIIS) / (QIS * QIS)) * ((ZSIN(DFLOAT(TILES) * F_ANGLE)) &
        / (ZSIN(F_ANGLE))) * ((ZSIN(DFLOAT(TILES) * F_ANGLE)) / (ZSIN(F_ANGLE)))))))

        R_TILES = 1.D0 - T_TILES
                            !-----------------------------------------------------------------------

                            !--------------Maximum of Reflectance Calculations----------------------
        T_MAX = REAL((1.D0) / (1.D0 + ((-((QIIS * QIIIS) / (QIS * QIS)) * ((1.D0) &
        / (ZSIN(F_ANGLE))) * ((1.D0) / (ZSIN(F_ANGLE)))))))

        R_MAX = 1.D0 - T_MAX
                            !-----------------------------------------------------------------------

        !---------------------------------------Extract for Python Plotting---------------------------

        IF (TILES .EQ. 8) THEN
            OPEN(727, FILE = '/home/varoth/Desktop/Physics/PLOTS/Python/rtq_plots/rtq_data_REF1.txt', status='unknown')
            WRITE(727,FORMAT) OMEGA(COUNTER), "            ", R_TILES, "            ", R_MAX
        ELSEIF (TILES .EQ. 16) THEN
            OPEN(728, FILE = '/home/varoth/Desktop/Physics/PLOTS/Python/rtq_plots/rtq_data_REF2.txt', status='unknown')
            WRITE(728,FORMAT) OMEGA(COUNTER), "            ", R_TILES, "            ", R_MAX
        ELSEIF (TILES .EQ. 32) THEN
            OPEN(729, FILE = '/home/varoth/Desktop/Physics/PLOTS/Python/rtq_plots/rtq_data_REF3.txt', status='unknown')
            WRITE(729,FORMAT) OMEGA(COUNTER), "            ", R_TILES, "            ", R_MAX
        ENDIF

        OPEN(77, FILE = '/home/varoth/Desktop/Physics/PLOTS/Python/rtq_plots/rtq_data.txt', status='unknown')
        IF (IMAGINE .EQ. 1) THEN
            WRITE(77,FORMAT) OMEGA(COUNTER), "            ", MU_IMAG(1), "            ",        &
            MU_IMAG(2), "            ", (R_TILES), "            ",  R_SEMI_INFINITY(1), "            ",  R_MAX
        ELSEIF (IMAGINE .EQ. 0) THEN
            WRITE(77,FORMAT) OMEGA(COUNTER), "            ", MU_REAL(1), "            ",        &
            MU_REAL(2), "            ", (R_TILES), "            ",  R_SEMI_INFINITY(1), "            ",  R_MAX
        ELSEIF (IMAGINE .EQ. 2) THEN
            WRITE(77,FORMAT) OMEGA(COUNTER), "            ", MU_IMAG(1), "            ",        &
            MU_IMAG(2), "            ", (R_TILES), "            ",  R_SEMI_INFINITY(1), "            ",  R_MAX
            WRITE(77,FORMAT) OMEGA(COUNTER), "            ", MU_REAL(1), "            ",        &
            MU_REAL(2), "            ", (R_TILES), "            ",  R_SEMI_INFINITY(1), "            ",  R_MAX
        END IF

        OPEN(717, FILE = '/home/varoth/Desktop/Physics/PLOTS/Python/rtq_plots/rtq_data_imaginary.txt', status='unknown')
        WRITE(717,FORMAT) OMEGA(COUNTER), "            ", MU_IMAG(1), "            ", MU_IMAG(2)
    END DO

CLOSE(77)
CLOSE(717)
!-----------------------------------------------------------------------------------------------------

!--------------------------------------------Stop Everything------------------------------------------
STOP
!-----------------------------------------------------------------------------------------------------

END PROGRAM PTC

!_____________________________________________________________________________________________________

!--------------------------------------------------------------------------
      SUBROUTINE PAIR(QI,QII,QIII,QIV,NINTERFD,I1,I2,QIS,QIIS,QIIIS,QIVS)
!--------------------------------------------------------------------------
!     This routine calculates the transmission and reflection
!     coefficients,QS, of a slab consisting of successive homogeneous
!     layers separated by I1,I1+1,..., I2
!     interfaces from those, Q, of the individual interfaces.
!--------------------------------------------------------------------------
!
!     .. PARAMETER STATEMENTS ..
!
    INTEGER*4 NINTERFD
!   PARAMETER(NINTERFD=20)
!
!     .. SCALAR ARGUMENTS ..
!
    INTEGER*4  I1,I2
    COMPLEX*16 QIS,QIIS,QIIIS,QIVS
!
!     .. ARRAY ARGUMENTS ..
!
    COMPLEX*16 QI(NINTERFD),QII(NINTERFD),QIII(NINTERFD),QIV(NINTERFD)

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
    END
!=====================================================================================================

!-----------------------------------------------------------------------------------------------------
!Prof. Dr. Nikolaos Stefanou
!Department of Solid State Physics
!National and Kapodistrian University of Athens
!Panepistimioupolis Zografou
!GR-157 84 Athens, GREECE
