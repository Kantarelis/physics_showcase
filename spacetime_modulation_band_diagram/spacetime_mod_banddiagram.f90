!___________NON-RECIPROCAL ELASTIC WAVE PROPAGATION IN SPATIOTEMPORAL PERIODIC STRUCTURES_____________

PROGRAM PTC
    IMPLICIT NONE

!-----------------------------------------Integer Parameters------------------------------------------

    INTEGER :: N_DIM, LDA, LDB, LDC, LDVL, LDVR

!-----------------------------------------------------------------------------------------------------

!-------------------------------Change The Parameter N_DIM If Necessary-------------------------------

    PARAMETER (N_DIM = 1500)
    PARAMETER (LDA = N_DIM)
    PARAMETER (LDB = N_DIM)
    PARAMETER (LDC = N_DIM)
    PARAMETER (LDVL = 2*N_DIM)
    PARAMETER (LDVR = 2*N_DIM)

!-----------------------------------------------------------------------------------------------------

!----------------------------------------Define Everything--------------------------------------------

    CHARACTER(LEN=30) :: FORMAT

    COMPLEX*16 :: CI, CZERO, CONE

    COMPLEX*16 :: A(LDA,LDA), B(LDB,LDB), C(LDC,LDC), ALPHA(2*N_DIM), BETA(2*N_DIM)
    COMPLEX*16 :: VL(LDVL,2*LDVL), VR(LDVR,2*LDVR), E_P(N_DIM), R_P(N_DIM)
    COMPLEX*16 :: E_PI(N_DIM,N_DIM), R_PI(N_DIM,N_DIM), F_INT1(N_DIM), F_INT2(N_DIM), F_INT3(N_DIM)
    COMPLEX*16 :: G_INT1(N_DIM), G_INT2(N_DIM), G_INT3(N_DIM), Q_1(N_DIM,N_DIM), Q_2(N_DIM,N_DIM)
    COMPLEX*16 :: Q_3(N_DIM,N_DIM), Q_INT1(N_DIM), Q_INT2(N_DIM), Q_INT3(N_DIM), G_1(N_DIM,N_DIM)
    COMPLEX*16 :: G_2(N_DIM,N_DIM), G_3(N_DIM,N_DIM), F_1(N_DIM,N_DIM), F_2(N_DIM,N_DIM), F_3(N_DIM,N_DIM)
    COMPLEX*16 :: FP_INT1(N_DIM), FP_INT2(N_DIM), FP_INT3(N_DIM), GP_INT1(N_DIM), GP_INT2(N_DIM), GP_INT3(N_DIM)
    COMPLEX*16 :: QP_INT1(N_DIM), QP_INT2(N_DIM), QP_INT3(N_DIM), GA1(N_DIM), GB1(N_DIM), PG1(N_DIM), GA2(N_DIM)
    COMPLEX*16 :: GB2(N_DIM), PG2(N_DIM), GA3(N_DIM), GB3(N_DIM), PG3(N_DIM), FA1(N_DIM), FB1(N_DIM), PF1(N_DIM)
    COMPLEX*16 :: FA2(N_DIM), FB2(N_DIM), PF2(N_DIM), FA3(N_DIM), FB3(N_DIM), PF3(N_DIM), QA1(N_DIM), QA2(N_DIM)
    COMPLEX*16 :: QA3(N_DIM), QB1(N_DIM), QB2(N_DIM), QB3(N_DIM), PQ1(N_DIM), PQ2(N_DIM), PQ3(N_DIM)
    COMPLEX*16 :: R_ZERO(N_DIM), R_ONE(N_DIM), R_PI_ONE(N_DIM,N_DIM), C_LIN(N_DIM), LAMBDA(2*N_DIM,N_DIM), REL(N_DIM)
    COMPLEX*16 :: REL_1(N_DIM,N_DIM), REL_2(N_DIM,N_DIM), REL_3(N_DIM,N_DIM)
    COMPLEX*16 :: A_COPY(N_DIM,N_DIM), B_COPY(N_DIM,N_DIM), C_COPY(N_DIM,N_DIM)

    REAL*8 :: S(2*N_DIM), BEVL(2*N_DIM), BEVR(2*N_DIM), X_G(N_DIM), X_F(N_DIM), K_AN_1(N_DIM,N_DIM), K_AN_2(N_DIM,N_DIM)
    REAL*8 :: X_Q(N_DIM), NQ(N_DIM), PQ(N_DIM), OMEGA(N_DIM)
    REAL*8 :: REAL_K(N_DIM,N_DIM), IMAG_K(N_DIM,N_DIM)

    REAL*8 :: TOL, PI, AM, BM, NM, R_0, R_M, E_0, E_M, OMEGA_M, N_M, K_M, C_0, L_M, T_M
    REAL*8 :: Q_M, MAXSTEP_REAL, H_G, H_F, H_Q, EMACH

    INTEGER :: I, J, TRUNCATION, N, SCAL, SENSE, IWARN, INFO, MAXSTEP, COUNTER, RCOS, SPACETIME, K, T

    DATA CI/(0.D0,1.D0)/, CZERO/(0.D0,0.D0)/, CONE/(1.D0,0.D0)/, EMACH/0.000001D0/

!-----------------------------------------------------------------------------------------------------

!----------------------------------------Define Constants---------------------------------------------

    PI = 4.D0*DATAN(1.D0)

!-----------------------------------------------------------------------------------------------------

!----------------------------------------Modulation Parameters----------------------------------------

    SPACETIME = 0        ! ..........It's either 0 = OFF (x,t) or 1 = ON (ξ=x-υt)........From 0 to 1
    RCOS = 2             ! ..It's either 0 = E(x,t), 1 = R(x,t) or 2 = R(x,t) and E(x)...From 0 to 2
    TRUNCATION = 3       ! ..............................................................From 1 to 7
    AM = 0.00D0          ! ...............................E(x)...........................From 0 to 1
    BM = 0.20D0          ! ..............................R(x,t)..........................From 0 to 1
    NM = 0.2D0           ! .......................(omega_m/k_m) / C_0....................From 0 to 1

!-----------------------------------------------------------------------------------------------------

!-------------------------------------Define Dimensions of Matrices-----------------------------------

    N = (2 * TRUNCATION) + 1

!-----------------------------------------------------------------------------------------------------

!----------------------------------------------Resolution---------------------------------------------

    MAXSTEP = 1000

    MAXSTEP_REAL = DFLOAT(MAXSTEP)

!-----------------------------------------------------------------------------------------------------

!----------------------Problem Related Constants that Cannot be Changed (Modulation)------------------

    K_M = (2.D0 * PI)
    R_0 = 1.D0
    C_0 = 1.D0
    E_0 = (C_0**2) * R_0
    IF (RCOS .EQ. 0) THEN
        E_M = E_0 * AM / 2.D0
    ELSEIF (RCOS .EQ. 1) THEN
        R_M = R_0 * BM / 2.D0
    ELSEIF (RCOS .EQ. 2) THEN
        E_M = E_0 * AM! / 2.D0
        R_M = R_0 * BM! / 2.D0
    ENDIF
    N_M = C_0 * NM
    OMEGA_M = N_M * K_M
    L_M = (2.D0 * PI) * (K_M**(-1))
    T_M = (2.D0 * PI) * (OMEGA_M**(-1))
    Q_M = L_M

    WRITE(210,*) N_M, T_M, Q_M, L_M

!-----------------------------------------------------------------------------------------------------

!----------------------------------------------File format--------------------------------------------

    !---------------------------------------------Notes-----------------------------------------------
    ! Choose how many decimals you want for the output data by changing the number after foolstop.
    !-------------------------------------------------------------------------------------------------

    FORMAT = '(100g20.9)'

!-----------------------------------------------------------------------------------------------------

!----------------------------------------Changeable Parameters----------------------------------------
!
!  ----------------------------------------------Notes------------------------------------------------
!    //SCAL// - On entry: determines the form of scaling to be performed on
!                    A, B and C.
!          scal = 0: no scaling
!          scal = 1: (the recommended value.) Fan, Lin and Van Dooren scaling
!                     if norm(B)/sqrt(norm(A)*norm(C)) < 10  and no scaling
!                     otherwise, where norm(Z) is the Frobenius norm of Z.
!          scal = 2: Fan, Lin and Van Dooren scaling.
!          scal = 3: tropical scaling with largest root.
!          scal = 4: tropical scaling with smallest root.
!
!  ---------------------------------------------------------------------------------------------------
!
!   //SENSE// - On entry: determines whether, or not, condition numbers and backward
!                    errors are computed.
!          sense = 0: do not compute condition numbers, or backward errors.
!          sense = 1: just compute condition numbers for the eigenvalues.
!          sense = 2: just compute backward errors for the left eigenpairs.
!          sense = 3: just compute backward errors for the right eigenpairs.
!          sense = 4: compute backward errors for the left and right eigenpairs.
!          sense = 5: compute condition numbers for the eigenvalues and
!                     backward errors for the left eigenpairs.
!          sense = 6: compute condition numbers for the eigenvalues and
!                     backward errors for the right eigenpairs.
!          sense = 7: compute condition numbers for the eigenvalues and
!                     backward errors for the left and right eigenpairs.
!
!  ---------------------------------------------------------------------------------------------------
!
!   //TOL// - On entry: tol is used as the tolerance for making decisions on rank
!                    in the deflation procedure.  If tol is zero on entry then
!                    n*eps  is used in place of tol, where eps is the machine
!                    precision as returned by routine call DLAMCH( 'E' ).
!                    A diagonal element of a triangular matrix, R, is regarded
!                    as zero if ABS( r(j,j) ) <= tol*size(X), or n*eps*size(X)
!                    when tol is zero, where size(X) is based on the size of the
!                    absolute values of the elements of the matrix X containing
!                    the matrix R.  See reference [3] for the motivation.
!                    If tol is -1.0 on entry then no deflation is attempted.
!                    The recommended value for tol is zero.
!  ---------------------------------------------------------------------------------------------------
!
!  ---------------------------------------------------------------------------------------------------

    SCAL = 1
    SENSE = 0
    TOL = 0.D0

!-----------------------------------------------------------------------------------------------------

!------------------------------------------Safety Measures--------------------------------------------

    IF (N .GT. N_DIM) STOP "N exceeds N_DIM: increase N_DIM"

    IF (TRUNCATION .GT. 7) STOP ! "Please increase output columns and plot columns.                   &
                                ! Then fix by increasing this safety measure."

!-----------------------------------------------------------------------------------------------------

!-----------------------------------Calculate p And n Integers----------------------------------------
    DO I=1, N
        PQ(I) = DFLOAT(I - (TRUNCATION + 1))
    END DO
    DO I=1, N
        NQ(I) = DFLOAT(I - (TRUNCATION + 1))
    END DO
!-----------------------------------------------------------------------------------------------------

!----------------------------------------Fourier Coefficients-----------------------------------------

    DO I=1, N

        !--------------Caclulate Variables x, t; Steps dx, dt; And All Functions----------------------
        DO COUNTER=0, MAXSTEP

            !--------------------------Calculate Steps dx and dt--------------------------------------

            H_G = ((0.5D0 * L_M)-(-0.5D0 * L_M)) * ((MAXSTEP_REAL)**(-1))
            H_F = ((0.5D0 * T_M)-(-0.5D0 * T_M)) * ((MAXSTEP_REAL)**(-1))
            H_Q = ((0.5D0 * Q_M)-(-0.5D0 * Q_M)) * ((MAXSTEP_REAL)**(-1))

            !-----------------------------------------------------------------------------------------

            !-------------------------Calculate Variables x and t-------------------------------------

            X_G(COUNTER) = ((-0.5D0 * L_M) + H_G * (COUNTER))
            X_F(COUNTER) = ((-0.5D0 * T_M) + H_F * (COUNTER))
            X_Q(COUNTER) = ((-0.5D0 * Q_M) + H_Q * (COUNTER))

            !-----------------------------------------------------------------------------------------

            !------------------------------Define All Functions---------------------------------------

                !---------------------------------------Notes-----------------------------------------
                ! All functions are obtained by the main functions of
                ! E_P(I) = (1/T_M)*(1/L_M)*INTEGRATE*INTEGRATE(E(x,t))*                             &
                ! (EXP(-CI*I*(OMEGA_M*t - K_M*x)))dxdt
                ! and
                ! R_P(I) = (1/T_M)*(1/L_M)*INTEGRATE*INTEGRATE(R(x,t))*                             &
                ! (EXP(-CI*I*(OMEGA_M*t - K_M*x)))dxdt
                ! with E(x,t) = E_0 + E_M * COS(OMEGA_M * Y - K_M * X)
                ! and R(x,t) = R_0 .
                ! All functions of time are symbolized by the letter F and the functions of space
                ! with G.
                ! Their integrals with F_INT and G_INT accordingly.
                !-------------------------------------------------------------------------------------

            G_1(I,COUNTER) = ZEXP(CI * (PQ(I)) * K_M * X_G(COUNTER))
            G_2(I,COUNTER) = ZEXP((CI * K_M * X_G(COUNTER)) * ((PQ(I)) - 1.D0))
            G_3(I,COUNTER) = ZEXP((CI * K_M * X_G(COUNTER)) * ((PQ(I)) + 1.D0))
            F_1(I,COUNTER) = ZEXP((-CI) * (PQ(I)) * OMEGA_M * X_F(COUNTER))
            F_2(I,COUNTER) = ZEXP((CI * OMEGA_M * X_F(COUNTER)) * (1.D0 - (PQ(I))))
            F_3(I,COUNTER) = ZEXP(((-CI) * OMEGA_M * X_F(COUNTER)) * (1.D0 + (PQ(I))))
            Q_1(I,COUNTER) = ZEXP(CI * (PQ(I)) * K_M * X_Q(COUNTER))
            Q_2(I,COUNTER) = ZEXP((CI * K_M * X_Q(COUNTER)) * ((PQ(I)) - 1.D0))
            Q_3(I,COUNTER) = ZEXP((CI * K_M * X_Q(COUNTER)) * ((PQ(I)) + 1.D0))

            !-----------------------------------------------------------------------------------------

            !---------------------Calculate Every F(A), F(B) and G(A), G(B)---------------------------

            GA1(I) = ZEXP(CI * (PQ(I)) * K_M * (-L_M * 0.5D0))
            GB1(I) = ZEXP(CI * (PQ(I)) * K_M * (L_M * 0.5D0))
            PG1(I) = (H_G * 0.5D0) * (GB1(I) + GA1(I))

            GA2(I) = ZEXP((CI * K_M * (-L_M * 0.5D0)) * ((PQ(I)) - 1.D0))
            GB2(I) = ZEXP((CI * K_M * (L_M * 0.5D0)) * ((PQ(I)) - 1.D0))
            PG2(I) = (H_G * 0.5D0) * (GB2(I) + GA2(I))

            GA3(I) = ZEXP((CI * K_M * (-L_M * 0.5D0)) * ((PQ(I)) + 1.D0))
            GB3(I) = ZEXP((CI * K_M * (L_M * 0.5D0)) * ((PQ(I)) + 1.D0))
            PG3(I) = (H_G * 0.5D0) * (GB3(I) + GA3(I))

            FA1(I) = ZEXP((-CI) * (PQ(I)) * OMEGA_M * (-T_M * 0.5D0))
            FB1(I) = ZEXP((-CI) * (PQ(I)) * OMEGA_M * (T_M * 0.5D0))
            PF1(I) = (H_F * 0.5D0) * (FB1(I) + FA1(I))

            FA2(I) = ZEXP((CI * OMEGA_M * (-T_M * 0.5D0)) * (1.D0 - (PQ(I))))
            FB2(I) = ZEXP((CI * OMEGA_M * (T_M * 0.5D0)) * (1.D0 - (PQ(I))))
            PF2(I) = (H_F * 0.5D0) * (FB2(I) + FA2(I))

            FA3(I) = ZEXP(((-CI) * OMEGA_M * (-T_M * 0.5D0)) * (1.D0 + (PQ(I))))
            FB3(I) = ZEXP(((-CI) * OMEGA_M * (T_M * 0.5D0)) * (1.D0 + (PQ(I))))
            PF3(I) = (H_F * 0.5D0) * (FB3(I) + FA3(I))

            QA1(I) = ZEXP(CI * (PQ(I)) * K_M * (-Q_M * 0.5D0))
            QB1(I) = ZEXP(CI * (PQ(I)) * K_M * (Q_M * 0.5D0))
            PQ1(I) = (H_Q * 0.5D0) * (QB1(I) + QA1(I))

            QA2(I) = ZEXP((CI * K_M * (-Q_M * 0.5D0)) * ((PQ(I)) - 1.D0))
            QB2(I) = ZEXP((CI * K_M * (Q_M * 0.5D0)) * ((PQ(I)) - 1.D0))
            PQ2(I) = (H_Q * 0.5D0) * (QB2(I) + QA2(I))

            QA3(I) = ZEXP((CI * K_M * (-Q_M * 0.5D0)) * ((PQ(I)) + 1.D0))
            QB3(I) = ZEXP((CI * K_M * (Q_M * 0.5D0)) * ((PQ(I)) + 1.D0))
            PQ3(I) = (H_Q * 0.5D0) * (QB3(I) + QA3(I))

            !-----------------------------------------------------------------------------------------
        END DO

        !--------------------------------Calculate All Integrals--------------------------------------

            !-----------------------------------Space Modulated---------------------------------------

            G_INT1(I) = CZERO
            G_INT2(I) = CZERO
            G_INT3(I) = CZERO

            DO COUNTER=1, MAXSTEP - 1
                G_INT1(I) = G_INT1(I) + G_1(I,COUNTER) * H_G
                G_INT2(I) = G_INT2(I) + G_2(I,COUNTER) * H_G
                G_INT3(I) = G_INT3(I) + G_3(I,COUNTER) * H_G
            END DO

            GP_INT1(I) = PG1(I) + G_INT1(I)
            GP_INT2(I) = PG2(I) + G_INT2(I)
            GP_INT3(I) = PG3(I) + G_INT3(I)

            !-----------------------------------------------------------------------------------------

            !------------------------------------Time Modulated---------------------------------------

            F_INT1(I) = CZERO
            F_INT2(I) = CZERO
            F_INT3(I) = CZERO

            IF (NM .EQ. 0.D0) THEN
                WRITE(99,*) "Spatial-Only Modulation"
            ELSE
                WRITE(99,*) "Space-Time Modulation"
                DO COUNTER=1, MAXSTEP - 1
                    F_INT1(I) = F_INT1(I) + F_1(I,COUNTER) * H_F
                    F_INT2(I) = F_INT2(I) + F_2(I,COUNTER) * H_F
                    F_INT3(I) = F_INT3(I) + F_3(I,COUNTER) * H_F
                END DO
            END IF

            FP_INT1(I) = PF1(I) + F_INT1(I)
            FP_INT2(I) = PF2(I) + F_INT2(I)
            FP_INT3(I) = PF3(I) + F_INT3(I)

            !-----------------------------------------------------------------------------------------

            !-----------------------------------Space-Time Modulated----------------------------------

            Q_INT1(I) = CZERO
            Q_INT2(I) = CZERO
            Q_INT3(I) = CZERO

            DO COUNTER=1, MAXSTEP - 1
                Q_INT1(I) = Q_INT1(I) + Q_1(I,COUNTER) * H_Q
                Q_INT2(I) = Q_INT2(I) + Q_2(I,COUNTER) * H_Q
                Q_INT3(I) = Q_INT3(I) + Q_3(I,COUNTER) * H_Q
            END DO

            QP_INT1(I) = PQ1(I) + Q_INT1(I)
            QP_INT2(I) = PQ2(I) + Q_INT2(I)
            QP_INT3(I) = PQ3(I) + Q_INT3(I)

            !-----------------------------------------------------------------------------------------

        !---------------------------------------------------------------------------------------------

        !-----------------------Calculate Fourier Coefficints E_P and R_P-----------------------------

        IF (SPACETIME .EQ. 0) THEN

            IF (RCOS .EQ. 0) THEN

                IF (NM .EQ. 0.D0) THEN
                    E_P(I) = ((L_M)**(-1))*(E_0*GP_INT1(I)+(E_M*0.5D0)*GP_INT2(I)+                  &
                    (E_M*0.5D0)*GP_INT3(I))
                ELSE
                    E_P(I) = (((T_M)**(-1))*((L_M)**(-1)))*(E_0*FP_INT1(I)*GP_INT1(I)+              &
                    (E_M*0.5D0)*FP_INT2(I)*GP_INT2(I)+(E_M*0.5D0)*FP_INT3(I)*GP_INT3(I))
                END IF

                IF (NM .EQ. 0.D0) THEN
                    R_P(I) = ((L_M)**(-1))*(R_0*GP_INT1(I))
                ELSE
                    R_P(I) = (((T_M)**(-1))*((L_M)**(-1)))*(R_0*FP_INT1(I)*GP_INT1(I))
                END IF

            ELSEIF (RCOS .EQ. 1) THEN

                IF (NM .EQ. 0.D0) THEN
                    R_P(I) = ((L_M)**(-1))*(R_0*GP_INT1(I)+(R_M*0.5D0)*GP_INT2(I)+                  &
                    (R_M*0.5D0)*GP_INT3(I))

                ELSE
                    R_P(I) = (((T_M)**(-1))*((L_M)**(-1)))*(R_0*FP_INT1(I)*GP_INT1(I)+              &
                    (R_M*0.5D0)*FP_INT2(I)*GP_INT2(I)+(R_M*0.5D0)*FP_INT3(I)*GP_INT3(I))
                END IF

                IF (NM .EQ. 0.D0) THEN
                    E_P(I) = ((L_M)**(-1))*(E_0*GP_INT1(I))
                ELSE
                    E_P(I) = (((T_M)**(-1))*((L_M)**(-1)))*(E_0*FP_INT1(I)*GP_INT1(I))
                END IF

            ELSEIF (RCOS .EQ. 2) THEN

                IF (NM .EQ. 0.D0) THEN
                    R_P(I) = ((L_M)**(-1))*(R_0*GP_INT1(I)+(R_M*0.5D0)*GP_INT2(I)+                  &
                    (R_M*0.5D0)*GP_INT3(I))
                ELSE
                    R_P(I) = (((T_M)**(-1))*((L_M)**(-1)))*(R_0*FP_INT1(I)*GP_INT1(I)+              &
                    (R_M*0.5D0)*FP_INT2(I)*GP_INT2(I)+(R_M*0.5D0)*FP_INT3(I)*GP_INT3(I))
                END IF

                E_P(I) = ((L_M)**(-1))*(E_0*GP_INT1(I)+(E_M*0.5D0)*GP_INT2(I)+                      &
                (E_M*0.5D0)*GP_INT3(I))

            ENDIF

        ELSEIF (SPACETIME .EQ. 1) THEN

            IF (RCOS .EQ. 0) THEN

                E_P(I) = ((Q_M)**(-1))*(E_0*QP_INT1(I)+(E_M*0.5D0)*QP_INT2(I)+                      &
                (E_M*0.5D0)*QP_INT3(I))

                R_P(I) = ((Q_M)**(-1))*(R_0*QP_INT1(I))

            ELSEIF (RCOS .EQ. 1) THEN

                R_P(I) = ((Q_M)**(-1))*(R_0*QP_INT1(I)+(R_M*0.5D0)*QP_INT2(I)+                      &
                (R_M*0.5D0)*QP_INT3(I))

                E_P(I) = ((Q_M)**(-1))*(E_0*QP_INT1(I))

            ELSEIF (RCOS .EQ. 2) THEN

                R_P(I) = ((Q_M)**(-1))*(R_0*QP_INT1(I)+(R_M*0.5D0)*QP_INT2(I)+                      &
                (R_M*0.5D0)*QP_INT3(I))

                E_P(I) = ((Q_M)**(-1))*(E_0*QP_INT1(I)+(E_M*0.5D0)*QP_INT2(I)+                      &
                (E_M*0.5D0)*QP_INT3(I))

            ENDIF

        ENDIF

        !IF (I .EQ. (TRUNCATION + 1)) THEN
        !    R_ZERO(I) = R_0
        !ELSEIF ((I .EQ. (TRUNCATION)) .OR. (I .EQ. (TRUNCATION + 2))) THEN
        !    R_ZERO(I) = R_M / 2.D0
        !ELSE
        !    R_ZERO(I) = CZERO
        !ENDIF

        !IF (I .EQ. (TRUNCATION + 1)) THEN
        !    C_LIN(I) = NQ(J)*NQ(J)*K_M*K_M*E_PI(I,J) - &
        !             R_0*(OMEGA(COUNTER) + NQ(J)*OMEGA_M)**(2.D0)
        !ELSEIF ((I .EQ. (TRUNCATION)) .OR. (I .EQ. (TRUNCATION + 2))) THEN
        !    C_LIN(I) = NQ(J)*NQ(J)*K_M*K_M*E_PI(I,J) - &
        !             R_M*(OMEGA(COUNTER) + NQ(J)*OMEGA_M)**(2.D0)
        !ELSE
        !    C_LIN(I) = CZERO
        !ENDIF

        WRITE(18,*) R_ZERO(I)
        WRITE(19,*) R_ONE(I)

        !---------------------------------------------------------------------------------------------

    END DO

!-----------------------------------------------------------------------------------------------------

!----------------------------Rectangularization of Coefficients E_P and R_P---------------------------

    DO I=1, N
        DO J = 1, N

            IF ((DFLOAT(IABS(I - J)) .GT. DFLOAT(TRUNCATION + 1)) .AND.                             &
                (DFLOAT(I - J) .GT. 0.D0)) THEN
                R_PI(I,J) = R_ZERO(((TRUNCATION + 1) + N) - (I - J))
            ELSEIF ((DFLOAT(IABS(I - J)) .GT. DFLOAT(TRUNCATION + 1)) .AND.                         &
                    (DFLOAT(I - J) .LT. 0.D0)) THEN
                R_PI(I,J) = R_ZERO(-((I - J) + TRUNCATION))
            ELSEIF ((DFLOAT(IABS(I - J)) .LT. DFLOAT(TRUNCATION + 1)) .AND.                         &
                    (DFLOAT(I - J) .GT. 0.D0)) THEN
                R_PI(I,J) = R_ZERO((TRUNCATION + 1) - (I - J))
            ELSEIF ((DFLOAT(IABS(I - J)) .LT. DFLOAT(TRUNCATION + 1)) .AND.                         &
                    (DFLOAT(I - J) .LT. 0.D0)) THEN
                R_PI(I,J) = R_ZERO(-(-(TRUNCATION + 1) + (I - J)))
            ELSEIF ((DFLOAT(IABS(I - J)) .LT. DFLOAT(TRUNCATION + 1)) .AND.                         &
                    (DFLOAT(I - J) .EQ. 0.D0)) THEN
                R_PI(I,J) = R_ZERO(TRUNCATION + 1)
            ELSEIF ((DFLOAT(IABS(I - J)) .EQ. DFLOAT(TRUNCATION + 1)) .AND.                         &
                    (DFLOAT(I - J) .GT. 0.D0)) THEN
                R_PI(I,J) = R_ZERO(N)
            ELSEIF ((DFLOAT(IABS(I - J)) .EQ. DFLOAT(TRUNCATION + 1)) .AND.                         &
                    (DFLOAT(I - J) .LT. 0.D0)) THEN
                R_PI(I,J) = R_ZERO(1)
            ENDIF

            IF ((DFLOAT(IABS(I - J)) .GT. DFLOAT(TRUNCATION + 1)) .AND.                             &
                (DFLOAT(I - J) .GT. 0.D0)) THEN
                R_PI_ONE(I,J) = R_ONE(((TRUNCATION + 1) + N) - (I - J))
            ELSEIF ((DFLOAT(IABS(I - J)) .GT. DFLOAT(TRUNCATION + 1)) .AND.                         &
                    (DFLOAT(I - J) .LT. 0.D0)) THEN
                R_PI_ONE(I,J) = R_ONE(-((I - J) + TRUNCATION))
            ELSEIF ((DFLOAT(IABS(I - J)) .LT. DFLOAT(TRUNCATION + 1)) .AND.                         &
                    (DFLOAT(I - J) .GT. 0.D0)) THEN
                R_PI_ONE(I,J) = R_ONE((TRUNCATION + 1) - (I - J))
            ELSEIF ((DFLOAT(IABS(I - J)) .LT. DFLOAT(TRUNCATION + 1)) .AND.                         &
                    (DFLOAT(I - J) .LT. 0.D0)) THEN
                R_PI_ONE(I,J) = R_ONE(-(-(TRUNCATION + 1) + (I - J)))
            ELSEIF ((DFLOAT(IABS(I - J)) .LT. DFLOAT(TRUNCATION + 1)) .AND.                         &
                    (DFLOAT(I - J) .EQ. 0.D0)) THEN
                R_PI_ONE(I,J) = R_ONE(TRUNCATION + 1)
            ELSEIF ((DFLOAT(IABS(I - J)) .EQ. DFLOAT(TRUNCATION + 1)) .AND.                         &
                    (DFLOAT(I - J) .GT. 0.D0)) THEN
                R_PI_ONE(I,J) = R_ONE(N)
            ELSEIF ((DFLOAT(IABS(I - J)) .EQ. DFLOAT(TRUNCATION + 1)) .AND.                         &
                    (DFLOAT(I - J) .LT. 0.D0)) THEN
                R_PI_ONE(I,J) = R_ONE(1)
            ENDIF

            IF ((DFLOAT(IABS(I - J)) .GT. DFLOAT(TRUNCATION + 1)) .AND.                             &
                (DFLOAT(I - J) .GT. 0.D0)) THEN
                E_PI(I,J) = E_P(((TRUNCATION + 1) + N) - (I - J))
            ELSEIF ((DFLOAT(IABS(I - J)) .GT. DFLOAT(TRUNCATION + 1)) .AND.                         &
                    (DFLOAT(I - J) .LT. 0.D0)) THEN
                E_PI(I,J) = E_P(-((I - J) + TRUNCATION))
            ELSEIF ((DFLOAT(IABS(I - J)) .LT. DFLOAT(TRUNCATION + 1)) .AND.                         &
                    (DFLOAT(I - J) .GT. 0.D0)) THEN
                E_PI(I,J) = E_P((TRUNCATION + 1) - (I - J))
            ELSEIF ((DFLOAT(IABS(I - J)) .LT. DFLOAT(TRUNCATION + 1)) .AND.                         &
                    (DFLOAT(I - J) .LT. 0.D0)) THEN
                E_PI(I,J) = E_P(-(-(TRUNCATION + 1) + (I - J)))
            ELSEIF ((DFLOAT(IABS(I - J)) .LT. DFLOAT(TRUNCATION + 1)) .AND.                         &
                    (DFLOAT(I - J) .EQ. 0.D0)) THEN
                E_PI(I,J) = E_P(TRUNCATION + 1)
            ELSEIF ((DFLOAT(IABS(I - J)) .EQ. DFLOAT(TRUNCATION + 1)) .AND.                         &
                    (DFLOAT(I - J) .GT. 0.D0)) THEN
                E_PI(I,J) = E_P(N)
            ELSEIF ((DFLOAT(IABS(I - J)) .EQ. DFLOAT(TRUNCATION + 1)) .AND.                         &
                    (DFLOAT(I - J) .LT. 0.D0)) THEN
                E_PI(I,J) = E_P(1)
            ENDIF

            WRITE(28,*) R_PI(I,J)
            WRITE(29,*) E_PI(I,J)
        END DO
        WRITE(28,*) "NEXT I"
        WRITE(29,*) "NEXT I"
    END DO

!-----------------------------------------------------------------------------------------------------

!-----------------------------------------------Main Loop---------------------------------------------

    !---------------------------------------------Notes-----------------------------------------------
    ! Quadratic Eigenvalue Problem in the form of:
    ! ((k**2)*L_2 + k*L_1 + L_0)u = 0
    !-------------------------------------------------------------------------------------------------

    DO COUNTER = 0, MAXSTEP
    !COUNTER = 1

        !---------------------------------Calculating Variable k--------------------------------------
        OMEGA(COUNTER) = ((0.D0) + ((COUNTER * (MAXSTEP_REAL**(-1))) * (6.5D0)))
        !---------------------------------------------------------------------------------------------

        !---------------------------------Calculating L_2 matrix--------------------------------------
        DO I=1, N
            DO J=1, N
                A(I,J) = CZERO
                A(J,J) = CONE
                WRITE(15,*) A(I,J)
                A_COPY(I,J) = A(I,J)
            END DO
            WRITE(15,*) "NEXT I"
        END DO
        !---------------------------------------------------------------------------------------------

        !---------------------------------Calculating L_1 matrix--------------------------------------
        DO I=1, N
            DO J=1, N
                B(I,J) = CZERO
                B(J,J) = 4.D0 * PI * NQ(J)
                WRITE(16,*) B(I,J)
                B_COPY(I,J) = B(I,J)
            END DO
            WRITE(16,*) "NEXT I"
        END DO
        !---------------------------------------------------------------------------------------------

        !---------------------------------Calculating L_0 matrix--------------------------------------
        DO I=1, N

                C_LIN(I) = CZERO

                C_LIN(TRUNCATION + 1) = 4.D0 * PI * PI * NQ(I) * NQ(I) - &
                                        (((OMEGA(COUNTER))) + 2.D0 * PI * NQ(I) * NM) ** (2.D0)

                C_LIN(TRUNCATION) = - (BM) * (((OMEGA(COUNTER))) + 2.D0 * PI * (NQ(I) - 1.D0) * NM) * &
                                           (((OMEGA(COUNTER))) + 2.D0 * PI * NQ(I) * NM)

                C_LIN(TRUNCATION + 2) = - (BM) * (((OMEGA(COUNTER))) + 2.D0 * PI * (NQ(I) + 1.D0) * NM) * &
                                               (((OMEGA(COUNTER))) + 2.D0 * PI * NQ(I) * NM)

            DO J=1, N
                IF ((DFLOAT(IABS(I - J)) .GT. DFLOAT(TRUNCATION + 1)) .AND.                             &
                    (DFLOAT(I - J) .GT. 0.D0)) THEN
                    C(I,J) = C_LIN(((TRUNCATION + 1) + N) - (I - J))
                ELSEIF ((DFLOAT(IABS(I - J)) .GT. DFLOAT(TRUNCATION + 1)) .AND.                         &
                        (DFLOAT(I - J) .LT. 0.D0)) THEN
                    C(I,J) = C_LIN(-((I - J) + TRUNCATION))
                ELSEIF ((DFLOAT(IABS(I - J)) .LT. DFLOAT(TRUNCATION + 1)) .AND.                         &
                        (DFLOAT(I - J) .GT. 0.D0)) THEN
                    C(I,J) = C_LIN((TRUNCATION + 1) - (I - J))
                ELSEIF ((DFLOAT(IABS(I - J)) .LT. DFLOAT(TRUNCATION + 1)) .AND.                         &
                        (DFLOAT(I - J) .LT. 0.D0)) THEN
                    C(I,J) = C_LIN(-(-(TRUNCATION + 1) + (I - J)))
                ELSEIF ((DFLOAT(IABS(I - J)) .LT. DFLOAT(TRUNCATION + 1)) .AND.                         &
                        (DFLOAT(I - J) .EQ. 0.D0)) THEN
                    C(I,J) = C_LIN(TRUNCATION + 1)
                ELSEIF ((DFLOAT(IABS(I - J)) .EQ. DFLOAT(TRUNCATION + 1)) .AND.                         &
                        (DFLOAT(I - J) .GT. 0.D0)) THEN
                    C(I,J) = C_LIN(N)
                ELSEIF ((DFLOAT(IABS(I - J)) .EQ. DFLOAT(TRUNCATION + 1)) .AND.                         &
                        (DFLOAT(I - J) .LT. 0.D0)) THEN
                    C(I,J) = C_LIN(1)
                ENDIF
                WRITE(17,*) C(I,J)

                C_COPY(I,J) = C(I,J)

            END DO
            WRITE(17,*) "NEXT I"
        END DO
        !---------------------------------------------------------------------------------------------

        !--------------------------------Calling Subroutine ZG3EVX------------------------------------
        CALL ZG3EVX( SCAL, 'N', 'V', SENSE, TOL, N, A, LDA, B, LDB, C, LDC, ALPHA, BETA, VL, LDVL,  &
         VR, LDVR, S, BEVL, BEVR, IWARN, INFO )
        !---------------------------------------------------------------------------------------------

        !------------------------Calculating Dimensionless Frequency OMEGA----------------------------

        DO I=1, 2*N
            LAMBDA(I,COUNTER) = (ALPHA(I) / BETA(I))
        ENDDO

        DO J=1, 2*N
            DO K=1, N
                REL(K) = CZERO
            ENDDO
        ENDDO

        DO I= 1, 2*N
            DO J=1, N
                DO K=1, N
                    REL_1(K,J) = LAMBDA(I,COUNTER) * LAMBDA(I,COUNTER) * A_COPY(K,J)
                    REL_2(K,J) = LAMBDA(I,COUNTER) * B_COPY(K,J)
                    REL_3(K,J) = REL_1(K,J) + REL_2(K,J) + C_COPY(K,J)
                ENDDO
            ENDDO

            DO K=1, N
                DO T =1, N
                    REL(K) = REL(K) + REL_3(K,T) * VR(T,I)
                ENDDO
                !PRINT *, REL(K)
                IF (DREAL(REL(K)) .GT. EMACH) STOP "!!! Critical error !!! Check Everything."
                IF (DIMAG(REL(K)) .GT. EMACH) STOP "!!! Critical error !!! Check Everything."
            ENDDO
        ENDDO

        DO I=1, 2*N
            REAL_K(I,COUNTER) = DREAL(LAMBDA(I,COUNTER))
            IMAG_K(I,COUNTER) = DIMAG(LAMBDA(I,COUNTER))
        ENDDO

        !--------------------------------------Analytical---------------------------------------------
        DO I=1, N
            K_AN_1(I,COUNTER) =  (OMEGA(COUNTER) + 2.D0 * PI * NQ(I) * (NM - 1.D0))
            K_AN_2(I,COUNTER) =  (-OMEGA(COUNTER) - 2.D0 * PI * NQ(I) * (NM + 1.D0))
        ENDDO

        OPEN(76, FILE = '/home/varoth/Desktop/Physics/PLOTS/Python/ptc_plots/analytical_data_1.txt', status='unknown')
        WRITE(76,FORMAT) OMEGA(COUNTER), "            ", K_AN_1(1,COUNTER), "            ", &
        K_AN_1(2,COUNTER), "            ", K_AN_1(3,COUNTER), "            ", K_AN_1(4,COUNTER), &
        "            ", K_AN_1(5,COUNTER), "            ", K_AN_1(6,COUNTER), "            ", &
        K_AN_1(7,COUNTER)

        OPEN(78, FILE = '/home/varoth/Desktop/Physics/PLOTS/Python/ptc_plots/analytical_data_2.txt', status='unknown')
        WRITE(78,FORMAT) OMEGA(COUNTER), "            ", K_AN_2(1,COUNTER), "            ", K_AN_2(2,COUNTER), &
        "            ", K_AN_2(3,COUNTER), "            ", K_AN_2(4,COUNTER), "            ", &
        K_AN_2(5,COUNTER), "            ", K_AN_2(6,COUNTER), "            ", K_AN_2(7,COUNTER)


        !---------------------------------------------------------------------------------------------

                        !-------------Printing for Python Ploting---------------
        OPEN(77, FILE = '/home/varoth/Desktop/Physics/PLOTS/Python/ptc_plots/data.txt', status='unknown')

        WRITE(77,FORMAT) OMEGA(COUNTER), "            ", REAL_K(1,COUNTER), "            ", &
        REAL_K(2,COUNTER), "            ", REAL_K(3,COUNTER), "            ", REAL_K(4,COUNTER),       &
        "            ", REAL_K(5,COUNTER), "            ", REAL_K(6,COUNTER), "            ",         &
        REAL_K(7,COUNTER), "            ", REAL_K(8,COUNTER), "            ", REAL_K(9,COUNTER),       &
        "            ", REAL_K(10,COUNTER), "            ", REAL_K(11,COUNTER), "            ",       &
        REAL_K(12,COUNTER), "            ", REAL_K(13,COUNTER), "            ", REAL_K(14,COUNTER),    &
        "            ", REAL_K(15,COUNTER), "            ", REAL_K(16,COUNTER), "            ",       &
        REAL_K(17,COUNTER), "            ", REAL_K(18,COUNTER), "            ", REAL_K(19,COUNTER),    &
        "            ", REAL_K(20,COUNTER), "            ", REAL_K(21,COUNTER), "            ",       &
        REAL_K(22,COUNTER), "            ", REAL_K(23,COUNTER), "            ", REAL_K(24,COUNTER),    &
        "            ", REAL_K(25,COUNTER), "            ", REAL_K(26,COUNTER), "            ",       &
        REAL_K(27,COUNTER), "            ", REAL_K(28,COUNTER), "            ", REAL_K(29,COUNTER),    &
        "            ", REAL_K(30,COUNTER)
        !---------------------------------------------------------------------------------------------

    ENDDO

!-------------------------------------Negative Values of Analytical-----------------------------------
    DO COUNTER = 0, MAXSTEP

        !---------------------------------Calculating Variable k--------------------------------------
        OMEGA(COUNTER) = ((-2.D0) + ((COUNTER * (MAXSTEP_REAL**(-1))) * (2.D0)))
        !---------------------------------------------------------------------------------------------

        DO I=1, N
            K_AN_1(I,COUNTER) =  (OMEGA(COUNTER) + 2.D0 * PI * NQ(I) * (NM - 1.D0))
            K_AN_2(I,COUNTER) =  (-OMEGA(COUNTER) - 2.D0 * PI * NQ(I) * (NM + 1.D0))
        ENDDO

        OPEN(76, FILE = '/home/varoth/Desktop/Physics/PLOTS/Python/ptc_plots/minus_analytical_data_1.txt', status='unknown')
        WRITE(76,FORMAT) OMEGA(COUNTER), "            ", K_AN_1(1,COUNTER), "            ", &
        K_AN_1(2,COUNTER), "            ", K_AN_1(3,COUNTER), "            ", K_AN_1(4,COUNTER), &
        "            ", K_AN_1(5,COUNTER), "            ", K_AN_1(6,COUNTER), "            ", &
        K_AN_1(7,COUNTER)

        OPEN(78, FILE = '/home/varoth/Desktop/Physics/PLOTS/Python/ptc_plots/minus_analytical_data_2.txt', status='unknown')
        WRITE(78,FORMAT) OMEGA(COUNTER), "            ", K_AN_2(1,COUNTER), "            ", K_AN_2(2,COUNTER), &
        "            ", K_AN_2(3,COUNTER), "            ", K_AN_2(4,COUNTER), "            ", &
        K_AN_2(5,COUNTER), "            ", K_AN_2(6,COUNTER), "            ", K_AN_2(7,COUNTER)
    ENDDO
!-----------------------------------------------------------------------------------------------------

!----------------------------------------------Unmodulated--------------------------------------------

DO COUNTER = 0, MAXSTEP

    !---------------------------------Calculating Variable k--------------------------------------
    OMEGA(COUNTER) = ((0.D0) + ((COUNTER * (MAXSTEP_REAL**(-1))) * (6.5D0)))
    !---------------------------------------------------------------------------------------------

    DO I=1, N
        K_AN_1(I,COUNTER) =  (OMEGA(COUNTER) + 2.D0 * PI * NQ(I) * (- 1.D0))
        K_AN_2(I,COUNTER) =  (-OMEGA(COUNTER) - 2.D0 * PI * NQ(I) * (+ 1.D0))
    ENDDO

    OPEN(76, FILE = '/home/varoth/Desktop/Physics/PLOTS/Python/ptc_plots/unmod_analytical_data_1.txt', status='unknown')
    WRITE(76,FORMAT) OMEGA(COUNTER), "            ", K_AN_1(1,COUNTER), "            ", &
    K_AN_1(2,COUNTER), "            ", K_AN_1(3,COUNTER), "            ", K_AN_1(4,COUNTER), &
    "            ", K_AN_1(5,COUNTER), "            ", K_AN_1(6,COUNTER), "            ", &
    K_AN_1(7,COUNTER)

    OPEN(78, FILE = '/home/varoth/Desktop/Physics/PLOTS/Python/ptc_plots/unmod_analytical_data_2.txt', status='unknown')
    WRITE(78,FORMAT) OMEGA(COUNTER), "            ", K_AN_2(1,COUNTER), "            ", K_AN_2(2,COUNTER), &
    "            ", K_AN_2(3,COUNTER), "            ", K_AN_2(4,COUNTER), "            ", &
    K_AN_2(5,COUNTER), "            ", K_AN_2(6,COUNTER), "            ", K_AN_2(7,COUNTER)
ENDDO
!-----------------------------------------------------------------------------------------------------

!------------------------------------Closing Files for Python Plotting--------------------------------

!-------------------------------Extract for Python Plotting---------------------------

OPEN(122, FILE = '/home/varoth/Desktop/Physics/PLOTS/Python/ptc_plots/speed.txt', status='unknown')
OPEN(123, FILE = '/home/varoth/Desktop/Physics/PLOTS/Python/ptc_plots/permaspeed.txt', status='unknown')
OPEN(124, FILE = '/home/varoth/Desktop/Physics/PLOTS/Python/ptc_plots/k_m.txt', status='unknown')

WRITE(122,FORMAT) NM
WRITE(123,FORMAT) C_0
WRITE(124,FORMAT) K_M

CLOSE(122)
CLOSE(123)
CLOSE(124)
CLOSE(76)
CLOSE(77)
CLOSE(78)
!-----------------------------------------------------------------------------------------------------

!-------------------------------------------Stop Everything-------------------------------------------

STOP

!-----------------------------------------------------------------------------------------------------

END PROGRAM PTC

!_____________________________________________________________________________________________________
