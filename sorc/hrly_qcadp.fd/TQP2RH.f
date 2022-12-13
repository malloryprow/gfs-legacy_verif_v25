
      FUNCTION TQP2RH(T,Q,LP)

C     This function compute relative humidity from 
C     temperature(T), specific humidity(Q), pressure(P).

      DATA F9/ 99999. /

      F = 1.0E-6
      A = 6.1078
      B = 17.269
      C = 237.3
      D = 0.622
      E = 1.-D
      IF(T.LT.F9.AND.Q.LT.F9) THEN
         ES = A*EXP(T*B/(T+C))
         QS = D*ES/(FLOAT(LP)-E*ES)
         TQP2RH = F*Q/QS*100.
         IF(TQP2RH.LT.0.) TQP2RH = 0.
         IF(TQP2RH.GT.100.) TQP2RH = 100.
      ELSE
         TQP2RH = F9
      END IF

      RETURN
      END
