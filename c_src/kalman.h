#ifndef KALMAN_H
#define KALMAN_H

/*
 * How the Kalman filter works:
 *
 * x -> mu. The current estimation of the value out of the kalman filter
 * k -> kalman gain. Moderates prediction
 * p -> current error. The "confidence" you have at the moment
 * q -> process noise. Lower value: smooths more noise. How much the process should remove jagged edges by.
 * r -> sensor noise. Lower value: trust the sensor more. How noisy the sensor is.
 * */

typedef struct{
  double input; //pointer to input object
  double r; //sensor noise
  double q; //process noise (how exact we are of the situation)
  double x; //mu
  double p; //current error variance
  double k; //kalman gain
} Kalman;
Kalman* newKalman();
void initKalman(Kalman*, double, double, double, double, double);
void updateKalman(Kalman*, double);

#endif
