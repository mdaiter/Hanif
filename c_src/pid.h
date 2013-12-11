#ifndef PID_H
#define PID_H
#include<stdbool.h>
#include<sys/time.h>

typedef struct{
  double kI;
  double kD;
  double kP;
  double output;
  double setpoint;
  unsigned char isForward;
  struct timeval lastTime; //only keeping track of sample time, don't want to have to set a value to millis()
  unsigned long sampleTime;
  double outMin;
  double outMax;

  double lastInput;
  double integralTerm;
} PID;


bool calcUpdate(PID*, double newInput);
void setTunings(double, double, double, PID*);
PID* newPID(void);
void initializePID(PID*, double, double, double, double, double);
int setSampleTime(unsigned long newSamp, PID* pid);
#endif
