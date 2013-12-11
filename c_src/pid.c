#include "pid.h"
#include<stdio.h>
#include<stdlib.h>
#include<unistd.h>

PID* newPID(void){
  PID* newPID = malloc(sizeof *newPID);
  return newPID;
}

void initializePID(PID* newPID, double kI, double kD, double kP,  double output, double setpoint){
  newPID->kI = kI;
  newPID->kD = kD;
  newPID->kP = kP;
  newPID->output = output;
  newPID->setpoint = setpoint;
  newPID->isForward = 0;
  newPID->sampleTime = 10; //just set a default of 100ms
  gettimeofday(&(newPID->lastTime), NULL);
  newPID->outMin = 0;
  newPID->lastInput = 3;
  newPID->outMax = 255;
  newPID->integralTerm = 0;
}

int setSampleTime(unsigned long newSamp, PID* pid){
  pid->sampleTime = newSamp;
}


/*
 * Allows us to make the PID control more aggressive or conservative while midway
 * */
void setTunings(double kI, double kP, double kD, PID* pid){
  pid->kD = kD;
  pid->kP = kP;
  pid->kI = kI;
}

bool calcUpdate(PID* pid, double newInput){
  struct timeval now;
  gettimeofday(&now, NULL);
  unsigned long timeDiff = now.tv_usec - pid->lastTime.tv_usec;
  if (timeDiff > pid->sampleTime){
    double error = (pid->setpoint) - newInput;
    pid->integralTerm += pid->kI * error;
    printf("ITERM:%g\n", pid->integralTerm);
    if (pid->integralTerm > pid->outMax){
      pid->integralTerm = pid->outMax; //Makes sure we don't go over the set amount
    }
    else if (pid->integralTerm < pid->outMin){
      pid->integralTerm = pid->outMin; //Makes sure we don't go under the set amount that we can output and fool the PID reader
    }
    double derivativeInput = newInput - pid->lastInput;
    double output = pid->kP * error + pid->integralTerm - pid->kD * derivativeInput;
    if (output > pid->outMax){
      output = pid->outMax;
    }
    else if (output < pid->outMin) {
      output = pid->outMin;
    }
    (pid->output) = output;
    pid->lastInput = newInput;
    pid->lastTime.tv_usec = now.tv_usec;
    return true;
  }
  else{
    return false;
  }
}

int main(){
  double input = 4;
  double output = 3;
  double setpoint = 5;
  PID *testPID = newPID();
  initializePID(testPID, 3.2, 4.2, 5.2, output, setpoint);
  setSampleTime(10, testPID);
  calcUpdate(testPID, 2);
  printf("First output: %g\n", (testPID->output));
  calcUpdate(testPID, 3);
  printf("Second output: %g\n", (testPID->output));
  calcUpdate(testPID, 2);
  printf("Third output: %g\n", (testPID->output));
  return 0;
}
