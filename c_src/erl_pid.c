#include<erlang/erl_nif.h>
#include "pid.h"
#include<stdio.h>
#include<string.h>
static ErlNifResourceType *MEM_RESOURCE;

static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info){
  ErlNifResourceFlags flags = (ErlNifResourceFlags)(ERL_NIF_RT_CREATE);
  printf("HIT?\n");
  if (
      (MEM_RESOURCE = enif_open_resource_type(env, NULL, "mem_resource", NULL, flags, NULL)) == NULL){
    return -1;
  }
  return 0;
};

static ERL_NIF_TERM new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
  PID* pid;
  int mem_size = sizeof(*pid);
  printf("Memsize: %d\n", mem_size);
  pid = enif_alloc_resource(MEM_RESOURCE, mem_size);
  printf("Alloced\n");
  if (pid == NULL){
    return enif_make_string(env, "could not alloc", ERL_NIF_LATIN1);
  }
  //Add in init data with dummy stuff
  initializePID(pid, 3.2, 4.2, 5.2, 3, 5);
  ERL_NIF_TERM term = enif_make_resource(env, pid);
  enif_release_resource(pid);
  return term;
};

static ERL_NIF_TERM update(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
  if (argc == 2){
    PID* pid;
    double measurement;
    if (!enif_get_resource(env, argv[0], MEM_RESOURCE, (void**) &pid)){
      return enif_make_badarg(env);
    }
    if (!enif_get_double(env, argv[1], &measurement)){
      return enif_make_badarg(env);
    }
    printf("%g\n", measurement);
    calcUpdate(pid, measurement);
    printf("Updated with: %g\n", pid->output);
    return enif_make_atom(env, "ok");
  }
  else{
    return enif_make_badarg(env);
  }
};

static ERL_NIF_TERM set(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
  if (argc == 3){
    PID* pid;
    double setVal;
    char *nameOfAttr;
    unsigned int lenOfAttr = 0;
    if (!enif_get_resource(env, argv[0], MEM_RESOURCE, (void**) &pid)){
      return enif_make_badarg(env);
    }
    if (!enif_get_double(env, argv[2], &setVal)){
      return enif_make_badarg(env);
    }
    if (!enif_get_atom_length(env, argv[1], &lenOfAttr, ERL_NIF_LATIN1)){
      return enif_make_badarg(env);
    }
    nameOfAttr = malloc(sizeof(char) * lenOfAttr);
    if (!enif_get_atom(env, argv[1], nameOfAttr, lenOfAttr * sizeof(nameOfAttr), ERL_NIF_LATIN1)){
      return enif_make_badarg(env);
    }
    if (!strcmp(nameOfAttr, "output")) {
      free(nameOfAttr);
      pid->output = setVal;
      return enif_make_atom(env, "ok");
    }
    else if (!strcmp(nameOfAttr, "setpoint")){
      free(nameOfAttr);
      pid->setpoint = setVal;
      return enif_make_atom(env, "ok");
    }
    else if (!strcmp(nameOfAttr, "kI")){
      free(nameOfAttr);
      pid->kI = setVal;
      return enif_make_atom(env, "ok");
    }
    else if (!strcmp(nameOfAttr, "kD")){
      free(nameOfAttr);
      pid->kD = setVal;
      return enif_make_atom(env, "ok");
    }
    else if (!strcmp(nameOfAttr, "kP")){
      free(nameOfAttr);
      pid->kP = setVal;
      return enif_make_atom(env, "ok");
    }
    else{
      free(nameOfAttr);
      return enif_make_atom(env, "noattr");
    }
  }
  else{
    return enif_make_badarg(env);
  }
};

static ERL_NIF_TERM get(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
  if (argc == 2){
    PID* pid;
    char *nameOfAttr;
    unsigned int lenOfAttr = 0;
    if (!enif_get_resource(env, argv[0], MEM_RESOURCE, (void**) &pid)){
      printf("Couldn't get resource\n");
      return enif_make_badarg(env);
    }
    if ( !enif_get_atom_length(env, argv[1], &lenOfAttr, ERL_NIF_LATIN1)){
      return enif_make_badarg(env);   
    }
    nameOfAttr = malloc(sizeof(char) * lenOfAttr);
    if ( !enif_get_atom(env, argv[1], nameOfAttr, lenOfAttr * sizeof(nameOfAttr), ERL_NIF_LATIN1)){
      printf("Failed to get atom\nYou input:%s\n", nameOfAttr);
      return enif_make_badarg(env);
    }
    printf("Length: %d\nAtom: %s\nTesting update\n", lenOfAttr, nameOfAttr);
    //Time for the long ass if-else comparison...
    printf("STRCMP result is: %d\n", strcmp(nameOfAttr, "output"));
    if (!strcmp(nameOfAttr, "output")){
      free(nameOfAttr);
      printf("Trying to return %g\n", pid->output);
      return enif_make_double(env, pid->output);
    }
    else if (!strcmp(nameOfAttr, "setpoint")){
      printf("Trying to return setpoint: %g", pid->setpoint);
      free(nameOfAttr);
      return enif_make_double(env, pid->setpoint);
    }
    else if (!strcmp(nameOfAttr, "kI")){
      free(nameOfAttr);
      return enif_make_double(env, pid->kI);
    }
    else if (!strcmp(nameOfAttr, "kD")){
      free(nameOfAttr);
      return enif_make_double(env, pid->kD);
    }
    else if (!strcmp(nameOfAttr, "kP")){
      free(nameOfAttr);  
      return enif_make_double(env, pid->kP);
    }
    else if (!strcmp(nameOfAttr, "sampleTime")){
      free(nameOfAttr);
      return enif_make_double(env, pid->sampleTime);
    }
    else{
      free(nameOfAttr);
      return enif_make_badarg(env);
    }
  }
  else{
    return enif_make_badarg(env);
  }
};

static ErlNifFunc nif_funcs[] = {
  {"new", 0, new},
  {"update", 2, update},
  {"get", 2, get},
  {"set", 3, set}
};

ERL_NIF_INIT(pid, nif_funcs, on_load, NULL, NULL, NULL);
