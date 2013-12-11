#include<erlang/erl_nif.h>
#include "kalman.h"
#include<stdio.h>
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
  Kalman* filter;
  int mem_size = sizeof(*filter);
  printf("Memsize: %d\n", mem_size);
  filter = enif_alloc_resource(MEM_RESOURCE, mem_size);
  printf("Alloced\n");
  if (filter == NULL){
    return enif_make_string(env, "could not alloc", ERL_NIF_LATIN1);
  }
  //Add in init data with dummy stuff
  initKalman(filter, 1, 1, 1, 1, 1);
  ERL_NIF_TERM term = enif_make_resource(env, filter);
  enif_release_resource(filter);
  return term;
};

static ERL_NIF_TERM update(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
  if (argc == 2){
    Kalman* filter;
    double measurement;
    if (!enif_get_resource(env, argv[0], MEM_RESOURCE, (void**) &filter)){
      return enif_make_badarg(env);
    }
    if (!enif_get_double(env, argv[1], &measurement)){
      return enif_make_badarg(env);
    }
    updateKalman(filter, measurement);
    printf("Updated with: %g\n", filter->x);
    return enif_make_atom(env, "ok");
  }
  else{
    return enif_make_badarg(env);
  }
};

static ERL_NIF_TERM get(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
  if (argc == 2){
    Kalman* filter;
    char *nameOfAttr;
    unsigned int lenOfAttr = 0;
    if (!enif_get_resource(env, argv[0], MEM_RESOURCE, (void**) &filter)){
      printf("Couldn't get resource\n");
      return enif_make_badarg(env);
    }
    printf("Checking if resource still available\nX: %g\nQ: %g\n", filter->x, filter->q);
    if ( !enif_get_atom_length(env, argv[1], &lenOfAttr, ERL_NIF_LATIN1)){
      return enif_make_badarg(env);   
    }
    nameOfAttr = malloc(sizeof(char) * lenOfAttr);
    if ( !enif_get_atom(env, argv[1], nameOfAttr, lenOfAttr * sizeof(nameOfAttr), ERL_NIF_LATIN1)){
      printf("Failed to get atom\nYou input:%s\n", nameOfAttr);
      return enif_make_badarg(env);
    }
    printf("Length: %d\nAtom: %s\n", lenOfAttr, nameOfAttr);
    switch (nameOfAttr[0]){
      case 'x':
        free(nameOfAttr);
        return enif_make_double(env, filter->x);
      case 'r':
        free(nameOfAttr);
        return enif_make_double(env, filter->r);
      case 'k':
        free(nameOfAttr);
        return enif_make_double(env, filter->k);
      case 'p':
        free(nameOfAttr);
        return enif_make_double(env, filter->p);
      case 'q':
        free(nameOfAttr);
        return enif_make_double(env, filter->q);
      default:
        printf("You input: %s as a lookup....\nJust to check, filter->x is %g\n", nameOfAttr, filter->x);
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
  {"get", 2, get}
};

ERL_NIF_INIT(kalman, nif_funcs, on_load, NULL, NULL, NULL);
