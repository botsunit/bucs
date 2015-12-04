#include <time.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "erl_nif.h"
#include "nif_helpers.h"

NIF(nif_local_timezone) {
  char tz[7];
  time_t currentTime = time(NULL);
  struct tm *localTime;
  localTime = localtime(&currentTime);
  long absDiff = labs(localTime->tm_gmtoff);
  long hour = absDiff/60/60;
  long min = (absDiff - (hour*60*60))/60;
  memset(tz, 0, 7);

  if(localTime->tm_gmtoff >= 0) {
    sprintf(tz, "+%02ld:%02ld", hour, min);
  } else {
    sprintf(tz, "-%02ld:%02ld", hour, min);
  }
  return enif_make_string(env, tz, ERL_NIF_LATIN1);
}

static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) {
  return 0;
}

static ErlNifFunc nif_funcs[] = {
  {"local_timezone", 0, nif_local_timezone}
};

ERL_NIF_INIT(bucdate, nif_funcs, &on_load, NULL, NULL, NULL);

