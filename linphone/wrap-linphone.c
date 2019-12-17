#include <stdio.h>
#include "linphone/linphonecore.h"

void state_changed(void* lc, void* call, int cstate, char* msg);

LinphoneCoreVTable vtable={0};
LinphoneCore *lc;
LinphoneCall *call=NULL;

LinphoneCore *lph_create() {
  vtable.call_state_changed=state_changed;

  lc=linphone_core_new(&vtable, NULL, "/home/pi/.linphonerc", NULL);
  return(lc);
}

void lph_core_destroy(LinphoneCore* arg_lc) {
  linphone_core_destroy(arg_lc);
  lc = NULL;
}

LinphoneCall *lph_call(LinphoneCore* arg_lc, char* dest){
  printf("arg_dest: %s", dest);
  printf("LC: here: %p got: %p\n", lc, arg_lc  );
  /*
   Place an outgoing call
  */
  call=linphone_core_invite(lc, dest);
  if (call==NULL){
    printf("Could not place call to %s\n", dest);
    return (void*)-1;
  }else{
    printf("Call to %s is in progress...", dest);
  }
  linphone_call_ref(call);
  printf("Call status: ");
  return(call);
}
