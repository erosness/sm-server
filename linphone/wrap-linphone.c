#include <stdio.h>
#include "linphone/linphonecore.h"

int berit(int a, int b) {
  printf("Tallene: %d %d", a, b);
  return (a+b);
}

/*
 * Call state notification callback
 */
static void call_state_changed(LinphoneCore *lc, LinphoneCall *call, LinphoneCallState cstate, const char *msg){
	switch(cstate){
		case LinphoneCallOutgoingRinging:
			printf("It is now ringing remotely !\n");
		break;
		case LinphoneCallOutgoingEarlyMedia:
			printf("Receiving some early media\n");
		break;
		case LinphoneCallConnected:
			printf("We are connected !\n");
		break;
		case LinphoneCallStreamsRunning:
			printf("Media streams established !\n");
		break;
		case LinphoneCallEnd:
			printf("Call is terminated.\n");
		break;
		case LinphoneCallError:
			printf("Call failure !");
		break;
		default:
			printf("Unhandled notification %i\n",cstate);
	}
}

LinphoneCoreVTable vtable={0};
LinphoneCore *lc;
LinphoneCall *call=NULL;
const char *dest=NULL;

LinphoneCore *lph_create() {
  vtable.call_state_changed=call_state_changed;

  lc=linphone_core_new(&vtable, NULL, "/home/erik/.linphonerc", NULL);
  return(lc);
}

int lph_call(LinphoneCore* arg_lc, const char* dest){
  printf("LC: here: %p got: %p\n", lc, arg_lc  );
  /*
   Place an outgoing call
  */
  call=linphone_core_invite(lc,dest);
  if (call==NULL){
    printf("Could not place call to %s\n",dest);
    return -1;
  }else{
    printf("Call to %s is in progress...",dest);
  }
  linphone_call_ref(call);
}
