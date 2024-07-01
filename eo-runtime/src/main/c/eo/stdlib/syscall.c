#include <unistd.h>
#include <jni.h>

JNIEXPORT jint JNICALL Java_org_example_Syscall_getpid
  (JNIEnv *env, jclass klass) {
    return getpid();
}
