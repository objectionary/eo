#include <unistd.h>
#include <jni.h>

JNIEXPORT jint JNICALL RENAME_THIS_FUNCTION
  (JNIEnv *env, jclass klass) {
    return getpid();
}
