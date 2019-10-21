#include <jni.h>
#include <iostream>
#include <ecl/ecl.h>
#include "ecl_boot.h"

#include <android/log.h>
#include <string>


#define ECL_TAG "example_tag"

#define LOGI(...) ((void)__android_log_print(ANDROID_LOG_INFO, ECL_TAG, __VA_ARGS__))
#define LOGW(...) ((void)__android_log_print(ANDROID_LOG_WARN, ECL_TAG, __VA_ARGS__))
#define LOGE(...) ((void)__android_log_print(ANDROID_LOG_ERROR, ECL_TAG, __VA_ARGS__))
#define LOGV(...) ((void)__android_log_print(ANDROID_LOG_VERBOSE, ECL_TAG, __VA_ARGS__))


// This is how to define an auto-discovered function for JNI calls from Kotlin/Java
/*
 * Class:     org_example_testapp_MainActivity
 * Method:    stringFromJNI
 * Signature: ()Ljava/lang/String;
 */
extern "C" JNIEXPORT jstring JNICALL
Java_org_example_testapp_MainActivity_stringFromJNI(JNIEnv *env, jobject /* this */) {
  std::string hello = "Hello from C++";
  return env->NewStringUTF(hello.c_str());
}

/*
 * Class:     org_example_testapp_EmbeddedCommonLisp
 * Method:    startJNI
 * Signature: (Ljava/lang/String;)V
 */
extern "C" JNIEXPORT void JNICALL
Java_org_example_testapp_EmbeddedCommonLisp_JNIstart(JNIEnv *env, jobject /* this */, jstring path) {
  const char *lisp_dir = env->GetStringUTFChars(path, NULL);
  LOGI("XXX: ECL starting: *default-pathname-defaults* to: %s\n", lisp_dir);
  ecl_boot(lisp_dir);
  LOGI("ECL started.");
}
