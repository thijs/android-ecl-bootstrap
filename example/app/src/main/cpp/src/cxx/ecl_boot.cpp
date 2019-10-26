#include <stdlib.h>
#include <string>
#include <sys/types.h>
#include <dirent.h>
#include <ecl/ecl.h>
#if ANDROID
#include <android/log.h>
#endif

#if ANDROID
#define ECL_TAG "example_tag"


#define LOGI(...) ((void)__android_log_print(ANDROID_LOG_INFO, ECL_TAG, __VA_ARGS__))
#define LOGW(...) ((void)__android_log_print(ANDROID_LOG_WARN, ECL_TAG, __VA_ARGS__))
#define LOGE(...) ((void)__android_log_print(ANDROID_LOG_ERROR, ECL_TAG, __VA_ARGS__))
#define LOGV(...) ((void)__android_log_print(ANDROID_LOG_VERBOSE, ECL_TAG, __VA_ARGS__))
#else
#define LOGI(...)
#define LOGW(...)
#define LOGE(...)
#endif

#include "ecl_boot.h"

#define DEFUN(name,fun,args) \
 cl_def_c_function(c_string_to_object(name), \
 (cl_objectfn_fixed)fun, \
 args)

#ifdef __cplusplus
#define ECL_CPP_TAG "C"
#else
#define ECL_CPP_TAG
#endif

std::string ecl_string_to_string(cl_object echar) {
  std::string res("");
  int j = echar->string.dim; //get dimension
  ecl_character* selv = echar->string.self; //get pointer

  //do simple pointer addition
  for(int i=0;i<j;i++){
    res += (*(selv+i));
  }
  return res;
}

void android_log(cl_object msg) {
  std::string str_msg = ecl_string_to_string(msg);
  LOGI( "%s", str_msg.c_str() );
}
 
int ecl_boot(const char *root_dir) {
  char *ecl = "ecl";
  char tmp[2048];

  LOGI("ECL boot beginning\n");

  LOGI("Setting directories\n");
  setenv("HOME", root_dir, 1);

  sprintf(tmp, "%s/lib/", root_dir);
  setenv("ECLDIR", tmp, 1);

  sprintf(tmp, "%s/etc/", root_dir);
  setenv("ETC", tmp, 1);

  sprintf(tmp, "%s/home/", root_dir);
  setenv("HOME", tmp, 1);

  LOGI("Directories set\n");

  /* ecl_set_option(ECL_OPT_TRAP_SIGFPE, 0); */
  /* ecl_set_option(ECL_OPT_TRAP_SIGSEGV, 0); */
  /* ecl_set_option(ECL_OPT_TRAP_SIGINT, 0); */
  /* ecl_set_option(ECL_OPT_TRAP_SIGILL, 0); */
  /* ecl_set_option(ECL_OPT_TRAP_SIGBUS, 0); */
  /* ecl_set_option(ECL_OPT_TRAP_INTERRUPT_SIGNAL, 0); */
  /* ecl_set_option(ECL_OPT_SIGNAL_HANDLING_THREAD, 0); */
  /* ecl_set_option(ECL_OPT_INCREMENTAL_GC, 0); */

  LOGI("cl_boot: %d",
       cl_boot(1, &ecl)
       );

  LOGI("installing bytecodes compiler\n");
  si_safe_eval(3, c_string_to_object("(si:install-bytecodes-compiler)"), ECL_NIL, OBJNULL);

  LOGI("Calling ecl_toplevel\n");
  ecl_toplevel(root_dir);

  // make android_log callable from Lisp (as android-log)
  DEFUN("android-log", android_log, 1);

  si_safe_eval(3, c_string_to_object("(android-log \"call android-log from Lisp\")"), Cnil, OBJNULL);

  LOGI("logging some stuff to android logs\n");
  si_safe_eval
    (3, c_string_to_object
     ("(android-log (format nil \"ECL_BOOT, features = ~s ~%\" *features*))"),
     Cnil, OBJNULL);
  si_safe_eval
    (3, c_string_to_object
     ("(dolist (item (si:environ)) (android-log (format nil \"~s\" item)))"),
     Cnil, OBJNULL);

  // load asdf
  // si_safe_eval(3, c_string_to_object("(load (format nil \"~acmp.fas\" (si:getenv \"ECLDIR\")))"), Cnil, OBJNULL);
  si_safe_eval(3, c_string_to_object("(load (format nil \"~aasdf.fas\" (si:getenv \"ECLDIR\")))"), Cnil, OBJNULL);
  si_safe_eval(3, c_string_to_object("(push (si:getenv \"ECLDIR\") asdf:*central-registry*)"), Cnil, OBJNULL);
  
  si_safe_eval
    (3, c_string_to_object
     ("(android-log (format nil \"require sb-bsd-sockets: ~s\" (require :SB-BSD-SOCKETS)))"),
     Cnil, OBJNULL);
  si_safe_eval
    (3, c_string_to_object
     ("(android-log (format nil \"sb-bsd-sockets: ~s\" (find-package \"SB-BSD-SOCKETS\")))"),
     Cnil, OBJNULL);

  si_safe_eval
    (3, c_string_to_object
     ("(android-log (format nil \"ECLDIR: ~s\" (si:getenv \"ECLDIR\")))"),
     Cnil, OBJNULL);
  si_safe_eval
    (3, c_string_to_object
     ("(android-log (format nil \"directory: ~s\" (directory (si:getenv \"ECLDIR\"))))"),
     Cnil, OBJNULL);
  si_safe_eval
    (3, c_string_to_object
     ("(android-log (format nil \"assets dir: ~s\" (format nil \"~a*\" (si:getenv \"ECLDIR\"))))"),
     Cnil, OBJNULL);
  si_safe_eval
    (3, c_string_to_object
     ("(android-log (format nil \"truename: ~s\" (truename (format nil \"~amodule.fas\" (si:getenv \"ECLDIR\")))))"),
     Cnil, OBJNULL);

  // load all modules (defined through load.lisp during build, see Makefile and utils/)
  si_safe_eval(3, c_string_to_object("(load (format nil \"~aload.lisp\" (si:getenv \"ECLDIR\")))"), Cnil, OBJNULL);

  si_safe_eval
    (3, c_string_to_object
     ("(android-log (format nil \"package: ~s\" (find-package \"MODULE\")))"),
     Cnil, OBJNULL);
  si_safe_eval
    (3, c_string_to_object
     ("(android-log (format nil \"android-log: ~s\" (multiple-value-list (find-symbol \"ANDROID-LOG\"))))"),
     Cnil, OBJNULL);
  si_safe_eval
    (3, c_string_to_object
     ("(android-log (format nil \"current package: ~s\" *package*))"),
     Cnil, OBJNULL);

  si_safe_eval
    (3, c_string_to_object
     ("(android-log (format nil \"dummy: ~a\" (module::dummy)))"),
     Cnil, OBJNULL);
  si_safe_eval
    (3, c_string_to_object
     ("(android-log (format nil \"dummy-bt: ~a\" (module::dummy-bt)))"),
     Cnil, OBJNULL);

  return 0;
}

void ecl_toplevel(const char *home) {
  char tmp[2048];

  LOGI("Executing the init scripts\n");

  CL_CATCH_ALL_BEGIN(ecl_process_env())
    {
      sprintf(tmp, "(setq *default-pathname-defaults* #p\"%s/\")", home);
      si_safe_eval(3, c_string_to_object(tmp), Cnil, OBJNULL);
      si_select_package(ecl_make_simple_base_string("CL-USER", 7));
      // si_safe_eval(3, c_string_to_object("(load \"etc/init\")"), Cnil, OBJNULL);
    } CL_CATCH_ALL_END;

  LOGI("Toplevel initialization done\n");
}
