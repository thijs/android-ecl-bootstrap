package org.example.testapp

import android.util.Log;

import android.os.Bundle
import android.content.Context
import android.content.ContextWrapper

import java.io.File
import java.io.FileOutputStream

private const val TAG = "example_tag"

class EmbeddedCommonLisp {

  // defined in main.cpp
  external fun JNIstart(path: String)

  fun initialize(context: Context) {
    ensureCacheDir(context)
    ensureFasFiles(context)
  }

  fun start(context: Context) {
    val dir = context.getFilesDir()

    // call to native method
    JNIstart(dir.toString())
  }

  fun ensureCacheDir(context: Context) {
    val filesDir = context.getFilesDir()
    val cacheDir = filesDir.resolve(File("lib"))
    if (!cacheDir.isDirectory()) {
      cacheDir.mkdirs()
    }
  }
  
  fun ensureFasFiles(context: Context) {
    context.assets.list("")?.forEach {
      if (it.endsWith(".fas") or
          it.endsWith(".asd") or
          it.endsWith(".lisp")) {
        ensureFasFile(context,it)
      }
    }
  }
  
  fun ensureFasFile(context: Context, path: String) {
    val cacheDir = context.getFilesDir().resolve(File("lib"))
    val fasPath = cacheDir.resolve(path)
    if( !fasPath.exists() ) {
      context.assets.open(path).use {
        inStream ->
          fasPath.outputStream().use{
            outStream ->
              inStream.copyTo(outStream)
          }
      }
    }
  }

  companion object {

    init {
      System.loadLibrary("ecl")
      Log.i(TAG,"loaded libecl.so library")
    }

  }
}
