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
    copyFasFile(context, "module.fas")
  }

  fun start(context: Context) {
    val dir = context.getFilesDir()

    // call to native method
    JNIstart(dir.toString())
  }

  fun copyFasFile(context: Context, path: String): File {
    val filesDir = context.getFilesDir()
    val cacheDir = filesDir.resolve(File("lib"))
    if (!cacheDir.isDirectory()) {
      cacheDir.mkdirs()
    }
    val tmpPath = cacheDir.resolve(path)
    if( !tmpPath.exists() ) {
      context.assets.open(path).use {
        inStream ->
          tmpPath.outputStream().use{
            outStream ->
              inStream.copyTo(outStream)
          }
      }
    }
    return tmpPath
  }

  companion object {

    init {
      System.loadLibrary("ecl")
      Log.i(TAG,"loaded libecl.so library")
    }

  }
}
