package org.example.testapp

import android.util.Log

import androidx.appcompat.app.AppCompatActivity

import android.os.Bundle
import android.content.Context
import android.content.ContextWrapper

import java.io.File

import kotlinx.android.synthetic.main.activity_main.*

private const val TAG = "example_tag"

class MainActivity : AppCompatActivity() {

  private val ECL = EmbeddedCommonLisp()

  override fun onCreate(savedInstanceState: Bundle?) {
    super.onCreate(savedInstanceState)
    setContentView(R.layout.activity_main)

    val context = ContextWrapper(getBaseContext())

    ECL.initialize(context)

    val filelist = listFasFiles(context)
    if (filelist == "") {
      sample_text.text = "empty"
    }
    else {
      sample_text.text = filelist
    }

    ECL.start(context)
  }

  fun listFasFiles(context: Context): String? {
    val assetManager = context.assets
    val assetFiles = assetManager.list("")

    return assetFiles?.joinToString(separator = " | ")
  }

  fun listAssets(path: String, c: Context) : String? {
    val assetManager = c.assets
    val assetFiles = assetManager.list(path)

    var ret = assetFiles?.joinToString(separator = " | ")

    if ( ret == "" ) {
      return "empty"
    }

    val newPath = "$path/"
    assetFiles?.forEach {
      val innerFiles = listAssets( "$newPath$it", c )
      ret = "$ret\n$innerFiles"
    }

    return ret
  }

  companion object {

    // load the 'cxx-lib' library on application startup.
    init {
      System.loadLibrary("cxx-lib")
      Log.i(TAG,"loaded libraries")
    }
  }
}
