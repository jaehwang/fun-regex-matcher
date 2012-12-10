package com.jaehwang.regex

import java.io.File
  
import org.codehaus.jparsec.Parser

import com.jaehwang.regex.parser.RegexParser
import com.jaehwang.regex.ast._
import java.io.{InputStream,FileInputStream,BufferedInputStream}   

/** Simple grep program based on functional regex matcher.
 * 
 */
object grep {
  
  val usage = """
Usage: grep [-r] [-b] [-e suffix] pattern filename
       -r : recursively traverse directory
       -I : ignore binary files
       -e suffix : file suffix pattern. e.g. txt, scala, ...
  """
    
  type OptionMap = Map[Symbol, String]
  
  def parseArgs(args:Array[String]):OptionMap = {
    // http://stackoverflow.com/questions/2315912/scala-best-way-to-parse-command-line-parameters-cli
    if (args.length == 0) {
       println(usage)
       exit(1)
     }
       
     val arglist = args.toList     
     
     def nextOption(map:OptionMap, list:List[String]):OptionMap = {
       list match {
         case Nil => map
         case "-r"::tail   => nextOption(map++Map('recurse -> "true"),tail)
         case "-I"::tail   => nextOption(map++Map('ignorebinary -> "true"),tail)
         case "-e"::suffix::tail => nextOption(map++Map('suffix -> suffix),tail)
         case pattern::filename::Nil  => nextOption(map++Map('pattern -> pattern,'filename -> filename),Nil)
         case option::tail => println("Unkown option:"+option)
                              exit(1)
       }
     }
     nextOption(Map(),arglist)
  } 

  /** Traverse given directory recursively and returns file stream.
   * Duncan McGregor's answer in the following question:
   * http://stackoverflow.com/questions/2637643/how-do-i-list-all-files-in-a-subdirectory-in-scala
   */
  def tree(root: File, skipHidden: Boolean = false): Stream[File] = 
    if (!root.exists || (skipHidden && root.isHidden)) Stream.empty 
    else root #:: (
      root.listFiles match {
        case null  => Stream.empty // listFiles returns null for a non-directory
        case files => files.toStream.flatMap(tree(_, skipHidden))
    })  
  
  /**
   * TODO: more accurate & efficient binary detection
   */
  def file_is_binary(is:InputStream):Boolean = {
      if (!is.markSupported()) false
      
      val readlimit:Int = 1024
      
      is.mark(readlimit)
          
      var idx = 0
      while(idx<readlimit) {
         var b:Int = is.read()
         if(b<0) {
           is.reset()
           return false
         }
         
         if(b==0) {
            is.reset()
            return true
         }
         idx = idx+1
      }
      is.reset()
      false
    }
  
  def main(args:Array[String]) 
  {
     val options =  parseArgs(args)
     val files:Stream[File] = 
         if (options.contains('recurse)) {
             val dir:File = new File(options('filename))  
             // tree()는 hidden file을 skip할 수 있음.
             // dir이 "."인 경우 tree()를 호출하면 hidden으로 처리됨.
             // 이를 막기 위해 canonical path로 변환할 수 있지만 
             // 파일명이 길어지는 문제가 있음.
             // dir이 "." 등 hidden인 경우에도 파일 목록 stream을 만들기 위해
             // dir.listFiles를 일차로 실행 후 tree()를 호출하도록 함. 
             dir.listFiles match {
                case null => Stream.empty
                case files => files.toStream.flatMap(tree(_, true))
             }
         } else {
             (new File(options('filename))) #:: Stream.empty
         }
     
     val ignore_binary_files = options.contains('ignorebinary)
       
     val regexParser:RegexParser = new RegexParser()
     val parser:Parser[Regex]    = regexParser.parser()
     val regex:Regex             = parser.parse(options('pattern))
     val matcher:RegexMatcher    = new DRegexMatcher(regex)     
     
     def matchFileLines(file:File) {      
         try {     
           import com.ibm.icu.text.{CharsetDetector,CharsetMatch}
           
           val is:InputStream = new BufferedInputStream(new FileInputStream(file))
           
           val is_binary = file_is_binary(is)
           
           if(ignore_binary_files && is_binary) return
           
           val detector:CharsetDetector = new CharsetDetector()           
           detector.setText(is)
           val m:CharsetMatch = detector.detect()
           val charset:String = m.getName()
           
           val source = scala.io.Source.fromInputStream(is,charset)
           
           if(is_binary && source.getLines().zipWithIndex.
                  filter(e => matcher.accept(e._1)).length > 0)            
               println("Binary file "+file+" matches")
            else 
               source.getLines().zipWithIndex.
                  filter(e => matcher.accept(e._1)).
                  foreach(e => println(file+":"+(e._2+1)+":"+e._1))                  
         } catch {           
           case malformedInput:java.nio.charset.MalformedInputException => Unit
           case unsupportedCharset:java.nio.charset.UnsupportedCharsetException => Unit
           case unmappableCharacter:java.nio.charset.UnmappableCharacterException => Unit
           case e:Exception => e.printStackTrace()
         }
     }
     
     def checkSuffix:File=>Boolean = 
       if (options.contains('suffix)) {
         val suffix = options('suffix)
         f => f.getName.endsWith(suffix)
       } else {
         f => true
       }
     files.filter(f => f.isFile  && checkSuffix(f)).foreach(matchFileLines)
  }
}
