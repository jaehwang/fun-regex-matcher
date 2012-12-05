package com.jaehwang.regex

import java.io.File
  
import org.codehaus.jparsec.Parser

import com.jaehwang.regex.parser.RegexParser
import com.jaehwang.regex.ast._

/** Simple grep program based on functional regex matcher.
 * 
 */
object grep {
  
  val usage = """
    Usage: grep [-r] [-e suffix] pattern filename
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
    
  def main(args:Array[String]) {     

     val options =  parseArgs(args)
     val files:Stream[File] = 
         if (options.contains('recurse)) {
             val dir:File = new File(options('filename))  
             // FIXME: filename이 "."인 경우 hidden file로 인식되는 문제가 있음.
             // Canonical file을 쓰면 "." 대신 canonical path가 출력됨.
             tree(dir.getCanonicalFile(),true)
         } else {
             (new File(options('filename))) #:: Stream.empty
         }
     
     val regexParser:RegexParser = new RegexParser()
     val parser:Parser[Regex]    = regexParser.parser()
     val regex:Regex             = parser.parse(options('pattern))
     val matcher:RegexMatcher    = new DRegexMatcher(regex)
     
     def matchFileLines(file:File) {      
         try {
           val source = scala.io.Source.fromFile(file)
      
           source.getLines().filter(matcher.accept).foreach(line => println(file+":"+line))
         } catch {
           // TODO: improve binary file process
           case malformedInput:java.nio.charset.MalformedInputException => Unit
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