/*
 * Copyright 2012 Jaehwang Kim
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
import java.util.regex.{Pattern,Matcher}

import org.codehaus.jparsec.Parser

import com.jaehwang.regex.parser.RegexParser
import com.jaehwang.regex.ast._
import com.jaehwang.regex._

object BMT {

    def measureTime (fa:RegexMatcher, str:String, count:Int):Double = {

      println("fa="+fa.accept(str))

      val start = System.currentTimeMillis()

      for(i <- 1 to count) { fa.accept(str) }

      (System.currentTimeMillis() - start)/1000.0
    }

    def measureJavaRegexTime (str:String, pat:String, count:Int):Double = {
        val p:Pattern = Pattern.compile(pat)
        val m:Matcher = p.matcher(str)

        val b = m.matches();

        println("java="+b)

        val start = System.currentTimeMillis()

        for(i <- 1 to count) { 
            val m:Matcher = p.matcher(str)
            m.matches() 
        }

        (System.currentTimeMillis() - start)/1000.0
    }

    def main(args:Array[String]) {
      val count = 5
      val ssize = 20

      // a^ssize
      val str = ((0 until ssize) foldLeft "") ((x,y) => x+"a")

      // (a?)^ssize a^ssize
      val pat1 = ((0 until ssize) foldLeft "") ((x,y) => x+"(a?)")
      val pat2 = ((0 until ssize) foldLeft "") ((x,y) => x+"a")
      val pat = pat1+pat2

      val regexParser:RegexParser = new RegexParser()
      val parser:Parser[Regex] = regexParser.parser()

      val re:Regex = parser.parse(pat)

      val nregexmatcher  :RegexMatcher = new NRegexMatcher(re)
      val nregexmatchero1:RegexMatcher = new NRegexMatcherO1(re)
      val dregexmatcher  :RegexMatcher = new DRegexMatcher(re)
      val dregexmatchero1:RegexMatcher = new DRegexMatcherO1(re)
      val dregexmatchero2:RegexMatcher = new DRegexMatcherO2(re)
      val cpsregexmatcher:RegexMatcher = new CPSMatcher(re)

      println("Java            = "+measureJavaRegexTime(str, pat, count))
      println("NRegexMatcher   = "+measureTime(nregexmatcher, str, count))
      println("DRegexMatcher   = "+measureTime(dregexmatcher, str, count))
      println("DRegexMatcherO1 = "+measureTime(dregexmatchero1, str, count))
      println("DRegexMatcherO2 = "+measureTime(dregexmatchero2, str, count))
      println("CPSMatcher      = "+measureTime(cpsregexmatcher, str, count))
    }
}

object DRegexTime {
    def measureTime (fa:RegexMatcher, str:String, count:Int):Double = {

      val start = System.currentTimeMillis()

      for(i <- 1 to count) { fa.accept(str) }

      (System.currentTimeMillis() - start)/(count*1000.0)
    }


    def main(args:Array[String]) {
      val cnt = 10
      for(ssize <- 1 to 60) { 
          // a^ssize
          val str = ((0 until ssize) foldLeft "") ((x,y) => x+"a")

          // (a?)^ssize a^ssize
          val pat1 = ((0 until ssize) foldLeft "") ((x,y) => x+"(a?)")
          val pat2 = ((0 until ssize) foldLeft "") ((x,y) => x+"a")
          val pat = pat1+pat2

          val regexParser:RegexParser = new RegexParser()
          val parser:Parser[Regex] = regexParser.parser()

          val re:Regex = parser.parse(pat)

          val regexmatcher:RegexMatcher   = new DRegexMatcher(re)

          println(ssize+","+measureTime(regexmatcher, str, cnt))
      }
    }
}

object CPSMain {
    def main(args:Array[String]) {

        val regexParser:RegexParser = new RegexParser()
        val parser:Parser[Regex] = regexParser.parser()

        if(args.length != 2) {
            println("CPSMain pattern filename")
            System.exit(1)
        }

        val pattern:String = args(0)
        val filename:String = args(1)

        val regex:Regex = parser.parse(pattern)
        val matcher:CPSMatcher = new CPSMatcher(regex)

        scala.io.Source.fromFile(filename).getLines().foreach { line =>
            if(matcher.accept(line)) {
                println(line)
            } else {
                println("F")
            }
        }
    }
}
