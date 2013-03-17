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

/** Functional regular expression matcher.
 *
 * @author Jaehwang Kim (kim.jaehwang@gmail.com)
 */
package com.jaehwang.regex

import org.codehaus.jparsec.Parser

import com.jaehwang.regex.utils._

import com.jaehwang.regex.ast._
import com.jaehwang.regex.parser.RegexParser

/** Class provides auxiliary functions.
 */
abstract class RegexMatcher(regex:Regex) {

    def accept(s:String):Boolean;

    def nullable(regex:Regex):Boolean = regex match {
      case EpsilonRegex()    => true
      case AnyRegex()      => false
      case CharRegex(_)    => false
      case PlusRegex(a,b)  => nullable(a) || nullable(b)
      case TimesRegex(a,b) => nullable(a) && nullable(b) 
      case StarRegex(_)    => true
    }

    def firstmatch (c:Char, re:Regex):Boolean = re match {
      case EpsilonRegex()    => false
      case AnyRegex()      => true
      case CharRegex(d)    => c == d
      case PlusRegex(a,b)  => if (firstmatch(c,a)) true else firstmatch(c,b)
      case TimesRegex(a,b) => if (nullable(a)) firstmatch(c,a) || firstmatch(c,b) 
                              else             firstmatch(c,a)
      case StarRegex(r)    => firstmatch(c,r)
    }

    def suffix (c:Char, regex:Regex):Set[Regex] = {

      val empty:Set[Regex] = Set.empty

      /** r1r2에서r1이eplsilon인 경우 r2를 리턴. 즉, TimesRegex(e,r) => r.
       * 불필요한 expression을 제거하므로 약간 최적화 된다고 볼 수 있음.
       *
       * TODO 생각해 보기:
       * 모든 times에서 epsilon을 빼는 최적화 코드 개발이 필요할까?
       */
      def mkTimesRegex(r1:Regex, r2:Regex):Regex = r1 match {
        case EpsilonRegex() => r2 
        case _            => TimesRegex(r1,r2)
      }

      regex match {
        case EpsilonRegex()      => empty
        case AnyRegex()        => Set(EpsilonRegex())
        case CharRegex(d)      => if (c==d) Set(EpsilonRegex()) else empty
        case PlusRegex(r1,r2)  => suffix(c,r1) | suffix(c,r2)
        case TimesRegex(r1,r2) => ((suffix(c,r1) foldLeft empty) ((s,x) => s + mkTimesRegex(x,r2))) | 
                                  (if (nullable(r1)) suffix(c,r2) else empty)
        case StarRegex(r)      => if (firstmatch(c,r)) suffix(c,r) map (s => mkTimesRegex(s,regex)) 
                                  else empty
      }
    }
}

/** Regular expression matcher with backtracking.
 */
class NRegexMatcher(regex:Regex) extends RegexMatcher(regex) {

    def accept(s:String):Boolean = {
        def M(re:Regex, s:String):Boolean = (re,destructString(s)) match {
            case (EpsilonRegex(),   None) => true
            case (AnyRegex(),       None) => false
            case (CharRegex(_),     None) => false
            case (PlusRegex(r1,r2) ,None) => M(r1,null) || M(r2,null)
            case (TimesRegex(r1,r2),None) => M(r1,null) && M(r2,null)
            case (StarRegex(_),     None) => true
            case (_,                Some((hd,tl))) => 
                if (firstmatch(hd,re)) 
                    (suffix(hd,re) foldLeft 
                        false) ((b,x) => if(b) b else M(x,tl)) 
                else 
                    false
        }

        M (regex,s)
    }
}

/** suffix(r1*r2)를 r1의 case 별로 정의.
 * 성능이 조금 향상됨. 왜 그럴까?
 */
class NRegexMatcherO1(regex:Regex) extends NRegexMatcher(regex) {
    override def suffix (c:Char, regex:Regex):Set[Regex] = {
      def suffix_times(c:Char, r1:Regex, r2:Regex):Set[Regex] = 
        r1 match {
          case EpsilonRegex()      => suffix(c,r2)
          case AnyRegex()          => Set(r2)
          case CharRegex(d)        => if (c==d) Set(r2) else Set.empty
          case TimesRegex(rr1,rr2) => 
                suffix(c,TimesRegex(rr1,TimesRegex(rr2,r2)))
          case PlusRegex(rr1,rr2)  => 
                suffix(c,TimesRegex(rr1,r2)) | suffix(c,TimesRegex(rr2,r2))
          case StarRegex(_)        => suffix(c,r2) | 
                (if (firstmatch(c,r1)) 
                   suffix(c,r1) map (r => TimesRegex(r,r2)) 
                 else Set.empty)
        }
     
      regex match {
        case EpsilonRegex()      => Set.empty
        case AnyRegex()        => Set(EpsilonRegex())
        case CharRegex(d)      => if(c==d) Set(EpsilonRegex()) else Set.empty
        case PlusRegex(r1,r2)  => suffix(c,r1) | suffix(c,r2)
        case TimesRegex(r1,r2) => suffix_times(c,r1,r2)
        case StarRegex(r)      => if (firstmatch(c,r)) 
                                    suffix(c,r) map (s => TimesRegex(s,regex)) 
                                  else 
                                    Set.empty
      }
    }
}

/* suffix() with memoize
 */
class NRegexMatcherO2(regex:Regex) extends NRegexMatcher(regex) {

    import scala.collection.mutable

    private[this] val vals = mutable.HashMap.empty[Regex,Set[Char]]

    type R = Set[Regex]

    private[this] val vals2 = mutable.HashMap.empty[Tuple2[Char,Regex],R]

    override def suffix (c:Char, r:Regex):Set[Regex] = {
        val k = (c,r)
        if(vals2.contains(k)) {
            vals2(k)
        } else {
            val y = super.suffix(c,r)
            vals2 + ((k, y))
            y
        }
    }
}

/** Regular expression matcher without backtracking. This class implements
 * Thompson's NFA simulation.
 */
class DRegexMatcher(regex:Regex) extends RegexMatcher(regex) {

  type R = Set[Regex]

  private def firstmatch2(c: Char, r:R): Boolean =
    if (r.isEmpty) {
      false
    } else {
      (r foldLeft false) ((b,x) => if (b) b else firstmatch(c,x))
    }

  private val e: R = Set.empty

  def suffix2(c: Char, r: R): R = (r foldLeft e) ((x, y) => x | suffix(c, y))

  def accept(s: String): Boolean = M(Set(regex), s)

  def M(r:R, s:String): Boolean = 
    (r.isEmpty, destructString(s)) match {
      case (true,None) => true
      case (_,   None) => 
            (r foldLeft false) ((x,y) => x || nullable(y))
      case (_,   Some((hd,tl))) => 
            if (firstmatch2(hd,r)) M(suffix2(hd,r), tl) else false
    }
}

// Memoized suffix version! 아직은 빠르지 않은 듯.
class DRegexMatcherO1(regex:Regex) extends DRegexMatcher(regex) {
    import scala.collection.mutable

    private[this] val vals = mutable.HashMap.empty[Tuple2[Char,Regex],R]

    override def suffix (c:Char, r:Regex):R = {
        val k = (c,r)
        if(vals.contains(k)) {
            vals(k)
        } else {
            val y = super.suffix(c,r)
            vals + ((k, y))
            y
        }
    }
}

// Memoized version
class DRegexMatcherO2(regex:Regex) extends DRegexMatcher(regex) {

    import scala.collection.mutable

    private[this] val vals = mutable.HashMap.empty[Tuple2[String,R],Boolean]

    override def M (r:R, s:String):Boolean = {
        val k = (s,r)
        if(vals.contains(k)) {
            vals(k)
        } else {
            val y = super.M(r,s)
            vals + ((k, y))
            y
        }
    }
}
