package com.jaehwang.regex

import org.codehaus.jparsec.Parser

import com.jaehwang.regex.utils._
import com.jaehwang.regex.ast._

/** This Class implements CPS version of regular expression matcher 
 * in "Proof-directed debugging"(Robert Harper, 1999)
 * 
 * @author Jaehwang Kim (kim.jaehwang@gmail.com)
 */
class CPSMatcher(regex:Regex) extends RegexMatcher(regex) {

    case class ZeroRegex() extends Regex

    def delta(r:Regex):Regex = {
        val zero  = ZeroRegex()
        val one   = EmptyRegex()
        r match {
            case ZeroRegex()       => zero
            case EmptyRegex()      => one
            case AnyRegex()        => zero
            case CharRegex(_)      => zero
            case PlusRegex(r1,r2)  => {
                if (one == delta(r1) || one == delta(r2)) one
                else zero
            }
            case TimesRegex(r1,r2) => {
                if (zero == delta(r1) || zero == delta(r2)) zero
                else one
            }
            case StarRegex(r1)     => one
        }
    }

    def standardize(r:Regex):Regex = r match {
        case ZeroRegex()       => ZeroRegex()
        case EmptyRegex()      => ZeroRegex()
        case AnyRegex()        => r
        case CharRegex(_)      => r
        case PlusRegex(r1, r2) => PlusRegex(standardize(r1),standardize(r2))
        case TimesRegex(r1,r2) => {
            val s1 = standardize(r1)
            val s2 = standardize(r2)
            PlusRegex(TimesRegex(delta(r1),s2),
            PlusRegex(TimesRegex(s1,delta(r2)),
                      TimesRegex(s1,s2)))
        }
        case StarRegex(r1)     => {
            val s1 = standardize(r1)
            TimesRegex(s1, StarRegex(s1))
        }
    }

    def acc(r:Regex, s:String, k:String=>Boolean):Boolean = {
        r match {
            case ZeroRegex()       => false
            case EmptyRegex()      => k(s)
            case AnyRegex()        => destructString(s) match {
                case None          => false
                case Some((hd,tl)) => k(tl)
            }
            case CharRegex(d)      => destructString(s) match {
                case None          => false
                case Some((hd,tl)) => if (hd==d) k(tl) else false
            }
            case PlusRegex(r1,r2)  => if (acc(r1,s,k)) true else acc(r2,s,k)
            case TimesRegex(r1,r2) => acc(r1,s, (x:String) => acc(r2,x,k))
            case StarRegex(r1)     => 
                if (k(s)) true else acc(r1,s,(x:String) => acc(r,x,k))
        }
    }

    def k(s:String):Boolean = if (s == null || s.length() < 1) true else false

    def accept(s:String):Boolean = {
        def S(r:Regex):Regex = r match {
            case ZeroRegex() | EmptyRegex() | AnyRegex() | CharRegex(_)              
                                           => r

            case PlusRegex(r1,ZeroRegex()) => S(r1)
            case PlusRegex(ZeroRegex(),r2) => S(r2)
            case PlusRegex(r1,r2)          => PlusRegex(S(r1),S(r2))

            case TimesRegex(ZeroRegex(),r2) => ZeroRegex()
            case TimesRegex(r1,ZeroRegex()) => ZeroRegex()
            case TimesRegex(r1,r2)          => TimesRegex(S(r1),S(r2))

            case StarRegex(r1)              => StarRegex(S(r1))
        }

        acc(S(PlusRegex(delta(regex),standardize(regex))), s, k)
    }
}
