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
import org.junit.runner.RunWith

import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

import com.jaehwang.regex.ast._
import com.jaehwang.regex.{NRegexMatcher,DRegexMatcher,CPSMatcher}

import com.jaehwang.regex.parser.RegexParser
import org.codehaus.jparsec.Parser

@RunWith(classOf[JUnitRunner])
class ParserTest extends FunSuite {

  test("dot char") {
      val regexParser:RegexParser = new RegexParser()
      val parser:Parser[Regex] = regexParser.parser()

      val re:Regex = parser.parse(".*(a|b).*")

      val r = TimesRegex(TimesRegex(StarRegex(AnyRegex()),
                                    PlusRegex(CharRegex('a'),
                                              CharRegex('b'))),
                         StarRegex(AnyRegex()))

      assert(re == r)
  }

  test("escape *") {
      val regexParser:RegexParser = new RegexParser()
      val parser:Parser[Regex] = regexParser.parser()

      val re:Regex = parser.parse("a*\\*")

      val r = TimesRegex(StarRegex(CharRegex('a')),
                         CharRegex('*'))

      assert(re == r)
  }
}

@RunWith(classOf[JUnitRunner])
class AuxTest extends FunSuite {

  test("nullable") {

    val matcher:NRegexMatcher = new NRegexMatcher(EmptyRegex())

    assert(matcher.nullable(EmptyRegex()) === true)
    assert(matcher.nullable(CharRegex('a')) === false)
  }

  test("firstmatch plus") {
    val matcher:NRegexMatcher = new NRegexMatcher(EmptyRegex())

    val charA = CharRegex('a')
    val charB = CharRegex('b')

    assert(matcher.firstmatch('a',PlusRegex(charA,charB)))
  }

  test("firstmatch times") {
    val matcher:NRegexMatcher = new NRegexMatcher(EmptyRegex())

    val charA = CharRegex('a')
    val charB = CharRegex('b')

    assert(matcher.firstmatch('a',TimesRegex(charA,charB)))
    assert(! (matcher.firstmatch('b',TimesRegex(charA,charB))))
  }

  test("firstmatch star") {
    val matcher:NRegexMatcher = new NRegexMatcher(EmptyRegex())

    val charA = CharRegex('a')
    val charB = CharRegex('b')

    assert(matcher.firstmatch('a',StarRegex(charA)))

    assert(matcher.firstmatch('a',TimesRegex(StarRegex(charA),charB)))
    assert(matcher.firstmatch('b',TimesRegex(StarRegex(charA),charB)))
  }

  test("firstmatch any") {
    val matcher:NRegexMatcher = new NRegexMatcher(EmptyRegex())
    assert(matcher.firstmatch('a',AnyRegex()))
  }

  test("suffix char") {
    val matcher:NRegexMatcher = new NRegexMatcher(EmptyRegex())

    val charB = CharRegex('b')
    val s = matcher.suffix ('a', charB)

    assert(s === Set.empty)
  }

  test("suffix plus") {
    val matcher:NRegexMatcher = new NRegexMatcher(EmptyRegex())

    val charA = CharRegex('a')
    val charB = CharRegex('b')

    val s = matcher.suffix ('a', PlusRegex(charA,charB))

    assert(s contains EmptyRegex())
  }

  test("suffix times") {
    val matcher:NRegexMatcher = new NRegexMatcher(EmptyRegex())

    val charA = CharRegex('a')
    val charB = CharRegex('b')
    val s = matcher.suffix ('a', TimesRegex(charA,charB))

    assert(s contains charB)
  }

  test("suffix star") {
    val matcher:NRegexMatcher = new NRegexMatcher(EmptyRegex())

    val charA = CharRegex('a')
    val star = StarRegex(charA)

    val s = matcher.suffix ('a', star)
    val expected  = star

    assert(s contains expected)
  }

  test("suffix: a, a*b") {
    val matcher:NRegexMatcher = new NRegexMatcher(EmptyRegex())

    val charA = CharRegex('a')
    val charB = CharRegex('b')
    val starA = StarRegex(charA)

    val re = TimesRegex(starA,charB)

    val s = matcher.suffix ('a', re)
    val expected  = TimesRegex(starA,charB)

    assert(s contains expected)
  }
}

@RunWith(classOf[JUnitRunner])
class NFATest extends FunSuite {
  test("NFA: ab ! a*b") {
      val regexParser:RegexParser = new RegexParser()
      val parser:Parser[Regex] = regexParser.parser()

      val re:Regex = parser.parse("a*b")

      val nfa:NRegexMatcher = new NRegexMatcher(re)
      assert(nfa.accept("ab"))
  }
}


@RunWith(classOf[JUnitRunner])
class DFATest extends FunSuite {
  test("DFA: \"\" ! a") {
    val regexParser:RegexParser = new RegexParser()
    val parser:Parser[Regex] = regexParser.parser()

    val re:Regex = parser.parse("a")

    val dfa:DRegexMatcher = new DRegexMatcher(re)
    assert(!dfa.accept(""))
  }

  test("DFA: ab ! a*b") {
      val regexParser:RegexParser = new RegexParser()
      val parser:Parser[Regex] = regexParser.parser()

      val re:Regex = parser.parse("a*b")

      val dfa:DRegexMatcher = new DRegexMatcher(re)
      assert(dfa.accept("ab"))
  }
}

@RunWith(classOf[JUnitRunner])
class CPSTest extends FunSuite {
  test("CPS: \"\" ! a") {
    val regexParser:RegexParser = new RegexParser()
    val parser:Parser[Regex] = regexParser.parser()

    val re:Regex = parser.parse("a")

    val cps:CPSMatcher = new CPSMatcher(re)
    assert(!cps.accept(""))
  }

  test("CPS: \"\" ! ab") {
    val regexParser:RegexParser = new RegexParser()
    val parser:Parser[Regex] = regexParser.parser()

    val re:Regex = parser.parse("ab")

    val cps:CPSMatcher = new CPSMatcher(re)
    assert(!cps.accept(""))
  }

  test("CPS: a ! a") {
    val regexParser:RegexParser = new RegexParser()
    val parser:Parser[Regex] = regexParser.parser()

    val re:Regex = parser.parse("a")

    val cps:CPSMatcher = new CPSMatcher(re)
    assert(cps.accept("a"))
  }

  test("CPS: ab ! ab") {
    val regexParser:RegexParser = new RegexParser()
    val parser:Parser[Regex] = regexParser.parser()

    val re:Regex = parser.parse("ab")

    val cps:CPSMatcher = new CPSMatcher(re)
    assert(cps.accept("ab"))
  }

 test("CPS: a ! a|b") {
    val regexParser:RegexParser = new RegexParser()
    val parser:Parser[Regex] = regexParser.parser()

    val re:Regex = parser.parse("a|b")

    val cps:CPSMatcher = new CPSMatcher(re)
    assert(cps.accept("a"))
  }

 test("CPS: \"\" ! a|b") {
    val regexParser:RegexParser = new RegexParser()
    val parser:Parser[Regex] = regexParser.parser()

    val re:Regex = parser.parse("a|b")

    val cps:CPSMatcher = new CPSMatcher(re)
    assert(!cps.accept(""))
  }

  test("CPS: a ! b|a") {
    val regexParser:RegexParser = new RegexParser()
    val parser:Parser[Regex] = regexParser.parser()

    val re:Regex = parser.parse("b|a")

    val cps:CPSMatcher = new CPSMatcher(re)
    assert(cps.accept("a"))
  }

  test("CPS: a ! a*") {
    val regexParser:RegexParser = new RegexParser()
    val parser:Parser[Regex] = regexParser.parser()

    val re:Regex = parser.parse("a*")

    val cps:CPSMatcher = new CPSMatcher(re)
    assert(cps.accept("a"))
  }

  test("CPS: \"\" ! a*") {
    val regexParser:RegexParser = new RegexParser()
    val parser:Parser[Regex] = regexParser.parser()

    val re:Regex = parser.parse("a*")

    val cps:CPSMatcher = new CPSMatcher(re)
    assert(cps.accept(""))
  }

  test("CPS: ab ! a*b") {
    val regexParser:RegexParser = new RegexParser()
    val parser:Parser[Regex] = regexParser.parser()

    val re:Regex = parser.parse("a*b")

    val cps:CPSMatcher = new CPSMatcher(re)
    assert(cps.accept("ab"))
  }
}

@RunWith(classOf[JUnitRunner])
class AnyRegexTest extends FunSuite { 
    test("nfa(.+abc.*, 123abc123)") {
        val regexParser:RegexParser = new RegexParser()
        val parser:Parser[Regex] = regexParser.parser()

        val re:Regex = parser.parse(".+abc.*")

        val nfa:NRegexMatcher = new NRegexMatcher(re)

        assert(nfa.accept("123abc123"))
        assert(!nfa.accept("abc123"))
        assert(nfa.accept("123abc"))
    }
}
