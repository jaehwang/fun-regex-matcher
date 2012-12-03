package com.jaehwang.regex.ast

abstract class Regex
case class PlusRegex(re1:Regex, re2:Regex) extends Regex
case class TimesRegex(re1:Regex, re2:Regex) extends Regex
case class StarRegex(re:Regex) extends Regex
case class CharRegex(c:Char) extends Regex
case class AnyRegex() extends Regex
case class EmptyRegex() extends Regex
