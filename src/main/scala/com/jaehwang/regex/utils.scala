package com.jaehwang.regex

object utils {
    def destructString(s:String):Option[(Char,String)] =     
        if(s==null || s.length() < 1) {
            None
        } else {
            val c = s.charAt(0) 
            val tl = if (s.length()>0) s.substring(1) else null
            Some(c,tl)
        }
} 
