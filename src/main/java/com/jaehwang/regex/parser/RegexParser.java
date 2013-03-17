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
package com.jaehwang.regex.parser;

import org.codehaus.jparsec.OperatorTable;
import org.codehaus.jparsec.Parser.Reference;
import org.codehaus.jparsec.Parser;
import org.codehaus.jparsec.Parsers;
import org.codehaus.jparsec.Scanners;
import org.codehaus.jparsec.Terminals;
import org.codehaus.jparsec.functors.Binary;
import org.codehaus.jparsec.functors.Map;
import org.codehaus.jparsec.functors.Unary;
import org.codehaus.jparsec.pattern.CharPredicate;

import com.jaehwang.regex.ast.Regex;
import com.jaehwang.regex.ast.CharRegex;
import com.jaehwang.regex.ast.PlusRegex;
import com.jaehwang.regex.ast.StarRegex;
import com.jaehwang.regex.ast.TimesRegex;
import com.jaehwang.regex.ast.AnyRegex;
import com.jaehwang.regex.ast.EpsilonRegex;

/** Regular expression parser.
 * 
 * We could not help but created Java code because of lazy() method call.
 * In Scala source code, lazy is a reserved word.
 * 
 * @author Jaehwang Kim (kim.jaehwang@gmail.com)
 */
public class RegexParser {
    
    enum BinaryOperator implements Binary<Regex> {
        PLUS {
            public Regex map(Regex a, Regex b) {
                return new PlusRegex(a,b);
            }
        },
        TIMES {
            public Regex map(Regex a, Regex b) {
                return new TimesRegex(a,b);
            }
        }
    }

    enum UnaryOperator implements Unary<Regex> {
        STAR {
            public Regex map(Regex a) {
                return new StarRegex(a);
            }
        },
        STARPLUS {
            public Regex map(Regex a) {
                return new TimesRegex(a, new StarRegex(a));
            }
        },
        ONEZERO {
            public Regex map(Regex a) {
                return new PlusRegex(a, new EpsilonRegex());
            }
        }
    }

    private final Terminals OPERATORS = 
                    Terminals.operators("|", "*", "+", "?", "(", ")");

    final Parser<BinaryOperator> WHITESPACE_TIMES = 
                    term("|", "*", "+", "?").not().retn(BinaryOperator.TIMES);

    Parser<String> escape = 
        Parsers.sequence(Scanners.string("\\"), 
                Scanners.isChar(new CharPredicate(){
                        //@Override
                        public boolean isChar(char c) {
                            //TODO: operator, '.' 등만 escape하도록 수정 필요.
                            return true;
                        }
                     }).source()).source();

    // TODO: Are the type castings the best solution?
    final Parser<?> TOKENIZER = Parsers.or(
            OPERATORS.tokenizer(),
            (Parser<?>)escape,
            (Parser<?>)Scanners.isChar(new CharPredicate(){
                //@Override
                public boolean isChar(char c) {
                    return true;//Character.isLetterOrDigit(c);
                }
            }).source());
    
    final Parser<Void> IGNORED = Parsers.or(Scanners.JAVA_LINE_COMMENT, 
                                            Scanners.JAVA_BLOCK_COMMENT, 
                                            Scanners.WHITESPACES).skipMany();
    
    Parser<?> term(String... names) {
        return OPERATORS.token(names);
    }
    
    <T> Parser<T> op(String name, T value) {
        return term(name).retn(value);
    }

    /*
     * re -> t + t | t x t | t*
     * t -> c | ( re )
     * 
     */
    public Parser<Regex> parser() 
    {
        Reference<Regex> ref = Parser.newReference();
        
        Parser<Regex> factor = ref.lazy().between(term("("), term(")"))
            .or(Terminals.StringLiteral.PARSER.map(new Map<String, Regex>() {
                @Override
                public Regex map(String from) {
                    if(from.startsWith("\\")) {
                        char c = from.charAt(1);
                        // TODO: we have to handle more special characters.
                        if(c == 'n') 
                            return new CharRegex('\n');
                        else if(c == 't') 
                            return new CharRegex('\t');
                        else
                            return new CharRegex(c);
                    } else {
                        char c = from.charAt(0);
                        if(c=='.') {
                            return new AnyRegex();
                        } else {
                            return new CharRegex(c);
                        }
                    }
                }
            }));
        
        Parser<Regex> parser = new OperatorTable<Regex>()
            .infixl(op("|",BinaryOperator.PLUS), 10)
            .infixl(WHITESPACE_TIMES,20)
            .postfix(op("*", UnaryOperator.STAR), 30)
            .postfix(op("+", UnaryOperator.STARPLUS), 30)
            .postfix(op("?", UnaryOperator.ONEZERO), 30)
            .build(factor);        
       
        ref.set(parser); 
        
        return parser.from(TOKENIZER, IGNORED);
    }
}
