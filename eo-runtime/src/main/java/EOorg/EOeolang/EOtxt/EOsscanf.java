/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2022 Yegor Bugayenko
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

package EOorg.EOeolang.EOtxt;

import EOorg.EOeolang.EOerror;
import org.eolang.AtComposite;
import org.eolang.AtFree;
import org.eolang.Data;
import org.eolang.Param;
import org.eolang.PhDefault;
import org.eolang.PhWith;
import org.eolang.Phi;
import org.eolang.XmirObject;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

/**
 * sscanf.
 *
 * @since 0.23
 */
@XmirObject(oname = "sscanf")
public class EOsscanf extends PhDefault {

    public EOsscanf(final Phi sigma) {
        super(sigma);
        this.add("format", new AtFree());
        this.add("read", new AtFree());
        this.add("φ", new AtComposite(this, rho -> {
            final String format = new Param(rho, "format").strong(String.class);
            final String read = new Param(rho, "read").strong(String.class);
            Phi phi;
            final List<Phi> buffer = new ArrayList<>();
            try (Scanner fsc = new Scanner(format);
            Scanner rsc = new Scanner(read)) {
                while (fsc.hasNext() && rsc.hasNext()) {
                    String pattern = fsc.next();
                    String val = rsc.next();
                    if (pattern.startsWith(String.valueOf(Conversion.PERCENT_SIGN))) {
                        final char c = pattern.charAt(1);
                        if (Conversion.isValid(c)) {
                            if (Conversion.isString(c) || Conversion.isCharacter(c)) {
                                buffer.add(new Data.ToPhi(val));
                            } else if (Conversion.isInteger(c)) {
                                buffer.add(new Data.ToPhi(Long.parseLong(val)));
                            } else if (Conversion.isFloat(c)) {
                                buffer.add(new Data.ToPhi(Double.parseDouble(val)));
                            } else if (Conversion.isBoolean(c)) {
                                buffer.add(new Data.ToPhi(Boolean.parseBoolean(val)));
                            } else {
                                return new PhWith(
                                    new EOerror(Phi.Φ), "msg",
                                    new Data.ToPhi(
                                        String.format("Format pattern not supported yet: %s", pattern)
                                    )
                                );
                            }
                        } else {
                            return new PhWith(
                                new EOerror(Phi.Φ), "msg",
                                new Data.ToPhi(
                                    String.format("Can't recognize format pattern: %s", pattern)
                                )
                            );
                        }
                    }
                }
            }
            phi = new Data.ToPhi(buffer.toArray(new Phi[0]));
            return phi;
        }));
    }

    private static class Conversion {
        // Byte, Short, Integer, Long, BigInteger
        // (and associated primitives due to autoboxing)
        static final char DECIMAL_INTEGER     = 'd';
        static final char OCTAL_INTEGER       = 'o';
        static final char HEXADECIMAL_INTEGER = 'x';
        static final char HEXADECIMAL_INTEGER_UPPER = 'X';

        // Float, Double, BigDecimal
        // (and associated primitives due to autoboxing)
        static final char SCIENTIFIC          = 'e';
        static final char SCIENTIFIC_UPPER    = 'E';
        static final char GENERAL             = 'g';
        static final char GENERAL_UPPER       = 'G';
        static final char DECIMAL_FLOAT       = 'f';
        static final char HEXADECIMAL_FLOAT   = 'a';
        static final char HEXADECIMAL_FLOAT_UPPER = 'A';

        // Character, Byte, Short, Integer
        // (and associated primitives due to autoboxing)
        static final char CHARACTER           = 'c';
        static final char CHARACTER_UPPER     = 'C';

        // java.util.Date, java.util.Calendar, long
        static final char DATE_TIME           = 't';
        static final char DATE_TIME_UPPER     = 'T';

        // if (arg.TYPE != boolean) return boolean
        // if (arg != null) return true; else return false;
        static final char BOOLEAN             = 'b';
        static final char BOOLEAN_UPPER       = 'B';
        // if (arg instanceof Formattable) arg.formatTo()
        // else arg.toString();
        static final char STRING              = 's';
        static final char STRING_UPPER        = 'S';
        // arg.hashCode()
        static final char HASHCODE            = 'h';
        static final char HASHCODE_UPPER      = 'H';

        static final char LINE_SEPARATOR      = 'n';
        static final char PERCENT_SIGN        = '%';

        static boolean isValid(char c) {
            switch (c) {
                case BOOLEAN:
                case BOOLEAN_UPPER:
                case STRING:
                case STRING_UPPER:
                case HASHCODE:
                case HASHCODE_UPPER:
                case CHARACTER:
                case CHARACTER_UPPER:
                case DECIMAL_INTEGER:
                case OCTAL_INTEGER:
                case HEXADECIMAL_INTEGER:
                case HEXADECIMAL_INTEGER_UPPER:
                case SCIENTIFIC:
                case SCIENTIFIC_UPPER:
                case GENERAL:
                case GENERAL_UPPER:
                case DECIMAL_FLOAT:
                case HEXADECIMAL_FLOAT:
                case HEXADECIMAL_FLOAT_UPPER:
                case LINE_SEPARATOR:
                case PERCENT_SIGN: return true;
                default: return false;
            }
        }

        // Returns true iff the Conversion is applicable to all objects.
        static boolean isGeneral(char c) {
            switch (c) {
                case BOOLEAN:
                case BOOLEAN_UPPER:
                case STRING:
                case STRING_UPPER:
                case HASHCODE:
                case HASHCODE_UPPER: return true;
                default: return false;
            }
        }

        // Returns true iff the Conversion is applicable to all objects.
        static boolean isString(char c) {
            switch (c) {
                case STRING:
                case STRING_UPPER: return true;
                default: return false;
            }
        }

        // Returns true iff the Conversion is applicable to all objects.
        static boolean isBoolean(char c) {
            switch (c) {
                case BOOLEAN:
                case BOOLEAN_UPPER: return true;
                default: return false;
            }
        }

        // Returns true iff the Conversion is applicable to character.
        static boolean isCharacter(char c) {
            switch (c) {
                case CHARACTER:
                case CHARACTER_UPPER: return true;
                default: return false;
            }
        }

        // Returns true iff the Conversion is an integer type.
        static boolean isInteger(char c) {
            switch (c) {
                case DECIMAL_INTEGER:
                case OCTAL_INTEGER:
                case HEXADECIMAL_INTEGER:
                case HEXADECIMAL_INTEGER_UPPER: return true;
                default: return false;
            }
        }

        // Returns true iff the Conversion is a floating-point type.
        static boolean isFloat(char c) {
            switch (c) {
                case SCIENTIFIC:
                case SCIENTIFIC_UPPER:
                case GENERAL:
                case GENERAL_UPPER:
                case DECIMAL_FLOAT:
                case HEXADECIMAL_FLOAT:
                case HEXADECIMAL_FLOAT_UPPER: return true;
                default: return false;
            }
        }

        // Returns true iff the Conversion does not require an argument
        static boolean isText(char c) {
            switch (c) {
                case LINE_SEPARATOR:
                case PERCENT_SIGN: return true;
                default: return false;
            }
        }
    }
}
