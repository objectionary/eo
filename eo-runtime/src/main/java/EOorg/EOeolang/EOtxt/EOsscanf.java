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
import java.util.ArrayList;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Scanner;
import org.eolang.AtComposite;
import org.eolang.AtFree;
import org.eolang.Data;
import org.eolang.Param;
import org.eolang.PhDefault;
import org.eolang.PhWith;
import org.eolang.Phi;
import org.eolang.XmirObject;

/**
 * Sscanf.
 *
 * @since 0.23
 */
@XmirObject(oname = "sscanf")
public class EOsscanf extends PhDefault {

    /**
     * Ctor.
     * @param sigma Sigma
     */
    public EOsscanf(final Phi sigma) {
        super(sigma);
        this.add("format", new AtFree());
        this.add("read", new AtFree());
        this.add("φ", new AtComposite(this, rho -> {
            final String format = new Param(rho, "format").strong(String.class);
            final String read = new Param(rho, "read").strong(String.class);
            final List<Phi> buffer = new ArrayList<>();
            try (Scanner fsc = new Scanner(format);
                 Scanner rsc = new Scanner(read)
            ) {
                while (fsc.hasNext() && rsc.hasNext()) {
                    String pattern = fsc.next();
                    String val = rsc.next();
                    boolean valid = pattern.contains(String.valueOf(Conversion.PERCENT_SIGN))
                        && pattern.length() > 1;
                    if (valid) {
                        int start = pattern.indexOf(Conversion.PERCENT_SIGN);
                        final char c = pattern.charAt(start + 1);
                        if (Conversion.isValid(c)) {
                            if (pattern.length() > 2) {
                                int end = start + 1 == pattern.length() - 1
                                    ? 0
                                    : pattern.length() - (start + 2);
                                val = val.substring(start, val.length() - end);
                            }
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
                                        String.format(
                                            "Format pattern not supported yet: %s",
                                            pattern
                                        )
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
            } catch (IllegalArgumentException | NullPointerException | NoSuchElementException ex) {
                return new PhWith(
                    new EOerror(Phi.Φ), "msg",
                    new Data.ToPhi(ex.getMessage())
                );
            }
            return new Data.ToPhi(buffer.toArray(new Phi[0]));
        }));
    }

    /**
     * Format conversion.
     * @since 0.23
     * @checkstyle JavadocVariableCheck (40 lines)
     */
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

        /**
         * Valiate char.
         * @param c Char to validate
         * @return True if valid char, otherwise false
         */
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

        /**
         * Check for object.
         * @param c Char to check
         * @return True iff the Conversion is applicable to all objects
         */
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

        /**
         * Check for string.
         * @param c Char to check
         * @return True iff the Conversion is applicable to string
         */
        static boolean isString(char c) {
            switch (c) {
                case STRING:
                case STRING_UPPER: return true;
                default: return false;
            }
        }

        /**
         * Check for boolean.
         * @param c Char to check
         * @return True iff the Conversion is applicable to boolean
         */
        static boolean isBoolean(char c) {
            switch (c) {
                case BOOLEAN:
                case BOOLEAN_UPPER: return true;
                default: return false;
            }
        }

        /**
         * Check for character.
         * @param c Char to check
         * @return True iff the Conversion is applicable to character
         */
        static boolean isCharacter(char c) {
            switch (c) {
                case CHARACTER:
                case CHARACTER_UPPER: return true;
                default: return false;
            }
        }

        /**
         * Check for integer.
         * @param c Char to check
         * @return True iff the Conversion is applicable to integer
         */
        static boolean isInteger(char c) {
            switch (c) {
                case DECIMAL_INTEGER:
                case OCTAL_INTEGER:
                case HEXADECIMAL_INTEGER:
                case HEXADECIMAL_INTEGER_UPPER: return true;
                default: return false;
            }
        }

        /**
         * Check for floating-point.
         * @param c Char to check
         * @return True iff the Conversion is applicable to floating-point
         */
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

        /**
         * Check for text.
         * @param c Char to check
         * @return True iff the Conversion does not require an argument
         */
        static boolean isText(char c) {
            switch (c) {
                case LINE_SEPARATOR:
                case PERCENT_SIGN: return true;
                default: return false;
            }
        }
    }
}
