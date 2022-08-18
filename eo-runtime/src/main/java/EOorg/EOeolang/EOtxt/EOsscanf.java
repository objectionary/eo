/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2022 Objectionary.com
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

/*
 * @checkstyle PackageNameCheck (4 lines)
 */
package EOorg.EOeolang.EOtxt;

import java.util.ArrayList;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Scanner;
import org.eolang.AtComposite;
import org.eolang.AtFree;
import org.eolang.Data;
import org.eolang.ExFailure;
import org.eolang.Param;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.XmirObject;

/**
 * Sscanf.
 *
 * @since 0.23
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "sscanf")
public class EOsscanf extends PhDefault {

    /**
     * Ctor.
     * @param sigma Sigma
     * @checkstyle CyclomaticComplexityCheck (75 lines)
     * @checkstyle NestedIfDepthCheck (75 lines)
     */
    public EOsscanf(final Phi sigma) {
        super(sigma);
        this.add("format", new AtFree());
        this.add("read", new AtFree());
        this.add(
            "φ",
            new AtComposite(
                this,
                rho -> {
                    final String format = new Param(rho, "format").strong(String.class);
                    final String read = new Param(rho, "read").strong(String.class);
                    final List<Phi> buffer = new ArrayList<>(0);
                    try (
                        Scanner fsc = new Scanner(format);
                        Scanner rsc = new Scanner(read)
                    ) {
                        while (fsc.hasNext() && rsc.hasNext()) {
                            final String pattern = fsc.next();
                            String val = rsc.next();
                            final boolean valid =
                                pattern.contains(String.valueOf(Conversion.PERCENT_SIGN))
                                && pattern.length() > 1;
                            if (valid) {
                                final int start = pattern.indexOf(Conversion.PERCENT_SIGN);
                                final char chr = pattern.charAt(start + 1);
                                if (!Conversion.isValid(chr)) {
                                    throw new ExFailure(
                                        "Can't recognize format pattern: %s",
                                        pattern
                                    );
                                }
                                if (pattern.length() > 2) {
                                    final int end;
                                    if (start + 1 == pattern.length() - 1) {
                                        end = 0;
                                    } else {
                                        end = pattern.length() - (start + 2);
                                    }
                                    val = val.substring(start, val.length() - end);
                                }
                                if (Conversion.isString(chr) || Conversion.isCharacter(chr)) {
                                    buffer.add(new Data.ToPhi(val));
                                } else if (Conversion.isInteger(chr)) {
                                    buffer.add(new Data.ToPhi(Long.parseLong(val)));
                                } else if (Conversion.isFloat(chr)) {
                                    buffer.add(new Data.ToPhi(Double.parseDouble(val)));
                                } else if (Conversion.isBoolean(chr)) {
                                    buffer.add(new Data.ToPhi(Boolean.parseBoolean(val)));
                                } else {
                                    throw new ExFailure(
                                        "Format pattern not supported yet: %s",
                                        pattern
                                    );
                                }
                            }
                        }
                    } catch (final IllegalArgumentException
                        | NullPointerException | NoSuchElementException ex) {
                        throw new ExFailure(ex.getMessage());
                    }
                    return new Data.ToPhi(buffer.toArray(new Phi[0]));
                }
            )
        );
    }

    /**
     * Format conversion.
     * @since 0.23
     * @checkstyle JavadocVariableCheck (70 lines)
     * @checkstyle CyclomaticComplexityCheck (75 lines)
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
         * @param character Char to validate
         * @return True if valid char, otherwise false
         */
        static boolean isValid(final char character) {
            final boolean result;
            switch (character) {
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
                case PERCENT_SIGN:
                    result = true;
                    break;
                default:
                    result = false;
            }
            return result;
        }

        /**
         * Check for object.
         * @param character Char to check
         * @return True iff the Conversion is applicable to all objects
         */
        static boolean isGeneral(final char character) {
            final boolean result;
            switch (character) {
                case BOOLEAN:
                case BOOLEAN_UPPER:
                case STRING:
                case STRING_UPPER:
                case HASHCODE:
                case HASHCODE_UPPER:
                    result = true;
                    break;
                default:
                    result = false;
            }
            return result;
        }

        /**
         * Check for string.
         * @param character Char to check
         * @return True iff the Conversion is applicable to string
         */
        static boolean isString(final char character) {
            final boolean result;
            switch (character) {
                case STRING:
                case STRING_UPPER:
                    result = true;
                    break;
                default:
                    result = false;
            }
            return result;
        }

        /**
         * Check for boolean.
         * @param character Char to check
         * @return True iff the Conversion is applicable to boolean
         */
        static boolean isBoolean(final char character) {
            final boolean result;
            switch (character) {
                case BOOLEAN:
                case BOOLEAN_UPPER:
                    result = true;
                    break;
                default:
                    result = false;
            }
            return result;
        }

        /**
         * Check for character.
         * @param character Char to check
         * @return True iff the Conversion is applicable to character
         */
        static boolean isCharacter(final char character) {
            final boolean result;
            switch (character) {
                case CHARACTER:
                case CHARACTER_UPPER:
                    result = true;
                    break;
                default:
                    result = false;
            }
            return result;
        }

        /**
         * Check for integer.
         * @param character Char to check
         * @return True iff the Conversion is applicable to integer
         */
        static boolean isInteger(final char character) {
            final boolean result;
            switch (character) {
                case DECIMAL_INTEGER:
                case OCTAL_INTEGER:
                case HEXADECIMAL_INTEGER:
                case HEXADECIMAL_INTEGER_UPPER:
                    result = true;
                    break;
                default:
                    result = false;
            }
            return result;
        }

        /**
         * Check for floating-point.
         * @param character Char to check
         * @return True iff the Conversion is applicable to floating-point
         */
        static boolean isFloat(final char character) {
            final boolean result;
            switch (character) {
                case SCIENTIFIC:
                case SCIENTIFIC_UPPER:
                case GENERAL:
                case GENERAL_UPPER:
                case DECIMAL_FLOAT:
                case HEXADECIMAL_FLOAT:
                case HEXADECIMAL_FLOAT_UPPER:
                    result = true;
                    break;
                default:
                    result = false;
            }
            return result;
        }

        /**
         * Check for text.
         * @param character Char to check
         * @return True iff the Conversion does not require an argument
         */
        static boolean isText(final char character) {
            final boolean result;
            switch (character) {
                case LINE_SEPARATOR:
                case PERCENT_SIGN:
                    result = true;
                    break;
                default:
                    result = false;
            }
            return result;
        }
    }
}
